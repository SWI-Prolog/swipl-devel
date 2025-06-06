/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2005-2024, University of Amsterdam
                              VU University Amsterdam
                              CWI, Amsterdam
                              SWI-Prolog Solutions b.v.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(prolog_clause,
          [ clause_info/4,              % +ClauseRef, -File, -TermPos, -VarNames
            clause_info/5,              % +ClauseRef, -File, -TermPos, -VarNames,
                                        % +Options
            initialization_layout/4,    % +SourceLoc, +Goal, -Term, -TermPos
            predicate_name/2,           % +Head, -Name
            clause_name/2               % +ClauseRef, -Name
          ]).
:- use_module(library(debug),[debugging/1,debug/3]).
:- autoload(library(listing),[portray_clause/1]).
:- autoload(library(lists),[append/3]).
:- autoload(library(occurs),[sub_term/2]).
:- autoload(library(option),[option/3]).
:- autoload(library(prolog_source),[read_source_term_at_location/3]).


:- public                               % called from library(trace/clause)
    unify_term/2,
    make_varnames/5,
    do_make_varnames/3.

:- multifile
    unify_goal/5,                   % +Read, +Decomp, +M, +Pos, -Pos
    unify_clause_hook/5,
    make_varnames_hook/5,
    open_source/2.                  % +Input, -Stream

:- predicate_options(prolog_clause:clause_info/5, 5,
                     [ head(-any),
                       body(-any),
                       variable_names(-list)
                     ]).

/** <module> Get detailed source-information about a clause

This module started life as part of the   GUI tracer. As it is generally
useful for debugging  purposes  it  has   moved  to  the  general Prolog
library.

The tracer library library(trace/clause) adds   caching and dealing with
dynamic predicates using listing to  XPCE   objects  to  this. Note that
clause_info/4 as below can be slow.
*/

%!  clause_info(+ClauseRef, -File, -TermPos, -VarOffsets) is semidet.
%!  clause_info(+ClauseRef, -File, -TermPos, -VarOffsets, +Options) is semidet.
%
%   Fetches source information for the  given   clause.  File is the
%   file from which the clause  was   loaded.  TermPos describes the
%   source layout in a format   compatible  to the subterm_positions
%   option  of  read_term/2.  VarOffsets  provides   access  to  the
%   variable allocation in a stack-frame.   See  make_varnames/5 for
%   details.
%
%   Note that positions are  _|character   positions|_,  i.e., _not_
%   bytes. Line endings count as a   single character, regardless of
%   whether the actual ending is =|\n|= or =|\r\n|_.
%
%   Defined options are:
%
%     - variable_names(-Names)
%       Unify Names with the variable names list (Name=Var) as
%       returned by read_term/3.  This argument is intended for
%       reporting source locations and refactoring based on
%       analysis of the compiled code.
%     - head(-Head)
%     - body(-Body)
%       Get the head and body as terms.   This is similar to
%       clause/3, but a seperate call would break the variable
%       identity.

clause_info(ClauseRef, File, TermPos, NameOffset) :-
    clause_info(ClauseRef, File, TermPos, NameOffset, []).

clause_info(ClauseRef, File, TermPos, NameOffset, Options) :-
    (   debugging(clause_info)
    ->  clause_name(ClauseRef, Name),
        debug(clause_info, 'clause_info(~w) (~w)... ',
              [ClauseRef, Name])
    ;   true
    ),
    clause_property(ClauseRef, file(File)),
    File \== user,                  % loaded using ?- [user].
    '$clause'(Head0, Body, ClauseRef, VarOffset),
    option(head(Head0), Options, _),
    option(body(Body), Options, _),
    (   module_property(Module, file(File))
    ->  true
    ;   strip_module(user:Head0, Module, _)
    ),
    unqualify(Head0, Module, Head),
    (   Body == true
    ->  DecompiledClause = Head
    ;   DecompiledClause = (Head :- Body)
    ),
    clause_property(ClauseRef, line_count(LineNo)),
    debug(clause_info, 'from ~w:~d ... ', [File, LineNo]),
    read_term_at_line(File, LineNo, Module, Clause, TermPos0, VarNames),
    option(variable_names(VarNames), Options, _),
    debug(clause_info, 'read ...', []),
    unify_clause(Clause, DecompiledClause, Module, TermPos0, TermPos),
    debug(clause_info, 'unified ...', []),
    make_varnames(Clause, DecompiledClause, VarOffset, VarNames, NameOffset),
    debug(clause_info, 'got names~n', []),
    !.

unqualify(Module:Head, Module, Head) :-
    !.
unqualify(Head, _, Head).


%!  unify_term(+T1, +T2)
%
%   Unify the two terms, where T2 is created by writing the term and
%   reading it back in, but  be   aware  that  rounding problems may
%   cause floating point numbers not to  unify. Also, if the initial
%   term has a string object, it is written   as "..." and read as a
%   code-list. We compensate for that.
%
%   NOTE: Called directly from  library(trace/clause)   for  the GUI
%   tracer.

unify_term(X, X) :- !.
unify_term(X1, X2) :-
    compound(X1),
    compound(X2),
    functor(X1, F, Arity),
    functor(X2, F, Arity),
    !,
    unify_args(0, Arity, X1, X2).
unify_term(X, Y) :-
    float(X), float(Y),
    !.
unify_term(X, '$BLOB'(_)) :-
    blob(X, _),
    \+ atom(X).
unify_term(X, Y) :-
    string(X),
    is_list(Y),
    string_codes(X, Y),
    !.
unify_term(_, Y) :-
    Y == '...',
    !.                          % elipses left by max_depth
unify_term(_:X, Y) :-
    unify_term(X, Y),
    !.
unify_term(X, _:Y) :-
    unify_term(X, Y),
    !.
unify_term(X, Y) :-
    format('[INTERNAL ERROR: Diff:~n'),
    portray_clause(X),
    format('~N*** <->~n'),
    portray_clause(Y),
    break.

unify_args(N, N, _, _) :- !.
unify_args(I, Arity, T1, T2) :-
    A is I + 1,
    arg(A, T1, A1),
    arg(A, T2, A2),
    unify_term(A1, A2),
    unify_args(A, Arity, T1, T2).


%!  read_term_at_line(+File, +Line, +Module,
%!                    -Clause, -TermPos, -VarNames) is semidet.
%
%   Read a term from File at Line.

read_term_at_line(File, Line, Module, Clause, TermPos, VarNames) :-
    setup_call_cleanup(
        '$push_input_context'(clause_info),
        read_term_at_line_2(File, Line, Module, Clause, TermPos, VarNames),
        '$pop_input_context').

read_term_at_line_2(File, Line, Module, Clause, TermPos, VarNames) :-
    catch(try_open_source(File, In), error(_,_), fail),
    set_stream(In, newline(detect)),
    call_cleanup(
        read_source_term_at_location(
            In, Clause,
            [ line(Line),
              module(Module),
              subterm_positions(TermPos),
              variable_names(VarNames)
            ]),
        close(In)).

%!  open_source(+File, -Stream) is semidet.
%
%   Hook into clause_info/5 that opens the stream holding the source
%   for a specific clause. Thus, the query must succeed. The default
%   implementation calls open/3 on the `File` property.
%
%     ==
%     clause_property(ClauseRef, file(File)),
%     prolog_clause:open_source(File, Stream)
%     ==

:- public try_open_source/2.            % used by library(prolog_breakpoints).

try_open_source(File, In) :-
    open_source(File, In),
    !.
try_open_source(File, In) :-
    open(File, read, In, [reposition(true)]).


%!  make_varnames(+ReadClause, +DecompiledClause,
%!                +Offsets, +Names, -Term) is det.
%
%   Create a Term varnames(...) where each argument contains the name
%   of the variable at that offset.  If the read Clause is a DCG rule,
%   name the two last arguments <DCG_list> and <DCG_tail>
%
%   This    predicate    calles     the      multifile     predicate
%   make_varnames_hook/5 with the same arguments   to allow for user
%   extensions. Extending this predicate  is   needed  if a compiler
%   adds additional arguments to the clause   head that must be made
%   visible in the GUI tracer.
%
%   @param Offsets  List of Offset=Var
%   @param Names    List of Name=Var

make_varnames(ReadClause, DecompiledClause, Offsets, Names, Term) :-
    make_varnames_hook(ReadClause, DecompiledClause, Offsets, Names, Term),
    !.
make_varnames(ReadClause, _, Offsets, Names, Bindings) :-
    dcg_head(ReadClause, Head),
    !,
    functor(Head, _, Arity),
    In is Arity,
    memberchk(In=IVar, Offsets),
    Names1 = ['<DCG_list>'=IVar|Names],
    Out is Arity + 1,
    memberchk(Out=OVar, Offsets),
    Names2 = ['<DCG_tail>'=OVar|Names1],
    make_varnames(xx, xx, Offsets, Names2, Bindings).
make_varnames(_, _, Offsets, Names, Bindings) :-
    length(Offsets, L),
    functor(Bindings, varnames, L),
    do_make_varnames(Offsets, Names, Bindings).

dcg_head((Head,_ --> _Body), Head).
dcg_head((Head   --> _Body), Head).
dcg_head((Head,_ ==> _Body), Head).
dcg_head((Head   ==> _Body), Head).

do_make_varnames([], _, _).
do_make_varnames([N=Var|TO], Names, Bindings) :-
    (   find_varname(Var, Names, Name)
    ->  true
    ;   Name = '_'
    ),
    AN is N + 1,
    arg(AN, Bindings, Name),
    do_make_varnames(TO, Names, Bindings).

find_varname(Var, [Name = TheVar|_], Name) :-
    Var == TheVar,
    !.
find_varname(Var, [_|T], Name) :-
    find_varname(Var, T, Name).

%!  unify_clause(+Read, +Decompiled, +Module, +ReadTermPos,
%!               -RecompiledTermPos).
%
%   What you read isn't always what goes into the database. The task
%   of this predicate is to establish  the relation between the term
%   read from the file and the result from decompiling the clause.
%
%   This predicate calls the multifile predicate unify_clause_hook/5
%   with the same arguments to support user extensions.
%
%   @arg Module is the source module that   was active when loading this
%   clause,  which  is  the  same  as  prolog_load_context/2  using  the
%   `module` context. If this cannot be established  it is the module to
%   which the clause itself is associated.   The argument may be used to
%   determine whether or not a specific user transformation is in scope.
%   See also term_expansion/2,4 and goal_expansion/2,4.
%
%   @tbd    This really must be  more   flexible,  dealing with much
%           more complex source-translations,  falling   back  to  a
%           heuristic method locating as much as possible.

unify_clause(Read, _, _, _, _) :-
    var(Read),
    !,
    fail.
unify_clause((RHead :- RBody), (CHead :- CBody), Module, TermPos1, TermPos) :-
    '$expand':f2_pos(TermPos1, HPos, BPos1,
                     TermPos2, HPos, BPos2),
    inlined_unification(RBody, CBody, RBody1, CBody1, RHead,
                        BPos1, BPos2),
    RBody1 \== RBody,
    !,
    unify_clause2((RHead :- RBody1), (CHead :- CBody1), Module,
                  TermPos2, TermPos).
unify_clause(Read, Decompiled, _, TermPos, TermPos) :-
    Read =@= Decompiled,
    !,
    Read = Decompiled.
unify_clause(Read, Decompiled, Module, TermPos0, TermPos) :-
    unify_clause_hook(Read, Decompiled, Module, TermPos0, TermPos),
    !.
                                        % XPCE send-methods
unify_clause(:->(Head, Body), (PlHead :- PlBody), M, TermPos0, TermPos) :-
    !,
    pce_method_clause(Head, Body, PlHead, PlBody, M, TermPos0, TermPos).
                                        % XPCE get-methods
unify_clause(:<-(Head, Body), (PlHead :- PlBody), M, TermPos0, TermPos) :-
    !,
    pce_method_clause(Head, Body, PlHead, PlBody, M, TermPos0, TermPos).
                                        % Unit test clauses
unify_clause((TH :- RBody), (CH :- !, CBody), Module, TP0, TP) :-
    plunit_source_head(TH),
    plunit_compiled_head(CH),
    !,
    TP0 = term_position(F,T,FF,FT,[HP,BP0]),
    ubody(RBody, CBody, Module, BP0, BP),
    TP  = term_position(F,T,FF,FT,[HP,term_position(0,0,0,0,[FF-FT,BP])]).
                                        % module:head :- body
unify_clause((Head :- Read),
             (Head :- _M:Compiled), Module, TermPos0, TermPos) :-
    unify_clause2((Head :- Read), (Head :- Compiled), Module, TermPos0, TermPos1),
    TermPos1 = term_position(TA,TZ,FA,FZ,[PH,PB]),
    TermPos  = term_position(TA,TZ,FA,FZ,
                             [ PH,
                               term_position(0,0,0,0,[0-0,PB])
                             ]).
                                        % DCG rules
unify_clause(Read, Compiled1, Module, TermPos0, TermPos) :-
    Read = (_ --> Terminal0, _),
    (   is_list(Terminal0)
    ->  Terminal = Terminal0
    ;   string(Terminal0)
    ->  string_codes(Terminal0, Terminal)
    ),
    ci_expand(Read, Compiled2, Module, TermPos0, TermPos1),
    (   dcg_unify_in_head(Compiled2, Compiled3)
    ->  true
    ;   Compiled2 = (DH :- _CBody),
        functor(DH, _, Arity),
        DArg is Arity - 1,
        append(Terminal, _Tail, List),
        arg(DArg, DH, List),
        Compiled3 = Compiled2
    ),
    TermPos1 = term_position(F,T,FF,FT,[ HP,
                                         term_position(_,_,_,_,[_,BP])
                                       ]),
    !,
    TermPos2 = term_position(F,T,FF,FT,[ HP, BP ]),
    match_module(Compiled3, Compiled1, Module, TermPos2, TermPos).
                                               % SSU rules
unify_clause((Head,RCond => Body), (CHead :- CCondAndBody), Module,
             term_position(F,T,FF,FT,
                           [ term_position(_,_,_,_,[HP,CP]),
                             BP
                           ]),
             TermPos) :-
    split_on_cut(CCondAndBody, CCond, CBody0),
    !,
    inlined_unification(RCond, CCond, RCond1, CCond1, Head, CP, CP1),
    TermPos1 = term_position(F,T,FF,FT, [HP, BP1]),
    BP2 = term_position(_,_,_,_, [FF-FT, BP]), % Represent (!, Body), placing
    (   CCond1 == true                         % ! at =>
    ->  BP1 = BP2,                             % Whole guard is inlined
        unify_clause2((Head :- !, Body), (CHead :- !, CBody0),
                      Module, TermPos1, TermPos)
    ;   mkconj_pos(RCond1, CP1, (!,Body), BP2, RBody, BP1),
        mkconj_npos(CCond1, (!,CBody0), CBody),
        unify_clause2((Head :- RBody), (CHead :- CBody),
                      Module, TermPos1, TermPos)
    ).
unify_clause((Head => Body), Compiled1, Module, TermPos0, TermPos) :-
    !,
    unify_clause2((Head :- Body), Compiled1, Module, TermPos0, TermPos).
unify_clause(Read, Compiled1, Module, TermPos0, TermPos) :-
    Read = (_ ==> _),
    ci_expand(Read, Compiled2, Module, TermPos0, TermPos1),
    Compiled2 \= (_ ==> _),
    !,
    unify_clause(Compiled2, Compiled1, Module, TermPos1, TermPos).
unify_clause(Read, Decompiled, Module, TermPos0, TermPos) :-
    unify_clause2(Read, Decompiled, Module, TermPos0, TermPos).

dcg_unify_in_head((Head :- L1=L2, Body), (Head :- Body)) :-
    functor(Head, _, Arity),
    DArg is Arity - 1,
    arg(DArg, Head, L0),
    L0 == L1,
    L1 = L2.

% mkconj, but also unify position info
mkconj_pos((A,B), term_position(F,T,FF,FT,[PA,PB]), Ex, ExPos, Code, Pos) =>
    Code = (A,B1),
    Pos = term_position(F,T,FF,FT,[PA,PB1]),
    mkconj_pos(B, PB, Ex, ExPos, B1, PB1).
mkconj_pos(Last, LastPos, Ex, ExPos, Code, Pos) =>
    Code = (Last,Ex),
    Pos = term_position(_,_,_,_,[LastPos,ExPos]).

% similar to mkconj, but we should __not__ optimize `true` away.
mkconj_npos((A,B), Ex, Code) =>
    Code = (A,B1),
    mkconj_npos(B, Ex, B1).
mkconj_npos(A, Ex, Code) =>
    Code = (A,Ex).

%!  unify_clause2(+Read, +Decompiled, +Module, +TermPosIn, -TermPosOut)
%
%   Stratified version to be used after the first match

unify_clause2(Read, Decompiled, _, TermPos, TermPos) :-
    Read =@= Decompiled,
    !,
    Read = Decompiled.
unify_clause2(Read, Compiled1, Module, TermPos0, TermPos) :-
    ci_expand(Read, Compiled2, Module, TermPos0, TermPos1),
    match_module(Compiled2, Compiled1, Module, TermPos1, TermPos),
    !.
unify_clause2(_, _, _, _, _) :-       % I don't know ...
    debug(clause_info, 'Could not unify clause', []),
    fail.

unify_clause_head(H1, H2) :-
    strip_module(H1, _, H),
    strip_module(H2, _, H).

plunit_source_head(test(_,_)) => true.
plunit_source_head(test(_)) => true.
plunit_source_head(_) => fail.

plunit_compiled_head(_:'unit body'(_, _)) => true.
plunit_compiled_head('unit body'(_, _)) => true.
plunit_compiled_head(_) => fail.

%!  inlined_unification(+BodyRead, +BodyCompiled,
%!                      -BodyReadOut, -BodyCompiledOut,
%!                      +HeadRead,
%!                      +BodyPosIn, -BodyPosOut) is det.

inlined_unification((V=T,RBody0), (CV=CT,CBody0),
                    RBody, CBody, RHead, BPos1, BPos),
    inlineable_head_var(RHead, V2),
    V == V2,
    (V=T) =@= (CV=CT) =>
    argpos(2, BPos1, BPos2),
    inlined_unification(RBody0, CBody0, RBody, CBody, RHead, BPos2, BPos).
inlined_unification((V=T), (CV=CT),
                    RBody, CBody, RHead, BPos1, BPos),
    inlineable_head_var(RHead, V2),
    V == V2,
    (V=T) =@= (CV=CT) =>
    RBody = true,
    CBody = true,
    argpos(2, BPos1, BPos).
inlined_unification((V=T,RBody0), CBody0,
                    RBody, CBody, RHead, BPos1, BPos),
    inlineable_head_var(RHead, V2),
    V == V2,
    \+ (CBody0 = (G1,_), G1 =@= (V=T)) =>
    argpos(2, BPos1, BPos2),
    inlined_unification(RBody0, CBody0, RBody, CBody, RHead, BPos2, BPos).
inlined_unification((V=_), true,
                    RBody, CBody, RHead, BPos1, BPos),
    inlineable_head_var(RHead, V2),
    V == V2 =>
    RBody = true,
    CBody = true,
    argpos(2, BPos1, BPos).
inlined_unification(RBody0, CBody0, RBody, CBody, _RHead,
                    BPos0, BPos) =>
    RBody = RBody0,
    BPos  = BPos0,
    CBody = CBody0.

%!  inlineable_head_var(+Head, -Var) is nondet
%
%   True when Var is a variable in  Head   that  may  be used for inline
%   unification. Currently we only inline direct arguments to the head.

inlineable_head_var(Head, Var) :-
    compound(Head),
    arg(_, Head, Var).

split_on_cut((Cond0,!,Body0), Cond, Body) =>
    Cond = Cond0,
    Body = Body0.
split_on_cut((!,Body0), Cond, Body) =>
    Cond = true,
    Body = Body0.
split_on_cut((A,B), Cond, Body) =>
    Cond = (A,Cond1),
    split_on_cut(B, Cond1, Body).
split_on_cut(_, _, _) =>
    fail.

ci_expand(Read, Compiled, Module, TermPos0, TermPos) :-
    catch(setup_call_cleanup(
              ( set_xref_flag(OldXRef),
                '$set_source_module'(Old, Module)
              ),
              expand_term(Read, TermPos0, Compiled, TermPos),
              ( '$set_source_module'(Old),
                set_prolog_flag(xref, OldXRef)
              )),
          E,
          expand_failed(E, Read)),
    compound(TermPos),                  % make sure somthing is filled.
    arg(1, TermPos, A1), nonvar(A1),
    arg(2, TermPos, A2), nonvar(A2).

set_xref_flag(Value) :-
    current_prolog_flag(xref, Value),
    !,
    set_prolog_flag(xref, true).
set_xref_flag(false) :-
    create_prolog_flag(xref, true, [type(boolean)]).

match_module((H1 :- B1), (H2 :- B2), Module, Pos0, Pos) :-
    !,
    unify_clause_head(H1, H2),
    unify_body(B1, B2, Module, Pos0, Pos).
match_module((H1 :- B1), H2, _Module, Pos0, Pos) :-
    B1 == true,
    unify_clause_head(H1, H2),
    Pos = Pos0,
    !.
match_module(H1, H2, _, Pos, Pos) :-    % deal with facts
    unify_clause_head(H1, H2).

%!  expand_failed(+Exception, +Term)
%
%   When debugging, indicate that expansion of the term failed.

expand_failed(E, Read) :-
    debugging(clause_info),
    message_to_string(E, Msg),
    debug(clause_info, 'Term-expand ~p failed: ~w', [Read, Msg]),
    fail.

%!  unify_body(+Read, +Decompiled, +Module, +Pos0, -Pos)
%
%   Deal with translations implied by the compiler.  For example,
%   compiling (a,b),c yields the same code as compiling a,b,c.
%
%   Pos0 and Pos still include the term-position of the head.

unify_body(B, C, _, Pos, Pos) :-
    B =@= C, B = C,
    does_not_dcg_after_binding(B, Pos),
    !.
unify_body(R, D, Module,
           term_position(F,T,FF,FT,[HP,BP0]),
           term_position(F,T,FF,FT,[HP,BP])) :-
    ubody(R, D, Module, BP0, BP).

%!  does_not_dcg_after_binding(+ReadBody, +ReadPos) is semidet.
%
%   True  if  ReadPos/ReadPos  does   not    contain   DCG   delayed
%   unifications.
%
%   @tbd    We should pass that we are in a DCG; if we are not there
%           is no reason for this test.

does_not_dcg_after_binding(B, Pos) :-
    \+ sub_term(brace_term_position(_,_,_), Pos),
    \+ (sub_term((Cut,_=_), B), Cut == !),
    !.


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Some remarks.

a --> { x, y, z }.
    This is translated into "(x,y),z), X=Y" by the DCG translator, after
    which the compiler creates "a(X,Y) :- x, y, z, X=Y".
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%!  unify_goal(+Read, +Decompiled, +Module,
%!             +TermPosRead, -TermPosDecompiled) is semidet.
%
%   This hook is called to  fix   up  source code manipulations that
%   result from goal expansions.

%!  ubody(+Read, +Decompiled, +Module, +TermPosRead, -TermPosForDecompiled)
%
%   @arg Read             Clause read _after_ expand_term/2
%   @arg Decompiled       Decompiled clause
%   @arg Module           Load module
%   @arg TermPosRead      Sub-term positions of source

ubody(B, DB, _, P, P) :-
    var(P),                        % TBD: Create compatible pos term?
    !,
    B = DB.
ubody(B, C, _, P, P) :-
    B =@= C, B = C,
    does_not_dcg_after_binding(B, P),
    !.
ubody(X0, X, M, parentheses_term_position(_, _, P0), P) :-
    !,
    ubody(X0, X, M, P0, P).
ubody(X, Y, _,                    % X = call(X)
      Pos,
      term_position(From, To, From, To, [Pos])) :-
    nonvar(Y),
    Y = call(X),
    !,
    arg(1, Pos, From),
    arg(2, Pos, To).
ubody(A, B, _, P1, P2) :-
    nonvar(A), A = (_=_),
    nonvar(B), B = (LB=RB),
    A =@= (RB=LB),
    !,
    P1 = term_position(F,T, FF,FT, [PL,PR]),
    P2 = term_position(F,T, FF,FT, [PR,PL]).
ubody(A, B, _, P1, P2) :-
    nonvar(A), A = (_==_),
    nonvar(B), B = (LB==RB),
    A =@= (RB==LB),
    !,
    P1 = term_position(F,T, FF,FT, [PL,PR]),
    P2 = term_position(F,T, FF,FT, [PR,PL]).
ubody(B, D, _, term_position(_,_,_,_,[_,RP]), TPOut) :-
    nonvar(B), B = M:R,
    ubody(R, D, M, RP, TPOut).
ubody(B, D, M, term_position(_,_,_,_,[RP0,RP1]), TPOut) :-
    nonvar(B), B = (B0,B1),
    (   maybe_optimized(B0),
        ubody(B1, D, M, RP1, TPOut)
    ->  true
    ;   maybe_optimized(B1),
        ubody(B0, D, M, RP0, TPOut)
    ),
    !.
ubody(B0, B, M,
      brace_term_position(F,T,A0),
      Pos) :-
    B0 = (_,_=_),
    !,
    T1 is T - 1,
    ubody(B0, B, M,
          term_position(F,T,
                        F,T,
                        [A0,T1-T]),
          Pos).
ubody(B0, B, M,
      brace_term_position(F,T,A0),
      term_position(F,T,F,T,[A])) :-
    !,
    ubody(B0, B, M, A0, A).
ubody(C0, C, M, P0, P) :-
    nonvar(C0), nonvar(C),
    C0 = (_,_), C = (_,_),
    !,
    conj(C0, P0, GL, PL),
    mkconj(C, M, P, GL, PL).
ubody(Read, Decompiled, Module, TermPosRead, TermPosDecompiled) :-
    unify_goal(Read, Decompiled, Module, TermPosRead, TermPosDecompiled),
    !.
ubody(X0, X, M,
      term_position(F,T,FF,TT,PA0),
      term_position(F,T,FF,TT,PA)) :-
    callable(X0),
    callable(X),
    meta(M, X0, S),
    !,
    X0 =.. [_|A0],
    X  =.. [_|A],
    S =.. [_|AS],
    ubody_list(A0, A, AS, M, PA0, PA).
ubody(X0, X, M,
      term_position(F,T,FF,TT,PA0),
      term_position(F,T,FF,TT,PA)) :-
    expand_goal(X0, X1, M, PA0, PA),
    X1 =@= X,
    X1 = X.

                                        % 5.7.X optimizations
ubody(_=_, true, _,                     % singleton = Any
      term_position(F,T,_FF,_TT,_PA),
      F-T) :- !.
ubody(_==_, fail, _,                    % singleton/firstvar == Any
      term_position(F,T,_FF,_TT,_PA),
      F-T) :- !.
ubody(A1=B1, B2=A2, _,                  % Term = Var --> Var = Term
      term_position(F,T,FF,TT,[PA1,PA2]),
      term_position(F,T,FF,TT,[PA2,PA1])) :-
    var(B1), var(B2),
    (A1==B1) =@= (B2==A2),
    !,
    A1 = A2, B1=B2.
ubody(A1==B1, B2==A2, _,                % const == Var --> Var == const
      term_position(F,T,FF,TT,[PA1,PA2]),
      term_position(F,T,FF,TT,[PA2,PA1])) :-
    var(B1), var(B2),
    (A1==B1) =@= (B2==A2),
    !,
    A1 = A2, B1=B2.
ubody(A is B - C, A is B + C2, _, Pos, Pos) :-
    integer(C),
    C2 =:= -C,
    !.

ubody_list([], [], [], _, [], []).
ubody_list([G0|T0], [G|T], [AS|ASL], M, [PA0|PAT0], [PA|PAT]) :-
    ubody_elem(AS, G0, G, M, PA0, PA),
    ubody_list(T0, T, ASL, M, PAT0, PAT).

ubody_elem(0, G0, G, M, PA0, PA) :-
    !,
    ubody(G0, G, M, PA0, PA).
ubody_elem(_, G, G, _, PA, PA).

%!  conj(+GoalTerm, +PositionTerm, -GoalList, -PositionList)
%
%   Turn a conjunctive body into a list   of  goals and their positions,
%   i.e., removing the positions of the (,)/2 terms.

conj(Goal, Pos, GoalList, PosList) :-
    conj(Goal, Pos, GoalList, [], PosList, []).

conj((A,B), term_position(_,_,_,_,[PA,PB]), GL, TG, PL, TP) :-
    !,
    conj(A, PA, GL, TGA, PL, TPA),
    conj(B, PB, TGA, TG, TPA, TP).
conj((A,B), brace_term_position(_,T,PA), GL, TG, PL, TP) :-
    B = (_=_),
    !,
    conj(A, PA, GL, TGA, PL, TPA),
    T1 is T - 1,
    conj(B, T1-T, TGA, TG, TPA, TP).
conj(A, parentheses_term_position(_,_,Pos), GL, TG, PL, TP) :-
    nonvar(Pos),
    !,
    conj(A, Pos, GL, TG, PL, TP).
conj((!,(S=SR)), F-T, [!,S=SR|TG], TG, [F-T,F1-T1|TP], TP) :-
    F1 is F+1,
    T1 is T+1.
conj(A, P, [A|TG], TG, [P|TP], TP).


%!  mkconj(+Decompiled, +Module, -Position, +ReadGoals, +ReadPositions)

mkconj(Goal, M, Pos, GoalList, PosList) :-
    mkconj(Goal, M, Pos, GoalList, [], PosList, []).

mkconj(Conj, M, term_position(0,0,0,0,[PA,PB]), GL, TG, PL, TP) :-
    nonvar(Conj),
    Conj = (A,B),
    !,
    mkconj(A, M, PA, GL, TGA, PL, TPA),
    mkconj(B, M, PB, TGA, TG, TPA, TP).
mkconj(A0, M, P0, [A|TG], TG, [P|TP], TP) :-
    ubody(A, A0, M, P, P0),
    !.
mkconj(A0, M, P0, [RG|TG0], TG, [_|TP0], TP) :-
    maybe_optimized(RG),
    mkconj(A0, M, P0, TG0, TG, TP0, TP).

maybe_optimized(debug(_,_,_)).
maybe_optimized(assertion(_)).
maybe_optimized(true).

%!  argpos(+N, +PositionTerm, -ArgPositionTerm) is det.
%
%   Get the position for the nth argument of PositionTerm.

argpos(N, parentheses_term_position(_,_,PosIn), Pos) =>
    argpos(N, PosIn, Pos).
argpos(N, term_position(_,_,_,_,ArgPos), Pos) =>
    nth1(N, ArgPos, Pos).
argpos(_, _, _) => true.


                 /*******************************
                 *    PCE STUFF (SHOULD MOVE)   *
                 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        <method>(Receiver, ... Arg ...) :->
                Body

mapped to:

        send_implementation(Id, <method>(...Arg...), Receiver)

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

pce_method_clause(Head, Body, M:PlHead, PlBody, _, TermPos0, TermPos) :-
    !,
    pce_method_clause(Head, Body, PlBody, PlHead, M, TermPos0, TermPos).
pce_method_clause(Head, Body,
                  send_implementation(_Id, Msg, Receiver), PlBody,
                  M, TermPos0, TermPos) :-
    !,
    debug(clause_info, 'send method ...', []),
    arg(1, Head, Receiver),
    functor(Head, _, Arity),
    pce_method_head_arguments(2, Arity, Head, Msg),
    debug(clause_info, 'head ...', []),
    pce_method_body(Body, PlBody, M, TermPos0, TermPos).
pce_method_clause(Head, Body,
                  get_implementation(_Id, Msg, Receiver, Result), PlBody,
                  M, TermPos0, TermPos) :-
    !,
    debug(clause_info, 'get method ...', []),
    arg(1, Head, Receiver),
    debug(clause_info, 'receiver ...', []),
    functor(Head, _, Arity),
    arg(Arity, Head, PceResult),
    debug(clause_info, '~w?~n', [PceResult = Result]),
    pce_unify_head_arg(PceResult, Result),
    Ar is Arity - 1,
    pce_method_head_arguments(2, Ar, Head, Msg),
    debug(clause_info, 'head ...', []),
    pce_method_body(Body, PlBody, M, TermPos0, TermPos).

pce_method_head_arguments(N, Arity, Head, Msg) :-
    N =< Arity,
    !,
    arg(N, Head, PceArg),
    PLN is N - 1,
    arg(PLN, Msg, PlArg),
    pce_unify_head_arg(PceArg, PlArg),
    debug(clause_info, '~w~n', [PceArg = PlArg]),
    NextArg is N+1,
    pce_method_head_arguments(NextArg, Arity, Head, Msg).
pce_method_head_arguments(_, _, _, _).

pce_unify_head_arg(V, A) :-
    var(V),
    !,
    V = A.
pce_unify_head_arg(A:_=_, A) :- !.
pce_unify_head_arg(A:_, A).

%       pce_method_body(+SrcBody, +DbBody, +M, +TermPos0, -TermPos
%
%       Unify the body of an XPCE method.  Goal-expansion makes this
%       rather tricky, especially as we cannot call XPCE's expansion
%       on an isolated method.
%
%       TermPos0 is the term-position term of the whole clause!
%
%       Further, please note that the body of the method-clauses reside
%       in another module than pce_principal, and therefore the body
%       starts with an I_CONTEXT call. This implies we need a
%       hypothetical term-position for the module-qualifier.

pce_method_body(A0, A, M, TermPos0, TermPos) :-
    TermPos0 = term_position(F, T, FF, FT,
                             [ HeadPos,
                               BodyPos0
                             ]),
    TermPos  = term_position(F, T, FF, FT,
                             [ HeadPos,
                               term_position(0,0,0,0, [0-0,BodyPos])
                             ]),
    pce_method_body2(A0, A, M, BodyPos0, BodyPos).


pce_method_body2(::(_,A0), A, M, TermPos0, TermPos) :-
    !,
    TermPos0 = term_position(_, _, _, _, [_Cmt,BodyPos0]),
    TermPos  = BodyPos,
    expand_goal(A0, A, M, BodyPos0, BodyPos).
pce_method_body2(A0, A, M, TermPos0, TermPos) :-
    A0 =.. [Func,B0,C0],
    control_op(Func),
    !,
    A =.. [Func,B,C],
    TermPos0 = term_position(F, T, FF, FT,
                             [ BP0,
                               CP0
                             ]),
    TermPos  = term_position(F, T, FF, FT,
                             [ BP,
                               CP
                             ]),
    pce_method_body2(B0, B, M, BP0, BP),
    expand_goal(C0, C, M, CP0, CP).
pce_method_body2(A0, A, M, TermPos0, TermPos) :-
    expand_goal(A0, A, M, TermPos0, TermPos).

control_op(',').
control_op((;)).
control_op((->)).
control_op((*->)).

                 /*******************************
                 *     EXPAND_GOAL SUPPORT      *
                 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
With the introduction of expand_goal, it  is increasingly hard to relate
the clause from the database to the actual  source. For one thing, we do
not know the compilation  module  of  the   clause  (unless  we  want to
decompile it).

Goal expansion can translate  goals   into  control-constructs, multiple
clauses, or delete a subgoal.

To keep track of the source-locations, we   have to redo the analysis of
the clause as defined in init.pl
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

expand_goal(G, call(G), _, P, term_position(0,0,0,0,[P])) :-
    var(G),
    !.
expand_goal(G, G1, _, P, P) :-
    var(G),
    !,
    G1 = G.
expand_goal(M0, M, Module, P0, P) :-
    meta(Module, M0, S),
    !,
    P0 = term_position(F,T,FF,FT,PL0),
    P  = term_position(F,T,FF,FT,PL),
    functor(M0, Functor, Arity),
    functor(M,  Functor, Arity),
    expand_meta_args(PL0, PL, 1, S, Module, M0, M).
expand_goal(A, B, Module, P0, P) :-
    goal_expansion(A, B0, P0, P1),
    !,
    expand_goal(B0, B, Module, P1, P).
expand_goal(A, A, _, P, P).

expand_meta_args([],      [],   _,  _, _,      _,  _).
expand_meta_args([P0|T0], [P|T], I, S, Module, M0, M) :-
    arg(I, M0, A0),
    arg(I, M,  A),
    arg(I, S,  AS),
    expand_arg(AS, A0, A, Module, P0, P),
    NI is I + 1,
    expand_meta_args(T0, T, NI, S, Module, M0, M).

expand_arg(0, A0, A, Module, P0, P) :-
    !,
    expand_goal(A0, A, Module, P0, P).
expand_arg(_, A, A, _, P, P).

meta(M, G, S) :- predicate_property(M:G, meta_predicate(S)).

goal_expansion(send(R, Msg), send_class(R, _, SuperMsg), P, P) :-
    compound(Msg),
    Msg =.. [send_super, Selector | Args],
    !,
    SuperMsg =.. [Selector|Args].
goal_expansion(get(R, Msg, A), get_class(R, _, SuperMsg, A), P, P) :-
    compound(Msg),
    Msg =.. [get_super, Selector | Args],
    !,
    SuperMsg =.. [Selector|Args].
goal_expansion(send_super(R, Msg), send_class(R, _, Msg), P, P).
goal_expansion(get_super(R, Msg, V), get_class(R, _, Msg, V), P, P).
goal_expansion(SendSuperN, send_class(R, _, Msg), P, P) :-
    compound(SendSuperN),
    compound_name_arguments(SendSuperN, send_super, [R,Sel|Args]),
    Msg =.. [Sel|Args].
goal_expansion(SendN, send(R, Msg), P, P) :-
    compound(SendN),
    compound_name_arguments(SendN, send, [R,Sel|Args]),
    atom(Sel), Args \== [],
    Msg =.. [Sel|Args].
goal_expansion(GetSuperN, get_class(R, _, Msg, Answer), P, P) :-
    compound(GetSuperN),
    compound_name_arguments(GetSuperN, get_super, [R,Sel|AllArgs]),
    append(Args, [Answer], AllArgs),
    Msg =.. [Sel|Args].
goal_expansion(GetN, get(R, Msg, Answer), P, P) :-
    compound(GetN),
    compound_name_arguments(GetN, get, [R,Sel|AllArgs]),
    append(Args, [Answer], AllArgs),
    atom(Sel), Args \== [],
    Msg =.. [Sel|Args].
goal_expansion(G0, G, P, P) :-
    user:goal_expansion(G0, G),     % TBD: we need the module!
    G0 \== G.                       % \=@=?


                 /*******************************
                 *        INITIALIZATION        *
                 *******************************/

%!  initialization_layout(+SourceLocation, ?InitGoal,
%!                        -ReadGoal, -TermPos) is semidet.
%
%   Find term-layout of :- initialization directives.

initialization_layout(File:Line, M:Goal0, Goal, TermPos) :-
    read_term_at_line(File, Line, M, Directive, DirectivePos, _),
    Directive    = (:- initialization(ReadGoal)),
    DirectivePos = term_position(_, _, _, _, [InitPos]),
    InitPos      = term_position(_, _, _, _, [GoalPos]),
    (   ReadGoal = M:_
    ->  Goal = M:Goal0
    ;   Goal = Goal0
    ),
    unify_body(ReadGoal, Goal, M, GoalPos, TermPos),
    !.


                 /*******************************
                 *        PRINTABLE NAMES       *
                 *******************************/

:- module_transparent
    predicate_name/2.
:- multifile
    user:prolog_predicate_name/2,
    user:prolog_clause_name/2.

hidden_module(user).
hidden_module(system).
hidden_module(pce_principal).           % should be config
hidden_module(Module) :-                % SWI-Prolog specific
    import_module(Module, system).

thaffix(1, st) :- !.
thaffix(2, nd) :- !.
thaffix(_, th).

%!  predicate_name(:Head, -PredName:string) is det.
%
%   Describe a predicate as [Module:]Name/Arity.

predicate_name(Predicate, PName) :-
    strip_module(Predicate, Module, Head),
    (   user:prolog_predicate_name(Module:Head, PName)
    ->  true
    ;   functor(Head, Name, Arity),
        (   hidden_module(Module)
        ->  format(string(PName), '~q/~d', [Name, Arity])
        ;   format(string(PName), '~q:~q/~d', [Module, Name, Arity])
        )
    ).

%!  clause_name(+Ref, -Name)
%
%   Provide a suitable description of the indicated clause.

clause_name(Ref, Name) :-
    user:prolog_clause_name(Ref, Name),
    !.
clause_name(Ref, Name) :-
    nth_clause(Head, N, Ref),
    !,
    predicate_name(Head, PredName),
    thaffix(N, Th),
    format(string(Name), '~d-~w clause of ~w', [N, Th, PredName]).
clause_name(Ref, Name) :-
    clause_property(Ref, erased),
    !,
    clause_property(Ref, predicate(M:PI)),
    format(string(Name), 'erased clause from ~q', [M:PI]).
clause_name(_, '<meta-call>').
