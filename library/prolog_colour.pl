/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org/projects/xpce/
    Copyright (c)  2011-2025, University of Amsterdam
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

:- module(prolog_colour,
          [ prolog_colourise_stream/3,  % +Stream, +SourceID, :ColourItem
            prolog_colourise_stream/4,  % +Stream, +SourceID, :ColourItem, +Opts
            prolog_colourise_term/4,    % +Stream, +SourceID, :ColourItem, +Opts
            prolog_colourise_query/3,   % +String, +SourceID, :ColourItem
            syntax_colour/2,            % +Class, -Attributes
            syntax_message//1           % +Class
          ]).
:- use_module(library(record),[(record)/1, op(_,_,record)]).
:- use_module(library(debug),[debug/3]).
:- autoload(library(apply),[maplist/3]).
:- autoload(library(error),[is_of_type/2]).
:- autoload(library(lists),[member/2,append/3]).
:- autoload(library(operators),
	    [push_operators/1,pop_operators/0,push_op/3]).
:- autoload(library(option),[option/3]).
:- autoload(library(predicate_options),
	    [current_option_arg/2,current_predicate_options/3]).
:- autoload(library(prolog_clause),[predicate_name/2]).
:- autoload(library(prolog_source),
	    [ load_quasi_quotation_syntax/2,
	      read_source_term_at_location/3,
	      prolog_canonical_source/2
	    ]).
:- autoload(library(prolog_xref),
	    [ xref_option/2,
	      xref_public_list/3,
	      xref_op/2,
	      xref_prolog_flag/4,
	      xref_module/2,
	      xref_meta/3,
	      xref_source_file/4,
	      xref_defined/3,
	      xref_called/3,
	      xref_defined_class/3,
	      xref_exported/2,
	      xref_hook/1
	    ]).

:- meta_predicate
    prolog_colourise_stream(+, +, 3),
    prolog_colourise_stream(+, +, 3, +),
    prolog_colourise_query(+, +, 3),
    prolog_colourise_term(+, +, 3, +).

:- predicate_options(prolog_colourise_term/4, 4,
                     [ subterm_positions(-any)
                     ]).
:- predicate_options(prolog_colourise_stream/4, 4,
                     [ operators(list(any))
                     ]).

/** <module> Prolog syntax colouring support.

This module defines reusable code to colourise Prolog source.

@tbd: The one-term version
*/


:- multifile
    style/2,                        % +ColourClass, -Attributes
    message//1,                     % +ColourClass
    term_colours/2,                 % +SourceTerm, -ColourSpec
    goal_colours/2,                 % +Goal, -ColourSpec
    goal_colours/3,                 % +Goal, +Class, -ColourSpec
    directive_colours/2,            % +Goal, -ColourSpec
    goal_classification/2,          % +Goal, -Class
    vararg_goal_classification/3.   % +Name, +Arity, -Class


:- record
    colour_state(source_id_list,
                 module,
                 stream,
                 closure,
                 singletons,
                 current_variable).

colour_state_source_id(State, SourceID) :-
    colour_state_source_id_list(State, SourceIDList),
    member(SourceID, SourceIDList).

%!  prolog_colourise_stream(+Stream, +SourceID, :ColourItem) is det.
%!  prolog_colourise_stream(+Stream, +SourceID, :ColourItem, +Opts) is det.
%
%   Determine colour fragments for the data   on Stream. SourceID is
%   the  canonical  identifier  of  the  input    as  known  to  the
%   cross-referencer, i.e., as created using xref_source(SourceID).
%
%   ColourItem is a closure  that  is   called  for  each identified
%   fragment with three additional arguments:
%
%     * The syntactical category
%     * Start position (character offset) of the fragment
%     * Length of the fragment (in characters).
%
%   Options
%
%     - operators(+Ops)
%       Provide an initial list of additional operators.

prolog_colourise_stream(Fd, SourceId, ColourItem) :-
    prolog_colourise_stream(Fd, SourceId, ColourItem, []).
prolog_colourise_stream(Fd, SourceId, ColourItem, Options) :-
    to_list(SourceId, SourceIdList),
    make_colour_state([ source_id_list(SourceIdList),
                        stream(Fd),
                        closure(ColourItem)
                      ],
                      TB),
    option(operators(Ops), Options, []),
    setup_call_cleanup(
        save_settings(TB, Ops, State),
        colourise_stream(Fd, TB),
        restore_settings(State)).

to_list(List, List) :-
    is_list(List),
    !.
to_list(One, [One]).


colourise_stream(Fd, TB) :-
    (   peek_char(Fd, #)            % skip #! script line
    ->  skip(Fd, 10)
    ;   true
    ),
    repeat,
        colour_state_module(TB, SM),
        character_count(Fd, Start),
        catch(read_term(Fd, Term,
                        [ subterm_positions(TermPos),
                          singletons(Singletons0),
                          module(SM),
                          comments(Comments)
                        ]),
              E,
              read_error(E, TB, Start, Fd)),
        fix_operators(Term, SM, TB),
        warnable_singletons(Singletons0, Singletons),
        colour_state_singletons(TB, Singletons),
        (   colourise_term(Term, TB, TermPos, Comments)
        ->  true
        ;   arg(1, TermPos, From),
            print_message(warning,
                          format('Failed to colourise ~p at index ~d~n',
                                 [Term, From]))
        ),
        Term == end_of_file,
    !.

save_settings(TB, Ops, state(Style, Flags, OSM, Xref)) :-
    (   source_module(TB, SM)
    ->  true
    ;   SM = prolog_colour_ops
    ),
    set_xref(Xref, true),
    '$set_source_module'(OSM, SM),
    colour_state_module(TB, SM),
    maplist(qualify_op(SM), Ops, QOps),
    push_operators(QOps),
    syntax_flags(Flags),
    '$style_check'(Style, Style).

qualify_op(M, op(P,T,[]), Q)            => Q = op(P,T,M:[]).
qualify_op(M, op(P,T,N), Q), atom(N)    => Q = op(P,T,M:N).
qualify_op(M, op(P,T,L), Q), is_list(Q) =>
    Q = op(P, T, QL),
    maplist(qualify_op_name(M), L, QL).
qualify_op(_, Op, Q)			=> Q = Op.

qualify_op_name(M, N,  Q), atom(N) => Q = M:N.
qualify_op_name(M, [], Q)          => Q = M:[].
qualify_op_name(_, V,  Q)          => Q = V.

restore_settings(state(Style, Flags, OSM, Xref)) :-
    restore_syntax_flags(Flags),
    '$style_check'(_, Style),
    pop_operators,
    '$set_source_module'(OSM),
    set_xref(_, Xref).

set_xref(Old, New) :-
    current_prolog_flag(xref, Old),
    !,
    set_prolog_flag(xref, New).
set_xref(false, New) :-
    set_prolog_flag(xref, New).


syntax_flags(Pairs) :-
    findall(set_prolog_flag(Flag, Value),
            syntax_flag(Flag, Value),
            Pairs).

syntax_flag(Flag, Value) :-
    syntax_flag(Flag),
    current_prolog_flag(Flag, Value).

restore_syntax_flags([]).
restore_syntax_flags([set_prolog_flag(Flag, Value)|T]) :-
    set_prolog_flag(Flag, Value),
    restore_syntax_flags(T).

%!  source_module(+State, -Module) is semidet.
%
%   True when Module is the module context   into  which the file is
%   loaded. This is the module of the file if File is a module file,
%   or the load context of  File  if   File  is  not included or the
%   module context of the file into which the file was included.

source_module(TB, Module) :-
    colour_state_source_id_list(TB, []),
    !,
    colour_state_module(TB, Module).
source_module(TB, Module) :-
    colour_state_source_id(TB, SourceId),
    xref_option(SourceId, module(Module)),
    !.
source_module(TB, Module) :-
    (   colour_state_source_id(TB, File),
        atom(File)
    ;   colour_state_stream(TB, Fd),
        is_stream(Fd),
        stream_property(Fd, file_name(File))
    ),
    module_context(File, [], Module).

module_context(File, _, Module) :-
    source_file_property(File, module(Module)),
    !.
module_context(File, Seen, Module) :-
    source_file_property(File, included_in(File2, _Line)),
    \+ memberchk(File, Seen),
    !,
    module_context(File2, [File|Seen], Module).
module_context(File, _, Module) :-
    source_file_property(File, load_context(Module, _, _)).


%!  read_error(+Error, +TB, +Start, +Stream) is failure.
%
%   If this is a syntax error, create a syntax-error fragment.

read_error(Error, TB, Start, EndSpec) :-
    (   syntax_error(Error, Id, CharNo)
    ->  message_to_string(error(syntax_error(Id), _), Msg),
        (   integer(EndSpec)
        ->  End = EndSpec
        ;   character_count(EndSpec, End)
        ),
        show_syntax_error(TB, CharNo:Msg, Start-End),
        fail
    ;   throw(Error)
    ).

syntax_error(error(syntax_error(Id), stream(_S, _Line, _LinePos, CharNo)),
             Id, CharNo).
syntax_error(error(syntax_error(Id), file(_S, _Line, _LinePos, CharNo)),
             Id, CharNo).
syntax_error(error(syntax_error(Id), string(_Text, CharNo)),
             Id, CharNo).

%!  warnable_singletons(+Singletons, -Warn) is det.
%
%   Warn is the subset of the singletons that we warn about.

warnable_singletons([], []).
warnable_singletons([H|T0], List) :-
    H = (Name=_Var),
    (   '$is_named_var'(Name)
    ->  List = [H|T]
    ;   List = T
    ),
    warnable_singletons(T0, T).

%!  colour_item(+Class, +TB, +Pos) is det.

colour_item(Class, TB, Pos) :-
    arg(1, Pos, Start),
    arg(2, Pos, End),
    Len is End - Start,
    colour_state_closure(TB, Closure),
    call(Closure, Class, Start, Len).


%!  safe_push_op(+Prec, +Type, :Name, +State)
%
%   Define operators into the default source module and register
%   them to be undone by pop_operators/0.

safe_push_op(P, T, N0, State) :-
    colour_state_module(State, CM),
    strip_module(CM:N0, M, N),
    (   is_list(N),
        N \== []                                % define list as operator
    ->  acyclic_term(N),
        forall(member(Name, N),
               safe_push_op(P, T, M:Name, State))
    ;   push_op(P, T, M:N)
    ),
    debug(colour, ':- ~w.', [op(P,T,M:N)]).

%!  fix_operators(+Term, +Module, +State) is det.
%
%   Fix flags that affect the  syntax,   such  as operators and some
%   style checking options. Src is the  canonical source as required
%   by the cross-referencer.

fix_operators((:- Directive), M, Src) :-
    callable(Directive),
    acyclic_term(Directive),
    catch(process_directive(Directive, M, Src), error(_,_), true),
    !.
fix_operators(_, _, _).

:- multifile
    prolog:xref_update_syntax/2.

process_directive(Directive, M, _Src),
    ground(Directive),
    prolog:xref_update_syntax((:- Directive), M) =>
    true.
process_directive(style_check(X), _, _), ground(X) =>
    style_check(X).
process_directive(set_prolog_flag(Flag, Value), M, _),
    ground(Flag+Value),
    syntax_flag(Flag) =>
    set_prolog_flag(M:Flag, Value).
process_directive(M:op(P,T,N), _, Src), ground(M) =>
    process_directive(op(P,T,N), M, Src).
process_directive(op(P,T,N), M, Src), ground(op(P,T,N)) =>
    safe_push_op(P, T, M:N, Src).
process_directive(module(_Name, Export), M, Src), ground(Export) =>
    forall(member(op(P,A,N), Export),
           safe_push_op(P,A,M:N, Src)).
process_directive(use_module(Spec), _, Src), ground(Spec) =>
    catch(process_use_module1(Spec, Src), _, true).
process_directive(use_module(Spec, Imports), _, Src), ground(Spec), is_list(Imports) =>
    catch(process_use_module2(Spec, Imports, Src), _, true).
process_directive(Directive, _, Src), ground(Directive) =>
    prolog_source:expand((:-Directive), Src, _).

syntax_flag(character_escapes).
syntax_flag(var_prefix).
syntax_flag(allow_variable_name_as_functor).
syntax_flag(allow_dot_in_atom).

%!  process_use_module1(+Imports, +Src)
%
%   Get the exported operators from the referenced files.

process_use_module1([], _) :- !.
process_use_module1([H|T], Src) :-
    !,
    process_use_module1(H, Src),
    process_use_module1(T, Src).
process_use_module1(File, Src) :-
    (   xref_public_list(File, Src,
                         [ exports(Exports),
                           silent(true),
                           path(Path)
                         ])
    ->  forall(member(op(P,T,N), Exports),
               safe_push_op(P,T,N,Src)),
        colour_state_module(Src, SM),
        (   member(Syntax/4, Exports),
            load_quasi_quotation_syntax(SM:Path, Syntax),
            fail
        ;   true
        )
    ;   true
    ).

process_use_module2(File, Imports, Src) :-
    (   xref_public_list(File, Src,
                         [ exports(Exports),
                           silent(true),
                           path(Path)
                         ])
    ->  forall(( member(op(P,T,N), Exports),
                 member(op(P,T,N), Imports)),
               safe_push_op(P,T,N,Src)),
        colour_state_module(Src, SM),
        (   member(Syntax/4, Exports),
            member(Syntax/4, Imports),
            load_quasi_quotation_syntax(SM:Path, Syntax),
            fail
        ;   true
        )
    ;   true
    ).

%!  prolog_colourise_query(+Query:string, +SourceId, :ColourItem)
%
%   Colourise a query, to be executed in the context of SourceId.
%
%   @arg    SourceId Execute Query in the context of
%           the cross-referenced environment SourceID.

prolog_colourise_query(QueryString, SourceID, ColourItem) :-
    query_colour_state(SourceID, ColourItem, TB),
    setup_call_cleanup(
        save_settings(TB, [], State),
        colourise_query(QueryString, TB),
        restore_settings(State)).

query_colour_state(module(Module), ColourItem, TB) :-
    !,
    make_colour_state([ source_id_list([]),
                        module(Module),
                        closure(ColourItem)
                      ],
                      TB).
query_colour_state(SourceID, ColourItem, TB) :-
    to_list(SourceID, SourceIDList),
    make_colour_state([ source_id_list(SourceIDList),
                        closure(ColourItem)
                      ],
                      TB).


colourise_query(QueryString, TB) :-
    colour_state_module(TB, SM),
    string_length(QueryString, End),
    (   catch(term_string(Query, QueryString,
                          [ subterm_positions(TermPos),
                            singletons(Singletons0),
                            module(SM),
                            comments(Comments)
                          ]),
              E,
              read_error(E, TB, 0, End))
    ->  warnable_singletons(Singletons0, Singletons),
        colour_state_singletons(TB, Singletons),
        colourise_comments(Comments, TB),
        (   Query == end_of_file
        ->  true
        ;   colourise_body(Query, TB, TermPos)
        )
    ;   true                        % only a syntax error
    ).

%!  prolog_colourise_term(+Stream, +SourceID, :ColourItem, +Options)
%
%   Colourise    the    next     term      on     Stream.     Unlike
%   prolog_colourise_stream/3, this predicate assumes  it is reading
%   a single term rather than the   entire stream. This implies that
%   it cannot adjust syntax according to directives that precede it.
%
%   Options:
%
%     - subterm_positions(-TermPos)
%       Return complete term-layout.  If an error is read, this is a
%       term error_position(StartClause, EndClause, ErrorPos)
%     - current_variable(+VarName)
%       Variable to highlight

prolog_colourise_term(Stream, SourceId, ColourItem, Options) :-
    to_list(SourceId, SourceIdList),
    make_colour_state([ source_id_list(SourceIdList),
                        stream(Stream),
                        closure(ColourItem)
                      ],
                      TB),
    option(subterm_positions(TermPos), Options, _),
    findall(Op, xref_op(SourceId, Op), Ops),
    debug(colour, 'Ops from ~p: ~p', [SourceId, Ops]),
    findall(Opt, xref_flag_option(SourceId, Opt), Opts),
    character_count(Stream, Start),
    (   source_module(TB, Module)
    ->  true
    ;   Module = prolog_colour_ops
    ),
    read_source_term_at_location(
        Stream, Term,
        [ module(Module),
          operators(Ops),
          error(Error),
          subterm_positions(TermPos),
          variable_names(VarNames),
          singletons(Singletons0),
          comments(Comments)
        | Opts
        ]),
    (   var(Error)
    ->  warnable_singletons(Singletons0, Singletons),
        colour_state_singletons(TB, Singletons),
        set_current_variable(TB, VarNames, Options),
        colour_item(range, TB, TermPos),            % Call to allow clearing
        colourise_term(Term, TB, TermPos, Comments)
    ;   character_count(Stream, End),
        TermPos = error_position(Start, End, Pos),
        colour_item(range, TB, TermPos),
        show_syntax_error(TB, Error, Start-End),
        Error = Pos:_Message
    ).

xref_flag_option(TB, var_prefix(Bool)) :-
    xref_prolog_flag(TB, var_prefix, Bool, _Line).

show_syntax_error(TB, Pos:Message, Range) :-
    integer(Pos),
    !,
    End is Pos + 1,
    colour_item(syntax_error(Message, Range), TB, Pos-End).
show_syntax_error(TB, _:Message, Range) :-
    colour_item(syntax_error(Message, Range), TB, Range).

%!  singleton(@Var, +TB) is semidet.
%
%   True when Var is a singleton.

singleton(Var, TB) :-
    colour_state_singletons(TB, Singletons),
    member_var(Var, Singletons).

member_var(V, [_=V2|_]) :-
    V == V2,
    !.
member_var(V, [_|T]) :-
    member_var(V, T).

set_current_variable(TB, VarNames, Options) :-
    option(current_variable(Name), Options),
    memberchk(Name=CV, VarNames),
    !,
    colour_state_current_variable(TB, CV).
set_current_variable(_, _, _).

current_variable(Var, TB) :-
    colour_state_current_variable(TB, Current),
    Var == Current.


%!  colourise_term(+Term, +TB, +Termpos, +Comments)
%
%   Colourise the next Term.
%
%   @bug    The colour spec is closed with =fullstop=, but the
%           position information does not include the full stop
%           location, so all we can do is assume it is behind the
%           term.

colourise_term(Term, TB, TermPos, Comments) :-
    colourise_comments(Comments, TB),
    (   Term == end_of_file
    ->  true
    ;   colourise_term(Term, TB, TermPos),
        colourise_fullstop(TB, TermPos)
    ).

colourise_fullstop(TB, TermPos) :-
    arg(2, TermPos, EndTerm),
    Start is EndTerm,
    End is Start+1,
    colour_item(fullstop, TB, Start-End).

colourise_comments(-, _).
colourise_comments([], _).
colourise_comments([H|T], TB) :-
    colourise_comment(H, TB),
    colourise_comments(T, TB).

colourise_comment((-)-_, _) :- !.
colourise_comment(Pos-Comment, TB) :-
    comment_style(Comment, Style),
    stream_position_data(char_count, Pos, Start),
    string_length(Comment, Len),
    End is Start + Len + 1,
    colour_item(comment(Style), TB, Start-End).

comment_style(Comment, structured) :-           % Starts %%, %! or /**
    structured_comment_start(Start),
    sub_string(Comment, 0, Len, _, Start),
    Next is Len+1,
    string_code(Next, Comment, NextCode),
    code_type(NextCode, space),
    !.
comment_style(Comment, line) :-                 % Starts %
    sub_string(Comment, 0, _, _, '%'),
    !.
comment_style(_, block).                        % Starts /*

%!  structured_comment_start(-Start)
%
%   Copied from library(pldoc/doc_process). Unfortunate,   but we do
%   not want to force loading pldoc.

structured_comment_start('%%').
structured_comment_start('%!').
structured_comment_start('/**').

%!  colourise_term(+Term, +TB, +Pos)
%
%   Colorise a file toplevel term.

colourise_term(Var, TB, Start-End) :-
    var(Var),
    !,
    colour_item(instantiation_error, TB, Start-End).
colourise_term(_, _, Pos) :-
    var(Pos),
    !.
colourise_term(Term, TB, parentheses_term_position(PO,PC,Pos)) :-
    !,
    colour_item(parentheses, TB, PO-PC),
    colourise_term(Term, TB, Pos).
colourise_term(Term, TB, Pos) :-
    term_colours(Term, FuncSpec-ArgSpecs),
    !,
    Pos = term_position(F,T,FF,FT,ArgPos),
    colour_item(term, TB, F-T),     % TBD: Allow specifying by term_colours/2?
    specified_item(FuncSpec, Term, TB, FF-FT),
    specified_items(ArgSpecs, Term, TB, ArgPos).
colourise_term((Pre=>Body), TB,
               term_position(F,T,FF,FT,[PP,BP])) :-
    nonvar(Pre),
    Pre = (Head,Cond),
    PP = term_position(_HF,_HT,_HFF,_HFT,[HP,CP]),
    !,
    colour_item(clause,         TB, F-T),
    colour_item(neck(=>),       TB, FF-FT),
    colourise_clause_head(Head, TB, HP),
    colour_item(rule_condition, TB, CP),
    colourise_body(Cond, Head,  TB, CP),
    colourise_body(Body, Head,  TB, BP).
colourise_term(Term, TB,
               term_position(F,T,FF,FT,[HP,BP])) :-
    neck(Term, Head, Body, Neck),
    !,
    colour_item(clause,         TB, F-T),
    colour_item(neck(Neck),     TB, FF-FT),
    colourise_clause_head(Head, TB, HP),
    colourise_body(Body, Head,  TB, BP).
colourise_term(((Head,RHC) --> Body), TB,
               term_position(F,T,FF,FT,
                             [ term_position(_,_,_,_,[HP,RHCP]),
                               BP
                             ])) :-
    !,
    colour_item(grammar_rule,       TB, F-T),
    colour_item(dcg_right_hand_ctx, TB, RHCP),
    colourise_term_arg(RHC, TB, RHCP),
    colour_item(neck(-->),          TB, FF-FT),
    colourise_extended_head(Head, 2, TB, HP),
    colourise_dcg(Body, Head,       TB, BP).
colourise_term((Head --> Body), TB,                     % TBD: expansion!
               term_position(F,T,FF,FT,[HP,BP])) :-
    !,
    colour_item(grammar_rule,       TB, F-T),
    colour_item(neck(-->),          TB, FF-FT),
    colourise_extended_head(Head, 2, TB, HP),
    colourise_dcg(Body, Head,       TB, BP).
colourise_term(((Head,RHC) ==> Body), TB,
               term_position(F,T,FF,FT,
                             [ term_position(_,_,_,_,[HP,RHCP]),
                               BP
                             ])) :-
    !,
    extend(Head, 2, HeadEx),
    colour_item(grammar_rule,        TB, F-T),
    colour_item(rule_condition,      TB, RHCP),
    colourise_body(RHC, HeadEx,      TB, RHCP),
    colour_item(neck(==>),           TB, FF-FT),
    colourise_extended_head(Head, 2, TB, HP),
    colourise_dcg(Body, Head,        TB, BP).
colourise_term((Head ==> Body), TB,                     % TBD: expansion!
               term_position(F,T,FF,FT,[HP,BP])) :-
    !,
    colour_item(grammar_rule,       TB, F-T),
    colour_item(neck(==>),          TB, FF-FT),
    colourise_extended_head(Head, 2, TB, HP),
    colourise_dcg(Body, Head,       TB, BP).
colourise_term(:->(Head, Body), TB,
               term_position(F,T,FF,FT,[HP,BP])) :-
    !,
    colour_item(method,             TB, F-T),
    colour_item(neck(:->), TB, FF-FT),
    colour_method_head(send(Head),  TB, HP),
    colourise_method_body(Body,     TB, BP).
colourise_term(:<-(Head, Body), TB,
               term_position(F,T,FF,FT,[HP,BP])) :-
    !,
    colour_item(method,            TB, F-T),
    colour_item(neck(:<-), TB, FF-FT),
    colour_method_head(get(Head),  TB, HP),
    colourise_method_body(Body,    TB, BP).
colourise_term((:- Directive), TB, Pos) :-
    !,
    colour_item(directive, TB, Pos),
    Pos = term_position(_F,_T,FF,FT,[ArgPos]),
    colour_item(neck(directive), TB, FF-FT),
    colourise_directive(Directive, TB, ArgPos).
colourise_term((?- Directive), TB, Pos) :-
    !,
    colourise_term((:- Directive), TB, Pos).
colourise_term(end_of_file, _, _) :- !.
colourise_term(Fact, TB, Pos) :-
    !,
    colour_item(clause, TB, Pos),
    colourise_clause_head(Fact, TB, Pos).

neck((Head  :- Body), Head, Body, :-).
neck((Head  => Body), Head, Body, =>).
neck(?=>(Head, Body), Head, Body, ?=>).

%!  colourise_extended_head(+Head, +ExtraArgs, +TB, +Pos) is det.
%
%   Colourise a clause-head that  is   extended  by  term_expansion,
%   getting ExtraArgs more  arguments  (e.g.,   DCGs  add  two  more
%   arguments.

colourise_extended_head(Head, N, TB, Pos) :-
    extend(Head, N, TheHead),
    colourise_clause_head(TheHead, TB, Pos).

extend(M:Head, N, M:ExtHead) :-
    nonvar(Head),
    !,
    extend(Head, N, ExtHead).
extend(Head, N, ExtHead) :-
    compound(Head),
    !,
    compound_name_arguments(Head, Name, Args),
    length(Extra, N),
    append(Args, Extra, NArgs),
    compound_name_arguments(ExtHead, Name, NArgs).
extend(Head, N, ExtHead) :-
    atom(Head),
    !,
    length(Extra, N),
    compound_name_arguments(ExtHead, Head, Extra).
extend(Head, _, Head).


colourise_clause_head(_, _, Pos) :-
    var(Pos),
    !.
colourise_clause_head(Head, TB, parentheses_term_position(PO,PC,Pos)) :-
    colour_item(parentheses, TB, PO-PC),
    colourise_clause_head(Head, TB, Pos).
colourise_clause_head(M:Head, TB, QHeadPos) :-
    QHeadPos = term_position(_,_,QF,QT,[MPos,HeadPos]),
    head_colours(M:Head, meta-[_, ClassSpec-ArgSpecs]),
    !,
    colourise_module(M, TB, MPos),
    colour_item(functor, TB, QF-QT),
    functor_position(HeadPos, FPos, ArgPos),
    (   ClassSpec == classify
    ->  classify_head(TB, Head, Class)
    ;   Class = ClassSpec
    ),
    colour_item(head_term(Class, Head), TB, QHeadPos),
    colour_item(head(Class, Head), TB, FPos),
    specified_items(ArgSpecs, Head, TB, ArgPos).
colourise_clause_head(#(Macro), TB, term_position(_,_,HF,HT,[MPos])) :-
    expand_macro(TB, Macro, Head),
    !,
    macro_term_string(Head, String),
    functor_position(MPos, FPos, _),
    classify_head(TB, Head, Class),
    colour_item(macro(String), TB, HF-HT),
    colour_item(head_term(Class, Head), TB, MPos),
    colour_item(head(Class, Head), TB, FPos),
    colourise_term_args(Macro, TB, MPos).
colourise_clause_head(Head, TB, Pos) :-
    head_colours(Head, ClassSpec-ArgSpecs),
    !,
    functor_position(Pos, FPos, ArgPos),
    (   ClassSpec == classify
    ->  classify_head(TB, Head, Class)
    ;   Class = ClassSpec
    ),
    colour_item(head_term(Class, Head), TB, Pos),
    colour_item(head(Class, Head), TB, FPos),
    specified_items(ArgSpecs, Head, TB, ArgPos).
colourise_clause_head(:=(Eval, Ret), TB,
                      term_position(_,_,AF,AT,
                                    [ term_position(_,_,SF,ST,
                                                    [ SelfPos,
                                                      FuncPos
                                                    ]),
                                      RetPos
                                    ])) :-
    Eval =.. [.,M,Func],
    FuncPos = term_position(_,_,FF,FT,_),
    !,
    colourise_term_arg(M, TB, SelfPos),
    colour_item(func_dot, TB, SF-ST),               % .
    colour_item(dict_function(Func), TB, FF-FT),
    colourise_term_args(Func, TB, FuncPos),
    colour_item(dict_return_op, TB, AF-AT),         % :=
    colourise_term_arg(Ret, TB, RetPos).
colourise_clause_head(Head, TB, Pos) :-
    functor_position(Pos, FPos, _),
    classify_head(TB, Head, Class),
    colour_item(head_term(Class, Head), TB, Pos),
    colour_item(head(Class, Head), TB, FPos),
    colourise_term_args(Head, TB, Pos).

%!  colourise_extern_head(+Head, +Module, +TB, +Pos)
%
%   Colourise the head specified as Module:Head. Normally used for
%   adding clauses to multifile predicates in other modules.

colourise_extern_head(Head, M, TB, Pos) :-
    functor_position(Pos, FPos, _),
    colour_item(head(extern(M), Head), TB, FPos),
    colourise_term_args(Head, TB, Pos).

colour_method_head(SGHead, TB, Pos) :-
    arg(1, SGHead, Head),
    functor_name(SGHead, SG),
    functor_position(Pos, FPos, _),
    colour_item(method(SG), TB, FPos),
    colourise_term_args(Head, TB, Pos).

%!  functor_position(+Term, -FunctorPos, -ArgPosList)
%
%   Get the position of a functor   and  its argument. Unfortunately
%   this goes wrong for lists, who have two `functor-positions'.

functor_position(term_position(_,_,FF,FT,ArgPos), FF-FT, ArgPos) :- !.
functor_position(list_position(F,_T,Elms,none), F-FT, Elms) :-
    !,
    FT is F + 1.
functor_position(dict_position(_,_,FF,FT,KVPos), FF-FT, KVPos) :- !.
functor_position(brace_term_position(F,T,Arg), F-T, [Arg]) :- !.
functor_position(Pos, Pos, []).

colourise_module(Term, TB, Pos) :-
    (   var(Term)
    ;   atom(Term)
    ),
    !,
    colour_item(module(Term), TB, Pos).
colourise_module(_, TB, Pos) :-
    colour_item(type_error(module), TB, Pos).

%!  colourise_directive(+Body, +TB, +Pos)
%
%   Colourise the body of a directive.

colourise_directive(_,_,Pos) :-
    var(Pos),
    !.
colourise_directive(Dir, TB, parentheses_term_position(PO,PC,Pos)) :-
    !,
    colour_item(parentheses, TB, PO-PC),
    colourise_directive(Dir, TB, Pos).
colourise_directive((A,B), TB, term_position(_,_,_,_,[PA,PB])) :-
    !,
    colourise_directive(A, TB, PA),
    colourise_directive(B, TB, PB).
colourise_directive(Body, TB, Pos) :-
    nonvar(Body),
    directive_colours(Body, ClassSpec-ArgSpecs),   % specified
    !,
    functor_position(Pos, FPos, ArgPos),
    (   ClassSpec == classify
    ->  goal_classification(TB, Body, [], Class)
    ;   Class = ClassSpec
    ),
    colour_item(goal(Class, Body), TB, FPos),
    specified_items(ArgSpecs, Body, TB, ArgPos).
colourise_directive(Body, TB, Pos) :-
    colourise_body(Body, TB, Pos).


%       colourise_body(+Body, +TB, +Pos)
%
%       Breaks down to colourise_goal/3.

colourise_body(Body, TB, Pos) :-
    colourise_body(Body, [], TB, Pos).

colourise_body(Body, Origin, TB, Pos) :-
    colour_item(body, TB, Pos),
    colourise_goals(Body, Origin, TB, Pos).

%!  colourise_method_body(+MethodBody, +TB, +Pos)
%
%   Colourise the optional "comment":: as pce(comment) and proceed
%   with the body.
%
%   @tbd    Get this handled by a hook.

colourise_method_body(_, _, Pos) :-
    var(Pos),
    !.
colourise_method_body(Body, TB, parentheses_term_position(PO,PC,Pos)) :-
    !,
    colour_item(parentheses, TB, PO-PC),
    colourise_method_body(Body, TB, Pos).
colourise_method_body(::(_Comment,Body), TB,
                      term_position(_F,_T,_FF,_FT,[CP,BP])) :-
    !,
    colour_item(comment(string), TB, CP),
    colourise_body(Body, TB, BP).
colourise_method_body(Body, TB, Pos) :-         % deal with pri(::) < 1000
    Body =.. [F,A,B],
    control_op(F),
    !,
    Pos = term_position(_F,_T,FF,FT,
                        [ AP,
                          BP
                        ]),
    colour_item(control, TB, FF-FT),
    colourise_method_body(A, TB, AP),
    colourise_body(B, TB, BP).
colourise_method_body(Body, TB, Pos) :-
    colourise_body(Body, TB, Pos).

control_op(',').
control_op((;)).
control_op((->)).
control_op((*->)).

%!  colourise_goals(+Body, +Origin, +TB, +Pos)
%
%   Colourise the goals in a body.

colourise_goals(_, _, _, Pos) :-
    var(Pos),
    !.
colourise_goals(Body, Origin, TB, parentheses_term_position(PO,PC,Pos)) :-
    !,
    colour_item(parentheses, TB, PO-PC),
    colourise_goals(Body, Origin, TB, Pos).
colourise_goals(Body, Origin, TB, term_position(_,_,FF,FT,ArgPos)) :-
    body_compiled(Body),
    !,
    colour_item(control, TB, FF-FT),
    colourise_subgoals(ArgPos, 1, Body, Origin, TB).
colourise_goals(Goal, Origin, TB, Pos) :-
    colourise_goal(Goal, Origin, TB, Pos).

colourise_subgoals([], _, _, _, _).
colourise_subgoals([Pos|T], N, Body, Origin, TB) :-
    arg(N, Body, Arg),
    colourise_goals(Arg, Origin, TB, Pos),
    NN is N + 1,
    colourise_subgoals(T, NN, Body, Origin, TB).

%!  colourise_dcg(+Body, +Head, +TB, +Pos)
%
%   Breaks down to colourise_dcg_goal/3.

colourise_dcg(Body, Head, TB, Pos) :-
    colour_item(dcg, TB, Pos),
    (   dcg_extend(Head, Origin)
    ->  true
    ;   Origin = Head
    ),
    colourise_dcg_goals(Body, Origin, TB, Pos).

colourise_dcg_goals(Var, _, TB, Pos) :-
    var(Var),
    !,
    colour_item(goal(meta,Var), TB, Pos).
colourise_dcg_goals(_, _, _, Pos) :-
    var(Pos),
    !.
colourise_dcg_goals(Body, Origin, TB, parentheses_term_position(PO,PC,Pos)) :-
    !,
    colour_item(parentheses, TB, PO-PC),
    colourise_dcg_goals(Body, Origin, TB, Pos).
colourise_dcg_goals({Body}, Origin, TB, brace_term_position(F,T,Arg)) :-
    !,
    colour_item(dcg(plain), TB, F-T),
    colourise_goals(Body, Origin, TB, Arg).
colourise_dcg_goals([], _, TB, Pos) :-
    !,
    colour_item(dcg(terminal), TB, Pos).
colourise_dcg_goals(List, _, TB, list_position(F,T,Elms,Tail)) :-
    List = [_|_],
    !,
    colour_item(dcg(terminal), TB, F-T),
    colourise_list_args(Elms, Tail, List, TB, classify).
colourise_dcg_goals(_, _, TB, string_position(F,T)) :-
    integer(F),
    !,
    colour_item(dcg(string), TB, F-T).
colourise_dcg_goals(Body, Origin, TB, term_position(_,_,FF,FT,ArgPos)) :-
    dcg_body_compiled(Body),       % control structures
    !,
    colour_item(control, TB, FF-FT),
    colourise_dcg_subgoals(ArgPos, 1, Body, Origin, TB).
colourise_dcg_goals(Goal, Origin, TB, Pos) :-
    colourise_dcg_goal(Goal, Origin, TB, Pos).

colourise_dcg_subgoals([], _, _, _, _).
colourise_dcg_subgoals([Pos|T], N, Body, Origin, TB) :-
    arg(N, Body, Arg),
    colourise_dcg_goals(Arg, Origin, TB, Pos),
    NN is N + 1,
    colourise_dcg_subgoals(T, NN, Body, Origin, TB).

dcg_extend(Term, _) :-
    var(Term), !, fail.
dcg_extend(M:Term, M:Goal) :-
    dcg_extend(Term, Goal).
dcg_extend(Term, Goal) :-
    compound(Term),
    !,
    compound_name_arguments(Term, Name, Args),
    append(Args, [_,_], NArgs),
    compound_name_arguments(Goal, Name, NArgs).
dcg_extend(Term, Goal) :-
    atom(Term),
    !,
    compound_name_arguments(Goal, Term, [_,_]).

dcg_body_compiled(G) :-
    body_compiled(G),
    !.
dcg_body_compiled((_|_)).

%       colourise_dcg_goal(+Goal, +Origin, +TB, +Pos).

colourise_dcg_goal(!, Origin, TB, TermPos) :-
    !,
    colourise_goal(!, Origin, TB, TermPos).
colourise_dcg_goal(Goal, Origin, TB, TermPos) :-
    dcg_extend(Goal, TheGoal),
    !,
    colourise_goal(TheGoal, Origin, TB, TermPos).
colourise_dcg_goal(Goal, _, TB, Pos) :-
    colourise_term_args(Goal, TB, Pos).


%!  colourise_goal(+Goal, +Origin, +TB, +Pos)
%
%   Colourise access to a single goal.
%
%   @tbd Quasi Quotations are coloured as a general term argument.
%   Possibly we should do something with the goal information it
%   refers to, in particular if this goal is not defined.

                                        % Deal with list as goal (consult)
colourise_goal(_,_,_,Pos) :-
    var(Pos),
    !.
colourise_goal(Goal, Origin, TB, parentheses_term_position(PO,PC,Pos)) :-
    !,
    colour_item(parentheses, TB, PO-PC),
    colourise_goal(Goal, Origin, TB, Pos).
colourise_goal(Goal, _, TB, Pos) :-
    Pos = list_position(F,T,Elms,TailPos),
    Goal = [_|_],
    !,
    FT is F + 1,
    AT is T - 1,
    colour_item(goal_term(built_in, Goal), TB, Pos),
    colour_item(goal(built_in, Goal), TB, F-FT),
    colour_item(goal(built_in, Goal), TB, AT-T),
    colourise_file_list(Goal, TB, Elms, TailPos, any).
colourise_goal(Goal, Origin, TB, Pos) :-
    Pos = list_position(F,T,Elms,Tail),
    callable(Goal),
    Goal =.. [_,GH,GT|_],
    !,
    goal_classification(TB, Goal, Origin, Class),
    FT is F + 1,
    AT is T - 1,
    colour_item(goal_term(Class, Goal), TB, Pos),
    colour_item(goal(Class, Goal), TB, F-FT),
    colour_item(goal(Class, Goal), TB, AT-T),
    colourise_list_args(Elms, Tail, [GH|GT], TB, classify).
colourise_goal(Goal, _Origin, TB, Pos) :-
    Pos = quasi_quotation_position(_F,_T,_QQType,_QQTypePos,_CPos),
    !,
    colourise_term_arg(Goal, TB, Pos).
colourise_goal(#(Macro), Origin, TB, term_position(_,_,HF,HT,[MPos])) :-
    expand_macro(TB, Macro, Goal),
    !,
    macro_term_string(Goal, String),
    goal_classification(TB, Goal, Origin, Class),
    (   MPos = term_position(_,_,FF,FT,_ArgPos)
    ->  FPos = FF-FT
    ;   FPos = MPos
    ),
    colour_item(macro(String), TB, HF-HT),
    colour_item(goal_term(Class, Goal), TB, MPos),
    colour_item(goal(Class, Goal), TB, FPos),
    colourise_goal_args(Goal, TB, MPos).
colourise_goal(Goal, Origin, TB, Pos) :-
    strip_module(Goal, _, PGoal),
    nonvar(PGoal),
    (   goal_classification(TB, Goal, Origin, ClassInferred),
        call_goal_colours(Goal, ClassInferred, ClassSpec-ArgSpecs)
    ->  true
    ;   call_goal_colours(Goal, ClassSpec-ArgSpecs)
    ),
    !,                                          % specified
    functor_position(Pos, FPos, ArgPos),
    (   ClassSpec == classify
    ->  goal_classification(TB, Goal, Origin, Class)
    ;   Class = ClassSpec
    ),
    colour_item(goal_term(Class, Goal), TB, Pos),
    colour_item(goal(Class, Goal), TB, FPos),
    colour_dict_braces(TB, Pos),
    specified_items(ArgSpecs, Goal, TB, ArgPos).
colourise_goal(Module:Goal, _Origin, TB, QGoalPos) :-
    QGoalPos = term_position(_,_,QF,QT,[PM,PG]),
    !,
    colourise_module(Module, TB, PM),
    colour_item(functor, TB, QF-QT),
    (   PG = term_position(_,_,FF,FT,_)
    ->  FP = FF-FT
    ;   FP = PG
    ),
    (   callable(Goal)
    ->  qualified_goal_classification(Module:Goal, TB, Class),
        colour_item(goal_term(Class, Goal), TB, QGoalPos),
        colour_item(goal(Class, Goal), TB, FP),
        colourise_goal_args(Goal, Module, TB, PG)
    ;   var(Goal)
    ->  colourise_term_arg(Goal, TB, PG)
    ;   colour_item(type_error(callable), TB, PG)
    ).
colourise_goal(Op, _Origin, TB, Pos) :-
    nonvar(Op),
    Op = op(_,_,_),
    !,
    colourise_op_declaration(Op, TB, Pos).
colourise_goal(Goal, Origin, TB, Pos) :-
    goal_classification(TB, Goal, Origin, Class),
    (   Pos = term_position(_,_,FF,FT,_ArgPos)
    ->  FPos = FF-FT
    ;   FPos = Pos
    ),
    colour_item(goal_term(Class, Goal), TB, Pos),
    colour_item(goal(Class, Goal), TB, FPos),
    colourise_goal_args(Goal, TB, Pos).

% make sure to emit a fragment for the braces of tag{k:v, ...} or
% {...} that is mapped to something else.

colour_dict_braces(TB, dict_position(_F,T,_TF,TT,_KVPos)) :-
    !,
    BStart is TT+1,
    colour_item(dict_content, TB, BStart-T).
colour_dict_braces(_, _).

%!  colourise_goal_args(+Goal, +TB, +Pos)
%
%   Colourise the arguments to a goal. This predicate deals with
%   meta- and database-access predicates.

colourise_goal_args(Goal, TB, Pos) :-
    colourization_module(TB, Module),
    colourise_goal_args(Goal, Module, TB, Pos).

colourization_module(TB, Module) :-
    (   colour_state_source_id(TB, SourceId),
        xref_module(SourceId, Module)
    ->  true
    ;   Module = user
    ).

colourise_goal_args(Goal, M, TB, term_position(_,_,_,_,ArgPos)) :-
    !,
    (   meta_args(Goal, TB, MetaArgs)
    ->  colourise_meta_args(1, Goal, M, MetaArgs, TB, ArgPos)
    ;   colourise_goal_args(1, Goal, M, TB, ArgPos)
    ).
colourise_goal_args(Goal, M, TB, brace_term_position(_,_,ArgPos)) :-
    !,
    (   meta_args(Goal, TB, MetaArgs)
    ->  colourise_meta_args(1, Goal, M, MetaArgs, TB, [ArgPos])
    ;   colourise_goal_args(1, Goal, M, TB, [ArgPos])
    ).
colourise_goal_args(_, _, _, _).                % no arguments

colourise_goal_args(_, _, _, _, []) :- !.
colourise_goal_args(N, Goal, Module, TB, [P0|PT]) :-
    colourise_option_arg(Goal, Module, N, TB, P0),
    !,
    NN is N + 1,
    colourise_goal_args(NN, Goal, Module, TB, PT).
colourise_goal_args(N, Goal, Module, TB, [P0|PT]) :-
    arg(N, Goal, Arg),
    colourise_term_arg(Arg, TB, P0),
    NN is N + 1,
    colourise_goal_args(NN, Goal, Module, TB, PT).


colourise_meta_args(_, _, _, _, _, []) :- !.
colourise_meta_args(N, Goal, Module, MetaArgs, TB, [P0|PT]) :-
    colourise_option_arg(Goal, Module, N, TB, P0),
    !,
    NN is N + 1,
    colourise_meta_args(NN, Goal, Module, MetaArgs, TB, PT).
colourise_meta_args(N, Goal, Module, MetaArgs, TB, [P0|PT]) :-
    arg(N, Goal, Arg),
    arg(N, MetaArgs, MetaSpec),
    colourise_meta_arg(MetaSpec, Arg, TB, P0),
    NN is N + 1,
    colourise_meta_args(NN, Goal, Module, MetaArgs, TB, PT).

colourise_meta_arg(MetaSpec, Arg, TB, Pos) :-
    nonvar(Arg),
    expand_meta(MetaSpec, Arg, Expanded),
    !,
    colourise_goal(Expanded, [], TB, Pos). % TBD: recursion
colourise_meta_arg(MetaSpec, Arg, TB, Pos) :-
    nonvar(Arg),
    MetaSpec == //,
    !,
    colourise_dcg_goals(Arg, //, TB, Pos).
colourise_meta_arg(_, Arg, TB, Pos) :-
    colourise_term_arg(Arg, TB, Pos).

%!  meta_args(+Goal, +TB, -ArgSpec) is semidet.
%
%   Return a copy of Goal, where   each  meta-argument is an integer
%   representing the number of extra arguments   or  the atom // for
%   indicating a DCG  body.  The   non-meta  arguments  are  unbound
%   variables.
%
%   E.g. meta_args(maplist(foo,x,y), X) --> X = maplist(2,_,_)
%
%   NOTE: this could be cached if performance becomes an issue.

meta_args(Goal, TB, VarGoal) :-
    colour_state_source_id(TB, SourceId),
    xref_meta(SourceId, Goal, _),
    !,
    compound_name_arity(Goal, Name, Arity),
    compound_name_arity(VarGoal, Name, Arity),
    xref_meta(SourceId, VarGoal, MetaArgs),
    instantiate_meta(MetaArgs).

instantiate_meta([]).
instantiate_meta([H|T]) :-
    (   var(H)
    ->  H = 0
    ;   H = V+N
    ->  V = N
    ;   H = //(V)
    ->  V = (//)
    ),
    instantiate_meta(T).

%!  expand_meta(+MetaSpec, +Goal, -Expanded) is semidet.
%
%   Add extra arguments to the goal if the meta-specifier is an
%   integer (see above).

expand_meta(MetaSpec, Goal, Goal) :-
    MetaSpec == 0.
expand_meta(MetaSpec, M:Goal, M:Expanded) :-
    atom(M),
    !,
    expand_meta(MetaSpec, Goal, Expanded).
expand_meta(MetaSpec, Goal, Expanded) :-
    integer(MetaSpec),
    MetaSpec > 0,
    (   atom(Goal)
    ->  functor(Expanded, Goal, MetaSpec)
    ;   compound(Goal)
    ->  compound_name_arguments(Goal, Name, Args0),
        length(Extra, MetaSpec),
        append(Args0, Extra, Args),
        compound_name_arguments(Expanded, Name, Args)
    ).

%!  colourise_setof(+Term, +TB, +Pos)
%
%   Colourise the 2nd argument of setof/bagof

colourise_setof(Var^G, TB, term_position(_,_,FF,FT,[VP,GP])) :-
    !,
    colourise_term_arg(Var, TB, VP),
    colour_item(ext_quant, TB, FF-FT),
    colourise_setof(G, TB, GP).
colourise_setof(Term, TB, Pos) :-
    colourise_goal(Term, [], TB, Pos).

%       colourise_db(+Arg, +TB, +Pos)
%
%       Colourise database modification calls (assert/1, retract/1 and
%       friends.

colourise_db((Head:-Body), TB, term_position(_,_,_,_,[HP,BP])) :-
    !,
    colourise_db(Head, TB, HP),
    colourise_body(Body, Head, TB, BP).
colourise_db(Module:Head, TB, term_position(_,_,QF,QT,[MP,HP])) :-
    !,
    colourise_module(Module, TB, MP),
    colour_item(functor, TB, QF-QT),
    (   atom(Module),
        colour_state_source_id(TB, SourceId),
        xref_module(SourceId, Module)
    ->  colourise_db(Head, TB, HP)
    ;   colourise_db(Head, TB, HP)
    ).
colourise_db(Head, TB, Pos) :-
    colourise_goal(Head, '<db-change>', TB, Pos).


%!  colourise_option_args(+Goal, +Module, +Arg:integer,
%!                        +TB, +ArgPos) is semidet.
%
%   Colourise  predicate  options  for  the    Arg-th   argument  of
%   Module:Goal

colourise_option_arg(Goal, Module, Arg, TB, ArgPos) :-
    goal_name_arity(Goal, Name, Arity),
    current_option_arg(Module:Name/Arity, Arg),
    current_predicate_options(Module:Name/Arity, Arg, OptionDecl),
    debug(emacs, 'Colouring option-arg ~w of ~p',
          [Arg, Module:Name/Arity]),
    arg(Arg, Goal, Options),
    colourise_option(Options, Module, Goal, Arg, OptionDecl, TB, ArgPos).

colourise_option(Options0, Module, Goal, Arg, OptionDecl, TB, Pos0) :-
    strip_option_module_qualifier(Goal, Module, Arg, TB,
                                  Options0, Pos0, Options, Pos),
    (   Pos = list_position(F, T, ElmPos, TailPos)
    ->  colour_item(list, TB, F-T),
        colourise_option_list(Options, OptionDecl, TB, ElmPos, TailPos)
    ;   (   var(Options)
        ;   Options == []
        )
    ->  colourise_term_arg(Options, TB, Pos)
    ;   colour_item(type_error(list), TB, Pos)
    ).

strip_option_module_qualifier(Goal, Module, Arg, TB,
                              M:Options, term_position(_,_,_,_,[MP,Pos]),
                              Options, Pos) :-
    predicate_property(Module:Goal, meta_predicate(Head)),
    arg(Arg, Head, :),
    !,
    colourise_module(M, TB, MP).
strip_option_module_qualifier(_, _, _, _,
                              Options, Pos, Options, Pos).


colourise_option_list(_, _, _, [], none) :- !.
colourise_option_list(Tail, _, TB, [], TailPos) :-
    !,
    colourise_term_arg(Tail, TB, TailPos).
colourise_option_list([H|T], OptionDecl, TB, [HPos|TPos], TailPos) :-
    colourise_option(H, OptionDecl, TB, HPos),
    colourise_option_list(T, OptionDecl, TB, TPos, TailPos).

colourise_option(Opt, _, TB, Pos) :-
    var(Opt),
    !,
    colourise_term_arg(Opt, TB, Pos).
colourise_option(Opt, OptionDecl, TB, term_position(_,_,FF,FT,ValPosList)) :-
    !,
    generalise_term(Opt, GenOpt),
    (   memberchk(GenOpt, OptionDecl)
    ->  colour_item(option_name, TB, FF-FT),
        Opt =.. [Name|Values],
        GenOpt =.. [Name|Types],
        colour_option_values(Values, Types, TB, ValPosList)
    ;   colour_item(no_option_name, TB, FF-FT),
        colourise_term_args(ValPosList, 1, Opt, TB)
    ).
colourise_option(_, _, TB, Pos) :-
    colour_item(type_error(option), TB, Pos).

colour_option_values([], [], _, _).
colour_option_values([V0|TV], [T0|TT], TB, [P0|TP]) :-
    (   (   var(V0)
        ;   is_of_type(T0, V0)
        ;   T0 = list(_),
            member(E, V0),
            var(E)
        ;   dict_field_extraction(V0)
        )
    ->  colourise_term_arg(V0, TB, P0)
    ;   callable(V0),
        (   T0 = callable
        ->  N = 0
        ;   T0 = (callable+N)
        )
    ->  colourise_meta_arg(N, V0, TB, P0)
    ;   colour_item(type_error(T0), TB, P0)
    ),
    colour_option_values(TV, TT, TB, TP).


%!  colourise_files(+Arg, +TB, +Pos, +Why)
%
%   Colourise the argument list of one of the file-loading predicates.
%
%   @param Why is one of =any= or =imported=

colourise_files(List, TB, list_position(F,T,Elms,TailPos), Why) :-
    !,
    colour_item(list, TB, F-T),
    colourise_file_list(List, TB, Elms, TailPos, Why).
colourise_files(M:Spec, TB, term_position(_,_,_,_,[MP,SP]), Why) :-
    !,
    colourise_module(M, TB, MP),
    colourise_files(Spec, TB, SP, Why).
colourise_files(Var, TB, P, _) :-
    var(Var),
    !,
    colour_item(var, TB, P).
colourise_files(Spec0, TB, Pos, Why) :-
    strip_module(Spec0, _, Spec),
    (   colour_state_source_id(TB, Source),
        prolog_canonical_source(Source, SourceId),
        catch(xref_source_file(Spec, Path, SourceId, [silent(true)]),
              _, fail)
    ->  (   Why = imported,
            \+ resolves_anything(TB, Path),
            exports_something(TB, Path)
        ->  colour_item(file_no_depend(Path), TB, Pos)
        ;   colour_item(file(Path), TB, Pos)
        )
    ;   colour_item(nofile, TB, Pos)
    ).

%!  colourise_file_list(+Files, +TB, +ElmPos, +TailPos, +Why)

colourise_file_list([], _, [], none, _).
colourise_file_list(Last, TB, [], TailPos, _Why) :-
    (   var(Last)
    ->  colourise_term(Last, TB, TailPos)
    ;   colour_item(type_error(list), TB, TailPos)
    ).
colourise_file_list([H|T], TB, [PH|PT], TailPos, Why) :-
    colourise_files(H, TB, PH, Why),
    colourise_file_list(T, TB, PT, TailPos, Why).

resolves_anything(TB, Path) :-
    colour_state_source_id(TB, SourceId),
    xref_defined(SourceId, Head, imported(Path)),
    xref_called(SourceId, Head, _),
    !.

exports_something(TB, Path) :-
    colour_state_source_id(TB, SourceId),
    xref_defined(SourceId, _, imported(Path)),
    !.

%!  colourise_directory(+Arg, +TB, +Pos)
%
%   Colourise argument that should be an existing directory.

colourise_directory(Spec, TB, Pos) :-
    (   colour_state_source_id(TB, SourceId),
        catch(xref_source_file(Spec, Path, SourceId,
                               [ file_type(directory),
                                 silent(true)
                               ]),
              _, fail)
    ->  colour_item(directory(Path), TB, Pos)
    ;   colour_item(nofile, TB, Pos)
    ).

%!  colourise_langoptions(+Term, +TB, +Pos) is det.
%
%   Colourise the 3th argument of module/3

colourise_langoptions([], _, _) :- !.
colourise_langoptions([H|T], TB, list_position(PF,PT,[HP|TP],_)) :-
    !,
    colour_item(list, TB, PF-PT),
    colourise_langoptions(H, TB, HP),
    colourise_langoptions(T, TB, TP).
colourise_langoptions(Spec, TB, Pos) :-
    colourise_files(library(dialect/Spec), TB, Pos, imported).

%!  colourise_class(ClassName, TB, Pos)
%
%   Colourise an XPCE class.

colourise_class(ClassName, TB, Pos) :-
    colour_state_source_id(TB, SourceId),
    classify_class(SourceId, ClassName, Classification),
    colour_item(class(Classification, ClassName), TB, Pos).

%!  classify_class(+SourceId, +ClassName, -Classification)
%
%   Classify an XPCE class. As long as   this code is in this module
%   rather than using hooks, we do not   want to load xpce unless it
%   is already loaded.

classify_class(SourceId, Name, Class) :-
    xref_defined_class(SourceId, Name, Class),
    !.
classify_class(_SourceId, Name, Class) :-
    current_predicate(pce:send_class/3),
    (   current_predicate(classify_class/2)
    ->  true
    ;   use_module(library(pce_meta), [classify_class/2])
    ),
    member(G, [classify_class(Name, Class)]),
    call(G).

%!  colourise_term_args(+Term, +TB, +Pos)
%
%   colourise head/body principal terms.

colourise_term_args(Term, TB,
                    term_position(_,_,_,_,ArgPos)) :-
    !,
    colourise_term_args(ArgPos, 1, Term, TB).
colourise_term_args(_, _, _).

colourise_term_args([], _, _, _).
colourise_term_args([Pos|T], N, Term, TB) :-
    arg(N, Term, Arg),
    colourise_term_arg(Arg, TB, Pos),
    NN is N + 1,
    colourise_term_args(T, NN, Term, TB).

%!  colourise_term_arg(+Term, +TB, +Pos)
%
%   Colourise an arbitrary Prolog term without context of its semantical
%   role.

colourise_term_arg(_, _, Pos) :-
    var(Pos),
    !.
colourise_term_arg(Arg, TB, parentheses_term_position(PO,PC,Pos)) :-
    !,
    colour_item(parentheses, TB, PO-PC),
    colourise_term_arg(Arg, TB, Pos).
colourise_term_arg(Var, TB, Pos) :-                     % variable
    var(Var), Pos = _-_,
    !,
    (   singleton(Var, TB)
    ->  colour_item(singleton, TB, Pos)
    ;   current_variable(Var, TB)
    ->  colour_item(current_variable, TB, Pos)
    ;   colour_item(var, TB, Pos)
    ).
colourise_term_arg(List, TB, list_position(F, T, Elms, Tail)) :-
    !,
    colour_item(list, TB, F-T),
    colourise_list_args(Elms, Tail, List, TB, classify).    % list
colourise_term_arg(String, TB, string_position(F, T)) :-    % string
    !,
    (   string(String)
    ->  colour_item(string, TB, F-T)
    ;   String = [H|_]
    ->  (   integer(H)
        ->  colour_item(codes, TB, F-T)
        ;   colour_item(chars, TB, F-T)
        )
    ;   String == []
    ->  colour_item(codes, TB, F-T)
    ).
colourise_term_arg(_, TB,
                   quasi_quotation_position(F,T,QQType,QQTypePos,CPos)) :-
    !,
    colourise_qq_type(QQType, TB, QQTypePos),
    functor_name(QQType, Type),
    colour_item(qq_content(Type), TB, CPos),
    arg(1, CPos, SE),
    SS is SE-2,
    FE is F+2,
    TS is T-2,
    colour_item(qq(open),  TB, F-FE),
    colour_item(qq(sep),   TB, SS-SE),
    colour_item(qq(close), TB, TS-T).
colourise_term_arg({Term}, TB, brace_term_position(F,T,Arg)) :-
    !,
    colour_item(brace_term, TB, F-T),
    colourise_term_arg(Term, TB, Arg).
colourise_term_arg(Map, TB, dict_position(F,T,TF,TT,KVPos)) :-
    !,
    is_dict(Map, Tag),
    colour_item(dict, TB, F-T),
    TagPos = TF-TT,
    (   var(Tag)
    ->  (   singleton(Tag, TB)
        ->  colour_item(singleton, TB, TagPos)
        ;   colour_item(var, TB, TagPos)
        )
    ;   colour_item(dict_tag, TB, TagPos)
    ),
    BStart is TT+1,
    colour_item(dict_content, TB, BStart-T),
    colourise_dict_kv(Map, TB, KVPos).
colourise_term_arg([](List,Term), TB,                   % [] as operator
                   term_position(_,_,0,0,[ListPos,ArgPos])) :-
    !,
    colourise_term_arg(List, TB, ListPos),
    colourise_term_arg(Term, TB, ArgPos).
colourise_term_arg(#(Macro), TB, term_position(_,_,HF,HT,[MPos])) :-
    expand_macro(TB, Macro, Term),
    !,
    macro_term_string(Term, String),
    colour_item(macro(String), TB, HF-HT),
    colourise_term_arg(Macro, TB, MPos).
colourise_term_arg(Compound, TB, Pos) :-                % compound
    compound(Compound),
    !,
    (   Pos = term_position(_F,_T,FF,FT,_ArgPos)
    ->  colour_item(functor, TB, FF-FT)             % TBD: Infix/Postfix?
    ;   true                                        % TBD: When is this
    ),
    colourise_term_args(Compound, TB, Pos).
colourise_term_arg(EmptyList, TB, Pos) :-
    EmptyList == [],
    !,
    colour_item(empty_list, TB, Pos).
colourise_term_arg(Atom, TB, Pos) :-
    atom(Atom),
    !,
    colour_item(atom, TB, Pos).
colourise_term_arg(Integer, TB, Pos) :-
    integer(Integer),
    !,
    colour_item(int, TB, Pos).
colourise_term_arg(Rational, TB, Pos) :-
    rational(Rational),
    !,
    colour_item(rational(Rational), TB, Pos).
colourise_term_arg(Float, TB, Pos) :-
    float(Float),
    !,
    colour_item(float, TB, Pos).
colourise_term_arg(_Arg, _TB, _Pos) :-
    true.

colourise_list_args([HP|TP], Tail, [H|T], TB, How) :-
    specified_item(How, H, TB, HP),
    colourise_list_args(TP, Tail, T, TB, How).
colourise_list_args([], none, _, _, _) :- !.
colourise_list_args([], TP, T, TB, How) :-
    specified_item(How, T, TB, TP).


%!  colourise_expression(+Term, +TB, +Pos)
%
%   colourise arithmetic expressions.

colourise_expression(_, _, Pos) :-
    var(Pos),
    !.
colourise_expression(Arg, TB, parentheses_term_position(PO,PC,Pos)) :-
    !,
    colour_item(parentheses, TB, PO-PC),
    colourise_expression(Arg, TB, Pos).
colourise_expression(Compound, TB, Pos) :-
    compound(Compound), Pos = term_position(_F,_T,FF,FT,_ArgPos),
    !,
    (   dict_field_extraction(Compound)
    ->  colourise_term_arg(Compound, TB, Pos)
    ;   (   current_arithmetic_function(Compound)
        ->  colour_item(function, TB, FF-FT)
        ;   colour_item(no_function, TB, FF-FT)
        ),
        colourise_expression_args(Compound, TB, Pos)
    ).
colourise_expression(Atom, TB, Pos) :-
    atom(Atom),
    !,
    (   current_arithmetic_function(Atom)
    ->  colour_item(function, TB, Pos)
    ;   colour_item(no_function, TB, Pos)
    ).
colourise_expression(NumOrVar, TB, Pos) :-
    Pos = _-_,
    !,
    colourise_term_arg(NumOrVar, TB, Pos).
colourise_expression(_Arg, TB, Pos) :-
    colour_item(type_error(evaluable), TB, Pos).

dict_field_extraction(Term) :-
    compound(Term),
    compound_name_arity(Term, '.', 2),
    Term \= [_|_].                        % traditional mode


colourise_expression_args(roundtoward(Expr, Mode), TB,
                          term_position(_,_,_,_,[ExprPos, ModePos])) :-
    !,
    colourise_expression(Expr, TB, ExprPos),
    colourise_round_mode(Mode, TB, ModePos).
colourise_expression_args(Term, TB,
                          term_position(_,_,_,_,ArgPos)) :-
    !,
    colourise_expression_args(ArgPos, 1, Term, TB).
colourise_expression_args(_, _, _).

colourise_expression_args([], _, _, _).
colourise_expression_args([Pos|T], N, Term, TB) :-
    arg(N, Term, Arg),
    colourise_expression(Arg, TB, Pos),
    NN is N + 1,
    colourise_expression_args(T, NN, Term, TB).

colourise_round_mode(Mode, TB, Pos) :-
    var(Mode),
    !,
    colourise_term_arg(Mode, TB, Pos).
colourise_round_mode(Mode, TB, Pos) :-
    round_mode(Mode),
    !,
    colour_item(identifier, TB, Pos).
colourise_round_mode(_Mode, TB, Pos) :-
    colour_item(domain_error(rounding_mode), TB, Pos).

round_mode(to_nearest).
round_mode(to_positive).
round_mode(to_negative).
round_mode(to_zero).

%!  colourise_qq_type(+QQType, +TB, +QQTypePos)
%
%   Colouring the type part of a quasi quoted term

colourise_qq_type(QQType, TB, QQTypePos) :-
    functor_position(QQTypePos, FPos, _),
    colour_item(qq_type, TB, FPos),
    colourise_term_args(QQType, TB, QQTypePos).

qq_position(quasi_quotation_position(_,_,_,_,_)).

%!  colourise_dict_kv(+Dict, +TB, +KVPosList)
%
%   Colourise the name-value pairs in the dict

colourise_dict_kv(_, _, []) :- !.
colourise_dict_kv(Dict, TB, [key_value_position(_F,_T,SF,ST,K,KP,VP)|KV]) :-
    colour_item(dict_key, TB, KP),
    colour_item(dict_sep, TB, SF-ST),
    get_dict(K, Dict, V),
    colourise_term_arg(V, TB, VP),
    colourise_dict_kv(Dict, TB, KV).


%!  colourise_exports(+List, +TB, +Pos)
%
%   Colourise the module export-list (or any other list holding
%   terms of the form Name/Arity referring to predicates).

colourise_exports([], TB, Pos) :- !,
    colourise_term_arg([], TB, Pos).
colourise_exports(List, TB, list_position(F,T,ElmPos,Tail)) :-
    !,
    colour_item(list, TB, F-T),
    (   Tail == none
    ->  true
    ;   colour_item(type_error(list), TB, Tail)
    ),
    colourise_exports2(List, TB, ElmPos).
colourise_exports(_, TB, Pos) :-
    colour_item(type_error(list), TB, Pos).

colourise_exports2([G0|GT], TB, [P0|PT]) :-
    !,
    colourise_declaration(G0, export, TB, P0),
    colourise_exports2(GT, TB, PT).
colourise_exports2(_, _, _).


%!  colourise_imports(+List, +File, +TB, +Pos)
%
%   Colourise import list from use_module/2, importing from File.

colourise_imports(List, File, TB, Pos) :-
    (   colour_state_source_id(TB, SourceId),
        ground(File),
        catch(xref_public_list(File, SourceId,
                               [ path(Path),
                                 public(Public),
                                 silent(true)
                               ] ), _, fail)
    ->  true
    ;   Public = [],
        Path = (-)
    ),
    colourise_imports(List, Path, Public, TB, Pos).

colourise_imports([], _, _, TB, Pos) :-
    !,
    colour_item(empty_list, TB, Pos).
colourise_imports(List, File, Public, TB, list_position(F,T,ElmPos,Tail)) :-
    !,
    colour_item(list, TB, F-T),
    (   Tail == none
    ->  true
    ;   colour_item(type_error(list), TB, Tail)
    ),
    colourise_imports2(List, File, Public, TB, ElmPos).
colourise_imports(except(Except), File, Public, TB,
                  term_position(_,_,FF,FT,[LP])) :-
    !,
    colour_item(keyword(except), TB, FF-FT),
    colourise_imports(Except, File, Public, TB, LP).
colourise_imports(_, _, _, TB, Pos) :-
    colour_item(type_error(list), TB, Pos).

colourise_imports2([G0|GT], File, Public, TB, [P0|PT]) :-
    !,
    colourise_import(G0, File, TB, P0),
    colourise_imports2(GT, File, Public, TB, PT).
colourise_imports2(_, _, _, _, _).


colourise_import(PI as Name, File, TB, term_position(_,_,FF,FT,[PP,NP])) :-
    pi_to_term(PI, Goal),
    !,
    colour_item(goal(imported(File), Goal), TB, PP),
    rename_goal(Goal, Name, NewGoal),
    goal_classification(TB, NewGoal, [], Class),
    colour_item(goal(Class, NewGoal), TB, NP),
    colour_item(keyword(as), TB, FF-FT).
colourise_import(PI, File, TB, Pos) :-
    pi_to_term(PI, Goal),
    colour_state_source_id(TB, SourceID),
    (   \+ xref_defined(SourceID, Goal, imported(File))
    ->  colour_item(undefined_import, TB, Pos)
    ;   \+ xref_called(SourceID, Goal, _)
    ->  colour_item(unused_import, TB, Pos)
    ),
    !.
colourise_import(PI, _, TB, Pos) :-
    colourise_declaration(PI, import, TB, Pos).

%!  colourise_declaration(+Decl, ?Which, +TB, +Pos) is det.
%
%   Colourise declaration sequences as used  by module/2, dynamic/1,
%   etc.

colourise_declaration(PI, _, TB, term_position(F,T,FF,FT,[NamePos,ArityPos])) :-
    pi_to_term(PI, Goal),
    !,
    goal_classification(TB, Goal, [], Class),
    colour_item(predicate_indicator(Class, Goal), TB, F-T),
    colour_item(goal(Class, Goal), TB, NamePos),
    colour_item(predicate_indicator, TB, FF-FT),
    colour_item(arity, TB, ArityPos).
colourise_declaration(Module:PI, _, TB,
                      term_position(_,_,QF,QT,[PM,PG])) :-
    atom(Module), pi_to_term(PI, Goal),
    !,
    colourise_module(M, TB, PM),
    colour_item(functor, TB, QF-QT),
    colour_item(predicate_indicator(extern(M), Goal), TB, PG),
    PG = term_position(_,_,FF,FT,[NamePos,ArityPos]),
    colour_item(goal(extern(M), Goal), TB, NamePos),
    colour_item(predicate_indicator, TB, FF-FT),
    colour_item(arity, TB, ArityPos).
colourise_declaration(Module:PI, _, TB,
                      term_position(_,_,QF,QT,[PM,PG])) :-
    atom(Module), nonvar(PI), PI = Name/Arity,
    !,                                  % partial predicate indicators
    colourise_module(Module, TB, PM),
    colour_item(functor, TB, QF-QT),
    (   (var(Name) ; atom(Name)),
        (var(Arity) ; integer(Arity), Arity >= 0)
    ->  colourise_term_arg(PI, TB, PG)
    ;   colour_item(type_error(predicate_indicator), TB, PG)
    ).
colourise_declaration(op(N,T,P), Which, TB, Pos) :-
    (   Which == export
    ;   Which == import
    ),
    !,
    colour_item(exported_operator, TB, Pos),
    colourise_op_declaration(op(N,T,P), TB, Pos).
colourise_declaration(Module:Goal, table, TB,
                      term_position(_,_,QF,QT,
                                    [PM,term_position(_F,_T,FF,FT,ArgPos)])) :-
    atom(Module), callable(Goal),
    !,
    colourise_module(Module, TB, PM),
    colour_item(functor, TB, QF-QT),
    goal_classification(TB, Module:Goal, [], Class),
    compound_name_arguments(Goal, _, Args),
    colour_item(goal(Class, Goal), TB, FF-FT),
    colourise_table_modes(Args, TB, ArgPos).
colourise_declaration(Goal, table, TB, term_position(_F,_T,FF,FT,ArgPos)) :-
    callable(Goal),
    !,
    compound_name_arguments(Goal, _, Args),
    goal_classification(TB, Goal, [], Class),
    colour_item(goal(Class, Goal), TB, FF-FT),
    colourise_table_modes(Args, TB, ArgPos).
colourise_declaration(Goal, table, TB, Pos) :-
    atom(Goal),
    !,
    goal_classification(TB, Goal, [], Class),
    colour_item(goal(Class, Goal), TB, Pos).
colourise_declaration(Partial, _Which, TB, Pos) :-
    compatible_with_pi(Partial),
    !,
    colourise_term_arg(Partial, TB, Pos).
colourise_declaration(_, Which, TB, Pos) :-
    colour_item(type_error(declaration(Which)), TB, Pos).

compatible_with_pi(Term) :-
    var(Term),
    !.
compatible_with_pi(Name/Arity) :-
    !,
    var_or_atom(Name),
    var_or_nonneg(Arity).
compatible_with_pi(Name//Arity) :-
    !,
    var_or_atom(Name),
    var_or_nonneg(Arity).
compatible_with_pi(M:T) :-
    var_or_atom(M),
    compatible_with_pi(T).

var_or_atom(X) :- var(X), !.
var_or_atom(X) :- atom(X).
var_or_nonneg(X) :- var(X), !.
var_or_nonneg(X) :- integer(X), X >= 0, !.

pi_to_term(Name/Arity, Term) :-
    (atom(Name)->true;Name==[]), integer(Arity), Arity >= 0,
    !,
    functor(Term, Name, Arity).
pi_to_term(Name//Arity0, Term) :-
    atom(Name), integer(Arity0), Arity0 >= 0,
    !,
    Arity is Arity0 + 2,
    functor(Term, Name, Arity).

colourise_meta_declarations((Head,Tail), Extra, TB,
                            term_position(_,_,_,_,[PH,PT])) :-
    !,
    colourise_meta_declaration(Head, Extra, TB, PH),
    colourise_meta_declarations(Tail, Extra, TB, PT).
colourise_meta_declarations(Last, Extra, TB, Pos) :-
    colourise_meta_declaration(Last, Extra, TB, Pos).

colourise_meta_declaration(M:Head, Extra, TB,
                           term_position(_,_,QF,QT,
                                         [ MP,
                                           term_position(_,_,FF,FT,ArgPos)
                                         ])) :-
    compound(Head),
    !,
    colourise_module(M, TB, MP),
    colour_item(functor, TB, QF-QT),
    colour_item(goal(extern(M),Head), TB, FF-FT),
    compound_name_arguments(Head, _, Args),
    colourise_meta_decls(Args, Extra, TB, ArgPos).
colourise_meta_declaration(Head, Extra, TB, term_position(_,_,FF,FT,ArgPos)) :-
    compound(Head),
    !,
    goal_classification(TB, Head, [], Class),
    colour_item(goal(Class, Head), TB, FF-FT),
    compound_name_arguments(Head, _, Args),
    colourise_meta_decls(Args, Extra, TB, ArgPos).
colourise_meta_declaration([H|T], Extra, TB, list_position(LF,LT,[HP],TP)) :-
    !,
    colour_item(list, TB, LF-LT),
    colourise_meta_decls([H,T], Extra, TB, [HP,TP]).
colourise_meta_declaration(_, _, TB, Pos) :-
    !,
    colour_item(type_error(compound), TB, Pos).

colourise_meta_decls([], _, _, []).
colourise_meta_decls([Arg|ArgT], Extra, TB, [PosH|PosT]) :-
    colourise_meta_decl(Arg, Extra, TB, PosH),
    colourise_meta_decls(ArgT, Extra, TB, PosT).

colourise_meta_decl(Arg, Extra, TB, Pos) :-
    nonvar(Arg),
    (   valid_meta_decl(Arg)
    ->  true
    ;   memberchk(Arg, Extra)
    ),
    colour_item(meta(Arg), TB, Pos).
colourise_meta_decl(_, _, TB, Pos) :-
    colour_item(error, TB, Pos).

valid_meta_decl(:).
valid_meta_decl(*).
valid_meta_decl(//).
valid_meta_decl(^).
valid_meta_decl(?).
valid_meta_decl(+).
valid_meta_decl(-).
valid_meta_decl(I) :- integer(I), between(0,9,I).

%!  colourise_declarations(+Term, +Which, +TB, +Pos)
%
%   Colourise  specification  for  dynamic/1,   table/1,  etc.  Includes
%   processing options such as ``:- dynamic p/1 as incremental.``.

colourise_declarations(List, Which, TB, list_position(F,T,Elms,none)) :-
    !,
    colour_item(list, TB, F-T),
    colourise_list_declarations(List, Which, TB, Elms).
colourise_declarations(Term, Which, TB, parentheses_term_position(PO,PC,Pos)) :-
    !,
    colour_item(parentheses, TB, PO-PC),
    colourise_declarations(Term, Which, TB, Pos).
colourise_declarations((Head,Tail), Which, TB,
                             term_position(_,_,_,_,[PH,PT])) :-
    !,
    colourise_declarations(Head, Which, TB, PH),
    colourise_declarations(Tail, Which, TB, PT).
colourise_declarations(as(Spec, Options), Which, TB,
                             term_position(_,_,FF,FT,[PH,PT])) :-
    !,
    colour_item(keyword(as), TB, FF-FT),
    colourise_declarations(Spec, Which, TB, PH),
    colourise_decl_options(Options, Which, TB, PT).
colourise_declarations(PI, Which, TB, Pos) :-
    colourise_declaration(PI, Which, TB, Pos).

colourise_list_declarations([], _, _, []).
colourise_list_declarations([H|T], Which, TB, [HP|TP]) :-
    colourise_declaration(H, Which, TB, HP),
    colourise_list_declarations(T, Which, TB, TP).


colourise_table_modes([], _, _).
colourise_table_modes([H|T], TB, [PH|PT]) :-
    colourise_table_mode(H, TB, PH),
    colourise_table_modes(T, TB, PT).

colourise_table_mode(H, TB, Pos) :-
    table_mode(H, Mode),
    !,
    colour_item(table_mode(Mode), TB, Pos).
colourise_table_mode(lattice(Spec), TB, term_position(_F,_T,FF,FT,[ArgPos])) :-
    !,
    colour_item(table_mode(lattice), TB, FF-FT),
    table_moded_call(Spec, 3, TB, ArgPos).
colourise_table_mode(po(Spec), TB, term_position(_F,_T,FF,FT,[ArgPos])) :-
    !,
    colour_item(table_mode(po), TB, FF-FT),
    table_moded_call(Spec, 2, TB, ArgPos).
colourise_table_mode(_, TB, Pos) :-
    colour_item(type_error(table_mode), TB, Pos).

table_mode(Var, index) :-
    var(Var),
    !.
table_mode(+, index).
table_mode(index, index).
table_mode(-, first).
table_mode(first, first).
table_mode(last, last).
table_mode(min, min).
table_mode(max, max).
table_mode(sum, sum).

table_moded_call(Atom, Arity, TB, Pos) :-
    atom(Atom),
    functor(Head, Atom, Arity),
    goal_classification(TB, Head, [], Class),
    colour_item(goal(Class, Head), TB, Pos).
table_moded_call(Atom/Arity, Arity, TB,
                 term_position(_,_,FF,FT,[NP,AP])) :-
    atom(Atom),
    !,
    functor(Head, Atom, Arity),
    goal_classification(TB, Head, [], Class),
    colour_item(goal(Class, Head), TB, NP),
    colour_item(predicate_indicator, TB, FF-FT),
    colour_item(arity, TB, AP).
table_moded_call(Head, Arity, TB, Pos) :-
    Pos = term_position(_,_,FF,FT,_),
    compound(Head),
    !,
    compound_name_arity(Head, _Name, Arity),
    goal_classification(TB, Head, [], Class),
    colour_item(goal(Class, Head), TB, FF-FT),
    colourise_term_args(Head, TB, Pos).
table_moded_call(_, _, TB, Pos) :-
    colour_item(type_error(predicate_name_or_indicator), TB, Pos).

colourise_decl_options(Options, Which, TB,
                       parentheses_term_position(_,_,Pos)) :-
    !,
    colourise_decl_options(Options, Which, TB, Pos).
colourise_decl_options((Head,Tail), Which, TB,
                        term_position(_,_,_,_,[PH,PT])) :-
    !,
    colourise_decl_options(Head, Which, TB, PH),
    colourise_decl_options(Tail, Which, TB, PT).
colourise_decl_options(Option, Which, TB, Pos) :-
    ground(Option),
    valid_decl_option(Option, Which),
    !,
    functor(Option, Name, _),
    (   Pos = term_position(_,_,FF,FT,[ArgPos])
    ->  colour_item(decl_option(Name), TB, FF-FT),
        (   arg(1, Option, Value),
            nonneg_or_false(Value)
        ->  colourise_term_arg(Value, TB, ArgPos)
        ;   colour_item(type_error(decl_option_value(Which)), TB, ArgPos)
        )
    ;   colour_item(decl_option(Name), TB, Pos)
    ).
colourise_decl_options(_, Which, TB, Pos) :-
    colour_item(type_error(decl_option(Which)), TB, Pos).

valid_decl_option(subsumptive,         table).
valid_decl_option(variant,             table).
valid_decl_option(incremental,         table).
valid_decl_option(monotonic,           table).
valid_decl_option(opaque,              table).
valid_decl_option(lazy,                table).
valid_decl_option(monotonic,           dynamic).
valid_decl_option(incremental,         dynamic).
valid_decl_option(abstract(_),         dynamic).
valid_decl_option(opaque,              dynamic).
valid_decl_option(shared,              table).
valid_decl_option(private,             table).
valid_decl_option(subgoal_abstract(_), table).
valid_decl_option(answer_abstract(_),  table).
valid_decl_option(max_answers(_),      table).
valid_decl_option(shared,              dynamic).
valid_decl_option(private,             dynamic).
valid_decl_option(local,               dynamic).
valid_decl_option(multifile,           _).
valid_decl_option(discontiguous,       _).
valid_decl_option(volatile,            _).

nonneg_or_false(Value) :-
    var(Value),
    !.
nonneg_or_false(Value) :-
    integer(Value), Value >= 0,
    !.
nonneg_or_false(off).
nonneg_or_false(false).

%!  colourise_op_declaration(Op, TB, Pos) is det.

colourise_op_declaration(op(P,T,N), TB, term_position(_,_,FF,FT,[PP,TP,NP])) :-
    colour_item(goal(built_in, op(N,T,P)), TB, FF-FT),
    colour_op_priority(P, TB, PP),
    colour_op_type(T, TB, TP),
    colour_op_name(N, TB, NP).

colour_op_name(_, _, Pos) :-
    var(Pos),
    !.
colour_op_name(Name, TB, parentheses_term_position(PO,PC,Pos)) :-
    !,
    colour_item(parentheses, TB, PO-PC),
    colour_op_name(Name, TB, Pos).
colour_op_name(Name, TB, Pos) :-
    var(Name),
    !,
    colour_item(var, TB, Pos).
colour_op_name(Name, TB, Pos) :-
    (atom(Name) ; Name == []),
    !,
    colour_item(identifier, TB, Pos).
colour_op_name(Module:Name, TB, term_position(_F,_T,QF,QT,[MP,NP])) :-
    !,
    colourise_module(Module, TB, MP),
    colour_item(functor, TB, QF-QT),
    colour_op_name(Name, TB, NP).
colour_op_name(List, TB, list_position(F,T,Elems,none)) :-
    !,
    colour_item(list, TB, F-T),
    colour_op_names(List, TB, Elems).
colour_op_name(_, TB, Pos) :-
    colour_item(error, TB, Pos).

colour_op_names([], _, []).
colour_op_names([H|T], TB, [HP|TP]) :-
    colour_op_name(H, TB, HP),
    colour_op_names(T, TB, TP).

colour_op_type(Type, TB, Pos) :-
    var(Type),
    !,
    colour_item(var, TB, Pos).
colour_op_type(Type, TB, Pos) :-
    op_type(Type),
    !,
    colour_item(op_type(Type), TB, Pos).
colour_op_type(_, TB, Pos) :-
    colour_item(error, TB, Pos).

colour_op_priority(Priority, TB, Pos) :-
    var(Priority), colour_item(var, TB, Pos).
colour_op_priority(Priority, TB, Pos) :-
    integer(Priority),
    between(0, 1200, Priority),
    !,
    colour_item(int, TB, Pos).
colour_op_priority(_, TB, Pos) :-
    colour_item(error, TB, Pos).

op_type(fx).
op_type(fy).
op_type(xf).
op_type(yf).
op_type(xfy).
op_type(xfx).
op_type(yfx).


%!  colourise_prolog_flag_name(+Name, +TB, +Pos)
%
%   Colourise the name of a Prolog flag

colourise_prolog_flag_name(_, _, Pos) :-
    var(Pos),
    !.
colourise_prolog_flag_name(Name, TB, parentheses_term_position(PO,PC,Pos)) :-
    !,
    colour_item(parentheses, TB, PO-PC),
    colourise_prolog_flag_name(Name, TB, Pos).
colourise_prolog_flag_name(Name, TB, Pos) :-
    atom(Name),
    !,
    (   current_prolog_flag(Name, _)
    ->  colour_item(flag_name(Name), TB, Pos)
    ;   known_flag(Name)
    ->  colour_item(known_flag_name(Name), TB, Pos)
    ;   colour_item(no_flag_name(Name), TB, Pos)
    ).
colourise_prolog_flag_name(Name, TB, Pos) :-
    colourise_term(Name, TB, Pos).

% Some flags are know, but can be unset.
known_flag(android).
known_flag(android_api).
known_flag(apple).
known_flag(apple_universal_binary).
known_flag(asan).
known_flag(associated_file).
known_flag(break_level).
known_flag(bundle).
known_flag(conda).
known_flag(dde).
known_flag(emscripten).
known_flag(engines).
known_flag(executable_format).
known_flag(gc_thread).
known_flag(gmp_version).
known_flag(gui).
known_flag(linux).
known_flag(max_rational_size).
known_flag(mitigate_spectre).
known_flag(msys2).
known_flag(pid).
known_flag(pipe).
known_flag(posix_shell).
known_flag(shared_home).
known_flag(shared_table_space).
known_flag(system_thread_id).
known_flag(threads).
known_flag(unix).
known_flag(windows).
known_flag(wine_version).
known_flag(xpce).

		 /*******************************
		 *             MACROS		*
		 *******************************/

%!  expand_macro(+TB, +Macro, -Expanded) is semidet.
%
%   @tbd This only works if the code is compiled. Ideally we'd also make
%   this work for not compiled code.

expand_macro(TB, Macro, Expanded) :-
    colour_state_source_id(TB, SourceId),
    (   xref_module(SourceId, M)
    ->  true
    ;   M = user
    ),
    current_predicate(M:'$macro'/2),
    catch(M:'$macro'(Macro, Expanded),
          error(_, _),
          fail),
    !.

macro_term_string(Term, String) :-
    copy_term_nat(Term, Copy),
    numbervars(Copy, 0, _, [singletons(true)]),
    term_string(Copy, String,
                [ portray(true),
                  max_depth(2),
                  numbervars(true)
                ]).


                 /*******************************
                 *        CONFIGURATION         *
                 *******************************/

%       body_compiled(+Term)
%
%       Succeeds if term is a construct handled by the compiler.

body_compiled((_,_)).
body_compiled((_->_)).
body_compiled((_*->_)).
body_compiled((_;_)).
body_compiled(\+_).

%!  goal_classification(+TB, +Goal, +Origin, -Class)
%
%   Classify Goal appearing in TB and called from a clause with head
%   Origin.  For directives, Origin is [].

goal_classification(_, QGoal, _, Class) :-
    strip_module(QGoal, _, Goal),
    (   var(Goal)
    ->  !, Class = meta
    ;   \+ callable(Goal)
    ->  !, Class = not_callable
    ).
goal_classification(_, Goal, Origin, recursion) :-
    callable(Origin),
    generalise_term(Goal, Origin),
    !.
goal_classification(TB, Goal, _, How) :-
    colour_state_source_id(TB, SourceId),
    xref_defined(SourceId, Goal, How),
    How \= public(_),
    !.
goal_classification(TB, Goal, _, Class) :-
    (   colour_state_source_id(TB, SourceId),
        xref_module(SourceId, Module)
    ->  true
    ;   Module = user
    ),
    call_goal_classification(Goal, Module, Class),
    !.
goal_classification(TB, Goal, _, How) :-
    colour_state_module(TB, Module),
    atom(Module),
    Module \== prolog_colour_ops,
    predicate_property(Module:Goal, imported_from(From)),
    !,
    How = imported(From).
goal_classification(_TB, _Goal, _, undefined).

%!  goal_classification(+Goal, +Module, -Class)
%
%   Multifile hookable classification for non-local goals.

call_goal_classification(Goal, Module, Class) :-
    catch(global_goal_classification(Goal, Module, Class), _,
          Class = type_error(callable)).

global_goal_classification(Goal, _, built_in) :-
    built_in_predicate(Goal),
    !.
global_goal_classification(Goal, _, autoload(From)) :-  % SWI-Prolog
    predicate_property(Goal, autoload(From)).
global_goal_classification(Goal, Module, Class) :-      % SWI-Prolog
    strip_module(Goal, _, PGoal),
    current_predicate(_, user:PGoal),
    !,
    (   Module == user
    ->  Class = global(GClass, Location),
        global_location(user:Goal, Location),
        global_class(user:Goal, GClass)
    ;   Class = global
    ).
global_goal_classification(Goal, _, Class) :-
    compound(Goal),
    compound_name_arity(Goal, Name, Arity),
    vararg_goal_classification(Name, Arity, Class).

global_location(Goal, File:Line) :-
    predicate_property(Goal, file(File)),
    predicate_property(Goal, line_count(Line)),
    !.
global_location(_, -).

global_class(Goal, dynamic)   :- predicate_property(Goal, dynamic), !.
global_class(Goal, multifile) :- predicate_property(Goal, multifile), !.
global_class(Goal, tabled)    :- predicate_property(Goal, tabled), !.
global_class(_,    static).


%!  vararg_goal_classification(+Name, +Arity, -Class) is semidet.
%
%   Multifile hookable classification for _vararg_ predicates.

vararg_goal_classification(call, Arity, built_in) :-
    Arity >= 1.
vararg_goal_classification(send_super, Arity, expanded) :- % XPCE (TBD)
    Arity >= 2.
vararg_goal_classification(get_super, Arity, expanded) :-  % XPCE (TBD)
    Arity >= 3.

%!  qualified_goal_classification(:Goal, +TB, -Class)
%
%   Classify an explicitly qualified goal.

qualified_goal_classification(Goal, TB, Class) :-
    goal_classification(TB, Goal, [], Class),
    Class \== undefined,
    !.
qualified_goal_classification(Module:Goal, _, extern(Module, How)) :-
    predicate_property(Module:Goal, visible),
    !,
    (   (   predicate_property(Module:Goal, public)
        ;   predicate_property(Module:Goal, exported)
        )
    ->  How = (public)
    ;   How = (private)
    ).
qualified_goal_classification(Module:_, _, extern(Module, unknown)).

%!  classify_head(+TB, +Head, -Class)
%
%   Classify a clause head

classify_head(TB, Goal, exported) :-
    colour_state_source_id(TB, SourceId),
    xref_exported(SourceId, Goal),
    !.
classify_head(_TB, Goal, hook) :-
    xref_hook(Goal),
    !.
classify_head(TB, Goal, hook) :-
    colour_state_source_id(TB, SourceId),
    xref_module(SourceId, M),
    xref_hook(M:Goal),
    !.
classify_head(TB, Goal, Class) :-
    built_in_predicate(Goal),
    (   system_module(TB)
    ->  (   predicate_property(system:Goal, iso)
        ->  Class = def_iso
        ;   goal_name(Goal, Name),
            \+ sub_atom(Name, 0, _, _, $)
        ->  Class = def_swi
        )
    ;   (   predicate_property(system:Goal, iso)
        ->  Class = iso
        ;   Class = built_in
        )
    ).
classify_head(TB, Goal, unreferenced) :-
    colour_state_source_id(TB, SourceId),
    \+ (xref_called(SourceId, Goal, By), By \= Goal),
    !.
classify_head(TB, Goal, test) :-
    Goal = test(_),
    colour_state_source_id(TB, SourceId),
    xref_called(SourceId, Goal, '<test_unit>'(_Unit)),
    !.
classify_head(TB, Goal, test) :-
    Goal = test(_, _),
    colour_state_source_id(TB, SourceId),
    xref_called(SourceId, Goal, '<test_unit>'(_Unit)),
    !.
classify_head(TB, Goal, How) :-
    colour_state_source_id(TB, SourceId),
    (   xref_defined(SourceId, Goal, imported(From))
    ->  How = imported(From)
    ;   xref_defined(SourceId, Goal, How)
    ),
    !.
classify_head(_TB, _Goal, undefined).

built_in_predicate(Goal) :-
    predicate_property(system:Goal, built_in),
    !.
built_in_predicate(module(_, _)).       % reserved expanded constructs
built_in_predicate(module(_, _, _)).
built_in_predicate(if(_)).
built_in_predicate(elif(_)).
built_in_predicate(else).
built_in_predicate(endif).

goal_name(_:G, Name) :- nonvar(G), !, goal_name(G, Name).
goal_name(G, Name) :- callable(G), functor_name(G, Name).

system_module(TB) :-
    colour_state_source_id(TB, SourceId),
    xref_module(SourceId, M),
    module_property(M, class(system)).

generalise_term(Specific, General) :-
    (   compound(Specific)
    ->  compound_name_arity(Specific, Name, Arity),
        compound_name_arity(General0, Name, Arity),
        General = General0
    ;   General = Specific
    ).

rename_goal(Goal0, Name, Goal) :-
    (   compound(Goal0)
    ->  compound_name_arity(Goal0, _, Arity),
        compound_name_arity(Goal, Name, Arity)
    ;   Goal = Name
    ).

functor_name(Term, Name) :-
    (   compound(Term)
    ->  compound_name_arity(Term, Name, _)
    ;   atom(Term)
    ->  Name = Term
    ).

goal_name_arity(Goal, Name, Arity) :-
    (   compound(Goal)
    ->  compound_name_arity(Goal, Name, Arity)
    ;   atom(Goal)
    ->  Name = Goal, Arity = 0
    ).


call_goal_colours(Term, Colours) :-
    goal_colours(Term, Colours),
    !.
call_goal_colours(Term, Colours) :-
    def_goal_colours(Term, Colours).

call_goal_colours(Term, Class, Colours) :-
    goal_colours(Term, Class, Colours),
    !.
%call_goal_colours(Term, Class, Colours) :-
%    def_goal_colours(Term, Class, Colours).


%       Specify colours for individual goals.

def_goal_colours(_ is _,                 built_in-[classify,expression]).
def_goal_colours(_ < _,                  built_in-[expression,expression]).
def_goal_colours(_ > _,                  built_in-[expression,expression]).
def_goal_colours(_ =< _,                 built_in-[expression,expression]).
def_goal_colours(_ >= _,                 built_in-[expression,expression]).
def_goal_colours(_ =\= _,                built_in-[expression,expression]).
def_goal_colours(_ =:= _,                built_in-[expression,expression]).
def_goal_colours(module(_,_),            built_in-[identifier,exports]).
def_goal_colours(module(_,_,_),          built_in-[identifier,exports,langoptions]).
def_goal_colours(use_module(_),          built_in-[imported_file]).
def_goal_colours(use_module(File,_),     built_in-[file,imports(File)]).
def_goal_colours(autoload(_),            built_in-[imported_file]).
def_goal_colours(autoload(File,_),       built_in-[file,imports(File)]).
def_goal_colours(reexport(_),            built_in-[file]).
def_goal_colours(reexport(File,_),       built_in-[file,imports(File)]).
def_goal_colours(dynamic(_),             built_in-[declarations(dynamic)]).
def_goal_colours(thread_local(_),        built_in-[declarations(thread_local)]).
def_goal_colours(module_transparent(_),  built_in-[declarations(module_transparent)]).
def_goal_colours(discontiguous(_),       built_in-[declarations(discontiguous)]).
def_goal_colours(multifile(_),           built_in-[declarations(multifile)]).
def_goal_colours(volatile(_),            built_in-[declarations(volatile)]).
def_goal_colours(public(_),              built_in-[declarations(public)]).
def_goal_colours(det(_),                 built_in-[declarations(det)]).
def_goal_colours(table(_),               built_in-[declarations(table)]).
def_goal_colours(meta_predicate(_),      built_in-[meta_declarations]).
def_goal_colours(mode(_),                built_in-[meta_declarations]).
def_goal_colours(consult(_),             built_in-[file]).
def_goal_colours(include(_),             built_in-[file]).
def_goal_colours(ensure_loaded(_),       built_in-[file]).
def_goal_colours(load_files(_),          built_in-[file]).
def_goal_colours(load_files(_,_),        built_in-[file,options]).
def_goal_colours(setof(_,_,_),           built_in-[classify,setof,classify]).
def_goal_colours(bagof(_,_,_),           built_in-[classify,setof,classify]).
def_goal_colours(predicate_options(_,_,_), built_in-[predicate,classify,classify]).
% Database access
def_goal_colours(assert(_),              built_in-[db]).
def_goal_colours(asserta(_),             built_in-[db]).
def_goal_colours(assertz(_),             built_in-[db]).
def_goal_colours(assert(_,_),            built_in-[db,classify]).
def_goal_colours(asserta(_,_),           built_in-[db,classify]).
def_goal_colours(assertz(_,_),           built_in-[db,classify]).
def_goal_colours(retract(_),             built_in-[db]).
def_goal_colours(retractall(_),          built_in-[db]).
def_goal_colours(clause(_,_),            built_in-[db,classify]).
def_goal_colours(clause(_,_,_),          built_in-[db,classify,classify]).
% misc
def_goal_colours(set_prolog_flag(_,_),   built_in-[prolog_flag_name,classify]).
def_goal_colours(current_prolog_flag(_,_), built_in-[prolog_flag_name,classify]).
% XPCE stuff
def_goal_colours(pce_autoload(_,_),      classify-[classify,file]).
def_goal_colours(pce_image_directory(_), classify-[directory]).
def_goal_colours(new(_, _),              built_in-[classify,pce_new]).
def_goal_colours(send_list(_,_,_),       built_in-pce_arg_list).
def_goal_colours(send(_,_),              built_in-[pce_arg,pce_selector]).
def_goal_colours(get(_,_,_),             built_in-[pce_arg,pce_selector,pce_arg]).
def_goal_colours(send_super(_,_),        built_in-[pce_arg,pce_selector]).
def_goal_colours(get_super(_,_),         built_in-[pce_arg,pce_selector,pce_arg]).
def_goal_colours(get_chain(_,_,_),       built_in-[pce_arg,pce_selector,pce_arg]).
def_goal_colours(Pce,                    built_in-pce_arg) :-
    compound(Pce),
    functor_name(Pce, Functor),
    pce_functor(Functor).

pce_functor(send).
pce_functor(get).
pce_functor(send_super).
pce_functor(get_super).


                 /*******************************
                 *        SPECIFIC HEADS        *
                 *******************************/

head_colours(file_search_path(_,_), hook-[identifier,classify]).
head_colours(library_directory(_),  hook-[file]).
head_colours(resource(_,_),         hook-[identifier,file]).
head_colours(resource(_,_,_),       hook-[identifier,file,classify]).

head_colours(Var, _) :-
    var(Var),
    !,
    fail.
head_colours(M:H, Colours) :-
    M == user,
    head_colours(H, HC),
    HC = hook - _,
    !,
    Colours = meta-[module(user), HC ].
head_colours(M:H, Colours) :-
    atom(M), callable(H),
    xref_hook(M:H),
    !,
    Colours = meta-[module(M), hook-classify ].
head_colours(M:_, meta-[module(M),extern(M)]).


                 /*******************************
                 *             STYLES           *
                 *******************************/

%!  def_style(+Pattern, -Style)
%
%   Define the style used for the   given  pattern. Definitions here
%   can     be     overruled     by       defining     rules     for
%   emacs_prolog_colours:style/2

def_style(goal(built_in,_),        [colour(blue)]).
def_style(goal(imported(_),_),     [colour(blue)]).
def_style(goal(autoload(_),_),     [colour(navy_blue)]).
def_style(goal(global,_),          [colour(navy_blue)]).
def_style(goal(global(dynamic,_),_), [colour(magenta)]).
def_style(goal(global(_,_),_),     [colour(navy_blue)]).
def_style(goal(undefined,_),       [colour(red)]).
def_style(goal(thread_local(_),_), [colour(magenta), underline(true)]).
def_style(goal(dynamic(_),_),      [colour(magenta)]).
def_style(goal(multifile(_),_),    [colour(navy_blue)]).
def_style(goal(expanded,_),        [colour(blue), underline(true)]).
def_style(goal(extern(_),_),       [colour(blue), underline(true)]).
def_style(goal(extern(_,private),_), [colour(red)]).
def_style(goal(extern(_,public),_), [colour(blue)]).
def_style(goal(recursion,_),       [underline(true)]).
def_style(goal(meta,_),            [colour(red4)]).
def_style(goal(foreign(_),_),      [colour(darkturquoise)]).
def_style(goal(local(_),_),        []).
def_style(goal(constraint(_),_),   [colour(darkcyan)]).
def_style(goal(not_callable,_),    [background(orange)]).

def_style(function,                [colour(blue)]).
def_style(no_function,             [colour(red)]).

def_style(option_name,             [colour('#3434ba')]).
def_style(no_option_name,          [colour(red)]).

def_style(neck(_),		   [bold(true)]).

def_style(head(exported,_),        [colour(blue), bold(true)]).
def_style(head(public(_),_),       [colour('#016300'), bold(true)]).
def_style(head(extern(_),_),       [colour(blue), bold(true)]).
def_style(head(dynamic,_),         [colour(magenta), bold(true)]).
def_style(head(multifile(_),_),    [colour(navy_blue), bold(true)]).
def_style(head(unreferenced,_),    [colour(red), bold(true)]).
def_style(head(hook,_),            [colour(blue), underline(true)]).
def_style(head(meta,_),            []).
def_style(head(constraint(_),_),   [colour(darkcyan), bold(true)]).
def_style(head(imported(_),_),     [colour(darkgoldenrod4), bold(true)]).
def_style(head(built_in,_),        [background(orange), bold(true)]).
def_style(head(iso,_),             [background(orange), bold(true)]).
def_style(head(def_iso,_),         [colour(blue), bold(true)]).
def_style(head(def_swi,_),         [colour(blue), bold(true)]).
def_style(head(test,_),            [colour('#01bdbd'), bold(true)]).
def_style(head(_,_),               [bold(true)]).
def_style(rule_condition,	   [background('#d4ffe3')]).

def_style(module(_),               [colour(dark_slate_blue)]).
def_style(comment(_),              [colour(dark_green)]).

def_style(directive,               [background(grey90)]).
def_style(method(_),               [bold(true)]).

def_style(var,                     [colour(red4)]).
def_style(singleton,               [bold(true), colour(red4)]).
def_style(unbound,                 [colour(red), bold(true)]).
def_style(quoted_atom,             [colour(navy_blue)]).
def_style(string,                  [colour(navy_blue)]).
def_style(rational(_),		   [colour(steel_blue)]).
def_style(codes,                   [colour(navy_blue)]).
def_style(chars,                   [colour(navy_blue)]).
def_style(nofile,                  [colour(red)]).
def_style(file(_),                 [colour(blue), underline(true)]).
def_style(file_no_depend(_),       [colour(blue), underline(true), background(pink)]).
def_style(directory(_),            [colour(blue)]).
def_style(class(built_in,_),       [colour(blue), underline(true)]).
def_style(class(library(_),_),     [colour(navy_blue), underline(true)]).
def_style(class(local(_,_,_),_),   [underline(true)]).
def_style(class(user(_),_),        [underline(true)]).
def_style(class(user,_),           [underline(true)]).
def_style(class(undefined,_),      [colour(red), underline(true)]).
def_style(prolog_data,             [colour(blue), underline(true)]).
def_style(flag_name(_),            [colour(blue)]).
def_style(known_flag_name(_),      [colour(blue), background(pink)]).
def_style(no_flag_name(_),         [colour(red)]).
def_style(unused_import,           [colour(blue), background(pink)]).
def_style(undefined_import,        [colour(red)]).

def_style(constraint(_),           [colour(darkcyan)]).

def_style(keyword(_),              [colour(blue)]).
def_style(identifier,              [bold(true)]).
def_style(delimiter,               [bold(true)]).
def_style(expanded,                [colour(blue), underline(true)]).
def_style(hook(_),                 [colour(blue), underline(true)]).
def_style(op_type(_),              [colour(blue)]).

def_style(qq_type,                 [bold(true)]).
def_style(qq(_),                   [colour(blue), bold(true)]).
def_style(qq_content(_),           [colour(red4)]).

def_style(dict_tag,                [bold(true)]).
def_style(dict_key,                [bold(true)]).
def_style(dict_function(_),        [colour(navy_blue)]).
def_style(dict_return_op,          [colour(blue)]).

def_style(hook,                    [colour(blue), underline(true)]).
def_style(dcg_right_hand_ctx,      [background('#d4ffe3')]).

def_style(error,                   [background(orange)]).
def_style(type_error(_),           [background(orange)]).
def_style(domain_error(_),         [background(orange)]).
def_style(syntax_error(_,_),       [background(orange)]).
def_style(instantiation_error,     [background(orange)]).

def_style(decl_option(_),	   [bold(true)]).
def_style(table_mode(_),	   [bold(true)]).

def_style(macro(_),                [colour(blue), underline(true)]).

%!  syntax_colour(?Class, ?Attributes) is nondet.
%
%   True when a range  classified  Class   must  be  coloured  using
%   Attributes.  Attributes is a list of:
%
%     * colour(ColourName)
%     * background(ColourName)
%     * bold(Boolean)
%     * underline(Boolean)
%
%   Attributes may be the empty list. This   is used for cases where
%   -for example- a  menu  is  associated   with  the  fragment.  If
%   syntax_colour/2 fails, no fragment is created for the region.

syntax_colour(Class, Attributes) :-
    (   style(Class, Attributes)            % user hook
    ;   def_style(Class, Attributes)        % system default
    ).


%!  term_colours(+Term, -FunctorColour, -ArgColours)
%
%   Define colourisation for specific terms.

term_colours((?- Directive), Colours) :-
    term_colours((:- Directive), Colours).
term_colours((prolog:Head --> _),
             neck(-->) - [ expanded - [ module(prolog),
                                        hook(message) - [ identifier
                                                        ]
                                      ],
                           dcg_body(prolog:Head)
                         ]) :-
    prolog_message_hook(Head).

prolog_message_hook(message(_)).
prolog_message_hook(deprecated(_)).
prolog_message_hook(error_message(_)).
prolog_message_hook(message_context(_)).
prolog_message_hook(message_location(_)).

%       XPCE rules

term_colours(variable(_, _, _, _),
             expanded - [ identifier,
                          classify,
                          classify,
                          comment(string)
                        ]).
term_colours(variable(_, _, _),
             expanded - [ identifier,
                          classify,
                          atom
                        ]).
term_colours(handle(_, _, _),
             expanded - [ classify,
                          classify,
                          classify
                        ]).
term_colours(handle(_, _, _, _),
             expanded - [ classify,
                          classify,
                          classify,
                          classify
                        ]).
term_colours(class_variable(_,_,_,_),
             expanded - [ identifier,
                          pce(type),
                          pce(default),
                          comment(string)
                        ]).
term_colours(class_variable(_,_,_),
             expanded - [ identifier,
                          pce(type),
                          pce(default)
                        ]).
term_colours(delegate_to(_),
             expanded - [ classify
                        ]).
term_colours((:- encoding(_)),
             expanded - [ expanded - [ classify
                                     ]
                        ]).
term_colours((:- pce_begin_class(_, _, _)),
             expanded - [ expanded - [ identifier,
                                       pce_new,
                                       comment(string)
                                     ]
                        ]).
term_colours((:- pce_begin_class(_, _)),
             expanded - [ expanded - [ identifier,
                                       pce_new
                                     ]
                        ]).
term_colours((:- pce_extend_class(_)),
             expanded - [ expanded - [ identifier
                                     ]
                        ]).
term_colours((:- pce_end_class),
             expanded - [ expanded
                        ]).
term_colours((:- pce_end_class(_)),
             expanded - [ expanded - [ identifier
                                     ]
                        ]).
term_colours((:- use_class_template(_)),
             expanded - [ expanded - [ pce_new
                                     ]
                        ]).
term_colours((:- emacs_begin_mode(_,_,_,_,_)),
             expanded - [ expanded - [ identifier,
                                       classify,
                                       classify,
                                       classify,
                                       classify
                                     ]
                        ]).
term_colours((:- emacs_extend_mode(_,_)),
             expanded - [ expanded - [ identifier,
                                       classify
                                     ]
                        ]).
term_colours((:- pce_group(_)),
             expanded - [ expanded - [ identifier
                                     ]
                        ]).
term_colours((:- pce_global(_, new(_))),
             expanded - [ expanded - [ identifier,
                                       pce_arg
                                     ]
                        ]).
term_colours((:- emacs_end_mode),
             expanded - [ expanded
                        ]).
term_colours(pce_ifhostproperty(_,_),
             expanded - [ classify,
                          classify
                        ]).
term_colours((_,_),
             error - [ classify,
                       classify
                     ]).

%!  specified_item(+Specified, +Term, +TB, +TermPosition) is det.
%
%   Colourise an item that is explicitly   classified  by the user using
%   term_colours/2 or goal_colours/2.

specified_item(_Class, _Term, _TB, Pos) :-
    var(Pos),
    !.
specified_item(Class, Term, TB, parentheses_term_position(PO,PC,Pos)) :-
    !,
    colour_item(parentheses, TB, PO-PC),
    specified_item(Class, Term, TB, Pos).
specified_item(_, Var, TB, Pos) :-
    (   var(Var)
    ;   qq_position(Pos)
    ),
    !,
    colourise_term_arg(Var, TB, Pos).
                                        % generic classification
specified_item(classify, Term, TB, Pos) :-
    !,
    colourise_term_arg(Term, TB, Pos).
                                        % classify as head
specified_item(head, Term, TB, Pos) :-
    !,
    colourise_clause_head(Term, TB, Pos).
                                        % expanded head (DCG=2, ...)
specified_item(head(+N), Term, TB, Pos) :-
    !,
    colourise_extended_head(Term, N, TB, Pos).
                                        % M:Head
specified_item(extern(M), Term, TB, Pos) :-
    !,
    colourise_extern_head(Term, M, TB, Pos).
                                        % classify as body
specified_item(body, Term, TB, Pos) :-
    !,
    colourise_body(Term, TB, Pos).
specified_item(body(Goal), _Term0, TB, Pos) :-
    !,
    colourise_body(Goal, TB, Pos).
specified_item(dcg_body(Head), Term, TB, Pos) :-
    !,
    colourise_dcg(Term, Head, TB, Pos).
specified_item(setof, Term, TB, Pos) :-
    !,
    colourise_setof(Term, TB, Pos).
specified_item(meta(MetaSpec), Term, TB, Pos) :-
    !,
    colourise_meta_arg(MetaSpec, Term, TB, Pos).
                                        % DCG goal in body
specified_item(dcg, Term, TB, Pos) :-
    !,
    colourise_dcg(Term, [], TB, Pos).
                                        % assert/retract arguments
specified_item(db, Term, TB, Pos) :-
    !,
    colourise_db(Term, TB, Pos).
                                        % error(Error)
specified_item(error(Error), _Term, TB, Pos) :-
    colour_item(Error, TB, Pos).
                                        % files
specified_item(file(Path), _Term, TB, Pos) :-
    !,
    colour_item(file(Path), TB, Pos).
specified_item(file, Term, TB, Pos) :-
    !,
    colourise_files(Term, TB, Pos, any).
specified_item(imported_file, Term, TB, Pos) :-
    !,
    colourise_files(Term, TB, Pos, imported).
specified_item(langoptions, Term, TB, Pos) :-
    !,
    colourise_langoptions(Term, TB, Pos).
specified_item(expression, Term, TB, Pos) :-
    !,
    colourise_expression(Term, TB, Pos).
                                        % directory
specified_item(directory, Term, TB, Pos) :-
    !,
    colourise_directory(Term, TB, Pos).
                                        % [Name/Arity, ...]
specified_item(exports, Term, TB, Pos) :-
    !,
    colourise_exports(Term, TB, Pos).
                                        % [Name/Arity, ...]
specified_item(imports(File), Term, TB, Pos) :-
    !,
    colourise_imports(Term, File, TB, Pos).
                                        % Name/Arity
specified_item(import(File), Term, TB, Pos) :-
    !,
    colourise_import(Term, File, TB, Pos).
                                        % Name/Arity, ...
specified_item(predicates, Term, TB, Pos) :-
    !,
    colourise_declarations(Term, predicate_indicator, TB, Pos).
                                        % Name/Arity
specified_item(predicate, Term, TB, Pos) :-
    !,
    colourise_declaration(Term, predicate_indicator, TB, Pos).
                                        % head(Arg, ...)
specified_item(meta_declarations, Term, TB, Pos) :-
    !,
    colourise_meta_declarations(Term, [], TB, Pos).
specified_item(meta_declarations(Extra), Term, TB, Pos) :-
    !,
    colourise_meta_declarations(Term, Extra, TB, Pos).
specified_item(declarations(Which), Term, TB, Pos) :-
    !,
    colourise_declarations(Term, Which, TB, Pos).
                                        % set_prolog_flag(Name, _)
specified_item(prolog_flag_name, Term, TB, Pos) :-
    !,
    colourise_prolog_flag_name(Term, TB, Pos).
                                        % XPCE new argument
specified_item(pce_new, Term, TB, Pos) :-
    !,
    (   atom(Term)
    ->  colourise_class(Term, TB, Pos)
    ;   compound(Term)
    ->  functor_name(Term, Class),
        Pos = term_position(_,_,FF, FT, ArgPos),
        colourise_class(Class, TB, FF-FT),
        specified_items(pce_arg, Term, TB, ArgPos)
    ;   colourise_term_arg(Term, TB, Pos)
    ).
                                        % Generic XPCE arguments
specified_item(pce_arg, new(X), TB,
               term_position(_,_,_,_,[ArgPos])) :-
    !,
    specified_item(pce_new, X, TB, ArgPos).
specified_item(pce_arg, new(X, T), TB,
               term_position(_,_,_,_,[P1, P2])) :-
    !,
    colourise_term_arg(X, TB, P1),
    specified_item(pce_new, T, TB, P2).
specified_item(pce_arg, @(Ref), TB, Pos) :-
    !,
    colourise_term_arg(@(Ref), TB, Pos).
specified_item(pce_arg, prolog(Term), TB,
               term_position(_,_,FF,FT,[ArgPos])) :-
    !,
    colour_item(prolog_data, TB, FF-FT),
    colourise_term_arg(Term, TB, ArgPos).
specified_item(pce_arg, Term, TB, Pos) :-
    compound(Term),
    Term \= [_|_],
    \+ is_dict(Term),
    !,
    specified_item(pce_new, Term, TB, Pos).
specified_item(pce_arg, Term, TB, Pos) :-
    !,
    colourise_term_arg(Term, TB, Pos).
                                        % List of XPCE arguments
specified_item(pce_arg_list, List, TB, list_position(F,T,Elms,Tail)) :-
    !,
    colour_item(list, TB, F-T),
    colourise_list_args(Elms, Tail, List, TB, pce_arg).
specified_item(pce_arg_list, Term, TB, Pos) :-
    !,
    specified_item(pce_arg, Term, TB, Pos).
                                        % XPCE selector
specified_item(pce_selector, Term, TB,
               term_position(_,_,_,_,ArgPos)) :-
    !,
    specified_items(pce_arg, Term, TB, ArgPos).
specified_item(pce_selector, Term, TB, Pos) :-
    colourise_term_arg(Term, TB, Pos).
                                        % Nested specification
specified_item(FuncSpec-ArgSpecs, Term, TB,
               term_position(_,_,FF,FT,ArgPos)) :-
    !,
    specified_item(FuncSpec, Term, TB, FF-FT),
    specified_items(ArgSpecs, Term, TB, ArgPos).
                                        % Nested for {...}
specified_item(FuncSpec-[ArgSpec], {Term}, TB,
               brace_term_position(F,T,ArgPos)) :-
    !,
    specified_item(FuncSpec, {Term}, TB, F-T),
    specified_item(ArgSpec, Term, TB, ArgPos).
                                        % Specified
specified_item(FuncSpec-ElmSpec, List, TB,
               list_position(F,T,ElmPos,TailPos)) :-
    !,
    colour_item(FuncSpec, TB, F-T),
    specified_list(ElmSpec, List, TB, ElmPos, TailPos).
specified_item(Class, _, TB, Pos) :-
    colour_item(Class, TB, Pos).

%!  specified_items(+Spec, +Term, +TB, +PosList)

specified_items(Specs, Term, TB, PosList) :-
    is_dict(Term),
    !,
    specified_dict_kv(PosList, Term, TB, Specs).
specified_items(Specs, Term, TB, PosList) :-
    is_list(Specs),
    !,
    specified_arglist(Specs, 1, Term, TB, PosList).
specified_items(Spec, Term, TB, PosList) :-
    specified_argspec(PosList, Spec, 1, Term, TB).


specified_arglist([], _, _, _, _).
specified_arglist(_, _, _, _, []) :- !.         % Excess specification args
specified_arglist([S0|ST], N, T, TB, [P0|PT]) :-
    (   S0 == options,
        colourization_module(TB, Module),
        colourise_option_arg(T, Module, N, TB, P0)
    ->  true
    ;   arg(N, T, Term),
        specified_item(S0, Term, TB, P0)
    ),
    NN is N + 1,
    specified_arglist(ST, NN, T, TB, PT).

specified_argspec([], _, _, _, _).
specified_argspec([P0|PT], Spec, N, T, TB) :-
    arg(N, T, Term),
    specified_item(Spec, Term, TB, P0),
    NN is N + 1,
    specified_argspec(PT, Spec, NN, T, TB).


%       specified_list(+Spec, +List, +TB, +PosList, TailPos)

specified_list([], [], _, [], _).
specified_list([HS|TS], [H|T], TB, [HP|TP], TailPos) :-
    !,
    specified_item(HS, H, TB, HP),
    specified_list(TS, T, TB, TP, TailPos).
specified_list(Spec, [H|T], TB, [HP|TP], TailPos) :-
    specified_item(Spec, H, TB, HP),
    specified_list(Spec, T, TB, TP, TailPos).
specified_list(_, _, _, [], none) :- !.
specified_list(Spec, Tail, TB, [], TailPos) :-
    specified_item(Spec, Tail, TB, TailPos).

%!  specified_dict_kv(+PosList, +Term, +TB, +Specs)
%
%   @arg Specs is a list of dict_kv(+Key, +KeySpec, +ArgSpec)

specified_dict_kv([], _, _, _).
specified_dict_kv([key_value_position(_F,_T,SF,ST,K,KP,VP)|Pos],
                  Dict, TB, Specs) :-
    specified_dict_kv1(K, Specs, KeySpec, ValueSpec),
    colour_item(KeySpec, TB, KP),
    colour_item(dict_sep, TB, SF-ST),
    get_dict(K, Dict, V),
    specified_item(ValueSpec, V, TB, VP),
    specified_dict_kv(Pos, Dict, TB, Specs).

specified_dict_kv1(Key, Specs, KeySpec, ValueSpec) :-
    Specs = [_|_],
    memberchk(dict_kv(Key, KeySpec, ValueSpec), Specs),
    !.
specified_dict_kv1(Key, dict_kv(Key2, KeySpec, ValueSpec), KeySpec, ValueSpec) :-
    \+ Key \= Key2,
    !.              % do not bind Key2
specified_dict_kv1(_, _, dict_key, classify).


                 /*******************************
                 *         DESCRIPTIONS         *
                 *******************************/

syntax_message(Class) -->
    message(Class),
    !.
syntax_message(qq(_)) -->
    [ 'Quasi quote delimiter' ].
syntax_message(qq_type) -->
    [ 'Quasi quote type term' ].
syntax_message(qq_content(Type)) -->
    [ 'Quasi quote content (~w syntax)'-[Type] ].
syntax_message(goal(Class, Goal)) -->
    !,
    goal_message(Class, Goal).
syntax_message(class(Type, Class)) -->
    !,
    xpce_class_message(Type, Class).
syntax_message(dict_return_op) -->
    !,
    [ ':= separates function from return value' ].
syntax_message(dict_function) -->
    !,
    [ 'Function on a dict' ].
syntax_message(ext_quant) -->
    !,
    [ 'Existential quantification operator' ].
syntax_message(hook(message)) -->
    [ 'Rule for print_message/2' ].
syntax_message(module(Module)) -->
    (   { current_module(Module) }
    ->  (   { module_property(Module, file(File)) }
        ->  [ 'Module ~w defined in ~w'-[Module,File] ]
        ;   [ 'Module ~w'-[Module] ]
        )
    ;   [ 'Module ~w (not loaded)'-[Module] ]
    ).
syntax_message(decl_option(incremental)) -->
    [ 'Keep affected tables consistent' ].
syntax_message(decl_option(abstract)) -->
    [ 'Add abstracted goal to table dependency graph' ].
syntax_message(decl_option(volatile)) -->
    [ 'Do not include predicate in a saved program' ].
syntax_message(decl_option(multifile)) -->
    [ 'Clauses are spread over multiple files' ].
syntax_message(decl_option(discontiguous)) -->
    [ 'Clauses are not contiguous' ].
syntax_message(decl_option(private)) -->
    [ 'Tables or clauses are private to a thread' ].
syntax_message(decl_option(local)) -->
    [ 'Tables or clauses are private to a thread' ].
syntax_message(decl_option(shared)) -->
    [ 'Tables or clauses are shared between threads' ].
syntax_message(decl_option(_Opt)) -->
    [ 'Predicate property' ].
syntax_message(rational(Value)) -->
    [ 'Rational number ~w'-[Value] ].
syntax_message(rule_condition) -->
    [ 'Guard' ].
syntax_message(neck(=>)) -->
    [ 'Rule' ].
syntax_message(neck(-->)) -->
    [ 'Grammar rule' ].
syntax_message(neck(==>)) -->
    [ 'SSU Grammar rule' ].
syntax_message(macro(String)) -->
    [ 'Macro indicator (expands to ~s)'-[String] ].
syntax_message(flag_name(Name)) -->
    [ 'Prolog flag ~w'-[Name] ].
syntax_message(known_flag_name(Name)) -->
    [ 'Prolog flag ~w (not set; known)'-[Name] ].
syntax_message(no_flag_name(Name)) -->
    [ 'Prolog flag ~w (not set)'-[Name] ].

goal_message(meta, _) -->
    [ 'Meta call' ].
goal_message(not_callable, _) -->
    [ 'Goal is not callable (type error)' ].
goal_message(expanded, _) -->
    [ 'Expanded goal' ].
goal_message(Class, Goal) -->
    { predicate_name(Goal, PI) },
    [ 'Call to ~q'-PI ],
    goal_class(Class).

goal_class(recursion) -->
    [ ' (recursive call)' ].
goal_class(undefined) -->
    [ ' (undefined)' ].
goal_class(global) -->
    [ ' (Auto-imported from module user)' ].
goal_class(global(Class, File:Line)) -->
    [ ' (~w in user module from '-[Class], url(File:Line), ')' ].
goal_class(global(Class, source_location(File,Line))) -->
    [ ' (~w in user module from '-[Class], url(File:Line), ')' ].
goal_class(global(Class, -)) -->
    [ ' (~w in user module)'-[Class] ].
goal_class(imported(From)) -->
    [ ' (imported from ~q)'-[From] ].
goal_class(extern(_, private)) -->
    [ ' (WARNING: private predicate)' ].
goal_class(extern(_, public)) -->
    [ ' (public predicate)' ].
goal_class(extern(_)) -->
    [ ' (cross-module call)' ].
goal_class(Class) -->
    [ ' (~p)'-[Class] ].

xpce_class_message(Type, Class) -->
    [ 'XPCE ~w class ~q'-[Type, Class] ].
