/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2017-2025, VU University Amsterdam
			      CWI Amsterdam
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

:- module(prolog_jiti,
          [ jiti_list/0,
            jiti_list/1,                % +Spec
            jiti_suggest_modes/1,       % :Spec
            jiti_suggest_modes/0
          ]).
:- autoload(library(apply), [maplist/2, foldl/4, convlist/3]).
:- autoload(library(dcg/basics), [number//1]).
:- autoload(library(ansi_term), [ansi_format/3, ansi_hyperlink/3]).
:- autoload(library(prolog_code), [pi_head/2, most_general_goal/2]).
:- autoload(library(listing), [portray_clause/1]).
:- autoload(library(lists), [append/2]).
:- autoload(library(ordsets), [ord_subtract/3]).


:- meta_predicate
    jiti_list(:),
    jiti_suggest_modes(:).

/** <module> Just In Time Indexing (JITI) utilities

This module provides utilities to   examine just-in-time indexes created
by the system and can help diagnosing space and performance issues.

@tbd	Use print_message/2 and dynamically figure out the column width.
*/


%!  jiti_list is det.
%!  jiti_list(:Spec) is det.
%
%   List the JITI (Just In  Time   Indexes)  of selected predicates. The
%   predicate jiti_list/0 list all just-in-time  indexed predicates. The
%   predicate jiti_list/1 takes one of  the   patterns  below. All parts
%   except for Name  can  be  variables.   The  last  pattern  takes  an
%   arbitrary number of arguments.
%
%     - Module:Head
%     - Module:Name/Arity
%     - Module:Name
%
%   The columns use the following notation:
%
%     - The _Indexed_ column describes the argument(s) indexed:
%       - A plain integer refers to a 1-based argument number
%       - ``A+B`` is a multi-argument index on the arguments `A` and `B`.
%       - ``P:L`` is a deep-index `L` on sub-argument `P`.  For example,
%         ``1/2:2+3`` is an index of the 2nd and 3rd argument of the
%         2nd argument of a compound on the first argument of the predicate.
%         This implies `x` and `y` in the head p(f(_,g(_,x,y)))
%     - The `Buckets` specifies the number of buckets of the hash table
%     - The `Speedup` specifies the selectivity of the index
%     - The `Flags` describes additional properties, currently:
%       - ``L`` denotes that the index contains multiple compound
%         terms with the same name/arity that may be used to create
%         deep indexes.  The deep indexes themselves are created
%         as just-in-time indexes.
%       - ``V`` denotes the index is _virtual_, i.e., it has not yet
%         been materialized.

jiti_list :-
    jiti_list(_:_).

jiti_list(Spec) :-
    spec_head(Spec, Head),
    !,
    jiti_list(Head).
jiti_list(Head) :-
    tty_width(TTYW),
    findall(PI-Indexed,
            (   predicate_property(Head, indexed(Indexed)),
                \+ predicate_property(Head, imported_from(_)),
                pi_head(PI, Head)
            ), Pairs0),
    sort(Pairs0, Pairs),
    PredColW is TTYW-47,
    TableWidth is TTYW-1,
    ansi_format(bold, 'Predicate~*|~w ~t~10+~w ~t~w~14+ ~t~w~9+ ~t~w~6+ ~t~w~6+~n',
                [PredColW, '#Clauses', 'Index','Buckets','Speedup','Coll','Flags']),
    format('~`\u2015t~*|~n', [TableWidth]),
    maplist(print_indexes(PredColW), Pairs).

print_indexes(PredColW, PI-List) :-
    foldl(print_index(PredColW, PI), List, 1, _).

:- det(print_index/5).
print_index(PredColW, PI0, Dict, N, N1) :-
    pi_head(PI0, Head),
    head_pi(Head, PI),                  % Create DCG PI
    N1 is N+1,
    _{arguments:Args, position:Pos,
      buckets:Buckets, speedup:Speedup, list:List, realised:R,
      collisions:Collisions0} :< Dict,
    predicate_property(Head, number_of_clauses(CCount)),
    phrase(iarg_spec(Pos, Args), ArgsS),
    phrase(iflags(List, R), Flags),
    istyle(R, Style),
    icoll(R, List, Collisions0, Collisions),
    CCountColZ is PredColW+8,
    (   N == 1
    ->  format_pi(PI),
        format(' ~t~D~*|  ', [CCount, CCountColZ])
    ;   format(' ~t~*|  ', [CCountColZ])
    ),
    ansi_format(Style, '~|~s ~t~D~14+ ~t~1f~9+ ~t~w~6+ ~s~n',
                [ArgsS,Buckets,Speedup,Collisions,Flags]).

format_pi(PI) :-
    pi_head(PI, Head),
    predicate_property(Head, file(File)),
    predicate_property(Head, line_count(Line)),
    !,
    format(string(Label), '~q', [PI]),
    ansi_hyperlink(user_output, File:Line, Label).
format_pi(PI) :-
    format('~q', [PI]).

%!  iarg_spec(+Position, +Args)//

iarg_spec([], [N]) ==>
    number(N).
iarg_spec([], List) ==>
    plus_list(List).
iarg_spec(Deep, Args) ==>
    deep_list(Deep),
    iarg_spec([], Args).

plus_list([H|T]) -->
    number(H),
    (   {T==[]}
    ->  []
    ;   "+",
        plus_list(T)
    ).

deep_list([Last]) -->
    !,
    number(Last),
    ":".
deep_list([H|T]) -->
    number(H),
    "/",
    deep_list(T).


iflags(true, R)  ==> "L", irealised(R).
iflags(false, R) ==> "", irealised(R).

irealised(false) ==> "V".
irealised(true)  ==> "".

istyle(true, code).
istyle(false, comment).

icoll(true,  false, Collisions0, Collisions) =>
    Collisions = Collisions0.
icoll(_, _, _, Collisions) =>
    Collisions = '-'.

head_pi(Head, PI) :-
    predicate_property(Head, non_terminal),
    !,
    pi_head(PI0, Head),
    dcg_pi(PI0, PI).
head_pi(Head, PI) :-
    pi_head(PI, Head).

dcg_pi(M:Name/Arity, DCG) =>
    Arity2 is Arity-2,
    DCG = M:Name//Arity2.
dcg_pi(Name/Arity, DCG) =>
    Arity2 is Arity-2,
    DCG = Name//Arity2.


                /*******************************
                *            MODES             *
                *******************************/

%!  jiti_suggest_modes is det.
%!  jiti_suggest_modes(:Spec) is det.
%
%   Propose modes for the predicates referenced   by  Spec. This utility
%   may be executed _after_ a  clean  load   of  your  program and after
%   running the program. It searches  for   static  predicates that have
%   been called and (thus) have been  examined for candidate indexes. If
%   candidate indexes have not been materialized   this implies that the
%   predicate was never called with a nonvar value for the corresponding
%   argument. Adding a mode/1 declaration  may   be  used  to inform the
%   system thereof. The system will never examine arguments for indexing
%   that have been declared as mode `-`.
%
%   __Note:__ This predicate merely detects that some predicate is never
%   called with instantiated specific arguments __during this run__. The
%   user should verify whether the suggested   `-` arguments are correct
%   and typically complete the mode by changing   `?`  into `+` (or `-`)
%   where applicable. Currently, in SWI-Prolog, mode/1 declarations have
%   no effect on the semantics of the   code. In particular, a predicate
%   that declares some argument as `-` may  be called with this argument
%   instantiated. This may change in the future.
%
%   @arg Spec uses the same conventions as jiti_list/1.

jiti_suggest_modes :-
    jiti_suggest_modes(_:_).

jiti_suggest_modes(Partial) :-
    spec_head(Partial, Head),
    !,
    jiti_suggest_modes(Head).
jiti_suggest_modes(Head) :-
    Head = M:_,
    freeze(M, module_property(M, class(user))),
    findall(Head-Indexed,
            (   predicate_property(Head, indexed(Indexed)),
                \+ predicate_property(Head, imported_from(_))
            ), Pairs),
    convlist(suggest_mode, Pairs, Modes),
    (   Modes == []
    ->  print_message(informational, jiti(no_modes(Head)))
    ;   maplist(portray_clause, Modes)
    ).

suggest_mode((M:Head)-Indexes, (:- mode(M:GenHead))) :-
    convlist(not_realised_index_arg, Indexes, FArgs),
    convlist(realised_index_arg, Indexes, ArgsL),
    append(ArgsL, Realised),
    sort(FArgs, Sargs),
    sort(Realised, RArgs),
    ord_subtract(Sargs, RArgs, Args),
    Args \== [],
    !,
    most_general_goal(Head, GenHead),
    make_mode(Args, GenHead).

not_realised_index_arg(Index, Arg) :-
    _{ arguments:[Arg], position:[], realised:false } :< Index.

realised_index_arg(Index, Args) :-
    _{ arguments:Args, position:[], realised:true } :< Index.

make_mode([], GenHead) =>
    functor(GenHead, _, Arity),
    set_any(1, Arity, GenHead).
make_mode([H|T], GenHead) =>
    arg(H, GenHead, -),
    make_mode(T, GenHead).

set_any(I, Arity, GenHead), arg(I, GenHead, Var) =>
    (   var(Var)
    ->  Var = '?'
    ;   true
    ),
    I2 is I+1,
    set_any(I2, Arity, GenHead).
set_any(_, _, _) =>
    true.


                /*******************************
                *      SPECIFY PREDICATES      *
                *******************************/

spec_head(Module:Name/Arity, Head), atom(Name), integer(Arity) =>
    Head = Module:Head0,
    functor(Head0, Name, Arity).
spec_head(Module:Name/Arity, Head), atom(Name), var(Arity) =>
    Head = Module:Head0,
    freeze(Head0, functor(Head0, Name, _)).
spec_head(Module:Name, Head), atom(Name) =>
    Head = Module:Head0,
    freeze(Head0, functor(Head0, Name, _)).
spec_head(_, _) =>
    fail.

                /*******************************
                *            OUTPUT            *
                *******************************/

tty_width(W) :-
    catch(tty_size(_, TtyW), _, fail),
    !,
    W is max(65, TtyW).
tty_width(80).

                /*******************************
                *           MESSAGES           *
                *******************************/

:- multifile prolog:message//1.

prolog:message(jiti(no_modes(M:Head))) -->
    { var(Head) },
    [ 'No mode suggestions for predicates in module ~p'-[M] ].
prolog:message(jiti(no_modes(Head))) -->
    { numbervars(Head, 0, _, [singletons(true)]) },
    [ 'No mode suggestions for ~p'-[Head] ].
