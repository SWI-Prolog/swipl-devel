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
            jiti_list/1                         % +Spec
          ]).
:- autoload(library(apply), [maplist/2, foldl/4]).
:- autoload(library(dcg/basics), [number/3]).
:- autoload(library(ansi_term), [ansi_format/3]).


:- meta_predicate
    jiti_list(:).

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

jiti_list(Module:Name/Arity) :-
    atom(Name),
    integer(Arity),
    !,
    functor(Head, Name, Arity),
    jiti_list(Module:Head).
jiti_list(Module:Name/Arity) :-
    atom(Name),
    var(Arity),
    !,
    freeze(Head, functor(Head, Name, _)),
    jiti_list(Module:Head).
jiti_list(Module:Name) :-
    atom(Name),
    !,
    freeze(Head, functor(Head, Name, _)),
    jiti_list(Module:Head).
jiti_list(Head) :-
    tty_width(TTYW),
    findall(Head-Indexed,
            (   predicate_property(Head, indexed(Indexed)),
                \+ predicate_property(Head, imported_from(_))
            ), Pairs),
    PredColW is TTYW-41,
    TableWidth is TTYW-1,
    ansi_format(bold, 'Predicate~*|~w ~t~10+~w ~t~w~14+ ~t~w~9+ ~t~w~7+~n',
                [PredColW, '#Clauses', 'Index','Buckets','Speedup','Flags']),
    format('~`\u2015t~*|~n', [TableWidth]),
    maplist(print_indexes(PredColW), Pairs).

print_indexes(PredColW, Head-List) :-
    foldl(print_index(PredColW, Head), List, 1, _).

:- det(print_index/5).
print_index(PredColW, QHead, Dict, N, N1) :-
    QHead = (M:Head),
    N1 is N+1,
    _{arguments:Args, position:Pos,
      buckets:Buckets, speedup:Speedup, list:List, realised:R} :< Dict,
    predicate_property(M:Head, number_of_clauses(CCount)),
    head_pi(QHead, PI),
    phrase(iarg_spec(Pos, Args), ArgsS),
    phrase(iflags(List, R), Flags),
    istyle(R, Style),
    CCountColZ is PredColW+8,
    (   N == 1
    ->  ansi_format(bold, '~q', [PI]),
        format(' ~t~D~*|  ', [CCount, CCountColZ])
    ;   format(' ~t~*|  ', [CCountColZ])
    ),
    ansi_format(Style, '~|~s ~t~D~14+ ~t~1f~9+  ~s~n',
                [ArgsS,Buckets,Speedup,Flags]).

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

tty_width(W) :-
    catch(tty_size(_, TtyW), _, fail),
    !,
    W is max(65, TtyW).
tty_width(80).
