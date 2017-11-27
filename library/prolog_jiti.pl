/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2017, VU University Amsterdam
			 CWI Amsterdam
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
:- use_module(library(apply)).
:- use_module(library(dcg/basics)).

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
%       - _|A+B|_ is a multi-argument index on the arguments _A_ and _B_.
%       - _|A/B|_ is a deep-index on sub-argument _B_ of argument _A_.
%     - The _Buckets_ specifies the number of buckets of the hash table
%     - The _Speedup_ specifies the selectivity of the index
%     - The _Flags_ describes additional properties, currently:
%       - =L= denotes that the index contains multiple compound
%         terms with the same name/arity that may be used to create
%         deep indexes.  The deep indexes themselves are created
%         as just-in-time indexes.

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
    findall(Head-Indexed,
            (   predicate_property(Head, indexed(Indexed)),
                \+ predicate_property(Head, imported_from(_))
            ), Pairs),
    format('Predicate~46|~w ~t~8+ ~t~w~6+ ~t~w~6+ ~t~w~5+~n',
           ['Indexed','Buckets','Speedup','Flags']),
    format('~`=t~76|~n'),
    maplist(print_indexed, Pairs).

print_indexed((M:Head)-[Args-hash(Buckets,Speedup,_Size,List)|More]) :-
    functor(Head, Name, Arity),
    phrase(iarg_spec(Args), ArgsS),
    phrase(iflags(List), Flags),
    format('~q ~t~48|~s ~t~8+ ~t~D~6+ ~t~1f~8+ ~t~s~3+~n',
           [M:Name/Arity, ArgsS,Buckets,Speedup,Flags]),
    maplist(print_secondary_index, More),
    !.
print_indexed(Pair) :-
    format('Failed: ~p~n', [Pair]).

print_secondary_index(Args-hash(Buckets,Speedup,_Size,List)) :-
    phrase(iarg_spec(Args), ArgsS),
    phrase(iflags(List), Flags),
    format('~t~48|~s ~t~8+ ~t~D~6+ ~t~1f~8+ ~t~s~3+~n',
           [ArgsS,Buckets,Speedup,Flags]),
    !.
print_secondary_index(Pair) :-
    format('Secondary failed: ~p~n', [Pair]).

iarg_spec(single(N)) -->
    number(N).
iarg_spec(multi(L)) -->
    plus_list(L).
iarg_spec(deep(List)) -->
    deep_list(List).

plus_list([H|T]) -->
    number(H),
    (   {T==[]}
    ->  []
    ;   "+",
        plus_list(T)
    ).

deep_list([Last]) -->
    !,
    iarg_spec(Last).
deep_list([H|T]) -->
    number(H),
    "/",
    deep_list(T).


iflags(true)  --> "L".
iflags(false) --> "".
