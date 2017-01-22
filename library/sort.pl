/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2006-2012, University of Amsterdam
                              Vu University Amsterdam
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

:- module(sort,
          [ predsort/3,                 % :Compare, +List, -Sorted
            locale_sort/2               % +ListOfAtoms, -Sorted
          ]).

:- set_prolog_flag(generate_debug_info, false).

:- meta_predicate
    predsort(3, +, -).              % 3: Delta, Left, Right

%!  predsort(:Compare, +List, -Sorted) is det.
%
%   Sorts similar to sort/2, but determines   the order of two terms
%   by calling Compare(-Delta, +E1, +E2). This call must unify Delta
%   with one of <, > or =.  If built-in predicate compare/3 is used,
%   the result is the same as sort/2 (but sort/2 is built using more
%   low-level primitives and is considerably faster).
%
%   @see keysort/2 provides an more portable way to sort on
%   arbitrary keys that is usually faster.

predsort(P, L, R) :-
    '$skip_list'(N, L, Tail),
    (   Tail == []
    ->  predsort(P, N, L, _, R1),
        R = R1
    ;   must_be(L, list)
    ).

predsort(P, 2, [X1, X2|L], L, R) :-
    !,
    call(P, Delta, X1, X2),
    sort2(Delta, X1, X2, R).
predsort(_, 1, [X|L], L, [X]) :- !.
predsort(_, 0, L, L, []) :- !.
predsort(P, N, L1, L3, R) :-
    N1 is N // 2,
    plus(N1, N2, N),
    predsort(P, N1, L1, L2, R1),
    predsort(P, N2, L2, L3, R2),
    predmerge(P, R1, R2, R).

sort2(<, X1, X2, [X1, X2]).
sort2(=, X1, _,  [X1]).
sort2(>, X1, X2, [X2, X1]).

predmerge(_, [], R, R) :- !.
predmerge(_, R, [], R) :- !.
predmerge(P, [H1|T1], [H2|T2], Result) :-
    call(P, Delta, H1, H2),
    !,
    predmerge(Delta, P, H1, H2, T1, T2, Result).

predmerge(>, P, H1, H2, T1, T2, [H2|R]) :-
    predmerge(P, [H1|T1], T2, R).
predmerge(=, P, H1, _, T1, T2, [H1|R]) :-
    predmerge(P, T1, T2, R).
predmerge(<, P, H1, H2, T1, T2, [H1|R]) :-
    predmerge(P, T1, [H2|T2], R).

%!  locale_sort(+List, -Sorted) is det.
%
%   Sort a list of atoms using the current locale.
%
%   @param List     List of atoms
%   @param Sorted   Sorted atoms.

locale_sort(List, Sorted) :-
    collation_keys(List, Keyed),
    keysort(Keyed, KeySorted),
    unkey(KeySorted, Sorted).

collation_keys([], []).
collation_keys([H|T0], [K-H|T]) :-
    collation_key(H, K),
    collation_keys(T0, T).


unkey([], []).
unkey([_-H|T0], [H|T]) :-
    unkey(T0, T).
