/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2020, VU University Amsterdam
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

:- module(test_hash_table,
          [ test_hash_table/0
          ]).
:- use_module(library(plunit)).
:- use_module(library(assoc)).
:- use_module(library(debug)).
:- use_module(library(random)).

:- use_module(library(hashtable)).

test_hash_table :-
    run_tests([ hashtable
              ]).

:- begin_tests(hashtable).

test(rfill) :-
    rfill(1000, 100, 1, HT, Assoc),
    assertion(ht_is_hashtable(HT)),
    assertion(same(HT, Assoc)).
test(rdel) :-
    rfill(1000, 100, 0.7, HT, Assoc),
    assertion(ht_is_hashtable(HT)),
    assertion(same(HT, Assoc)).
test(empty) :-
    ht_new(HT),
    assertion(\+ ht_get(HT, 1, _)).
test(empty, Size == 0) :-
    ht_new(HT),
    \+ ht_del(HT, 1, _),
    ht_size(HT, Size).
test(size, Size = 3) :-
    ht_pairs(HT, [1-a,2-b,3-c]),
    ht_size(HT, Size).
test(put5, List == [b,a]) :-
    ht_new(HT),
    ht_put(HT, 1, [a|O0], [], O0),
    ht_put(HT, 1, [b|O1], [], O1),
    ht_get(HT, 1, List).
test(put5, Pairs == [1-b, 3-4]) :-
    ht_pairs(HT, [1-2,3-4]),
    ht_update(HT, 1, Old, b),
    assertion(Old == 2),
    ht_pairs(HT, Pairs).
test(wc, Pairs == [aap-2, noot-1]) :-
    ht_new(HT),
    update_word_count(HT, aap),
    update_word_count(HT, noot),
    update_word_count(HT, aap),
    ht_pairs(HT, Pairs).

rfill(N, Max, FD, HT, Assoc) :-
    ht_new(HT),
    empty_assoc(A0),
    rfill(N, Max, FD, HT, A0, Assoc).

rfill(N, Max, FD, HT, A0, A) :-
    N > 0,
    !,
    random_between(1, Max, K),
    (   maybe(FD)
    ->  ht_put(HT, K, K),
        put_assoc(K, A0, K, A1)
    ;   (   del_assoc(K, A0, V0, A1)
        ->  (   ht_del(HT, K, V1)
            ->  assertion(V1 == V0)
            ;   format('Failed: ~p~n', ht_del(HT, K, _)),
                fail
            )
        ;   A1 = A0,
            assertion(\+ht_del(HT, K, _))
        )
    ),
    N2 is N - 1,
    rfill(N2, Max, FD, HT, A1, A).
rfill(_, _, _, _, A, A).

same(HT, A) :-
    assoc_to_list(A, AList),
    ht_pairs(HT, HList),
    sort(HList, SList),
    AList == SList.

update_word_count(HT, Word) :-
    (   ht_update(HT, Word, Old, New)
    ->  New is Old+1
    ;   ht_put(HT, Word, 1)
    ).

:- end_tests(hashtable).
