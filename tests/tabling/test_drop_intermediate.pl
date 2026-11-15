/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    Copyright (c)  2026, SWI-Prolog Solutions b.v.
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

:- module(test_drop_intermediate,
          [ test_drop_intermediate/0
          ]).
:- use_module(library(plunit)).
:- use_module(library(lists)).
:- use_module(library(aggregate)).

/** <module> Test dropping intermediate tables

The Prolog flag `table_drop_intermediate` abolishes tables of completed
child components that are cheap to recompute when the root component
completes.
*/

test_drop_intermediate :-
    run_tests([ drop_intermediate
              ]).

:- table nrev/2, fib/2.

nrev([], []).
nrev([H|T], R) :-
    nrev(T, RT),
    append(RT, [H], R).

fib(0, 0).
fib(1, 1).
fib(N, F) :-
    N > 1,
    N1 is N-1,
    N2 is N-2,
    fib(N1, F1),
    fib(N2, F2),
    F is F1+F2.

with_drop(F, G) :-
    current_prolog_flag(table_drop_intermediate, Old),
    setup_call_cleanup(
        set_prolog_flag(table_drop_intermediate, F),
        G,
        set_prolog_flag(table_drop_intermediate, Old)).

tables(Name, N) :-
    aggregate_all(count,
                  ( current_table(test_drop_intermediate:V, _),
                    functor(V, Name, _)
                  ), N).

:- begin_tests(drop_intermediate, [cleanup(abolish_all_tables)]).

test(off, N == 301) :-
    abolish_all_tables,
    numlist(1, 300, L),
    nrev(L, R),
    assertion(reverse(L, R)),
    tables(nrev, N).
test(chain, true(N < 150)) :-
    abolish_all_tables,
    numlist(1, 300, L),
    with_drop(10, nrev(L, R)),
    assertion(reverse(L, R)),
    tables(nrev, N),
    assertion(N > 1),
    assertion(current_table(test_drop_intermediate:nrev(L, _), _)).
test(recompute) :-
    abolish_all_tables,
    numlist(1, 300, L),
    with_drop(10, nrev(L, _)),
    forall(between(1, 300, I),
           ( numlist(I, 300, S),
             nrev(S, R),
             assertion(reverse(S, R))
           )).
test(reused, N == 201) :-               % fib/2 tables are called again
    abolish_all_tables,
    with_drop(10, fib(200, F)),
    assertion(F =:= 280571172992510140037611932413038677189525),
    tables(fib, N).

:- end_tests(drop_intermediate).
