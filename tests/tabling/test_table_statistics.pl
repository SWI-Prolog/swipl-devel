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

:- module(test_table_statistics,
          [ test_table_statistics/0
          ]).
:- use_module(library(plunit)).
:- use_module(library(tableutil)).
:- use_module(library(pairs), [pairs_keys/2]).


/** <module> Test table_statistics/2,3

The variant table also holds, for incremental dynamic predicates, the
variants that incremental tables depend on.  These are nodes of the
dependency graph without answers, which table_statistics/3 reports as
`dependency_nodes` rather than as answer tables.
*/

test_table_statistics :-
    run_tests([ table_statistics
              ]).

:- dynamic([ts_data/1], [incremental(true)]).
:- table ts_inc/1 as incremental.
:- table ts_plain/1.

ts_data(1).
ts_data(2).

ts_inc(X) :- ts_data(X).
ts_plain(X) :- between(1, 3, X).

fill :-
    abolish_all_tables,
    forall(ts_inc(_), true),
    forall(between(1, 3, I), ignore(ts_inc(I))),
    forall(ts_plain(_), true).

stats(Variant, Pairs) :-
    findall(K-V, table_statistics(Variant, K, V), Pairs0),
    msort(Pairs0, Pairs).

:- begin_tests(table_statistics).

test(dependency_nodes, [ Keys == [dependency_nodes, dependency_space],
                         Nodes == 4,
                         true(Space > 0) ]) :-
    fill,
    stats(test_table_statistics:ts_data(_), Pairs),
    pairs_keys(Pairs, Keys),
    memberchk(dependency_nodes-Nodes, Pairs),
    memberchk(dependency_space-Space, Pairs).
test(answer_tables, [ Tables-Answers == 4-4,
                      true(\+ memberchk(dependency_nodes-_, Pairs)) ]) :-
    fill,
    stats(test_table_statistics:ts_inc(_), Pairs),
    memberchk(tables-Tables, Pairs),
    memberchk(answers-Answers, Pairs).
test(plain_answers, Answers == 3) :-
    fill,
    table_statistics(test_table_statistics:ts_plain(_), answers, Answers).
test(no_tables, Tables == 0) :-
    abolish_all_tables,
    table_statistics(test_table_statistics:ts_plain(_), tables, Tables).

:- end_tests(table_statistics).
