/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
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


:- module(test_incr_dyn_nodes,
          [ test_incr_dyn_nodes/0
          ]).
:- use_module(library(plunit)).

/** <module> Reclaiming the IDG nodes of incremental dynamic predicates

A call from a table to an incremental  dynamic predicate adds a dynamic
pseudo table to the  variant trie, whose IDG node records  which tables
depend on  the call variant.  Once  the last of these  tables is  gone the
node is useless.  Abolishing  tables per module or per subgoal  used to
leave all of them behind, so a process that loads and abolishes many KBs
kept growing.  Only abolish_all_tables/0 reclaimed them.  Reevaluation
drops the edges of the calls it no longer makes; these nodes are
reclaimed by a sweep after completing an evaluation.
*/

test_incr_dyn_nodes :-
    run_tests([ incr_dyn_nodes
              ]).

:- dynamic p/1 as incremental.
:- dynamic r/1 as incremental.
:- table q/1 as incremental.
:- table s/1 as incremental.

q(X) :- p(X), r(X).
s(X) :- r(X).

:- dynamic key/1 as incremental.
:- dynamic val/1 as incremental.
:- table kv/1 as incremental.

kv(K) :- key(K), val(K).                % each reevaluation calls a new val(K)

%   The number of dynamic pseudo tables in this thread's variant table.

dyn_nodes(N) :-
    '$tbl_local_variant_table'(VTrie),
    aggregate_all(count,
                  ( trie_gen(VTrie, _, T),
                    '$tbl_table_status'(T, dynamic)
                  ),
                  N).

fill :-
    abolish_all_tables,
    forall(between(1, 10, I), ( ignore(q(I)), ignore(s(I)) )).

reeval_kv(Count) :-
    abolish_all_tables,
    forall(between(1, Count, I),
           ( retractall(key(_)),
             assertz(key(I)),
             ignore(kv(_))
           )),
    retractall(key(_)).

:- begin_tests(incr_dyn_nodes).

test(subgoals, N == 10) :-
    fill,
    abolish_table_subgoals(q(_)),       % r(I) are still used by s/1
    dyn_nodes(N).
test(module, N == 0) :-
    fill,
    abolish_module_tables(test_incr_dyn_nodes),
    dyn_nodes(N).
test(still_incremental,
     [ cleanup(( retractall(test_incr_dyn_nodes:p(_)),
                 retractall(test_incr_dyn_nodes:r(_)) ))
     ]) :-
    fill,
    abolish_table_subgoals(q(_)),
    assertz(test_incr_dyn_nodes:r(5)),  % invalidates s(5) through a kept node
    s(5),
    assertz(test_incr_dyn_nodes:p(5)),
    q(5),
    abolish_module_tables(test_incr_dyn_nodes),
    retract(test_incr_dyn_nodes:r(5)),
    \+ q(5),
    \+ s(5).
test(reeval, true(N < 2000)) :-
    reeval_kv(5000),
    dyn_nodes(N).

:- end_tests(incr_dyn_nodes).
