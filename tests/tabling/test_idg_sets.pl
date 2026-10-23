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


:- module(test_idg_sets,
          [ test_idg_sets/0
          ]).
:- use_module(library(plunit)).
:- use_module(library(aggregate)).

/** <module> Test the edge sets of the incremental dependency graph

The edges of an IDG node are kept in a set that is a single pointer for
one edge, a linear block for a few and a hashed block for many (see
src/pl-idgset.h).  These tests create nodes with many edges in both
directions, invalidate and re-evaluate them and delete edges by
abolishing tables.  The unit idg_reader reads the IDG of a thread from
another thread while that thread modifies it.
*/

test_idg_sets :-
    run_tests([ idg_sets,
                idg_reader
              ]).

:- dynamic([d/1], [incremental(true)]).
:- table p/1 as incremental.		% p(I) depends on d(_)
:- table q/1 as incremental.		% q(N) depends on d(1) ... d(N)

d(1).

p(I) :- d(X), X >= 0, I >= 0.
q(N) :- aggregate_all(count, (between(1, N, I), d(I)), N0), N0 >= 0.

fill(N) :-
    abolish_all_tables,
    retractall(d(_)),
    assertz(d(1)),
    forall(between(1, N, I), p(I)),
    q(N).

variant_trie(Goal, Trie) :-
    '$tbl_variant_table'(VT),
    trie_gen(VT, test_idg_sets:Goal, Trie).

edges(Goal, Dir, Count) :-
    once(variant_trie(Goal, Trie)),
    aggregate_all(count, '$idg_edge'(Trie, Dir, _), Count).

%   Assert in this module; the tests run in the module of the unit.

add_fact(X) :-
    assertz(d(X)).

invalid_tables(Count) :-
    aggregate_all(count,
                  ( variant_trie(p(_), T),
                    '$idg_falsecount'(T, FC), FC > 0
                  ), Count).

:- begin_tests(idg_sets).

test(affected_many, Count == 100) :-
    fill(100),
    edges(d(_), affected, Count).
test(dependent_many, Count == 100) :-
    fill(100),
    edges(q(100), dependent, Count).
test(edge_lookup, true) :-
    fill(100),
    once(variant_trie(d(_), D)),
    once(variant_trie(p(57), P)),
    '$idg_edge'(D, affected, P).
test(invalidate_all, Invalid == 100) :-
    fill(100),
    add_fact(2),
    invalid_tables(Invalid).
test(reevaluate, [Count-Invalid == 100-0]) :-
    fill(100),
    add_fact(2),
    forall(between(1, 100, I), p(I)),
    invalid_tables(Invalid),
    edges(d(_), affected, Count).
test(delete_edges, Count == 50) :-
    fill(100),
    forall(between(1, 50, I), abolish_table_subgoals(p(I))),
    edges(d(_), affected, Count).
test(delete_and_add, Count == 100) :-
    fill(100),
    forall(between(1, 50, I), abolish_table_subgoals(p(I))),
    forall(between(1, 50, I), p(I)),
    edges(d(_), affected, Count).
test(single_edge, Count == 1) :-
    fill(1),
    edges(d(_), affected, Count).

:- end_tests(idg_sets).

%   own_summary(-Nodes, -Edges)
%
%   Count the IDG nodes and affected edges of this thread from its
%   variant table.

own_summary(Nodes, Edges) :-
    '$tbl_local_variant_table'(VT),
    aggregate_all(count-sum(E),
                  ( trie_gen(VT, _, T),
                    '$idg_falsecount'(T, _),     % has an IDG node
                    aggregate_all(count, '$idg_edge'(T, affected, _), E)
                  ),
                  Nodes-Edges).

%   modify(+Deadline)
%
%   Keep changing the IDG of this thread until Deadline.

modify(Deadline) :-
    get_time(Now),
    (   Now > Deadline
    ->  true
    ;   fill(50),
        add_fact(3),
        forall(between(1, 25, I), p(I)),
        forall(between(26, 50, I), abolish_table_subgoals(p(I))),
        q(50),
        retract(d(3)),
        modify(Deadline)
    ).

read_idg(Thread, Deadline, Reads0, Reads) :-
    get_time(Now),
    (   Now > Deadline
    ->  Reads = Reads0
    ;   '$idg_thread_summary'(Thread, Nodes, Edges),
        assertion((Nodes >= 0, Edges >= 0)),
        '$idg_predicate_edges'(Thread, PNodes, _),
        assertion(is_list(PNodes)),
        Reads1 is Reads0+1,
        read_idg(Thread, Deadline, Reads1, Reads)
    ).

:- begin_tests(idg_reader).

test(summary, Outside == Inside) :-
    thread_self(Me),
    thread_create(( fill(100),
                    own_summary(N, E),
                    thread_send_message(Me, inside(N-E)),
                    thread_get_message(done)
                  ), Id),
    thread_get_message(inside(Inside)),
    '$idg_thread_summary'(Id, N1, E1),
    Outside = N1-E1,
    thread_send_message(Id, done),
    thread_join(Id).
test(concurrent, true(Reads > 0)) :-
    get_time(Now),
    Deadline is Now+1.5,
    thread_create(modify(Deadline), Id),
    read_idg(Id, Deadline, 0, Reads),
    thread_join(Id).
test(exited, Nodes-Edges == 0-0) :-
    thread_create(fill(10), Id),
    thread_join(Id),
    catch('$idg_thread_summary'(Id, Nodes, Edges), _, (Nodes-Edges = 0-0)).
%   '$idg_predicate_edges'/3 summarizes the IDG per predicate.

test(predicate_edges, [ Nodes-Edges ==
                        [ node(test_idg_sets:d/1, 101, 0),
                          node(test_idg_sets:p/1, 100, 0),
                          node(test_idg_sets:q/1, 1, 0)
                        ] -
                        [ edge(test_idg_sets:d/1, test_idg_sets:p/1, 100),
                          edge(test_idg_sets:d/1, test_idg_sets:q/1, 100)
                        ]
                      ]) :-
    fill(100),
    thread_self(Me),
    '$idg_predicate_edges'(Me, Nodes0, Edges0),
    msort(Nodes0, Nodes),
    msort(Edges0, Edges).
test(predicate_edges_invalid, Invalid == 100) :-
    fill(100),
    add_fact(2),
    thread_self(Me),
    '$idg_predicate_edges'(Me, Nodes, _),
    memberchk(node(test_idg_sets:p/1, _, Invalid), Nodes).
test(predicate_edges_outside, Outside == Inside) :-
    thread_self(Me),
    thread_create(( fill(100),
                    thread_self(Self),
                    '$idg_predicate_edges'(Self, N, E),
                    msort(N, SN), msort(E, SE),
                    thread_send_message(Me, inside(SN-SE)),
                    thread_get_message(done)
                  ), Id),
    thread_get_message(inside(Inside)),
    '$idg_predicate_edges'(Id, N1, E1),
    msort(N1, SN1), msort(E1, SE1),
    Outside = SN1-SE1,
    thread_send_message(Id, done),
    thread_join(Id).
test(predicate_edges_prolog, Edges == PrologEdges) :-
    fill(30),
    thread_self(Me),
    '$idg_predicate_edges'(Me, _, Edges0),
    msort(Edges0, Edges),
    '$tbl_local_variant_table'(VT),
    findall(F-T, ( trie_gen(VT, FM:FV, FT),
                   '$idg_edge'(FT, affected, DT),
                   '$tbl_table_status'(DT, _, TM:TV, _),
                   functor(FV, FN, FA), functor(TV, TN, TA),
                   F = FM:FN/FA, T = TM:TN/TA
                 ), Pairs),
    msort(Pairs, Sorted),
    clumped(Sorted, Counted),
    findall(edge(F,T,C), member((F-T)-C, Counted), PrologEdges).
test(shared, true) :-
    '$idg_thread_summary'(shared, Nodes, Edges),
    integer(Nodes), integer(Edges).

:- end_tests(idg_reader).
