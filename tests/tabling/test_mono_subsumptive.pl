/*  Part of SWI-Prolog

    Author:        MesTTo
    Copyright (c)  2026, MesTTo
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

:- module(test_mono_subsumptive,
          [ test_mono_subsumptive/0
          ]).
:- use_module(library(plunit)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(aggregate)).

/** <module> Monotonic tabling through subsumptive and abstract tables

Test monotonic tabling where a call is answered from a table that is not
its own new variant table:  a  subsumed  call  reading  a  more  general
table, a call reusing an existing variant table and a call  abstracted
by subgoal_abstract(N).  Such calls did not record their dependency on
the table they read, so answers that monotonic propagation added to that
table later never reached them.

Each such call records its dependency with its own call as the skeleton,
so several calls on one table hold dependencies that a new answer may or
may not match.  Propagation must skip the dependencies an answer does not
match rather than stop at the first of them.

subgoal_abstract(N) abstracts a call only while the thread's flag
max_table_subgoal_size_action is `abstract`.  Under the default, `error`,
such a call raises the max_table_subgoal_size tripwire, so the unit
mono_abstract sets the flag and restores it afterwards.
*/

test_mono_subsumptive :-
    run_tests([ mono_subsumptive,
                mono_abstract
              ]).

:- begin_tests(mono_subsumptive).

:- dynamic d/1 as monotonic.
:- table p/1 as (subsumptive, monotonic).
:- table q/1 as monotonic.

p(X) :- d(X).
q(X) :- p(X).                           % q(a) calls the subsumed p(a)

init :-
    abolish_all_tables,
    retractall(d(_)).

test(subsumed_empty_then_added, [cleanup(init)]) :-
    init,
    forall(p(_), true),                 % create the general table
    \+ q(a),
    assertz(d(a)),
    assertion(q(a)),
    assertion(\+ q(b)).
test(subsumed_warm, [cleanup(init)]) :-
    init,
    assertz(d(a)),
    forall(p(_), true),
    assertion(q(a)),
    assertz(d(b)),
    assertion(q(b)).
test(existing_variant, [cleanup(init)]) :-
    init,
    \+ p(a),                            % p(a) gets its own variant table
    \+ q(a),                            % and q(a) reuses it
    assertz(d(a)),
    assertion(q(a)).
test(retract_reassert, [cleanup(init)]) :-
    init,
    forall(p(_), true),
    assertz(d(a)),
    assertion(q(a)),
    retract(d(a)),
    assertion(\+ q(a)),
    assertz(d(a)),
    assertion(q(a)).

:- end_tests(mono_subsumptive).

:- begin_tests(mono_abstract,
               [ setup(abstract_subgoals),
                 cleanup(restore_subgoal_action)
               ]).

:- dynamic e/1 as monotonic.
:- dynamic edge/2 as monotonic.
:- dynamic e2/2 as monotonic.

:- table a/1 as (subgoal_abstract(0), monotonic).
:- table b/1 as monotonic.
:- table v/1 as (subgoal_abstract(0), monotonic).
:- table w/1 as monotonic.
:- table reach/1 as (subgoal_abstract(0), monotonic).
:- table from/2 as monotonic.
:- table la/1 as (subgoal_abstract(0), monotonic, lazy).
:- table lb/1 as (monotonic, lazy).
:- table pairs/1 as (subgoal_abstract(0), monotonic).
:- table same/1 as monotonic.
:- table firsts/1 as monotonic.

a(args(X)) :- e(X).                     % a(args(c)) is abstracted to a(_)
b(X) :- a(args(X)).
v(X) :- e(X).                           % v(c) is not abstracted
w(X) :- v(X).
reach(t(X,Y)) :- edge(X,Y).
reach(t(X,Z)) :- reach(t(X,Y)), edge(Y,Z).
from(X, Y) :- reach(t(X,Y)).
la(args(X)) :- e(X).
lb(X) :- la(args(X)).
pairs(args(X, Y)) :- e2(X, Y).
same(X) :- pairs(args(X, X)).          % a non-linear skeleton
firsts(X) :- pairs(args(X, _)).

abstract_subgoals :-
    current_prolog_flag(max_table_subgoal_size_action, Action),
    nb_setval(mono_subgoal_action, Action),
    set_prolog_flag(max_table_subgoal_size_action, abstract).

restore_subgoal_action :-
    nb_getval(mono_subgoal_action, Action),
    set_prolog_flag(max_table_subgoal_size_action, Action).

init :-
    abolish_all_tables,
    retractall(e(_)),
    retractall(edge(_,_)),
    retractall(e2(_,_)).

answers(Goal, Template, List) :-
    findall(Template, Goal, List0),
    msort(List0, List).

a_tables(Count) :-
    aggregate_all(count, current_table(a(_), _), Count).

test(empty_then_added, [cleanup(init)]) :-
    init,
    \+ b(c),
    assertz(e(c)),
    assertion(b(c)),
    assertion(\+ b(d)),
    assertz(e(d)),
    assertion(b(d)).
test(many_consumers, [Gained == Keys, cleanup(init)]) :-
    init,
    numlist(1, 20, Keys),
    forall(member(K, Keys), \+ b(K)),   % 20 dependencies on one a(_)
    forall(member(K, Keys), assertz(e(K))),
    findall(K, (member(K, Keys), b(K)), Gained).
test(index_shapes, [S1-F1-S2-F2 == []-[a]-[c]-[a,c], cleanup(init)]) :-
    init,
    \+ same(_),
    \+ firsts(_),
    assertz(e2(a, b)),                  % matches firsts' skeleton only
    answers(same(X), X, S1),
    answers(firsts(X), X, F1),
    assertz(e2(c, c)),                  % matches both
    answers(same(X), X, S2),
    answers(firsts(X), X, F2).
test(one_shared_table, [Count == 1, cleanup(init)]) :-
    init,
    \+ b(c),
    \+ b(d),
    assertz(e(c)),
    assertion(b(c)),
    a_tables(Count).
test(warm, [cleanup(init)]) :-
    init,
    assertz(e(c)),
    assertion(b(c)),
    assertz(e(d)),
    assertion(b(d)).
test(not_abstracted, [cleanup(init)]) :-
    init,
    \+ w(c),
    assertz(e(c)),
    assertion(w(c)).
test(recursive, [L1-L2-L3 == [b]-[b,c]-[a,b,c], cleanup(init)]) :-
    init,
    assertz(edge(a,b)),
    answers(from(a,Y), Y, L1),
    assertz(edge(b,c)),
    answers(from(a,Y), Y, L2),
    assertz(edge(c,a)),
    answers(from(a,Y), Y, L3).
test(new_answer, [Events == [c], cleanup((init, prolog_unlisten(b/1, on_answer)))]) :-
    init,
    nb_setval(mono_events, []),
    prolog_listen(b/1, on_answer),
    \+ b(c),
    assertz(e(c)),
    nb_getval(mono_events, Events).
test(retract_reassert, [cleanup(init)]) :-
    init,
    assertz(e(c)),
    assertion(b(c)),
    retract(e(c)),
    assertion(\+ b(c)),
    assertz(e(c)),
    assertion(b(c)).
test(rollback, [cleanup(init)]) :-
    init,
    assertz(e(c)),
    assertion(b(c)),
    \+ b(d),                            % b(d) and b(f) exist before the
    \+ b(f),                            % transaction and the snapshot
    \+ transaction((assertz(e(d)), assertion(b(d)), fail)),
    assertion(\+ b(d)),
    snapshot((assertz(e(f)), assertion(b(f)))),
    assertion(\+ b(f)),
    assertion(b(c)).
test(lazy, [cleanup(init)]) :-
    init,
    \+ lb(c),
    assertz(e(c)),
    assertion(lb(c)).

on_answer(new_answer, _:b(X)) :-
    nb_getval(mono_events, Events),
    append(Events, [X], Events1),
    nb_setval(mono_events, Events1).

:- end_tests(mono_abstract).
