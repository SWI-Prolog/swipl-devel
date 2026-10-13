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

:- module(test_mono_reuse,
          [ test_mono_reuse/0
          ]).
:- use_module(library(plunit)).
:- use_module(library(lists)).

/** <module> More tests for monotonic propagation to reused tables

Complements test_mono_subsumptive.pl.  Each consumer is called with the
same variant before and after the update, so a stale table is not hidden
by a fresh one.
*/

test_mono_reuse :-
    run_tests([ mono_reuse_subsumptive,
                mono_reuse_abstract
              ]).

:- meta_predicate all(?, 0, -).
all(T, G, L) :- findall(T, G, L0), msort(L0, L).

:- begin_tests(mono_reuse_subsumptive).

:- dynamic e/2 as monotonic.
:- dynamic s0/1 as monotonic.
:- dynamic inc/1 as incremental.

:- table p/2 as (subsumptive, monotonic).
:- table r1/1 as monotonic.
:- table r2/1 as monotonic.
:- table r3/1 as monotonic.
p(X,Y) :- e(X,Y).
r1(Y) :- p(a, Y).
r2(X) :- p(X, b).
r3(Z) :- p(Z, Z).

:- table sp/2 as (subsumptive, monotonic).
:- table sq/1 as monotonic.
sp(X,Y) :- e(X,Y).
sp(X,Y) :- e(X,Z), sp(Z,Y).
sq(Y) :- sp(k, Y).

:- table tq/1 as monotonic.
tq(Y) :- s0(X), p(X, Y).

:- table ip/2 as (subsumptive, monotonic, incremental).
:- table iq/1 as (monotonic, incremental).
ip(X,Y) :- e(X,Y), \+ inc(Y).
iq(Y) :- ip(k, Y).

init :-
    abolish_all_tables,
    retractall(e(_,_)),
    retractall(s0(_)),
    retractall(inc(_)).

test(binding_patterns, [cleanup(init)]) :-
    init,
    all(X-Y, p(X,Y), []),
    all(Y, r1(Y), []), all(X, r2(X), []), all(Z, r3(Z), []),
    assertz(e(a, b)),
    assertion(all(Y, r1(Y), [b])),
    assertion(all(X, r2(X), [a])),
    assertion(all(Z, r3(Z), [])),
    assertz(e(c, c)),
    assertion(all(Z, r3(Z), [c])),
    assertz(e(a, a)),
    assertion(all(Y, r1(Y), [a,b])),
    assertion(all(X, r2(X), [a])),
    assertion(all(Z, r3(Z), [a,c])).
test(subsumed_recursive, [cleanup(init)]) :-
    init,
    assertz(e(k, m)),
    all(X-Y, sp(X,Y), _),
    assertion(all(Y, sq(Y), [m])),
    assertz(e(m, n)),
    assertion(all(Y, sq(Y), [m,n])),
    assertz(e(q, k)),
    assertion(all(Y, sq(Y), [m,n])),
    assertz(e(n, q)),
    assertion(all(Y, sq(Y), [k,m,n,q])).
test(dep_during_propagation, [cleanup(init)]) :-
    init,
    assertz(e(a, 1)), assertz(e(b, 2)),
    all(X-Y, p(X,Y), _),
    assertion(all(Y, tq(Y), [])),
    assertz(s0(a)),                     % propagation calls subsumed p(a,Y)
    assertion(all(Y, tq(Y), [1])),
    assertz(e(a, 3)),                   % reaches tq/1 through the new dep
    assertion(all(Y, tq(Y), [1,3])),
    assertz(e(b, 4)),
    assertion(all(Y, tq(Y), [1,3])).
test(incremental, [cleanup(init)]) :-
    init,
    all(X-Y, ip(X,Y), _),
    assertion(all(Y, iq(Y), [])),
    assertz(e(k, 1)),
    assertion(all(Y, iq(Y), [1])),
    assertz(inc(1)),
    assertion(all(Y, iq(Y), [])),
    assertz(e(k, 2)),
    assertion(all(Y, iq(Y), [2])),
    retract(inc(1)),
    assertion(all(Y, iq(Y), [1,2])).

:- end_tests(mono_reuse_subsumptive).

:- begin_tests(mono_reuse_abstract,
               [ setup(abstract_subgoals),
                 cleanup(restore_subgoal_action)
               ]).

:- dynamic kb/2 as monotonic.
:- dynamic d/2 as incremental.

:- table kp/1 as (subgoal_abstract(0), monotonic).
:- table kq/2 as monotonic.
kp(k(X,Y)) :- kb(X,Y).
kq(X,Y) :- kp(k(X,Y)).

:- table ia/1 as (subgoal_abstract(0), incremental).
ia(args(X,Y)) :- d(X,Y).

abstract_subgoals :-
    current_prolog_flag(max_table_subgoal_size_action, Action),
    nb_setval(mono_reuse_subgoal_action, Action),
    set_prolog_flag(max_table_subgoal_size_action, abstract).

restore_subgoal_action :-
    nb_getval(mono_reuse_subgoal_action, Action),
    set_prolog_flag(max_table_subgoal_size_action, Action).

init :-
    abolish_all_tables,
    retractall(kb(_,_)),
    retractall(d(_,_)).

test(indirect_keys, [cleanup(init)]) :-
    init,
    B is 2^100, S = "a string", F = 1.5,
    all(Y, kq(B,Y), []), all(Y, kq(S,Y), []), all(Y, kq(F,Y), []),
    assertz(kb(B, big)), assertz(kb(S, str)), assertz(kb(F, flt)),
    assertz(kb(2^100, wrong)),
    assertion(all(Y, kq(B,Y), [big])),
    assertion(all(Y, kq(S,Y), [str])),
    assertion(all(Y, kq(F,Y), [flt])).
test(incremental_reevaluation, L2-L3 == [a,b]-[c]) :-
    init,
    assertz(d(1,a)),
    all(Y, ia(args(1,Y)), [a]),
    assertz(d(1,b)), assertz(d(2,c)),
    all(Y, ia(args(1,Y)), L2),
    all(Y, ia(args(2,Y)), L3),
    init.

:- end_tests(mono_reuse_abstract).
