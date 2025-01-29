/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2021, University of Amsterdam
                         VU University Amsterdam
		         CWI, Amsterdam
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

:- module(test_monotonic,
          [ test_monotonic/0
          ]).
:- use_module(library(plunit)).
:- use_module(library(debug)).
:- use_module(library(tables)).
:- use_module(library(random)).
:- use_module(library(increval)).

test_monotonic :-
    run_tests([ monotonic_tabling,
                monotonic_tabling_2,
                monotonic_tabling_3
              ]).

:- meta_predicate
    init(0),
    cleanup(:),
    queued_answers(:,-,-),
    expect(?, 0, +),
    expect_valid(0),
    expect_invalid(0),
    expect_forced(0),
    incr_is_forced(0),
    expect_queued_answers(:, +).

:- begin_tests(monotonic_tabling).

term_expansion((test(Name, Cond0) :- Body),
               (test(Name, Cond) :- Body)) :-
    (   is_list(Cond0)
    ->  Cond = [cleanup(cleanup)|Cond0]
    ;   Cond = [cleanup(cleanup),Cond0]
    ).

cleanup :-
    abolish_all_tables,
    retractall(da(_)),
    retractall(db(_)),
    retractall(dx(_)),
    retractall(lm(_,_)),
    retractall(la(_,_)),
    retractall(nd1(_)),
    retractall(nd2(_)).

% Very basic test

:- dynamic da/1 as monotonic.
:- table pa/1 as monotonic.

pa(X) :- da(Y), X is Y+1.

test(pa1, X == 1) :-
    assertion(\+ pa(_)),
    assert(da(0)),
    assertion(get_returns_for_call(pa(_), pa(1))),
    pa(X).
test(pa2, X == 1) :-
    assert(da(0), Ref),
    pa(X),
    erase(Ref),
    assertion(\+ pa(_)).

% Combining monotonic and incremental

:- dynamic db/1 as (incremental,monotonic).
:- table pb/1 as monotonic.
:- table qb/1 as incremental.

pb(X) :- db(Y), X is Y+1.
qb(X) :- db(Y), X is Y+2.

test(pb, true) :-
    assertion(\+ pb(_)),
    assertion(\+ qb(_)),
    assert(db(0)),
    assertion((pb(X), X == 1)),
    assertion(\+ qb(1)),
    assertion((qb(X), X == 2)).

% Deal with new dependencies while propagating

:- table cm/2 as monotonic.
:- dynamic lm/2 as monotonic.

cm(X, Y) :-
    cm(Y, X).
cm(X, Z) :-
    cm(X, Y),
    cm(Y, Z).
cm(X, Y) :-
    lm(X, Y).

:- table ca/2.
:- dynamic la/2.

ca(X, Y) :-
    ca(Y, X).
ca(X, Z) :-
    ca(X, Y),
    ca(Y, Z).
ca(X, Y) :-
    la(X, Y).

test(closure, Len == 9) :-
    ignore(cm(_,_)),
    assert(lm(a, b)),
    assert(lm(a, c)),
    findall(X-Y, cm(X,Y), Pairs),
    length(Pairs, Len).
test(closure2, PairsMS == PairsAS) :-
    N = 16,
    Max is 10*N,
    ignore(cm(_,_)),
    (   between(1, N, _),
        random_between(1, Max, X),
        random_between(1, Max, Y),
        assert(lm(X,Y)),
        assert(la(X,Y)),
        fail
    ;   true
    ),
    findall(X-Y, cm(X,Y), PairsM),
    findall(X-Y, ca(X,Y), PairsA),
    sort(PairsM, PairsMS),
    sort(PairsA, PairsAS).

% Incremental tables can depend on monotonic tables

:- dynamic dx/1 as (incremental,monotonic).
:- table px/1 as monotonic.
:- table qx/1 as incremental.

px(X) :- dx(Y), X is Y+1.
qx(X) :- px(Y), X is Y*2.

test(incr_mono, true) :-
    retractall(dx(_)),
    assert(dx(1)),
    qx(X),
    assertion(X == 4),
    assert(dx(2)),
    assertion(incr_is_invalid(qx(_))),
    forall(qx(_), true),
    assertion(get_returns_for_call(qx(_), qx(6))).

% Monotonic table can have both monotonic and incremental dependents

:- table pmi/1 as monotonic.
:- dynamic dm/1 as monotonic.
:- dynamic di/1 as incremental.

pmi(X) :-
    dm(X),
    \+ di(X).

test(incr_mono, L == [1]) :-
    forall(pmi(_), true),
    asserta(dm(1)),
    asserta(dm(2)),
    asserta(di(2)),
    assertion(incr_is_invalid(pmi(_))),
    findall(X, pmi(X), L).

% Test nested reevaluation of a monotonic table that was invalidated
% due to a retract

:- dynamic (nd1/1, nd2/1) as (incremental,monotonic).
:- table (np/1,ni/1) as (incremental).
:- table (nm/1) as monotonic.

np(X) :-
    (   ni(X)
    ;   nm(X)
    ).

ni(X) :- nd1(X).
nm(X) :- nd2(X).

test(nested_reeval_mono, true) :-
    assert(nd1(1)),
    assert(nd2(2)),
    setof(X, np(X), Xs1),
    assertion(Xs1 == [1,2]),
    assert(nd1(3)),
    retract(nd2(2)),
    assert(nd2(3)),
    setof(X, np(X), Xs2),
    assertion(Xs2 == [1,3]).

:- end_tests(monotonic_tabling).

% Monotonic table that is invalidated due to retract and is
% re-evaluated twice.

:- begin_tests(monotonic_tabling_2).

:- dynamic (d1/1, d2/1, d3/1) as (incremental,monotonic).
:- table (p/1,i/1) as (incremental).
:- table (m/1,n/1) as (monotonic,lazy).

p(X) :-
    (   i(X)
    ;   m(X)
    ).

i(X) :- d3(X).
m(X) :- n(X).
m(X) :- d2(X).

n(X) :- d1(X).

clean :-
    retractall(d1(_)),
    retractall(d2(_)),
    retractall(d3(_)),
    abolish_all_tables.

test(twice_invalid, [Xs == [1,2,3], cleanup(clean)]) :-
    assert(d1(1)),
    assert(d2(2)),
    forall(p(_), true),
    retract(d1(1)),
    assert(d1(1)),
    retract(d2(2)),
    assert(d2(2)),
    assert(d3(3)),
    setof(X, p(X), Xs).

:- end_tests(monotonic_tabling_2).

:- begin_tests(monotonic_tabling_3).

:- dynamic data/1.
:- dynamic d/1 as monotonic.
:- table p/1 as monotonic.

p(X) :- d(X).
d(X) :- data(X).

test(incr_propagate) :-
    cleanup([data/1]),
    assert(data(1)),
    expect(X, p(X), [1]),
    assert(data(2)),
    expect(X, p(X), [1]),
    incr_propagate_calls(d(2)),
    expect(X, p(X), [1,2]),
    retractall(data(2)),
    expect(X, p(X), [1,2]),
    incr_invalidate_calls(d(2)),
    expect(X, p(X), [1]).

:- end_tests(monotonic_tabling_3).



		 /*******************************
		 *         TEST HELPERS		*
		 *******************************/

init(P) :-
    forall(P, true).

cleanup(M:List) :-
    abolish_all_tables,
    maplist(cleanup(M), List).

cleanup(M, PI) :-
    pi_head(PI, Head),
    retractall(M:Head).

expect(Templ, Goal, Answer) :-
    findall(Templ, Goal, R0),
    sort(R0, R),
    sort(Answer, Answer1),
    assertion(Answer1 == R).

expect_valid(Goal) :-
    assertion(\+ incr_is_invalid(Goal)).

expect_invalid(Goal) :-
    assertion(incr_is_invalid(Goal)).

expect_forced(Goal) :-
    assertion(incr_is_forced(Goal)).

incr_is_forced(Goal) :-
    get_call(Goal, ATrie, _Templ),
    '$idg_forced'(ATrie).

queued_answers(To, From, Count) :-
    get_calls(To, DstTrie, _Ret),
    '$idg_mono_affects_lazy'(DstTrie, SrcTrie, _Dep, _DepRef, Answers),
    '$tbl_table_status'(SrcTrie, _Status, From0, _Skeleton),
    unqualify(To, From0, From),
    length(Answers, Count).

unqualify(M:_, M:From, From) :- !.
unqualify(_, From, From).


expect_queued_answers(To, Expected0) :-
    findall(From-Count, queued_answers(To, From, Count), Pairs1),
    sort(Pairs1, Found),
    sort(Expected0, Expected),
    assertion(Found =@= Expected).
