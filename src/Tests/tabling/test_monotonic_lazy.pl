/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2020-2021, SWI-Prolog Solutions b.v.
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

:- module(test_monotonic_lazy,
          [ test_monotonic_lazy/0
          ]).
:- use_module(library(plunit)).
:- use_module(library(dialect/xsb/increval)).
:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(prolog_code)).
:- use_module(library(tables)).

/** <module> Test lazy monotonic tabling
*/

test_monotonic_lazy :-
    run_tests([ tabling_monotonic_lazy_1,
                tabling_monotonic_lazy_2,
                tabling_monotonic_lazy_3,
                tabling_monotonic_lazy_4,
                tabling_monotonic_lazy_5,
                tabling_monotonic_lazy_6,
                tabling_monotonic_lazy_7,
                tabling_monotonic_lazy_8
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

% ================================================================
% Elementary testing

:- begin_tests(tabling_monotonic_lazy_1).

:- dynamic d/1 as monotonic.
:- table p/1 as (monotonic,lazy).

p(X) :- d(X).

test(simple, [cleanup(cleanup([d/1]))]) :-
    init(p(_)),
    assertz(d(1)),
    expect_invalid(p(_)),
    expect(X, p(X), [1]),
    expect_valid(p(_)),
    assertz(d(2)),
    expect_invalid(p(_)),
    expect(X, p(X), [1,2]),
    expect_valid(p(_)).
test(retract, [cleanup(cleanup([d/1]))]) :-
    init(p(_)),
    assertz(d(1)),
    assertz(d(2)),
    expect(X, p(X), [1,2]),
    retract(d(1)),
    expect(X, p(X), [2]).

:- end_tests(tabling_monotonic_lazy_1).

% ================================================================
% Both lazy and non-lazy dependents

:- begin_tests(tabling_monotonic_lazy_2).

:- dynamic d/1 as monotonic.
:- table p1/1 as (monotonic,lazy).
:- table p2/1 as (monotonic).

p1(X) :- d(X).
p2(X) :- d(X).

test(mon_and_lazy, [cleanup(cleanup([d/1]))]) :-
    init(p1(_)),
    init(p2(_)),
    assertz(d(1)),
    expect_invalid(p1(_)),
    expect_queued_answers(p1(_), [d(_)-1]),
    expect_valid(p2(_)),
    expect(X, p1(X), [1]),
    expect(X, p2(X), [1]),
    expect_valid(p1(_)).

:- end_tests(tabling_monotonic_lazy_2).

% ================================================================
% Lazy intermediate tables

:- begin_tests(tabling_monotonic_lazy_3).

:- dynamic d/1 as monotonic.
:- table (p1/1,p2/1) as (monotonic,lazy).

p1(X) :- d(X).
p2(X) :- p1(X).

test(intermediate) :-
    cleanup([d/1]),
    init(p2(_)),
    assertz(d(1)),
    expect_invalid(p1(_)),
    expect_invalid(p2(_)),
    expect(X, p2(X), [1]),
    expect_valid(p1(_)),
    expect_valid(p2(_)),
    assertz(d(2)),
    expect(X, p2(X), [1,2]).

:- end_tests(tabling_monotonic_lazy_3).

% ================================================================
% Lazy table depending on eager

:- begin_tests(tabling_monotonic_lazy_4).

:- dynamic d/1 as monotonic.
:- table p1/1 as monotonic.
:- table p2/1 as (monotonic,lazy).

p1(X) :- d(X).
p2(X) :- p1(X).

test(lazy_on_eager) :-
    cleanup([d/1]),
    assert(d(1)),
    expect(X, p1(X), [1]),
    expect(X, p2(X), [1]),
    assert(d(2)),
    expect(X, p1(X), [1,2]),
    expect_invalid(p2(_)),
    expect(X, p2(X), [1,2]),
    true.

:- end_tests(tabling_monotonic_lazy_4).

% ================================================================
% Eager table depending on lazy

:- begin_tests(tabling_monotonic_lazy_5).

:- dynamic d/1 as monotonic.
:- table p1/1 as (monotonic,lazy).
:- table p2/1 as monotonic.

p1(X) :- d(X).
p2(X) :- p1(X).

test(lazy_on_eager) :-
    cleanup([d/1]),
    assert(d(1)),
    expect(X, p1(X), [1]),
    expect(X, p2(X), [1]),
    assert(d(2)),
    expect_invalid(p2(_)),
    expect(X, p2(X), [1,2]).
test(lazy_on_eager_retract) :-
    cleanup([d/1]),
    assert(d(1), Ref),
    expect(X, p1(X), [1]),
    expect(X, p2(X), [1]),
    assert(d(2)),
    expect_invalid(p2(_)),
    expect(X, p2(X), [1,2]),
    erase(Ref),
    expect_invalid(p1(_)),
    expect_invalid(p2(_)),
    expect(X, p2(X), [2]),
    expect(X, p1(X), [2]).

:- end_tests(tabling_monotonic_lazy_5).

:- begin_tests(tabling_monotonic_lazy_6).

% Deal with depending answer subsumptive tabling

:- dynamic (d1/1,d2/1) as monotonic.
:- table (p(min),r(min),q(min)) as (monotonic,lazy).
:- dynamic reeval/0.

r(X) :- d1(X).
q(X) :- d2(X).
p(X) :- assert(reeval), r(X) ; q(X).

test(lazy_reeval) :-
    cleanup([d1/1, d2/1]),
    expect(X, p(X), []),
    retractall(reeval),
    assert(d1(1)),
    expect(X, p(X), [1]),
    assertion(\+ reeval),
    assert(d1(0)),
    expect(X, p(X), [0]),
    assertion(\+ reeval),
    assert(d2(-1)),
    expect(X, p(X), [-1]),
    assertion(\+ reeval),
    assert(d2(2)),
    expect(X, p(X), [-1]),
    assertion(\+ reeval).

:- end_tests(tabling_monotonic_lazy_6).

:- begin_tests(tabling_monotonic_lazy_7).

% Deal with depending answer subsumptive tabling (non-lazy)

:- dynamic (d1/1,d2/1) as monotonic.
:- table (p(min),r(min),q(min)) as (monotonic).
:- dynamic reeval/0.

r(X) :- d1(X).
q(X) :- d2(X).
p(X) :- assert(reeval), r(X) ; q(X).

test(lazy_reeval) :-
    cleanup([d1/1, d2/1]),
    expect(X, p(X), []),
    retractall(reeval),
    assert(d1(1)),
    expect(X, p(X), [1]),
    assertion(\+ reeval),
    assert(d1(0)),
    expect(X, p(X), [0]),
    assertion(\+ reeval),
    assert(d2(-1)),
    expect(X, p(X), [-1]),
    assertion(\+ reeval),
    assert(d2(2)),
    expect(X, p(X), [-1]),
    assertion(\+ reeval).


:- end_tests(tabling_monotonic_lazy_7).


:- begin_tests(tabling_monotonic_lazy_8).

% Verify that a switch to incremental due to a retract propagates.
% Note that eventually this may change.

:- dynamic dm/1 as monotonic.
:- dynamic xm/1 as monotonic.
:- table aml/1 as (monotonic,lazy).
:- table bml/1 as (monotonic,lazy).

bml(X) :- aml(X) ; xm(X).
aml(X) :- dm(X).

test(indirect_lazy_retract) :-
    assert(dm(1)),
    assert(dm(2)),
    expect(X, bml(X), [1,2]),
    assert(xm(3)),
    assert(dm(4)),
    retract(dm(1)),
    expect_invalid(aml(_)),
    expect_invalid(bml(_)),
    expect_forced(aml(_)),
    expect_forced(bml(_)),
    expect(X, bml(X), [2,3,4]).

:- end_tests(tabling_monotonic_lazy_8).


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
    '$idg_mono_affects_lazy'(DstTrie, SrcTrie, _Dep, Answers),
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

