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
:- use_module(library(increval)).
:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(prolog_code)).
:- use_module(library(tables)).
:- use_module(library(lists)).

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
                tabling_monotonic_lazy_8,
                tabling_monotonic_lazy_9,
                tabling_monotonic_lazy_10,
                tabling_monotonic_lazy_11,
                tabling_monotonic_lazy_12,
                tabling_monotonic_lazy_13,
                tabling_monotonic_lazy_14,
                tabling_monotonic_lazy_15,
                tabling_monotonic_lazy_16
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
    cleanup([dm/1, xm/1]),
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

:- begin_tests(tabling_monotonic_lazy_9).

:- dynamic d/1 as monotonic.
:- table p/1 as (monotonic,lazy).

p(c) :-
    d(a),
    d(b).

test(capture_new_dependencies) :-
    cleanup([d/1]),
    expect(X, p(X), []),
    assert(d(a)),
    expect(X, p(X), []),                % evaluation must create dependency on d(b).
    assert(d(b)),
    expect(X, p(X), [c]).

:- end_tests(tabling_monotonic_lazy_9).

:- begin_tests(tabling_monotonic_lazy_10).

% We can only  only  update  a  lazy   monotonic  table  after  all  its
% dependencies are updated. As we currently work bottom-up breath-first,
% this does not always has to be the   case.  Therefore we keep nodes we
% cannot yet reevaluate in the falsepaths.
%
% TBD: Eventually we may opt for a better planning of the reevaluation.

:- dynamic (d1/1,d2/1,d3/1) as monotonic.
:- table (p/1,a/1,b/1,c/1) as (monotonic,lazy).

p(X) :- ( a(X) ; b(X); d3(X) ).
b(X) :- c(X).
c(X) :- d1(X).
a(X) :- d2(X).

test(false_deps) :-
    cleanup([d1/1,d2/1,d3/1]),
    expect(X, p(X), []),
    assert(d1(1)),
    assert(d2(2)),
    assert(d3(3)),
    expect(X, p(X), [1,2,3]).

:- end_tests(tabling_monotonic_lazy_10).

:- begin_tests(tabling_monotonic_lazy_11).

:- dynamic d/1 as monotonic.
:- table p/1 as (monotonic, lazy).

p(X) :- d(X).

test(rollback) :-
    cleanup([d/1]),
    expect(X, p(X), []),
    \+ transaction(
           ( assert(d(a)),
             expect(X, p(X), [a]),
             fail
           )),
    expect(X, p(X), []).

:- end_tests(tabling_monotonic_lazy_11).

:- begin_tests(tabling_monotonic_lazy_12).

:- dynamic d/1 as monotonic.
:- table (p/1,q/1) as (monotonic,lazy).

p(X) :-
    q(X).

q(X) :-
    d(X).

test(indirect) :-
    cleanup([d/1]),
    expect(X, q(X), []),
    assert(d(1)),
    expect(X, p(X), [1]).

:- end_tests(tabling_monotonic_lazy_12).


:- begin_tests(tabling_monotonic_lazy_13).

:- dynamic data/1.
:- dynamic d/1 as monotonic.
:- table p/1 as (monotonic,lazy).

p(X) :- d(X).
d(X) :- data(X).

test(incr_propagate_lazy) :-
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

:- end_tests(tabling_monotonic_lazy_13).


:- begin_tests(tabling_monotonic_lazy_14).
% Test completion on lazy monotonic tables when there are
% cyclic dependencies.

:- dynamic (d/2) as monotonic.
:- table (p/2) as (monotonic,lazy).

p(X, Y) :-
    d(X, Y).
p(X, Y) :-
    d(X, M),
    p(M, Y).

test(mono_completion, fail) :-
    cleanup([d/2]),
    assert(d(a, b)),
    init(p(a,c)),
    assert(d(b, a)),
    p(a,c).

:- end_tests(tabling_monotonic_lazy_14).

:- begin_tests(tabling_monotonic_lazy_15).
% Test transitive closure under different regimes.  Both
% validate results and termination.

test(closure) :-
    test(50, 20, [tm,tml]).

:- use_module(library(random)).
:- meta_predicate solve(2, -).

:- dynamic dm/2 as monotonic.
:- dynamic dml/2 as (monotonic).
:- dynamic dt/2.

:- dynamic do_test/1.

:- table t/2.

t(A,B) :-
    dt(A,B).
t(A,B) :-
    t(A,M),
    dt(M,B).

:- table tm/2 as monotonic.

tm(A,B) :-
    dm(A,B).
tm(A,B) :-
    tm(A,M),
    dm(M,B).

:- table tml/2 as (monotonic,lazy).

tml(A,B) :-
    dml(A,B).
tml(A,B) :-
    tml(A,M),
    dml(M,B).

test(Count, Size, DoTest) :-
    clean,
    forall(member(X, DoTest), assertz(do_test(X))),
    test_loop(1, Count, Size).

test_loop(I, N, M) :-
    I =< N,
    !,
    random_between(1,M,A),
    random_between(1,M,B),
    assertz(dm(A,B)),
    assertz(dml(A,B)),
    assertz(dt(A,B)),
    I2 is I+1,
    verify(Len),
    debug(tc, 'Interation ~D: ~D connections', [I, Len]),
    test_loop(I2, N, M).
test_loop(_,_,_).

verify(Len) :-
    solve(t, Ok),
    verify(tm, Ok),
    verify(tml, Ok),
    length(Ok, Len).

solve(Pred, Pairs) :-
    (   Pred = M:t
    ->  abolish_table_subgoals(M:t(_,_))
    ;   true
    ),
    findall(A-B, call(Pred, A,B), Pairs0),
    sort(Pairs0, Pairs).

verify(Pred, Ok) :-
    do_test(Pred),
    !,
    solve(Pred, Pairs),
    (   Pairs == Ok
    ->  true
    ;   format('Wrong result for ~p~n', [Pred]),
        abort
    ).
verify(_,_).

clean :-
    abolish_all_tables,
    retractall(dt(_,_)),
    retractall(dm(_,_)),
    retractall(dml(_,_)),
    retractall(do_test(_)).


:- end_tests(tabling_monotonic_lazy_15).

:- begin_tests(tabling_monotonic_lazy_16).
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Problem:

  - Lazy reevaluation creates a fresh table that depends on an
    invalid lazy table because:
    - The dependency gets to complete_or_invalid_status()
    - As our dependency is lazy and we are in_assert_propagation
      our table is considered complete.
    - The new table is created from the old (invalid) data and
      marked as complete and valid.
      - The table is NOT valid!
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- table (p/1,q/1,r/1) as (monotonic,lazy).
:- dynamic d/1 as monotonic.

p(X) :-
    d(1),
    q(X).
p(X) :-
    d(11),
    asserta(d(12)),
    r(X).

r(X) :-
    q(X).

q(X) :-
    d(X),
    X < 10.

test(reeval) :-
    cleanup([d/1]),
    asserta(d(1)),
    init(p(_)),
    asserta(d(11)),
    expect(X, p(X), [1]).

:- end_tests(tabling_monotonic_lazy_16).




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

