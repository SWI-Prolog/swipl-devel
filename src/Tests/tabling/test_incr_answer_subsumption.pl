/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2020, University of Amsterdam
                         VU University Amsterdam
		         CWI, Amsterdam
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

:- module(test_incr_answer_subsumption,
          [ test_incr_answer_subsumption/0
          ]).
:- use_module(library(plunit)).
:- use_module(library(increval)).
:- use_module(library(tables)).
:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(prolog_code)).

%!  test_incr_answer_subsumption
%
%   Tests dealing with answer subsumption using incremental tabling.

test_incr_answer_subsumption :-
    run_tests([ incr_answer_subsumption_1,
                mono_answer_subsumption_1,
                mono_answer_subsumption_lazy_1
              ]).

:- meta_predicate
    init(0),
    cleanup(:),
    queued_answers(:,-,-),
    expect(?, 0, +),
    expect_valid(0),
    expect_invalid(0),
    expect_queued_answers(:, +).

:- begin_tests(incr_answer_subsumption_1).

:- dynamic d/1 as incremental.
:- table p(max) as incremental.
:- table p2/1 as incremental.

p(X) :- d(X).
p2(X) :- p(X).

                                        % Verify normal update
test(update, [cleanup(cleanup([d/1]))]) :-
    init(p(_)),
    assertz(d(1)),
    expect_invalid(p(_)),
    expect(X, p(X), [1]),
    expect_valid(p(_)),
    assertz(d(2)),
    expect_invalid(p(_)),
    expect(X, p(X), [2]),
    expect_valid(p(_)).
                                        % Verify non-changing update
test(keep, [cleanup(cleanup([d/1]))]) :-
    init(p(_)),
    assertz(d(1)),
    expect_invalid(p(_)),
    expect(X, p(X), [1]),
    expect_valid(p(_)),
    assertz(d(0)),
    expect_invalid(p(_)),
    expect(X, p(X), [1]),
    expect_valid(p(_)).
                                        % Verify falsecount propagation
test(reeval, [cleanup(cleanup([d/1]))]) :-
    init(p2(_)),
    assertz(d(1)),
    expect_invalid(p(_)),
    expect_invalid(p2(_)),
    expect(X, p(X), [1]),
    expect_valid(p(_)),
    expect_invalid(p2(_)),
    expect(X, p2(X), [1]),
    assertz(d(0)),
    expect_invalid(p(_)),
    expect_invalid(p2(_)),
    expect(X, p(X), [1]),
    expect_valid(p(_)),
    expect_valid(p2(_)).

:- end_tests(incr_answer_subsumption_1).


:- begin_tests(mono_answer_subsumption_1).

:- dynamic d/1 as monotonic.
:- table p(max) as monotonic.
:- table p2/1 as incremental.

p(X) :- d(X).
p2(X) :- p(X).

                                        % Verify normal update
test(update, [cleanup(cleanup([d/1]))]) :-
    init(p(_)),
    assertz(d(1)),
    expect(X, p(X), [1]),
    expect_valid(p(_)),
    assertz(d(2)),
    expect(X, p(X), [2]),
    expect_valid(p(_)).
                                        % Verify non-changing update
test(keep, [cleanup(cleanup([d/1]))]) :-
    init(p(_)),
    assertz(d(1)),
    expect(X, p(X), [1]),
    expect_valid(p(_)),
    assertz(d(0)),
    expect(X, p(X), [1]),
    expect_valid(p(_)).
                                        % Verify falsecount propagation
test(reeval, [cleanup(cleanup([d/1]))]) :-
    init(p2(_)),
    assertz(d(1)),
    expect(X, p(X), [1]),
    expect_valid(p(_)),
    expect_invalid(p2(_)),
    expect(X, p2(X), [1]),
    assertz(d(0)),
    expect_valid(p2(_)),
    expect(X, p2(X), [1]).

test(reeval, [cleanup(cleanup([d/1]))]) :-
    init(p2(_)),
    assertz(d(1)),
    expect(X, p(X), [1]),
    expect_valid(p(_)),
    expect_invalid(p2(_)),
    expect(X, p2(X), [1]),
    assertz(d(2)),
    expect_invalid(p2(_)),
    expect(X, p2(X), [2]),
    expect_valid(p(_)).

:- end_tests(mono_answer_subsumption_1).

:- begin_tests(mono_answer_subsumption_lazy_1).

:- dynamic d/1 as monotonic.
:- table p(max) as (monotonic,lazy).
:- table p2/1 as incremental.

p(X) :- d(X).
p2(X) :- p(X).

test(update, [cleanup(cleanup([d/1]))]) :-
    init(p2(_)),
    assertz(d(1)),
    expect_invalid(p(_)),
    expect_invalid(p2(_)),
    expect(X, p(X), [1]),
    expect_invalid(p2(_)),
    expect(X, p2(X), [1]),
    assertz(d(2)),
    expect_invalid(p(_)),
    expect_invalid(p2(_)),
    expect(X, p2(X), [2]),
    expect_valid(p2(_)).

test(update, [cleanup(cleanup([d/1]))]) :-
    init(p2(_)),
    assertz(d(1)),
    expect_invalid(p(_)),
    expect_invalid(p2(_)),
    expect(X, p(X), [1]),
    expect_invalid(p2(_)),
    expect(X, p2(X), [1]),
    assertz(d(2)),
    expect_invalid(p(_)),
    expect_invalid(p2(_)),
    expect(X, p(X), [2]),
    expect_invalid(p2(_)),
    expect(X, p2(X), [2]),
    expect_valid(p2(_)).

:- end_tests(mono_answer_subsumption_lazy_1).


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

