/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2019, VU University Amsterdam
		   2021, SWI-Prolog Solutions b.v.
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

:- module(test_reeval,
          [ test_reeval/0
          ]).
:- use_module(library(plunit)).
:- use_module(library(debug)).
:- use_module(library(wfs)).

/** <module> Incremental tabling reevaluation tests
*/

test_reeval :-
    run_tests([ tabling_reeval,
                tabling_reeval_merged,
                dynamic_tabled,
                dynamic_tabled2,
                dynamic_tabled3,
                dynamic_tabled4
              ]).

:- begin_tests(tabling_reeval, [ sto(rational_trees),
                                 cleanup(abolish_all_tables)
                               ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Two mutual dependent goals depending on   the same dynamic predicate. We
should _not_ create a sub-environment for solving the dependent goal.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- table (p/1, q/1) as incremental.
:- dynamic([max/1], [incremental(true)]).

max(2).

set_max(Max) :-
    retractall(max(_)),
    assert(max(Max)).

p(Y) :- q(X), max(Max), X < Max, Y is X+1.
p(0).
q(Y) :- p(X), max(Max), X < Max, Y is X+1.
q(0).

test(mutal_dependent, Ys = [0,1,2,3]) :-
    set_max(2),
    setof(X, p(X), Xs),
    assertion(Xs == [0,1,2]),
    set_max(3),
    setof(Y, p(Y), Ys).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Test reseting the `falsecount` for a dynamic node when it is called from
a new tabled goal.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- table (p2/1, q2/1) as incremental.
:- dynamic([d2/1], [incremental(true)]).

p2(X) :- d2(X).
q2(X) :- d2(X).

test(multiple_dependents,
     [cleanup(retractall(d2(_)))]) :-
    asserta(d2(1)),
    answers(X, p2(X), [1]),
    assert(d2(2)),
    answers(X, q2(X), [1,2]),
    assert(d2(3)),
    answers(X, q2(X), [1,2,3]).

:- end_tests(tabling_reeval).


:- meta_predicate
    answers(?, 0, +).

answers(Templ, Goal, Expected) :-
    findall(Templ, Goal, Got0),
    sort(Got0, Got),
    sort(Expected, Expected1),
    assertion(Got =@= Expected1).

:- begin_tests(tabling_reeval_merged,
               [ sto(rational_trees),
                 cleanup(abolish_all_tables)
               ]).

:- dynamic d/2 as incremental.
:- table p/2 as incremental.

p(X,Y) :-
    d(X,Y), Y > 10.
p(X,Z) :-
    d(X,Y), p(Y,Z).

% Tables being re-evaluated end up  in  a   merged  SCC.  Now we must be
% careful  to  first  propagate  the  no-changes   and  then  clear  the
% reevaluation state of the nodes.

test(only) :-
    assert(d(1,2)),
    assert(d(3,4)),
    eval(p(1,_)),
    eval(p(3,_)),
    assert(d(3,1)),
    assert(d(1,3)),
    eval(p(1,_)).                       % caused assertion on falsecount >= 0

:- end_tests(tabling_reeval_merged).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(dynamic_tabled,
               [ sto(rational_trees),
                 cleanup(abolish_all_tables)
               ]).
% A incrementally tabled predicate can also be dynamic.

:- table (p/1,q/1) as incremental.
:- dynamic (p/1,q/1) as incremental.

p(X) :- tnot(q(X)), tnot(p(X)).

test(wfs,
     [ cleanup(retractall(q(_))),
       P == [(p(1):-tnot(p(1)))]
     ]) :-
    assert(q(1)),
    eval(p(1)),
    retract(q(1)),
    call_residual_program(p(1), P).

:- end_tests(dynamic_tabled).

:- begin_tests(dynamic_tabled2,
               [ sto(rational_trees),
                 cleanup(abolish_all_tables)
               ]).
% A incrementally tabled predicate can also be dynamic.

:- table (p/1,qs/1) as incremental.
:- dynamic q/1 as incremental.

p(X) :- tnot(qs(X)), tnot(p(X)).
p(X) :- qs(X).

qs(X) :- q(X).

test(wfs,
     [ cleanup(retractall(q(_))),
       P == [(p(1):-tnot(p(1)))]
     ]) :-
    assert(q(1)),
    expect(x, p(1), [x]),
    retract(q(1)),
    call_residual_program(p(1), P).
% Validate that a conditional answer that became unconditional
% worls and proper reinitialization of the worklist when it is used
% again.
test(wfs2,
     [ cleanup(retractall(q(_))),
       P == [(p(1):-tnot(p(1)))]
     ]) :-
    assert(q(1)),
    expect(x, p(1), [x]),
    retract(q(1)),
    call_residual_program(p(1), P1),
    assertion(P1 == [(p(1):-tnot(p(1)))]),
    assert(q(1)),
    expect(x, p(1), [x]),
    retract(q(1)),
    call_residual_program(p(1), P).

:- end_tests(dynamic_tabled2).


:- begin_tests(dynamic_tabled3,
               [ sto(rational_trees),
                 cleanup(abolish_all_tables)
               ]).
% Test that tnot creates a dependency

:- table (p/1,qs/1,rs/1)  as incremental.
:- dynamic q/1 as incremental.

p(X) :- tnot(qs(X)), tnot(p(X)).
p(X) :- tnot(rs(X)), tnot(p(X)).

qs(X) :- q(X).
rs(X) :- q(X).

test(wfs, [ cleanup(retractall(q(_)))
          ]) :-
    assert(q(1)),
    expect(x, p(1), []),
    retract(q(1)),
    call_residual_program(p(1), P),
    assertion(P == [(p(1):-tnot(p(1)))]),
    assert(q(1)),
    expect(x, p(1), []).

:- end_tests(dynamic_tabled3).

:- begin_tests(dynamic_tabled4,
               [ sto(rational_trees),
                 cleanup(abolish_all_tables)
               ]).

:- table (p/1,q/1,r/1) as incremental.
:- dynamic r1/1 as incremental.

p(X) :- tnot(q(X)), r(X), tnot(p(X)).
p(X) :- r(X).

q(X) :- tnot(r(X)).
r(X) :- r1(X).

test(wfs,
     [ cleanup(retractall(r1(_)))
     ]) :-
    assert(r1(1)),
    expect(x, p(1), [x]),
    retract(r1(1)),
    expect(y, p(1), []).

:- end_tests(dynamic_tabled4).


		 /*******************************
		 *       SHARED TEST CODE	*
		 *******************************/

:- meta_predicate
    eval(0),
    expect(?, 0, +).

eval(G) :-
    forall(G, true).

expect(Templ, Goal, Answer) :-
    findall(Templ, Goal, R0),
    sort(R0, R),
    sort(Answer, Answer1),
    assertion(Answer1 == R).
