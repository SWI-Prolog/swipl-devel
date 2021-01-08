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

:- module(test_monotonic,
          [ test_monotonic/0
          ]).
:- use_module(library(plunit)).
:- use_module(library(debug)).
:- use_module(library(tables)).
:- use_module(library(random)).
:- use_module(library(dialect/xsb/increval)).

test_monotonic :-
    run_tests([ monotonic_tabling
              ]).

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
    retractall(la(_,_)).

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

:- end_tests(monotonic_tabling).
