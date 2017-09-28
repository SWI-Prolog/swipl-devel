/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2009-2011, University of Amsterdam
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

:- module(test_meta_predicate, [test_meta_predicate/0]).
:- use_module(library(plunit)).
:- use_module(library(debug)).

/** <module> Test meta predicate handling

@author	Jan Wielemaker
*/

test_meta_predicate :-
	run_tests([ meta_predicate
		  ]).

:- meta_predicate
	m(:, -),
	m(:, +, -),
	m2(+, :, -).

m(In, Out) :-
	Out = In.
m(_, X, X).
m2(X, _, X).

:- begin_tests(meta_predicate).

:- dynamic this/1.
:- (   this(_)
   ->  true
   ;   prolog_load_context(module, M),
       assert(this(M))
   ).

a(_).

mtry:no :- fail.

mdepart(_) :-
	mtry:no, !.
mdepart(X) :-
	context_module(X).

% What a variable is qualified, a term is pushed onto the global stack
% and the frame argument becomes a reference pointer to a var in this
% term.  This should be trailed.  Backtracking should also maintain the
% qualification.

:- meta_predicate mq(:).

mq(T) :-
    assertion(functor(T, :, 2)),
    fail.
mq(T) :-
    assertion(functor(T, :, 2)),
    fail.

tbacktrack :-
    tbacktrack(X),
    term(T),
    term(T),
    assertion(var(X)).

tbacktrack(V) :-
    mq(V).
tbacktrack(V) :-
    assertion(var(V)).

term(t(42)).

		 /*******************************
		 *	       TESTS		*
		 *******************************/

test(qualify, X == M:x) :-
	this(M),
	m(x, X).
test(qualify, X == m1:x) :-
	m(m1:x, X).
test(qualify, X == m1:Y) :-
	a(Y),
	m(m1:Y, X).
test(qualify, X == m1:Y) :-
	a(Y), freeze(Y, fail),
	m(m1:Y, X).
test(qualify, X == m1:x) :-
	m(m2:m1:x, X).
test(qualify, X == 42:x) :-		% do not check module-type
	m(m2:42:x, X).
test(alias, X == Out) :-		% shared variables
	m(X, X, Out).
test(alias2, X == Out) :-		% shared variables
	m2(X, X, Out).
test(i_departm, X == Me) :-
	context_module(Me),
	mdepart(X).
test(backtrack) :-
	tbacktrack.

:- end_tests(meta_predicate).
