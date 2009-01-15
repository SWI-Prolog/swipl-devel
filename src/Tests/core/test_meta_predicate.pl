/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemak@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2009, University of Amsterdam

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

:- module(test_meta_predicate, [test_meta_predicate/0]).
:- use_module(library(plunit)).

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

:- end_tests(meta_predicate).
