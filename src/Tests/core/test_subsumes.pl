/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2007, University of Amsterdam

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

:- module(test_subsumes, [test_subsumes/0]).
:- use_module(library(plunit)).

/** <module> Test Prolog core meta-subsumesing primitives

This module is a Unit test for Prolog built-ins that deal with term
subsumption. Please define a test-set for each predicate.

@author	Jan Wielemaker
*/

test_subsumes :-
	run_tests([ subsumes,
		    subsumes_chk
		  ]).

:- begin_tests(subsumes).

test(simple_true, A == a) :-
	subsumes(A, a).
test(simple_false, fail) :-
	subsumes(a, _A).
test(double, X==Y) :-
	subsumes(a(X,f(X)), a(Y, f(Y))).
test(structure, A==f(C,D)) :-
	subsumes(f(A, A), f(_B, f(C, D))).
test(shared_true, true(X-Y == Z-Z)) :-
	subsumes(a(X,Y), a(Z,Z)).
test(shared_false, fail) :-
	subsumes(a(Z,Z), a(_X,_Y)).
test(shared_false_2, fail) :-
	A = a(_X, _Y),
	B = a(Z, Z),
	subsumes(B, A).
test(cyclic1, [sto(rational_trees),A==B]) :-
	A = a(A),
	B = a(B),
	subsumes(B, A).
test(cyclic2, [sto(rational_trees),A==B]) :-
	A = a(A),
	B = a(_),
	subsumes(B, A).
test(cyclic_fail, [sto(rational_trees),fail]) :-
	A = a(A),
	B = a(_),
	subsumes(A, B).

:- end_tests(subsumes).


:- begin_tests(subsumes_chk).

test(simple, true) :-
	subsumes_chk(A, a),
	var(A).

:- end_tests(subsumes_chk).

