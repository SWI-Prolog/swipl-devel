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

/** <module> Test Prolog core meta-subsumption primitives

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
test(shared_true, true(X-Y == Z-Z)) :-
	subsumes(a(X,Y), a(Z,Z)).
test(shared_false, fail) :-
	subsumes(a(Z,Z), a(_X,_Y)).
test(shared_false_2, fail) :-
	A = a(_X, _Y),
	B = a(Z, Z),
	subsumes(B, A).
test(shared_false_3, fail) :-
	subsumes(f(A, A), f(_, f(_, _))).
test(shared_false_4, fail) :-
	subsumes(f(A, A), f(f(_, _), f(_, _))).
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
test(cyclic_fail_1, [sto(rational_trees),fail]) :-
	general_specific_1(General, Specific),
	Goal = subsumes(General, Specific),
	term_variables(Specific, SVars),
	Goal,
	\+ term_variables(SVars,SVars). % untouched
test(cyclic_fail_2, [sto(rational_trees),fail]) :-
	general_specific_2(General, Specific),
	Goal = subsumes(General, Specific),
	term_variables(Specific, SVars),
	Goal,
	\+ term_variables(SVars,SVars). % untouched

general_specific_1(General, Specific) :-
	Specific = f(_,S1), S1 = f(S1,S1),
	General = f(_,General).
general_specific_2(General, Specific) :-
	General = f(_,General),
	Specific = f(_,S1), S1 = f(S1,S1).

:- end_tests(subsumes).


:- begin_tests(subsumes_chk).

test(simple, true) :-
	subsumes_chk(A, a),
	var(A).
test(min, [fail]) :-
       Goal = subsumes_chk(_, _),
       copy_term(Goal,_),
       Goal,
       fail.
test(sharing,[fail]) :-
	subsumes_chk(f(A, A), f(_, f(_, _))).

:- end_tests(subsumes_chk).

