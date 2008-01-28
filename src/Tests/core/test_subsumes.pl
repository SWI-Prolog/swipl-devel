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

This module is  a  Unit  test  for   Prolog  built-ins  that  deal  with
meta-subsumesing. Please define a test-set for each predicate.

@author	Jan Wielemaker
*/

test_subsumes :-
	run_tests([ subsumes,
		    subsumes_chk
		  ]).

:- begin_tests(subsumes).

test(simple, A == a) :-
	subsumes(A, a).
test(simple, fail) :-
	subsumes(a, _A).
test(simple, true(X-Y == Z-Z)) :-
	subsumes(a(X,Y), a(Z,Z)).

:- end_tests(subsumes).


:- begin_tests(subsumes_chk).

test(simple, true) :-
	subsumes_chk(A, a),
	var(A).

:- end_tests(subsumes_chk).

