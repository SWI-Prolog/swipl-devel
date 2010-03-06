/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
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

:- module(test_dbref, [test_dbref/0]).
:- use_module(library(plunit)).

/** <module> Test Prolog database references

@author	Jan Wielemaker
*/

test_dbref :-
	run_tests([ assert2,
		    recorded
		  ]).

:- begin_tests(assert2).

:- dynamic
	term/1.

test(bound, error(_)) :-
	assert(term(a), noref).
test(erase, true) :-
	assert(term(a), Ref),
	erase(Ref),
	\+ term(a).
test(double_erase, true) :-
	assert(term(a), Ref),
	erase(Ref),
	\+ erase(Ref).
test(retract_erase, true) :-
	assert(term(a), Ref),
	retractall(term(_)),
	\+ erase(Ref).

:- end_tests(assert2).

:- begin_tests(recorded).

test(erase, true) :-
	recorda(test, a, Ref),
	recorded(test, a),
	erase(Ref),
	\+ recorded(test, a).
test(double_erase, true) :-
	recorda(test, a, Ref),
	recorded(test, a),
	erase(Ref),
	\+ erase(Ref).
test(recorded_vt, [true([K1,K2] == [test_1,test_2]), nondet]) :-
	recorda(test_1, a1, R1),
	recorda(test_2, a2, R2),
	recorded(K2, a2),
	recorded(K1, a1),
	erase(R1),
	erase(R2).

:- end_tests(recorded).
