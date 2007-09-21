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

:- module(test_sort, [test_sort/0]).
:- use_module(library(plunit)).

/** <module> Test Prolog core sort processing primitives

This module is a Unit test for  Prolog built-ins that process characters
or character codes.  Please define a test-set for each predicate.

@author	Jan Wielemaker
*/

test_sort :-
	run_tests([ sort,
		    msort,
		    keysort
		  ]).

:- begin_tests(sort).

test(empty, R == []) :-
	sort([], R).
test(unique, R == [a,b]) :-
	sort([a,b,a], R).
test(cyclic, R == [a,b,c]) :-
	L = [a,b,c|L],
	sort(L, R).

:- end_tests(sort).

:- begin_tests(msort).

test(empty, R == []) :-
	msort([], R).
test(unique, R == [a,a,b]) :-
	msort([a,b,a], R).
test(cyclic, [ setup(L=[a,b,c|L]),
	       error(type_error(list, L))
	     ]) :-
	msort(L, _).

:- end_tests(msort).

:- begin_tests(keysort).

test(empty, R == []) :-
	keysort([], R).
test(cyclic, [ setup(L=[a-1,b-2,c-3|L]),
	       error(type_error(list, L))
	     ]) :-
	msort(L, _).

:- end_tests(keysort).
