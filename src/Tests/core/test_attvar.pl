/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemak@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2008, University of Amsterdam

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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

:- module(test_attvar, [test_attvar/0]).
:- use_module(library(plunit)).

/** <module> Test Prolog attvaring

This module is a Unit test for Prolog unification oddities.  If basic
unification is wrong you won't get as far as running this test :-)

@author	Jan Wielemaker
*/

test_attvar :-
	run_tests([ attvar,
		    freeze
		  ]).

:- begin_tests(attvar).

test(s_list, L=="hello") :-		% Verify wakeup on S_LIST
	freeze(X, X="hello"),
	append(X, [], L).
test(true_ndet, error(existence_error(procedure,_))) :-
	freeze(X, wake(X)),
	between(-2, 2, X).

wake(2) :-
	i_am_undefined.

:- end_tests(attvar).

:- begin_tests(freeze).

test(freeze_and, true) :-
	freeze(X, true),
	freeze(Y, true),
	X=Y,
	freeze(X, true),
	X=a.

:- end_tests(freeze).
