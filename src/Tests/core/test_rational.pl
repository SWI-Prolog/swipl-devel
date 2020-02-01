/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2020, University of Amsterdam
			      VU University Amsterdam
			      CWI, Amsterdam

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

:- module(test_rational, [test_rational/0]).
:- use_module(library(plunit)).
:- use_module(library(debug)).

/** <module> Test Prolog core arithmetic functions

@author	Jan Wielemaker
*/

test_rational :-
    run_tests([ rational
	      ]).

:- begin_tests(rational,
	       [ condition(current_prolog_flag(bounded, false))
	       ]).
:- set_prolog_flag(rational_syntax, natural).

test(is_unify) :-                               % test is(+,+)
    X is 1/3,
    X is 1/3,
    assertion(rational(X)).

:- end_tests(rational).
