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

:- module(test_arith, [test_arith/0]).
:- use_module(library(plunit)).

/** <module> Test Prolog core arithmetic functions

@author	Jan Wielemaker
*/

test_arith :-
	run_tests([ rem
		  ]).

:- begin_tests(rem).

test(small, R == 2) :-
	R is 5 rem 3.
test(small_neg, R == -2) :-
	R is -5 rem 3.
test(big, [condition(current_prolog_flag(bounded, false)), R == 6]) :-
	R is (1<<100) rem 10.
test(big_neg, [condition(current_prolog_flag(bounded, false)), R == -6]) :-
	R is -(1<<100) rem 10.

:- end_tests(rem).
