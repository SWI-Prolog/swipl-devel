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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

:- module(test_op, [test_op/0]).
:- use_module(library(plunit)).

/** <module> Test Prolog operator handling

This module is a Unit test for  Prolog op/3, current_op/3, etc.

@author	Jan Wielemaker
*/

test_op :-
	run_tests([ current_op
		  ]).

:- begin_tests(current_op).

test(plus, all(P-T == [200-fy, 500-yfx])) :-
	current_op(P, T, +).
test(no_atom, error(type_error(atom, 1))) :-
	current_op(_, _, 1).
test(no_atom, error(type_error(atom, 1))) :-
	current_op(_, 1, _).
test(bad_type, error(domain_error(operator_specifier, xxx))) :-
	current_op(_, xxx, _).
test(bad_precedence, error(type_error(integer, x))) :-
	current_op(x, _, _).
test(bad_precedence, error(type_error(integer, 1.2))) :-
	current_op(1.2, _, _).
test(inherit, true) :-
	current_op(500, yfx, +),
	op(0, xfx, +),
	(   current_op(_, yfx, +)
	->  fail
	;   op(500, yfx, +)
	).

:- end_tests(current_op).
