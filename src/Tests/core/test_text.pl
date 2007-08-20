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

:- module(test_text, [test_text/0]).
:- use_module(library(plunit)).

/** <module> Test Prolog core text processing primitives

This module is a Unit test for  Prolog built-ins that process characters
or character codes.  Please define a test-set for each predicate.

@author	Jan Wielemaker
*/

test_text :-
	run_tests([ char_code,
		    term_to_atom,
		    atom_to_term
		  ]).

:- begin_tests(char_code).

test(ascii, C == 97) :-
	char_code(a, C).
test(ascii, A == a) :-
	char_code(A, 97).
test(wide, true) :-
	char_code(A, 1050),
	atom_codes(A, [1050]).
test(wide, true) :-
	atom_codes(A, [1050]),
	char_code(A, 1050).
test(error, error(instantiation_error)) :-
	char_code(_,_).
test(error, error(type_error(character, 42))) :-
	char_code(42,_).
test(error, error(type_error(integer, x))) :-
	char_code(_,x).
test(error, error(representation_error(character_code))) :-
	char_code(_,-1).
test(error, error(representation_error(character_code))) :-
	char_code(_,0xfffffff).

:- end_tests(char_code).

:- begin_tests(term_to_atom).

test(write, A == 'foo(a)') :-
	term_to_atom(foo(a), A).
test(read, T == foo(a)) :-
	term_to_atom(T, 'foo(a)').

:- end_tests(term_to_atom).

:- begin_tests(atom_to_term).

test(read, T-V == foo(A)-[A = 'A'] ) :-
	atom_to_term('foo(A)', T, V).
test(error, error(instantiation_error)) :-
	atom_to_term(_, _, _).

:- end_tests(atom_to_term).
