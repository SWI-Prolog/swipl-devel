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
		    atom_concat,
		    term_to_atom,
		    atom_to_term,
		    number_codes,
		    number_chars
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

:- begin_tests(atom_concat).

test(shared, X == ab) :-		% deal with atom_concat(X,X,...)
	atom_concat(X, X, abab).
test(shared, X == '') :-
	atom_concat(X, X, '').
test(shared, fail) :-
	atom_concat(X, X, abac).
test(attvar, X-Y == ab-ok) :-
	freeze(X, Y = ok),
	atom_concat(a, b, X).

:- end_tests(atom_concat).


:- begin_tests(term_to_atom).

test(write, A == 'foo(a)') :-
	term_to_atom(foo(a), A).
test(read, T == foo(a)) :-
	term_to_atom(T, 'foo(a)').

:- end_tests(term_to_atom).

:- begin_tests(atom_to_term).

test(read, T-V =@= foo(A)-['A' = A] ) :-
	atom_to_term('foo(A)', T, V).
test(error, error(instantiation_error)) :-
	atom_to_term(_, _, _).
test(eof, error(syntax_error(_))) :-
	atom_to_term('x /* comment', _, _).

:- end_tests(atom_to_term).


:- begin_tests(number_codes).

test(whitespace, X == 42) :-
	number_codes(X, "  42").	% ISO
test(whitespace, X == 42) :-
	number_codes(X, "\n 42").
test(whitespace, error(syntax_error(_))) :-
	number_codes(_, "42 ").		% ISO (dubious)
test(whitespace, error(syntax_error(_))) :-
	number_codes(_, "/**/42").	% ISO demands acceptance!?
test(unify, fail) :-
	number_codes(0, [C,C]).

:- end_tests(number_codes).

% See    http://www.complang.tuwien.ac.at/ulrich/iso-prolog/number_chars
% SWI-specific tests are names swi*. Some of   these  are because both a
% code-list and character list  are  accepted.   Some  are  because  SWI
% accepts 0-code/characters.

:- begin_tests(number_chars).

test(iso, true) :-
	number_chars(1, ['0','1']).
test(iso, N = 0'a) :-
	number_chars(N, [' ','0','''',a]) .
test(swi, error(syntax_error(_))) :-
	number_chars(_, [/,*,*,/,'1']).
test(iso, error(instantiation_error)) :-
	number_chars(_,[_]).
test(iso, error(instantiation_error)) :-
	number_chars(_,['0'|_]).
test(swi, error(type_error(_, []))) :-
	number_chars(1,[[]]).
test(iso, error(type_error(list, '1'))) :-
	number_chars(_,'1').
test(swi, N==1) :-
	number_chars(N,[0'1]).
test(iso, error(syntax_error(_))) :-
	number_chars(1,[a]).
test(iso, error(type_error(list, [a|a]))) :-
	number_chars(_,[a|a]).
test(swi, error(syntax_error(_))) :-
	number_chars(1,[0]).
test(iso, error(syntax_error(_))) :-
	number_chars(1,[]).
test(iso, error(syntax_error(_))) :-
	number_chars(_,[]).
test(iso, error(syntax_error(_))) :-
	number_chars(_,['3',' ']).
test(iso, error(syntax_error(_))) :-
	number_chars(_,[-,/,*,*,/,'1']).
test(float, true) :-
	forall(between(-500, 500, E),
	       test_float(E)).

test_float(E) :-
	(   catch(X is 10.0**E, _, fail)
	->  number_codes(X, Codes),
	    number_codes(X2, Codes),
	    (   X == X2
	    ->  true
	    ;   format(user_error, '~w \== ~w~n', [X, X2]),
		fail
	    )
	;   true
	).

% 8.16.7.3 proposal

test(iso2, C == '1') :-
	number_chars(1,[C]).
test(iso2, fail) :-
	number_chars(1,[_,_]).
test(iso2, fail) :-
	number_chars(1,[C,C]).
test(iso2, fail) :-
	number_chars(0,[C,C]).
test(iso2, [C-D == '1'-'0']) :-
	number_chars(10,[C,D]).
test(iso2, fail) :-
	number_chars(100,[_,_]).
test(iso2, error(instantiation_error)) :-
	number_chars(_,[_|1]).
test(iso2, error(instantiation_error)) :-
	number_chars(_, [1|_]).
test(iso2, error(type_error(list, [1|2]))) :-
	number_chars(_, [1|2]).
test(iso2, error(type_error(list, 1))) :-
	number_chars(1,1).
test(iso2, error(type_error(list, [a|1]))) :-
	number_chars(1, [a|1]).

:- end_tests(number_chars).
