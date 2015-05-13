/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2013, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(test_string, [test_string/0]).
:- use_module(library(plunit)).
:- use_module(library(lists)).

/** <module> Test string manipulation primitives

This module is a Unit test for predicates that manage string objects.

@author	Jan Wielemaker
*/

test_string :-
	run_tests([ string
		  ]).

:- begin_tests(string).

:- set_prolog_flag(double_quotes, string).

test(number_string, S == "42") :-
	number_string(42, S).
test(number_string, N == 42) :-
	number_string(N, "42").
test(number_string, fail) :-
	number_string(_, "42x").
test(number_string, error(type_error(number, '42'))) :-
	number_string('42', _S).
test(number_string, error(instantiation_error)) :-
	number_string(_, _).
test(string_codes, S == [97,98]) :-
	string_codes("ab", S).
test(string_codes, Out == Codes) :-
	numlist(0, 2000, Codes),		% verify 0-bytes and Unicode
	string_codes(S, Codes),
	string_length(S, 2001),
	string_codes(S, Out).
test(string_chars, S == [a,b]) :-
	string_chars("ab", S).
test(split_string, L == ["a", "b", "c", "d"]) :-
	split_string("a.b.c.d", ".", "", L).
test(split_string, L == ["SWI-Prolog", "7.0"]) :-
	split_string("SWI-Prolog, 7.0", ",", " ", L).
test(split_string, L == ["SWI-Prolog"]) :-
	split_string("  SWI-Prolog  ", "", "\s\t\n", L).
test(split_string, L == [""]) :-
	split_string(" ", "", " ", L).
test(string_lower, L == "abc") :-
	string_lower("aBc", L).
test(string_upper, L == "ABC") :-
	string_upper("aBc", L).

:- end_tests(string).
