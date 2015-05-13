/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2015, University of Amsterdam
			      VU University Amsterdam

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
		    keysort,
		    sort4
		  ]).

:- begin_tests(sort).

test(empty, R == []) :-
	sort([], R).
test(unique, R == [a,b]) :-
	sort([a,b,a], R).
test(instantiation, [error(instantiation_error)]) :-
	sort([a,b,a|_],_).
test(type, [error(type_error(list,L))]) :-
	L = [a,b|a],
	sort(L,_).
test(cyclic, [sto(rational_trees),R == [a,b,c]]) :-
	L = [a,b,c|L],
	sort(L, R).

:- end_tests(sort).

:- begin_tests(msort).

test(empty, R == []) :-
	msort([], R).
test(unique, R == [a,a,b]) :-
	msort([a,b,a], R).
test(instantiation, [error(instantiation_error)]) :-
	msort([a,b,a|_],_).
test(type, [error(type_error(list,L))]) :-
	L = [a,b|a],
	msort(L,_).
test(cyclic, [ sto(rational_trees),setup(L=[a,b,c|L]),
	       error(type_error(list, L))
	     ]) :-
	msort(L, _).

:- end_tests(msort).

:- begin_tests(keysort).

test(empty, R == []) :-
	keysort([], R).
test(instantiation, [error(instantiation_error)]) :- % 8.18.5.3 a
	keysort([a-b,b-b,a-b|_],_).
test(type, [error(type_error(list,L))]) :- % 8.18.5.3 b
	L = [a-b,b-b|a],
	keysort(L,_).
test(cyclic, [ sto(rational_trees),setup(L=[a-1,b-2,c-3|L]),
	       error(type_error(list, L))
	     ]) :- % per analogiam 8.18.5.3 b
	keysort(L, _).
test(element, [error(instantiation_error)]) :- % 8.18.5.3.c
	keysort([a-b,_],_).
test(element, [error(type_error(pair,1))]) :- % 8.18.5.3 d
	keysort([1],_).
%test(output, [error(type_error(list,[_|a]))]) :- % 8.18.5.3 e missing
%	keysort([a-b],[_|a]).
:- end_tests(keysort).

:- begin_tests(sort4).

test(zero, List == [1,2,3]) :-
	sort(0, @<, [1,2,2,3], List).
test(zero, List == [1,2,2,3]) :-
	sort(0, @=<, [1,2,2,3], List).
test(zero, List == [3,2,1]) :-
	sort(0, @>, [1,2,2,3], List).
test(zero, List == [3,2,2,1]) :-
	sort(0, @>=, [1,2,2,3], List).
test(one, List == [a(1),a(2),a(3)]) :-
	sort(1, @<, [a(1),a(2),a(2),a(3)], List).
test(one, List == [c(1),a(2),a(3)]) :-
	sort(1, @<, [c(1),a(2),a(2),a(3)], List).
test(one, List == [c(1),b(2),a(2),a(3)]) :-
	sort(1, @=<, [c(1),b(2),a(2),a(3)], List).
test(list, List == [c(1),b(2),a(2),a(3)]) :-
	sort([1], @=<, [c(1),b(2),a(2),a(3)], List).
test(list, List == [c(t(1),x),b(t(2),x),a(t(2),x),a(t(3),x)]) :-
	sort([1,1], @=<, [c(t(1),x),b(t(2),x),a(t(2),x),a(t(3),x)], List).
test(list, List == [c(t(1),x),b(t(2),x),a(t(3),x)]) :-
	sort([1,1], @<, [c(t(1),x),b(t(2),x),a(t(2),x),a(t(3),x)], List).

% dicts
test(dict, List == [a{a:1}, a{a:2}]) :-
	sort(a, @<, [a{a:1}, a{a:2}], List).

% errors
test(key, error(type_error(sort_key,_))) :-
	sort(a(1), @<, [1,2,2,3], _).
test(key, error(instantiation_error)) :-
	sort(_, @<, [1,2,2,3], _).
test(order, error(type_error(atom,_))) :-
	sort(0, 1, [1,2,2,3], _).
test(order, error(instantiation_error)) :-
	sort(0, _, [1,2,2,3], _).
test(list, error(type_error(compound,1))) :-
	sort(1, @<, [1], _).
test(list, error(existence_error(argument,2,a(1)))) :-
	sort(2, @<, [a(1)], _).
test(dict, error(existence_error(key,a,a{b:2}))) :-
	sort(a, @<, [a{a:1}, a{b:2}], _).
test(dict, error(type_error(dict,a(1)))) :-
	sort(a, @<, [a(1), a(2)], _).

:- end_tests(sort4).
