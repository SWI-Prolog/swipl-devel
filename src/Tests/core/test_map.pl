/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2013 VU University Amsterdam

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

:- module(test_map,
	  [ test_map/0
	  ]).
:- use_module(library(plunit)).
:- use_module(library(random)).
:- use_module(library(apply)).
:- use_module(library(pairs)).

/** <module> Test the map data structure

This module tests the implementation of the map datastructure.
*/

test_map :-
	run_tests([ map_create,
		    expand_functions
		  ]).

:- begin_tests(map_create).

test_create(Size) :-
	length(List, Size),
	maplist(random_between(0, 1000000), List),
	sort(List, Set),
	random_permutation(Set, RandSet),
	map_list_to_pairs(=, RandSet, RandPairs),
	map_create(Map, -, RandPairs),
	maplist(map_lookup(Map), RandSet, RandSet).

map_lookup(Map, Key, Value) :-
	get_map(Key, Map, Value).

test(create, true) :-
	forall(between(1, 100, S),
	       test_create(S)).

:- end_tests(map_create).


:- begin_tests(expand_functions).

map(m{a:1, b:2}).
map(m{a:3, b:4}).

test(field, X == 3) :-
	X = _{a:3}.a.
test(conj, X == 3) :-
	Map = _{a:3},
	X = Map.a.
test(meta, X == 3) :-
	ignore(( Map = _{a:3},
		 X = Map.a)).
test(forall, true) :-
	forall(map(M), M.b mod 2 =:= 0).
test(findall, As = [1,3]) :-
	findall(A, (map(M), A = M.a), As).

:- end_tests(expand_functions).
