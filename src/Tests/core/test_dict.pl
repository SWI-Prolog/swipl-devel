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

:- module(test_dict,
	  [ test_dict/0
	  ]).
:- use_module(library(plunit)).
:- use_module(library(random)).
:- use_module(library(apply)).
:- use_module(library(pairs)).

/** <module> Test the dict data structure

This module tests the implementation of the dict datastructure.
*/

test_dict :-
	run_tests([ dict_create,
		    dict_bips,
		    expand_functions
		  ]).

:- begin_tests(dict_create).

test_create(Size) :-
	length(List, Size),
	maplist(random_between(0, 1000000), List),
	sort(List, Set),
	random_permutation(Set, RandSet),
	map_list_to_pairs(=, RandSet, RandPairs),
	dict_create(Dict, -, RandPairs),
	maplist(dict_lookup(Dict), RandSet, RandSet).

dict_lookup(Dict, Key, Value) :-
	get_dict(Key, Dict, Value).

test(create, true) :-
	forall(between(1, 100, S),
	       test_create(S)).

:- end_tests(dict_create).


:- begin_tests(dict_bips).

test(put, D = a{x:2}) :-
	put_dict(x, a{x:1}, 2, D).
test(put, D = a{x:1, y:2}) :-
	put_dict(y, a{x:1}, 2, D).
test(put, D = a{x:1, y:2, z:3}) :-
	put_dict(n{y:2,z:3}, a{x:1}, D).
test(select, X == 1) :-
	a{x:X} :< a{x:1, y:2}.
test(select, fail) :-
	a{x:_} :< a{y:2}.
test(select, R =@= _{z:3}) :-		% implicit conversion
	select_dict([x(1)], [x(1),z(3)], R).

:- end_tests(dict_bips).

:- begin_tests(expand_functions).

dict(m{a:1, b:2}).
dict(m{a:3, b:4}).

test(field, X == 3) :-
	X = _{a:3}.a.
test(conj, X == 3) :-
	Dict = _{a:3},
	X = Dict.a.
test(meta, X == 3) :-
	ignore(( Dict = _{a:3},
		 X = Dict.a)).
test(forall, true) :-
	forall(dict(M), M.b mod 2 =:= 0).
test(findall, As = [1,3]) :-
	findall(A, (dict(M), A = M.a), As).

:- end_tests(expand_functions).
