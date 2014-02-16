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
:- use_module(library(debug)).

/** <module> Test the dict data structure

This module tests the implementation of the dict datastructure.
*/

test_dict :-
	run_tests([ dict_create,
		    dict_bips,
		    dict_overflow,
		    expand_functions,
		    define_functions
		  ]).

:- meta_predicate
	test_overflow(0),
	test_overflow(0, +).

test_overflow(Goal) :-
	forall(between(1, 25, _),
	       ( GSize is 10+random(50),
		 test_overflow(overflow(Goal), GSize))).

test_overflow(Goal, Size) :-
	thread_create(Goal, Id, [global(Size)]),
	thread_join(Id, Result),
	assertion(subsumes_term(exception(error(resource_error(_),_)), Result)).

overflow(Goal) :-
	overflow(Goal, L), is_list(L).

overflow(Goal, [_|T]) :-
	\+ \+ call(Goal),
	overflow(Goal, T).


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

:- begin_tests(dict_overflow).

test(put, true) :-
	test_overflow(put_dict([a(1)], [b(2),c(3),d(4)], _)).
test(select, true) :-
	test_overflow(select_dict([x(1)], [x(1),z(3)], _)).

:- end_tests(dict_overflow).

:- begin_tests(expand_functions).

dict(m{a:1, b:2}).
dict(m{a:3, b:4}).

x_dict1(X, X.x).
x_dict2(X, X.put(y, X.x)).
x_dict3(X, X.put(y, Y)) :-
	Y is X.x^2.

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

test(head, X == 1) :-
	x_dict1(_{x:1}, X).
test(put, Y == 1) :-
	x_dict2(_{x:1}, X),
	Y = X.y.
test(put2, Y == 4) :-
	x_dict3(_{x:2}, X),
	Y = X.y.

:- end_tests(expand_functions).

% define in module
_.test_na() := 1.
D.test_aa() := D.a.
D.test_a2() := X :- X is D.a^2.

:- begin_tests(define_functions).

% define in other module
test_dict2:_.test_na() := 1.
test_dict2:D.test_a2() := X :- X is D.a^2.

test(na, X == 1) :-
	X = test_dict{}.test_na().
test(aa, X == 2) :-
	X = test_dict{a:2}.test_aa().
test(a2, X == 4) :-
	X = test_dict{a:2}.test_a2().
test(qna, X == 1) :-
	X = test_dict2{}.test_na().
test(qa2, X == 4) :-
	X = test_dict{a:2}.test_a2().

:- end_tests(define_functions).
