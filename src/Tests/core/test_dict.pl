/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2013-2016, VU University Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
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
		    dict_dot3,
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

test('dict_create/3') :-
	dict_create(D, a, []),
	D == a{}.
test('dict_create/3') :-
	dict_create(D, a, [k1:v1]),
	D == a{k1:v1}.
test('dict_create/3') :-
	dict_create(D, a, [k1:v1, k2:v2]),
	D == a{k1:v1, k2:v2}.
test('dict_create/3') :-
	dict_create(D, a, [k1=v1]),
	D == a{k1:v1}.
test('dict_create/3') :-
	dict_create(D, a, [k1-v1]),
	D == a{k1:v1}.
test('dict_create/3') :-
	dict_create(D, a, [k1(v1)]),
	D == a{k1:v1}.
test(create, true) :-
	forall(between(1, 100, S),
	       test_create(S)).

:- end_tests(dict_create).


:- begin_tests(dict_bips).

test('is_dict/1') :-
	is_dict(a{}).
test('is_dict/1') :-
	is_dict(a{k1:v1, k2:v2}).
test('is_dict/2') :-
	is_dict(a{}, A),
	A == a.
test('is_dict/2') :-
	is_dict(a{k1:v1, k2:v2}, A),
	A == a.
test('dict_pairs/3') :-
	dict_pairs(a{}, A, P),
	A == a,
	P == [].
test('dict_pairs/3') :-
	dict_pairs(a{k1:v1, k2:v2}, A, P),
	A == a,
	P == [k1-v1, k2-v2].
test('dict_pairs/3') :-
	dict_pairs(D, a, []),
	D == a{}.
test('dict_pairs/3') :-
	dict_pairs(D, a, [k1-v1, k2-v2]),
	D == a{k1:v1, k2:v2}.
test('get_dict/3') :-
	get_dict(k1, a{k1:v1}, V),
	V == v1.
test('get_dict/3') :-
	\+ get_dict(k2, a{k1:v1}, _).
test('get_dict/3') :-
	findall(K-V, get_dict(K, a{k1:v1, k2:v2}, V), KVs),
        msort(KVs, [k1-v1, k2-v2]).
test('get_dict/5') :-
	get_dict(k1, a{k1:v1}, V, D, v2),
	V == v1,
	D == a{k1:v2}.
test('get_dict/5') :-
	get_dict(k1, [k1:v1], V, D, v2),
	V == v1,
	D = a{k1:v2}.
test('get_dict/5') :-
	\+ get_dict(k2, a{k1:v1}, _, _, v2).
test('get_dict/5') :-
	\+ get_dict(k1, a{k1:v1}, v2, _, v2).
test('put_dict/4') :-
	put_dict(k1, a{k1:v1}, v2, D),
	D == a{k1:v2}.
test('put_dict/4') :-
	put_dict(k2, a{k1:v1}, v2, D),
	D == a{k1:v1, k2:v2}.
test('put_dict/4') :-
	put_dict(k1, a{}, v1, D),
	D == a{k1:v1}.
test('put_dict/3') :-
	put_dict(n{k1:v2}, a{k1:v1}, D),
	D == a{k1:v2}.
test('put_dict/3') :-
	put_dict(n{k2:v2}, a{k1:v1}, D),
	D == a{k1:v1, k2:v2}.
test('put_dict/3') :-
	put_dict(n{k1:v1}, a{k2:v2}, D),
	D == a{k1:v1, k2:v2}.
test('put_dict/3') :-
	put_dict(n{}, a{k1:v1}, D),
	D == a{k1:v1}.
test('put_dict/3') :-
	put_dict(n{k1:v1}, a{}, D),
	D == a{k1:v1}.
test('del_dict/4') :-
	del_dict(k1, a{k1:v1}, V, D),
	V == v1,
	D == a{}.
test('del_dict/4') :-
	del_dict(k1, a{k1:v1}, v1, D),
	D == a{}.
test('del_dict/4') :-
	del_dict(k1, a{k1:v1, k2:v2}, V, D),
	V == v1,
	D == a{k2:v2}.
test('del_dict/4') :-
	\+ del_dict(k2, a{k1:v1}, _, _).
test('del_dict/4') :-
	\+ del_dict(k1, a{k1:v1}, v2, _).
test('select_dict/3') :-
	select_dict(T{}, a{k1:v1, k2:v2}, R),
	T == a,
	R =@= _{k1:v1, k2:v2}.
test('select_dict/3') :-
	select_dict(T{k1:V1}, a{k1:v1, k2:v2}, R),
	T == a, V1 == v1,
	R =@= _{k2:v2}.
test('select_dict/3') :-
	select_dict(T{k1:V1, k2:V2}, a{k1:v1, k2:v2}, R),
	T == a, V1 == v1, V2 == v2,
	R =@= _{}.
test('select_dict/3') :-
	select_dict(a{k1:v1, k2:v2}, T{k1:V1, k2:V2}, R),
	T == a, V1 == v1, V2 == v2,
	R =@= _{}.
test('select_dict/3') :-
	\+ select_dict(b{k1:v1}, a{k1:v1}, _).
test('select_dict/3') :-
	\+ select_dict(a{k1:v2}, a{k1:v1}, _).
test('select_dict/3') :-
	\+ select_dict(a{k2:v1}, a{k1:v1}, _).
test(':</2') :-
	T{k1:V1} :< a{k1:v1, k2:v2},
	T == a, V1 == v1.
test(':</2') :-
	T{k1:V1, k2:V2} :< a{k1:v1, k2:v2},
	T == a, V1 == v1, V2 == v2.
test('>:</2') :-
	T{k1:V1, k2:V2} >:< a{k1:v1, k2:v2},
	T == a, V1 == v1, V2 == v2.
test('>:</2') :-
	a{k1:v1, k2:v2} >:< T{k1:V1, k2:V2},
	T == a, V1 == v1, V2 == v2.
test('>:</2') :-
	T{k1:V1, k2:v2} >:< a{k1:v1, k3:v3},
	T == a, V1 == v1.
test('>:</2') :-
	T{k1:V1, k2:V2} >:< a{k1:v1, k3:v3},
	T == a, V1 == v1, var(V2).
test('>:</2') :-
	\+ a{k1:v1} >:< b{k1:v1}.
test('>:</2') :-
	\+ a{k1:v1} >:< a{k1:v2}.
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

:- begin_tests(dict_order).

test(dict_order) :-
	atom(freshk0),
	atom(freshk1),
	atom(freshk2),
	atom(freshk3),
	atom(freshk4),
	atom(freshk5),
	D1 = a{freshk1:v1, freshk2:v2},
	\+ get_dict(freshk0, D1, _),
	get_dict(freshk1, D1, v1),
	get_dict(freshk2, D1, v2),
	\+ get_dict(freshk5, D1, _),
	D2 = a{freshk1:v1, freshk2:v2, freshk3:v3},
	\+ get_dict(freshk0, D2, _),
	get_dict(freshk1, D2, v1),
	get_dict(freshk2, D2, v2),
	get_dict(freshk3, D2, v3),
	\+ get_dict(freshk5, D2, _),
	D3 = a{freshk1:v1, freshk2:v2, freshk3:v3, freshk4:v4},
	\+ get_dict(freshk0, D3, _),
	get_dict(freshk1, D3, v1),
	get_dict(freshk2, D3, v2),
	get_dict(freshk3, D3, v3),
	get_dict(freshk4, D3, v4),
	\+ get_dict(freshk5, D3, _).

:- end_tests(dict_order).

:- begin_tests(destructive_assignment).

test('b_set_dict/3') :-
	D = a{k1:v1},
	b_set_dict(k1, D, v2),
	D == a{k1:v2}.
test('b_set_dict/3') :-
	D = a{k1:v1},
	(  b_set_dict(k1, D, v2),
	   fail
	;  D == a{k1:v1}
	).
test('nb_set_dict/3') :-
	D = a{k1:v1},
	nb_set_dict(k1, D, v2),
	D == a{k1:v2}.
test('nb_set_dict/3') :-
	D = a{k1:v1},
	(  nb_set_dict(k1, D, v2),
	   fail
	;  D == a{k1:v2}
	).

:- end_tests(destructive_assignment).

:- begin_tests(dict_dot3).

test(field, X == 3) :-
	X = _{a:1,b:2,c:3}.c.
test(field, X == 3) :-
	X = [a(1),b(2),c(3)].c.
test(no_field, error(existence_error(key,d,Dict))) :-
	Dict = _{a:1,b:2,c:3},
	ground(Dict.d).
test(no_dict, error(type_error(dict, "hello"))) :-
	ground("hello".x).

:- end_tests(dict_dot3).


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
x_dict4(D, In, Out) :-
	maplist(plus(D.x), In, Out).

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
test(put2, Y =:= 4) :-
	x_dict3(_{x:2}, X),
	Y = X.y.
test(metaarg, Out == [2]) :-
	x_dict4(_{x:1}, [1], Out).

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
test(a2, X =:= 4) :-
	X = test_dict{a:2}.test_a2().
test(qna, X == 1) :-
	X = test_dict2{}.test_na().
test(qa2, X =:= 4) :-
	X = test_dict{a:2}.test_a2().

:- end_tests(define_functions).
