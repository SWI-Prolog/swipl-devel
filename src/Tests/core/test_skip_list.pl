:- module(test_skip_list, [test_skip_list/0]).
:- use_module(library(plunit)).


test_skip_list :-
	run_tests([ skip_list ]).



:- begin_tests(skip_list).

test(empty_list) :-
	'$skip_list'(Len, [], Diff),
	Len == 0,
	Diff == [].

test(proper_list_small) :-
	'$skip_list'(Len, [_], Diff),
	Len == 1,
	Diff == [].

test(proper_list_long) :-
	length(List, 10000),
	'$skip_list'(Len, List, Diff),
	Len == 10000,
	Diff == [].

test(partial_list) :-
	'$skip_list'(Len, [_|X], Diff),
	Len == 1,
	Diff == X.

test(cyclic_list, [sto(rational_trees)]) :-
	List = [_|List],
	'$skip_list'(Len, List, Diff),
	Len >= 1,
	nonvar(Diff),
	Diff = [_|_].

test(cyclic_list_long, [sto(rational_trees)]) :-
	List = [_|X],
	length(List0, 10000),
	append(List0, List, X),
	'$skip_list'(Len, List, Diff),
	Len >= 10001,
	nonvar(Diff),
	Diff = [_|_].

test(not_a_list_1, [sto(rational_trees)]) :-
	'$skip_list'(Len, X, Diff),
	Len == 0,
	Diff == X.

test(not_a_list_2, [sto(rational_trees)]) :-
	'$skip_list'(Len, x, Diff),
	Len == 0,
	Diff == x.

test(not_a_list_3, [sto(rational_trees)]) :-
	'$skip_list'(Len, [_|x], Diff),
	Len == 1,
	Diff == x.

:- end_tests(skip_list).
