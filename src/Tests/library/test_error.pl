:- module(test_error, [test_error/0]).
:- use_module(library(plunit)).

test_error :-
	run_tests([ must_be ]).

:- begin_tests(must_be, []).
:- use_module(library(error)).

test(integer, []) :-
	1 = Culprit, must_be(integer,Culprit).
test(integer, [error(instantiation_error)]) :-
	_ = Culprit, must_be(integer, Culprit).
test(integer, [error(instantiation_error)]) :-
	_ = Culprit, must_be(Culprit, integer).
test(integer, [error(type_error(integer,Culprit))]) :-
	x = Culprit, must_be(integer, Culprit).
test(integer, [error(type_error(integer,Culprit))]) :-
	[_] = Culprit, must_be(integer, Culprit).
test(rational, [error(type_error(rational,Culprit))]) :-
	a(_) = Culprit, must_be(rational,Culprit).
test(rational, []) :-
	1 = Culprit, must_be(rational,Culprit).
test(rational, [error(type_error(rational,Culprit))]) :-
	a = Culprit, must_be(rational,Culprit).
test(rational, [error(type_error(integer,x))]) :-
	rdiv(x,2) = Culprit, must_be(rational,Culprit).
test(rational, [error(instantiation_error)]) :-
	rdiv(_,2) = Culprit, must_be(rational,Culprit).
test(list_of_integer, [error(instantiation_error)]) :-
	_ = Culprit, must_be(list(integer), Culprit).
test(list_of_integer, [error(instantiation_error)]) :-
	[_] = Culprit, must_be(list(integer), Culprit).
test(list_of_integer, [error(instantiation_error)]) :-
	[1,_] = Culprit, must_be(list(integer), Culprit).
test(list_of_integer, [error(type_error(integer,x))]) :-  % per analogiam 8.16.4.3 d
	[x] = Culprit, must_be(list(integer), Culprit).
test(list_of_integer, [error(instantiation_error)]) :-  % per analogiam 8.16.4.3 a
	[x|_] = Culprit, must_be(list(integer), Culprit).
test(list_of_integer, [sto(rational_trees),error(type_error(list, Culprit))]) :-
	[1|Culprit] = Culprit, must_be(list(integer),Culprit).
test(list_of_integer, [error(type_error(list, Culprit))]) :-
	[1|x] = Culprit, must_be(list(integer),Culprit).
test(list_of_integer, [error(type_error(list, Culprit))]) :-
	[1|s(_)] = Culprit, must_be(list(integer),Culprit).

:- end_tests(must_be).
