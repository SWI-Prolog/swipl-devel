:- module(test_thread, [test_thread/0]).

test_thread :-
	run_tests(thread).

:- use_module(library(plunit)).
:- use_module(library(thread)).

:- begin_tests(thread).

test(true, true) :-
	concurrent(2, [true], []).
test(unify, true(A==3)) :-
	concurrent(2, [A=3], []).
test(unify, true([A,B]==[3,4])) :-
	concurrent(2, [A=3, B = 4], []).
test(fail, fail) :-
	concurrent(2, [_A=3, fail, _B = 4], []).
test(error, throws(x)) :-
	concurrent(2, [_A=3, throw(x), _B = 4], []).
test(concur, true) :-
	forall(between(0, 20, _),
	       (   concurrent(2, [X=1,Y=2], []),
		   ground(X-Y))).

test(first, true(X==1)) :-
	first_solution(X, [X=1,X=1], []).
test(first, fail) :-
	first_solution(X, [fail,X=1], []).
test(first, true(X==1)) :-
	first_solution(X, [fail,X=1], [on_fail(continue)]).
test(first, true(X==1)) :-
	first_solution(X, [(repeat,fail), X=1], []).

:- end_tests(thread).
