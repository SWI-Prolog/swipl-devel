:- module(test_jit,
	  [ test_jit/0
	  ]).
:- use_module(library(plunit)).

test_jit :-
	run_tests([ jit
		  ]).

/** <module> Test unit for Just-In-Time indexing

This module tests behaviour of the just-in-time indexes.
*/

:- begin_tests(jit).

:- dynamic
	d/2.

test(remove, [cleanup(retractall(d(_,_)))]) :-
	forall(between(1,50,X), assertz(d(X,X))),
	d(_,30),
	predicate_property(d(_,_), indexed([2-_])),
	forall(between(51,125,X), assertz(d(X,X))),
	\+ predicate_property(d(_,_), indexed(_)),
	d(30,_),
	predicate_property(d(_,_), indexed([1-_])).
test(remove, [cleanup(retractall(d(_,_)))]) :-
	forall(between(1,40,X), assertz(d(X,a))),
	forall(between(41,50,X), assertz(d(X,X))),
	d(30,a),
	predicate_property(d(_,_), indexed([1-_])),
	retractall(d(_,a)),
	\+ predicate_property(d(_,_), indexed(_)),
	d(_,45),
	predicate_property(d(_,_), indexed([2-_])).
test(retract, [cleanup(retractall(d(_,_))), Xs == Xsok]) :-
	forall(between(1,10,X), assertz(d(X,X))),
	forall(between(11,100,X), assertz(d(a,X))),
	findall(X, retract(d(a,X)), Xs),
	numlist(11, 100, Xsok).

:- end_tests(jit).
