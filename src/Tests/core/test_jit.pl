:- module(test_jit,
	  [ test_jit/0
	  ]).
:- use_module(library(plunit)).
:- use_module(library(debug)).

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
	assertion(predicate_property(d(_,_), indexed([2-_]))),
	forall(between(51,125,X), assertz(d(X,X))),
	assertion(\+ predicate_property(d(_,_), indexed(_))),
	d(30,_),
	assertion(predicate_property(d(_,_), indexed([1-_]))).
test(remove, [cleanup(retractall(d(_,_)))]) :-
	forall(between(1,40,X), assertz(d(X,a))),
	forall(between(41,50,X), assertz(d(X,X))),
	d(30,a),
	assertion(predicate_property(d(_,_), indexed([1-_]))),
	retractall(d(_,a)),
	assertion(\+ predicate_property(d(_,_), indexed(_))),
	d(_,45),
	assertion(predicate_property(d(_,_), indexed([2-_]))).
test(retract, [cleanup(retractall(d(_,_))), Xs == Xsok]) :-
	forall(between(1,10,X), assertz(d(X,X))),
	forall(between(11,100,X), assertz(d(a,X))),
	findall(X, retract(d(a,X)), Xs),
	numlist(11, 100, Xsok).
test(retract2, [cleanup(retractall(d(_,_))), Xs == Xsok]) :-
	forall(between(1,10,X), assertz(d(X,X))),
	forall(between(11,100,X), assertz(d(a,X))),
	findall(X, rmd(a,X), Xs),
	numlist(11, 100, Xsok).
test(clause, [cleanup(retractall(d(_,_))), Xs == Xsok]) :-
	forall(between(1,10,X), assertz(d(X,X))),
	forall(between(11,100,X), assertz(d(a,X))),
	findall(X, claused(a,X), Xs),
	numlist(11, 100, Xsok).

rmd(X,Y) :-
	retract(d(X, Y)),
	(   Y == 89
	->  garbage_collect_clauses
	;   true
	).

claused(X,Y) :-
	clause(d(X, Y), true),
	(   Y == 89
	->  garbage_collect_clauses
	;   true
	).

:- end_tests(jit).
