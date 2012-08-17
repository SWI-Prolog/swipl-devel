:- module(test_unit,
	  [ test_unit/0
	  ]).
:- use_module(library(plunit)).

test_unit :-
	run_tests([expand_unit]).

term_expansion(check_foo,
	       [ (test(foo1):-assertz(done(1))),
		 (test(foo2):-assertz(done(2)))
	       ]).

:- begin_tests(expand_unit, [sto(rational_trees)]).
:- dynamic done/1.

check_foo.

test(expand, all(Done == [1,2])) :-
	retract(done(Done)).

:- end_tests(expand_unit).

