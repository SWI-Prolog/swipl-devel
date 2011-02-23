:- module(test_list,
	  [ test_list/0
	  ]).
:- use_module(library(plunit)).
:- use_module(library(terms)).
:- use_module(library(apply)).

test_list :-
	run_tests([ memberchk
		  ]).

:- begin_tests(memberchk, []).

test(memberchk, X == y) :-		% Verify unbinding after failing PL_unify()
	memberchk(f(X,a), [f(x,b), f(y,a)]).

:- end_tests(memberchk).
