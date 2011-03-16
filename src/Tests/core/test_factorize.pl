:- module(test_factorize,
	  [ test_factorize/0
	  ]).
:- use_module(library(plunit)).
:- use_module(library(terms)).
:- use_module(library(apply)).

test_factorize :-
	run_tests([ factorize
		  ]).

:- meta_predicate ok(+, 0).

factorize_term(Term, Skeleton, Substitution) :-
	F = f(Term),
	'$factorize_term'(F, Substitution),
	arg(1, F, Skeleton).

test_factor(Term) :-
	copy_term(Term, Save),
	term_factorized(Term, FOK, _BOK),
	(   factorize_term(Term, FT, BT),
	    ok(skeleton, variant(FOK, FT)),
	    maplist(call, BT),
	    ok(rebind, variant(FT, Save)),
	    garbage_collect,
	    fail
	;   ok(backtrack, variant(Term, Save))
	).

ok(Id, G) :-
	(   G
	->  true
	;   throw(failed(Id, G))
	).

fumo(0,fumo) :- !.
fumo(N,[F|F]) :-
        N1 is N-1,
        fumo(N1,F).

:- begin_tests(factorize, [sto(rational_trees)]).

test(simple, true) :-
	X = a, test_factor(X).
test(simple, true) :-
	X = a(1), test_factor(X).
test(cyclic, true) :-
	X = a(X), test_factor(X).
test(double, true) :-
	X = a(X,X), test_factor(X).
test(double_cyclic, true) :-
	A = a(X), X = a(A), test_factor(X).
test(double_cyclic, true) :-
	A = a(A), X = x(A,A), test_factor(X).
test(double_cyclic, true) :-
	X = x(A,A), A = a(A), test_factor(X).
test(double_cyclic, true) :-
	A = a(A), X = x(b(A),A), test_factor(X).
test(fumo, true) :-
	fumo(20, X), test_factor(X).

:- end_tests(factorize).
