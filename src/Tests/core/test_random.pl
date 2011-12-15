:- module(test_random,
	  [ test_random/0
	  ]).
:- use_module(library(plunit)).

test_random :-
	run_tests([ random
		  ]).

/** <module> Test unit for random number handling
*/

:- begin_tests(random).

:- if(current_predicate(random_property/1)).

tr(N, L1, L2) :-
	set_random(seed(random)),
	random_property(state(State)),
	random_seq(N, L1),
	set_random(state(State)),
	random_seq(N, L2).

random_seq(N, [H|T]) :-
	succ(N2, N), !,
	H is random_float,
	random_seq(N2, T).
random_seq(0, []).

test(state, [X==Y]) :-
	tr(100, X,Y).

:- endif.

:- end_tests(random).
