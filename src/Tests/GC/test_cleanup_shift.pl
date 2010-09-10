:- module(test_cleanup_shift,
	  [ test_cleanup_shift/0
	  ]).
:- use_module(library(plunit)).

/** <module> Test setup_call_cleanup with shift and GC.
*/

test_cleanup_shift :-
	run_tests([ cleanup_shift
		  ]).

:- begin_tests(cleanup_shift).

test(gtrue, true) :- t_true(gshift).
test(ltrue, true) :- t_true(lshift).
test(ttrue, true) :- t_true(tshift).

test(gfalse, fail) :- t_false(gshift).
test(lfalse, fail) :- t_false(lshift).
test(tfalse, fail) :- t_false(tshift).

test(gcut, true) :- t_cut(gshift).
test(lcut, true) :- t_cut(lshift).
test(tcut, true) :- t_cut(tshift).

test(gexcept, true) :- t_except(gshift).
test(lexcept, true) :- t_except(lshift).
test(texcept, true) :- t_except(tshift).

:- end_tests(cleanup_shift).

t_true(Cleanup) :-
	garbage_collect,
	trim_stacks,
	setup_call_cleanup(true, true, Cleanup).
t_false(Cleanup) :-
	garbage_collect,
	trim_stacks,
	setup_call_cleanup(true, fail, Cleanup).
t_cut(Cleanup) :-
	garbage_collect,
	trim_stacks,
	setup_call_cleanup(true, (true;true), Cleanup), !.
t_except(Cleanup) :-
	garbage_collect,
	trim_stacks,
	catch(setup_call_cleanup(true, succ(_, -1), Cleanup), E, true),
	(   subsumes_term(error(domain_error(not_less_than_zero, -1), _), E)
	->  true
	;   format(user_error, 'Wrong error: ~p', [E]),
	    fail
	).

%%	gshift.
%%	tshift.
%%	lshift.
%
%	Force a stack-shift of the global, trail or local stack.

gshift :- shift(global_shifts).
tshift :- shift(trail_shifts).

shift(Stat) :-
	statistics(Stat, S0),
	shift(S0, Stat, X),
	nonvar(X).

shift(S0, Stat, s(X)) :-
	statistics(Stat, S0), !,
	shift(S0, Stat, X).
shift(_, _, _).

lshift :-
	statistics(local_shifts, S0),
	lshift(S0), !.

lshift(S0) :-
	statistics(local_shifts, S0),
	lshift(S0).
lshift(_).
