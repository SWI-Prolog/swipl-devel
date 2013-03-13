:- module(test_tracer_callback,
	  [ test_tracer_callback/0
	  ]).
:- use_module(library(debug)).

%%	test_tracer_callback
%
%	Test callbacks for the tracer.  The   idea  is  simple: fake the
%	tracer using the tracer hook and make sure each hook-call shifts
%	the stacks. Next, run programs that invoke the tracer from every
%	possible hook in the VM.

test_tracer_callback :-
	forall(test_goal(Goal),
	       test_tracer(Goal)).

test_goal(p_simple).
test_goal(p_backtrack).
test_goal(p_cut).
test_goal(p_ifthen).
test_goal(p_error1).
test_goal(p_error2).
test_goal(p_error3).

test_tracer(G) :-
	trim_stacks,			% there must be enough space to force
	'$visible'(Old, Old),		% a stack shift
	visible(+all),
	visible(+cut_call),
	assert((user:prolog_trace_interception(P, F, Ch, A) :-
			intercept(P, F, Ch, A)), Ref),
	trace,
	call_cleanup(G,
		     (nodebug, '$visible'(_, Old), erase(Ref))),
	put_char(user_error, '.').


%%	p_simple
%
%	Simple recursive calling test

p_simple :-
	app([a,b], [c,d], X),
	is_list(X).

%%	p_backtrack
%
%	Test backtracking

p_backtrack :-
	app(_, Y, [h,e,l,l,o]),
	Y = [l|_].

%%	p_cut
%
%	Test interception of the !

p_cut :-
	app(_, Y, [h,e,l,l,o]),
	Y = [l|_], !,
	a.

%%	p_ifthen
%
%	Test simple if-then-else backtracking

p_ifthen :-
	(   no
	->  true
	;   a
	).

%%	p_error1 is det.
%%	p_error2 is det.
%%	p_error3 is det.
%
%	Test recovery from exceptions.

p_error1 :-
	catch(error1, _, true).

error1 :-
	a, b, e1, c.

p_error2 :-
	catch(error2, _, true).

error2 :-
	a, b, e2, c.

p_error3 :-
	catch(error3([a,b,c,d,e]), _, true).

error3([X]) :-
	throw(bad(X)).
error3([_|T]) :-
	error3(T).



		 /*******************************
		 *	      BLOCKS		*
		 *******************************/

a.
b.
c.

no :-
	fail.

e1 :-
	X is 1/0,
	number(X).

e2 :-
	a(V),
	X is V+1,
	number(X).

app([], L, L).
app([H|T], L, [H|L2]) :-
	app(T, L, L2).


		 /*******************************
		 *     INTERCEPTION ENGINE	*
		 *******************************/

intercept(Port, Frame, _Choice, continue) :-
	prolog_frame_attribute(Frame, predicate_indicator, PI),
	prolog_frame_attribute(Frame, level, Level),
	debug(trace, '~t[~d]~6| ~w ~q ..', [Level, Port, PI]),
	shift.


shift :-
	flag(shift, N, N+1),
	I is N mod 4,
	garbage_collect,
	(   catch(action(I), E, true)
	->  (   var(E)
	    ->	true
	    ;	message_to_string(E, Msg),
		format(user_error, '~p: ~s~n', [action(I), Msg])
	    )
	;   format(user_error, '~p failed~n', [action(I)])
	).


action(0) :- gshift.
action(1) :- tshift.
action(2) :- lshift.
action(3) :- trim_stacks.

%%      gshift.
%%      tshift.
%%      lshift.
%
%       Force a stack-shift of the global, trail or local stack.

gshift :- shift_stack(global_shifts).
tshift :- shift_stack(trail_shifts).

shift_stack(Stat) :-
	statistics(Stat, S0),
	shift_stack(S0, Stat, X),
	a(X).				% ensure term is used.

a(_).

shift_stack(S0, Stat, s(X)) :-
	statistics(Stat, S0), !,
	shift_stack(S0, Stat, X).
shift_stack(_, _, _).

lshift :-
	statistics(local_shifts, S0),
	lshift(S0), !.

lshift(S0) :-
	statistics(local_shifts, S0),
	lshift(S0).
lshift(_).

