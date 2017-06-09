/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2013-2014, VU University Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(test_d_break,
	  [ test_d_break/0
	  ]).
:- use_module(library(plunit)).
:- use_module(library(debug)).

% otherwise optimised clauses are not loaded (already set in suite)
%:- set_test_options([load(always)]).
%:- debug(break_hook).

test_d_break :-
	run_tests([ d_break
		  ]).


:- begin_tests(d_break, [sto(rational_trees)]).

%%	break_me(?Instruction)
%
%	Provide a clause for each of the   VM  instructions we can break
%	on.

:- dynamic
	old_optimise/1.

:- current_prolog_flag(optimise, Old),
   set_prolog_flag(optimise, true),
   asserta(old_optimise(Old)).

:- '$clausable'(t_break/1).

t_break(i_depart(c0/0)) :- c0.
t_break(i_call(c0/0)) :- c0, c1.
t_break(i_call(c1/1)) :- c1(42), c1.
t_break(i_call(shared/2)) :- shared(A,A), c1.
t_break(i_call(findall/3)) :- findall(X, between(1, 5, X), Xs), Xs == [1,2,3,4,5].
t_break(i_enter) :- c0.
t_break(i_exit) :- c2.
t_break(i_cut) :- c2, !.
t_break(b_unify_firstvar(_)):- A = [a], nonvar(A).
t_break(b_unify_var(_)):- v(A), A = [a].
t_break(b_unify_ff(_,_)) :- A = B, v(A), v(B).
t_break(b_unify_fv(_,_)) :- v(B), A = B, v(A).
t_break(b_unify_vv(_,_)) :- B = f(a), v(A), v(B), A = B, A == f(a).
t_break(b_unify_fc(_,_)) :- B = 42, B == 42.
t_break(b_unify_vc(_,_)) :- v(B), B = 42, B == 42.
t_break(b_eq_vv(_,_)) :- A=B, A == B.
t_break(b_eq_vc(_,_)) :- A = b, A == b.
t_break(b_neq_vv(_,_)) :- v(A), v(B), A \== B.
t_break(b_neq_vc(_,_)) :- A = a, A \== b.
t_break(i_fail) :- \+ fail.
t_break(i_true) :- c0 -> true.
t_break(i_var(_)) :- v(A), var(A).
t_break(i_nonvar(_)) :- A=a, nonvar(A).
t_break(a_add_fc(_,_,_)) :- A = 1, B is A+1, v(B).
t_break(a_lt) :- A = 1, B = 2, A < B.
t_break(a_le) :- A = 1, B = 2, A =< B.
t_break(a_gt) :- A = 3, B = 2, A > B.
t_break(a_ge) :- A = 3, B = 2, A >= B.
t_break(a_eq) :- A = 3, B = 3, A =:= B.
t_break(a_ne) :- A = 2, B = 3, A =\= B.
t_break(a_is) :- B = 3, v(A), A is B*3.	% TBD: fails after callback!
t_break(a_firstvar_is(_)) :- B = 3, A is B*3, v(A).
t_break(i_usercall0) :- A = c0, call(A).
t_break(i_usercalln(_)) :- A = v, call(A, V), v(V).
t_break(i_departm(_,_)) :- test:c0.
t_break(i_callm(_,_)) :- test:c0, c1.
t_break(i_departatm(_,_,_)) :- @(test:c1(foo),foo).
t_break(i_callatm(_,_,_)) :- @(test:c1(foo),foo), c1.
t_break(i_departatmv(_,_,_)) :- Ctx = foo, @(test:c1(Ctx),Ctx).
t_break(i_callatmv(_,_,_))   :- Ctx = foo, @(test:c1(Ctx),Ctx), c1.
:- retract(old_optimise(Old)),
   set_prolog_flag(optimise, Old).

c0 :- stress(call).
c1 :- stress(call).
c2 :- stress(call).
c2 :- stress(call).
v(_) :- stress(call).
c1(42) :- stress(call).

shared(X,Y) :-
	stress(call),
	X == Y.

test:c0 :- stress(call).

:- meta_predicate test:c1(:).

test:c1(M:M).

:- '$clausable'(t_call/0).
t_call :- t_called([a,b], [c], [a,b,c]), c1.
t_call :- t_called([a,b], [c], [a,b,c]), c1.

t_called(L1, L2, L3) :-
	assertion(is_list(L1)),
	assertion(is_list(L2)),
	assertion(is_list(L3)),
	append(L1, L2, L3).

		 /*******************************
		 *	       TESTS		*
		 *******************************/

test(call) :-
	set_action(call),
	break(t_call, i_call(t_called/3)),
	forall(t_call, garbage_collect).

:- dynamic
	error/1.			% Call

test(t_break) :-
	break_all,
	retractall(error(_)),
	(   member(Action, [call,continue]),
	    member(StressWhen, [call,hook,wrap]),
	    member(StressWhat, [gc, lshift, gshift]),
	      set_action(Action),
	      set_stress([StressWhen], [StressWhat]),
	      (   debug(break_hook(progress),
			'Test ~p ~p ~p', [Action, StressWhen, StressWhat]),
		  run(_)
	      ->  true
	      ;   format(user_error, 'Run failed for ~w ~p ~p~n',
			 [Action, StressWhen, StressWhat]), !, fail
	      ),
	    fail
	;   \+ error(_)
	).


		 /*******************************
		 *	     SUPPORT CODE	*
		 *******************************/

break_all :-
	setup_call_cleanup(
	    asserta(user:thread_message_hook(breakpoint(_,_), informational, _),
		    Ref),
	    forall(clause(t_break(Instr), _),
		   break(t_break(Instr), _)),
	    erase(Ref)).

break(Head, Instr) :-
	(   nth_clause(Head, I, Clause),
	    clause(Head, _, Clause),
	    '$break_pc'(Clause, PC, _NextPC1),
	    '$fetch_vm'(Clause, PC, _NextPC2, TInstr),
	    Instr = TInstr,
	    (	TInstr = break(_)
	    ->	true
	    ;	'$break_at'(Clause, PC, true),
		used(I),
		debug(break_hook, 'Put breakpoint on cl ~d ~w@~d on ~w',
		      [I, Clause, PC, Instr])
	    ),
	    fail
	;   true
	).

used(_).					% avoid warning

:- dynamic
	action/1,
	stressor/1.

stress(When) :-
	forall(stressor(When=Actions),
	       stress_actions(Actions)).

stress_actions([]).
stress_actions([H|T]) :- stress_action(H), stress_actions(T).

stress_action(gc)     :- garbage_collect.
stress_action(lshift) :- lshift.
stress_action(gshift) :- gshift.
stress_action(tshift) :- tshift.

set_stress(When, What) :-
	retractall(stressor(_)),
	forall(member(W, When),
	       assertz(stressor(W=What))).

set_action(Action) :-
	retractall(action(_)),
	asserta(action(Action)).

%%	run(+Head)
%
%	Run t_break/1 test set for all clauses that match Head.

run(Head) :-
	forall(clause(t_break(Head), _),
	       (   trim_stacks,
		   catch(t_break(Head), E, true)
	       ->  (   var(E)
		   ->  true
		   ;   debug(break_hook, 'Error: ~q', [E])
		   )
	       ;   (   action(Action)
		   ->  true
		   ;   Action = continue
		   ),
		   findall(S, stressor(S), Stress),
	           print_message(error, failed(Head,Action,Stress)),
		   assert(error(Head))
	       )).

prolog:break_hook(Clause, PC, FR, BFR, call(What), Action) :- !,
	debug(break_hook, 'Break on ~p@~p ~p ~p ~p', [Clause, PC,FR, BFR, What]),
	stress(hook),
	(   action(call)
	->  Action = call(wrap(What))
	;   Action = continue
	).
prolog:break_hook(Clause, PC, FR, BFR, What, continue) :-
	debug(break_hook, 'Break on ~p@~p ~p ~p ~p', [Clause, PC,FR, BFR, What]),
	stress(hook).


gshift :- shift(global_shifts).
tshift :- shift(trail_shifts).

shift(Stat) :-
        statistics(Stat, S0),
	shift(S0, Stat, X),
        shift_a(X).			% ensure term is used.

shift_a(_).

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

%%	wrap(Goal)
%
%	Wrapped execution of Goal from a breakpoint.

:- meta_predicate wrap(0).

wrap(G) :-
	call(G),
	stress(wrap).

%%	sublist(?Sub, +List) is nondet.
%
%	True if all elements of Sub appear in List in the same order.

sublist(L, L).
sublist(Sub, [H|T]) :-
	sublist_(T, H, Sub).

sublist_(Sub, _, Sub).
sublist_([H|T], _, Sub) :-
	sublist_(T, H, Sub).
sublist_([H|T], X, [X|Sub]) :-
	sublist_(T, H, Sub).

:- end_tests(d_break).





