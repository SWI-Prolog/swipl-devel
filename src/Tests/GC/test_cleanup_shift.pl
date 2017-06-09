/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2009-2013, University of Amsterdam
			      VU University Amsterdam
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

gshift :- shift_stack(global_shifts).
tshift :- shift_stack(trail_shifts).

shift_stack(Stat) :-
	statistics(Stat, S0),
	shift_stack(S0, Stat, X),
	nonvar(X).

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
