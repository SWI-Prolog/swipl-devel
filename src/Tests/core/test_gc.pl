/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2008-2015, University of Amsterdam
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

:- module(test_gc, [test_gc/0]).
:- use_module(library(plunit)).

/** <module> Test garbage collection

This unit contains small tests for the garbage collector.
*/

test_gc :-
	run_tests([ gc_leak,
		    gc_reset,
		    gc_crash,
		    gc_crash2,
		    gc_mark,
		    agc
		  ]).

:- module_transparent
	space/3,
	nospace/1.

space(T, G) :-
	garbage_collect,
	statistics(trailused, T),
	statistics(globalused, G).

must_space(T0, G0) :-
	space(T1, G1),
	(   T0 == T1,
	    G0 == G1
	->  true
	;   T is T1-T0,
	    G is G1-G0,
	    format(user_error, 'Used ~D+~D~n', [T,G]),
	    fail
	).

space(Goal, T, G) :-
	space(T0,G0),
	Goal,
	space(T1,G1),
	T is T1-T0,
	G is G1-G0.

nospace(Goal) :-
	space(Goal, T, G),
	(   T == 0, G == 0
	->  true
	;   format(user_error, 'Used ~D+~D~n', [T,G]),
	    fail
	).

:- begin_tests(gc_leak, [sto(rational_trees)]).

det_freeze_loop(N, T) :-
	(   succ(N2, N)
	->  freeze(X, true),
	    X = a,
	    det_freeze_loop(N2, T)
	;   garbage_collect,
	    statistics(trailused, T)
	).

early_reset :-
	early_reset(_).

early_reset(X) :- space(T,G), length(X, 10), must_space(T,G), !.
early_reset(_) :- fail.

test(det_freeze_no_space) :-
	garbage_collect,
	statistics(trailused, T0),
	det_freeze_loop(1000, T1),
	T is T1 - T0,
	T < 100.			% A small constant use is ok
test(early_reset) :-
	early_reset.
test(throw_gc, G0 == G1) :-	% See a9832d10f6de4f46d559bcd74aa2c9fe3b8588ab
	statistics(stack_shifts, [G0,_,_]),
	(   between(1, 10000, _),
	    catch(throw(foo),_, true),
	    fail
	;   true
	),
	statistics(stack_shifts, [G1,_,_]).

:- end_tests(gc_leak).

:- begin_tests(gc_reset).

deep_reset :-
        X = a(A),
        deep_reset(A), !,
        X == a(42).

deep_reset(A) :-
        (   A = 42, garbage_collect
        ;   true
        ).

test(deep_reset) :- deep_reset.


:- end_tests(gc_reset).

:- begin_tests(gc_crash).

:- set_prolog_flag(double_quotes, string).

t1 :-
	garbage_collect,
	t("hello world").

t("hello world").

loop(0) :- !.
loop(N) :-
	setup_call_catcher_cleanup(
	    true,
	    setup_call_cleanup(
		true,
		between(1, 2, _),
		garbage_collect),
	    Reason,
	    Reason == true),
	N2 is N - 1,
	loop(N2).

trail_shift :-
        statistics(trail_shifts, S0),
        shift(S0, X),
        nonvar(X).

shift(S0, s(X)) :-
        statistics(trail_shifts, S0), !,
        shift(S0, X).
shift(_, _).

test(b_string) :-
	t1.
test(wakeup_two) :-
	freeze(V1, true),
	freeze(V2, (garbage_collect,V2==y)),
	(   x(V1,V2) = x(a,b)
	;   x(V1,V2) = x(x,y)
	).
test(cut) :-
	loop(10), !.
test(c_cut) :-
	loop(10) -> true.
test(cleanup_shift, [throws(foo)]) :-
        setup_call_catcher_cleanup(
	    true,
	    member(_,[a,b]),
	    _Reason,
	    trail_shift),
        throw(foo).

:- end_tests(gc_crash).

:- begin_tests(gc_crash2).

test(cleanup) :-		% patch 33c661dca59ba3c007348533bd3e4687585c9e7a
    catch(( go_1(1),
            go_1(2)
          ),
          _,
          true).

go_1(N) :-
        setup_call_cleanup(mysetup,
                       ( ( true ; true ),
                         process_n(N)
                       ),
                       mycleanup).

mysetup.
mycleanup.

process_n(1) :- !.
process_n(2) :- throw(foo).

:- end_tests(gc_crash2).

:- begin_tests(gc_mark).

test(s_list, true) :-			% S_NEXTCLAUSE must mark args of next
	length(_List, N),		% clause (broken in 5.7.0)
	garbage_collect,
	N == 4, !.
test(s_fredo, true) :-
	A = a(1,2,3,4,5,6),
	arg(_I, A, N),
	garbage_collect,
	N == 5, !.
test(c_ifthen, true) :-			% Scanning C_NOT should not end due
	A = {null},			% to the C_CUT of (a->b)
	garbage_collect,
	\+ (  true,
	      \+ ( ( true ->
		     true
		   ; otherwise ->
		     true
		   ),
		   A == {null}
		 )
	   ).
test(b_neq_vv, true) :-
	numlist(1, 100, X),
	numlist(1, 100, Y),
	garbage_collect,
	\+ (X \== Y),
	a(Y).

a(Y) :- length(Y,_).

:- end_tests(gc_mark).


:- begin_tests(agc).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Verify marking atoms from temporary clauses  for meta-calling. Using the
enhanced garbage collector access through  the   variables  is no longer
guaranteed.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- dynamic
	v/1.

test :-
        atom_concat(abcd, efgh, A),
        Goal = ( garbage_collect,
                 garbage_collect_atoms,
                 make_atoms,
		 assert_atom(A)
               ),
	test_c(Goal).

test_c(Goal) :-
        Goal.

assert_atom(A) :-
	assert(v(A)).

make_atoms :-
        forall(between(1, 10000, X),
               atom_concat(foo, X, _)).

test(usercall, A == Ok) :-
	retractall(v(_)),
	test,
	retract(v(A)),
	atom_concat(abcd, efgh, Ok).

:- end_tests(agc).
