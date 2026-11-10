/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2026, SWI-Prolog Solutions b.v.
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

:- module(test_deep,
	  [ test_deep/0
	  ]).
:- use_module(library(plunit)).

/** <module> Test handling of deeply nested terms and clauses

These tests verify that deeply nested terms and clauses do not exhaust
the C stack, also in threads with a small C stack.
*/

test_deep :-
	run_tests([ deep_compile
		  ]).

:- dynamic p/0, fv/2.

nest(0, _, T, T) :- !.
nest(N, F, T0, T) :-
	N1 is N-1,
	call(F, T0, T1),
	nest(N1, F, T1, T).

disj(A, (A;fail)).
neg(A, \+ A).

goal(N, F, G) :-
	nest(N, F, true, G).

small_stack(Goal) :-
	thread_create(Goal, Id, [c_stack(65536)]),
	thread_join(Id, Status),
	assertion(Status == true).

:- begin_tests(deep_compile, [cleanup((retractall(p), retractall(fv(_,_))))]).

% Nested control structures used to allocate variable tables on the
% C stack for each nesting level.
test(disjunction) :-			% compilation is quadratic
	goal(20 000, disj, G),
	assertz((p :- G)),
	retract((p :- _)).
test(negation) :-
	goal(100 000, neg, G),
	assertz((p :- G)),
	retract((p :- _)).
test(disjunction_thread, [condition(current_prolog_flag(threads, true))]) :-
	small_stack(( goal(10 000, disj, G),
		      assertz((p :- G)),
		      retract((p :- _))
		    )).
% A clause with many variables used to allocate its variable table on
% the C stack.
test(many_vars_thread, [condition(current_prolog_flag(threads, true))]) :-
	small_stack(( length(L, 1 000 000),
		      assertz(fv(L, L)),
		      retract(fv(_, _))
		    )).

:- end_tests(deep_compile).
