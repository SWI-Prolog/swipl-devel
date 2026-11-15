/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2015, University of Amsterdam
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

:- module(test_cgc, [test_cgc/0]).
:- use_module(library(plunit)).
:- use_module(library(debug)).
:- use_module(library(apply)).

/** <module> Test clause garbage collection

This module tests clause gc. The current   test is about the interaction
between predicate marking and (local) stack shifts.
*/

test_cgc :-
	run_tests([ cgc,
		    erased
		  ]).

shift_cgc(Steps, Threads) :-
	length(L, Threads),
	maplist(count_thread(Steps), L),
	maplist(join, L).

join(Id) :-
	thread_join(Id, Status),
	assertion(Status == true).

count_thread(N, Id) :-
	thread_create(count(N), Id, [ stack_limit(2_000_000) ]).

count(N) :-
	between(1, N, _),
	catch(count2,
	      error(resource_error(stack), _),
	      fail).
count(_).

count2 :-
	step,
	(   maybe(0.01)
	->  lshift
	;   true
	),
	count2.

step :-
	with_mutex(step, step_).

:- dynamic counter/1.

step_ :-
	(   retract(counter(X))
	->  X2 is X+1
	;   X2 = 1
	),
	assert(counter(X2)).

lshift :-
	statistics(local_shifts, S0),
	lshift(S0), !.

lshift(S0) :-
	statistics(local_shifts, S0),
	lshift(S0).
lshift(_).


:- begin_tests(cgc, [ sto(rational_trees),
		      condition(current_prolog_flag(threads, true))
		    ]).

test(shift_cgc) :-
	shift_cgc(4, 4).

:- end_tests(cgc).

%	Erased clauses that clause GC cannot reclaim, because a call to
%	the predicate is still active, are reported by the properties
%	erased_clauses/1 and erased_clause_space/1 of the predicate and
%	by the statistics keys of the same name.  They are included in
%	its size/1.  As there, the space of a clause includes its clause
%	reference, so it is more than its clause_property/2 size.

:- dynamic erased_fact/1.

:- begin_tests(erased).

test(erased_clauses, [ N-Size1 == 100-Size0,
		       true(Space > ClauseSpace), true(Space < Size0),
		       true(GN >= 100), true(GS >= Space)
		     ]) :-
	retractall(erased_fact(_)),
	garbage_collect_clauses,
	forall(between(1, 100, I), assertz(erased_fact(I))),
	predicate_property(erased_fact(_), size(Size0)),
	findall(S, ( clause(erased_fact(_), true, Ref),
		     clause_property(Ref, size(S))
		   ), Sizes),
	sum_list(Sizes, ClauseSpace),
	once(( erased_fact(_),
	       retractall(erased_fact(_)),
	       predicate_property(erased_fact(_), erased_clauses(N)),
	       predicate_property(erased_fact(_), erased_clause_space(Space)),
	       predicate_property(erased_fact(_), size(Size1)),
	       statistics(erased_clauses, GN),
	       statistics(erased_clause_space, GS)
	     )).
test(no_erased_clauses, fail) :-
	predicate_property(test_cgc:step_, erased_clauses(_)).

:- end_tests(erased).
