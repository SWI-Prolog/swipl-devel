/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2015, University of Amsterdam
			 VU University Amsterdam

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
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
	run_tests([ cgc
		  ]).

shift_cgc(Steps, Threads) :-
	length(L, Threads),
	maplist(count_thread(Steps), L),
	maplist(join, L).

join(Id) :-
	thread_join(Id, Status),
	assertion(Status == true).

count_thread(N, Id) :-
	thread_create(count(N), Id, [ local(2000) ]).

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


:- begin_tests(cgc, [sto(rational_trees)]).

test(shift_cgc) :-
	shift_cgc(4, 4).

:- end_tests(cgc).
