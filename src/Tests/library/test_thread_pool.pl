/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2008-2011, University of Amsterdam
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

:- module(test_thread_pool,
	  [ test_thread_pool/0,
	    t/0, d/0
	  ]).
:- use_module(library(plunit)).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(thread_pool)).

d :-
	debug(thread_pool),
	debug(thread_pool(_)).

t :-
	test_thread_pool.

test_thread_pool :-
	run_tests([ thread_pool
		  ]).

start :-
	start([detached(true)]).
start(Options) :-
	thread_pool_create(test, 3, Options).
stop :-
	thread_pool_destroy(test).

join_all([]).
join_all([H|T]) :-
	thread_join(H, Status),
	assertion(Status==true),
	join_all(T).

:- begin_tests(thread_pool, [sto(rational_trees),
			     condition(current_prolog_flag(threads, true))]).

:- dynamic
	v/1.

test(current, [setup(start),cleanup(stop)]) :-
	current_thread_pool(test).
test(free, [setup(start),cleanup(stop),X==3]) :-
	thread_pool_property(test, free(X)).
test(free, [setup(start),cleanup(stop),X==0]) :-
	thread_pool_property(test, backlog(X)).
test(loop, [setup(start([])),cleanup(stop), L == Vs]) :-
	numlist(0, 10, L),
	findall(Id,
		(   between(0, 10, I),
		    thread_create_in_pool(test, run(I), Id, [])
		), Ids),
	join_all(Ids),
	setof(V, retract(v(V)), Vs).

run(I) :-
	sleep(0.05),
	assert(v(I)).

:- end_tests(thread_pool).
