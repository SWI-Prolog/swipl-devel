/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2014, University of Amsterdam
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

:- module(queue_gc,
	  [ queue_gc/0
	  ]).
:- use_module(library(debug)).
:- use_module(library(plunit)).
:- use_module(library(ordsets)).
:- use_module(library(apply)).

/** <module> Test cases for anonymous message queues

These tests are about anonymous message queues. As of SWI-Prolog 7.1.18,
such queues use a blob for   identification  and (thus) provide ABA-safe
handles and can be garbage collected.
*/

queue_gc :-
	run_tests([ queue_gc
		  ]).

:- meta_predicate
	in_thread(0),
	new_queues(0, -),
	no_new_queues(0).

new_queues(G, L) :-
	setup_call_cleanup(
	    queue_set(S0),
	    call(G),
	    ( queue_set(S1),
	      ord_subtract(S1, S0, L))).

queue_set(S) :-
	findall(Q, message_queue_property(Q, size(_)), L),
	sort(L, S).

in_thread(G) :-
	thread_create(G, Id, []),
	thread_join(Id, Result),
	assertion(Result == true).

%%	no_new_queues(:Goal)
%
%	Run Goal (which must succeed) and check  that it didn't leak any
%	threads. As we have a conservative   atom  garbage collector, we
%	run it in a thread to reduce the   risk that the handle is still
%	on an unused part of our stack and we try a few times.

no_new_queues(G) :-
	between(1, 10, I),
	new_queues((in_thread(G),garbage_collect_atoms), L),
	L == [], !,
	(   I == 1
	->  true
	;   format(user_error,
		   '~NRequired ~D retrys before all queues were GCed~n', [])
	).

create_and_destroy :-
	message_queue_create(Q),
	message_queue_destroy(Q).

create :-
	message_queue_create(_).

create_non_empty :-
	message_queue_create(Q),
	forall(between(1, 10, X),
	       ( atom_concat(a, X, A),
		 thread_send_message(Q, A))).

:- begin_tests(queue_gc).

test(exists, error(existence_error(message_queue, Q))) :-
	message_queue_create(Q),
	message_queue_destroy(Q),
	thread_send_message(Q, hello).
test(exists, error(existence_error(message_queue, Q))) :-
	message_queue_create(Q),
	thread_send_message(Q, hello),
	message_queue_destroy(Q),
	message_queue_property(Q, size(_S)).
test(exists, fail) :-
	message_queue_create(Q),
	message_queue_destroy(Q),
	message_queue_property(Q2, size(_S)),
	Q == Q2.
test(alias, true) :-
	message_queue_create(Q),
	\+ message_queue_property(Q, alias(_)),
	message_queue_destroy(Q).

test(new) :-
	new_queues(message_queue_create(Q), L),
	assertion(L == [Q]),
	maplist(message_queue_destroy, L).
test(destroy) :-
	no_new_queues(create_and_destroy).
test(gc) :-
	no_new_queues(create).
test(gc) :-
	no_new_queues(create_non_empty).

:- end_tests(queue_gc).


