/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2007-2016, University of Amsterdam
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

:- module(test_threads, [test_threads/0]).
:- use_module(library(plunit)).
:- use_module(library(debug)).

/** <module> Test basic threading predicates

This module is a Unit test for Prolog built-ins that process threads.

@author	Jan Wielemaker
*/


test_threads :-
	run_tests([ thread_create,
		    thread_errors,
		    thread_property,
		    mutex,
		    mutex_property,
		    message_queue
		  ]).


:- begin_tests(thread_create).

test(alias, Id == a_thread_name) :-
	thread_create(true, Id, [alias(a_thread_name)]),
	thread_join(Id, Status),
	assertion(Status == true).
test(anonymous) :-
	thread_create(true, Id, []),
	assertion(current_blob(Id, thread)),
	thread_join(Id, Status),
	assertion(Status == true).

:- end_tests(thread_create).

:- begin_tests(thread_errors).

test(null, error(type_error(message_queue, 0))) :-
	thread_send_message(0, foo).
test(null, error(existence_error(message_queue, not_a_thread))) :-
	thread_send_message(not_a_thread, foo).

:- end_tests(thread_errors).


:- begin_tests(thread_property).

receive(Sender, Goal) :-
	thread_send_message(Sender, ready),
	Goal,
	thread_get_message(done).

wait_status(Id, Status) :-
	repeat,
	thread_property(Id, status(Status)),
	(   Status == running
	->  sleep(0.01),
	    fail
	;   !,
	    thread_join(Id, _)
	).

test(alias, Alias == th42) :-
	thread_self(Me),
	thread_create(receive(Me, true), Id, [alias(th42)]),
	thread_get_message(ready),
	thread_property(Id, alias(Alias)),
	thread_send_message(Id, done),
	thread_join(Id, true).

test(detached_false, State == false) :-
	thread_self(Me),
	thread_create(receive(Me, true), Id, []),
	thread_get_message(ready),
	thread_property(Id, detached(State)),
	thread_send_message(Id, done),
	thread_join(Id, true).

test(detached_true, State == true) :-
	thread_self(Me),
	thread_create(receive(Me, true), Id, [detached(true)]),
	thread_get_message(ready),
	thread_property(Id, detached(State)),
	thread_send_message(Id, done).

test(status_running, Status == running) :-
	thread_self(Me),
	thread_create(receive(Me, true), Id, []),
	thread_get_message(ready),
	thread_property(Id, status(Status)),
	thread_send_message(Id, done),
	thread_join(Id, true).

test(status_true, Status == true) :-
	thread_self(Me),
	thread_create(receive(Me, true), Id, []),
	thread_get_message(ready),
	thread_send_message(Id, done),
	wait_status(Id, Status).

test(status_fail, Status == false) :-
	thread_self(Me),
	thread_create(receive(Me, fail), Id, []),
	thread_get_message(ready),
	wait_status(Id, Status).

test(status_exception, Status == exception(error)) :-
	thread_self(Me),
	thread_create(receive(Me, throw(error)), Id, []),
	thread_get_message(ready),
	wait_status(Id, Status).

test(status_enum1, true) :-
	thread_self(Me),
	thread_create(receive(Me, true), Id, []),
	thread_get_message(ready),
	thread_property(Thread, status(_Status)),
	Thread == Id, !,
	thread_send_message(Id, done),
	thread_join(Id, true).

test(status_enum2, true) :-
	thread_self(Me),
	thread_create(receive(Me, true), Id, []),
	thread_get_message(ready),
	thread_property(Thread, status(_Status)),
	Thread == Me, !,
	thread_send_message(Id, done),
	thread_join(Id, true).

test(existence, error(existence_error(thread, foobar))) :-
	thread_property(foobar, _).

test(existence, error(domain_error(thread_property, a))) :-
	thread_property(_, a).

:- end_tests(thread_property).


		 /*******************************
		 *	       MUTEXES		*
		 *******************************/

:- begin_tests(mutex).

test(lock) :-
	mutex_create(M),
	mutex_lock(M),
	mutex_unlock(M),
	mutex_destroy(M),
	assertion(gone(M)).
test(destroy, error(existence_error(mutex, M))) :-
	mutex_create(M),
	mutex_destroy(M),
	mutex_lock(M).
test(destroy_locked) :-
	mutex_create(M),
	mutex_lock(M),
	mutex_destroy(M),
	mutex_unlock(M),
	assertion(gone(M)).
test(destroy_locked_other_thread) :-
	mutex_create(M),
	mutex_lock(M),
	in_thread(mutex_destroy(M)),
	mutex_unlock(M),
	assertion(gone(M)).

:- meta_predicate in_thread(0).

in_thread(G) :-
	thread_create(G, Id, []),
	thread_join(Id, Status),
	assertion(Status == true).

gone(M) :-
	catch(mutex_property(M, status(_)),
	      error(existence_error(mutex, M), _),
	      Gone = true),
	Gone == true.

:- end_tests(mutex).

:- begin_tests(mutex_property).

test(alias, Alias == m42) :-
	mutex_create(X, [alias(m42)]),
	mutex_property(X, alias(Alias)),
	mutex_destroy(X).

test(generate, true) :-
	mutex_create(X, [alias(m43)]),
	mutex_create(Y, [alias(m44)]),
	findall(I, mutex_property(_, alias(I)), Is),
	mutex_destroy(X),
	mutex_destroy(Y),
	subset([m43,m44], Is).

test(locked, [By,Count] == [Me,1]) :-
	thread_self(Me),
	mutex_create(X, []),
	mutex_lock(X),
	mutex_property(X, status(locked(By, Count))),
	mutex_unlock(X),
	mutex_destroy(X).

:- end_tests(mutex_property).


		 /*******************************
		 *	       QUEUES		*
		 *******************************/

:- begin_tests(message_queue).

test(max_size_prop, true) :-
	message_queue_create(Queue, [max_size(5)]),
	message_queue_property(Queue, P),
	P = max_size(5), !,
	message_queue_destroy(Queue).

test(size_prop, true) :-
	message_queue_create(Queue, []),
	thread_send_message(Queue, 1),
	thread_send_message(Queue, 2),
	message_queue_property(Queue, P),
	P = size(2), !,
	message_queue_destroy(Queue).

:- end_tests(message_queue).
