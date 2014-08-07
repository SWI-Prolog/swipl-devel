/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2014, University of Amsterdam
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

:- module(test_threads, [test_threads/0]).
:- use_module(library(plunit)).
:- use_module(library(debug)).

/** <module> Test basic threading predicates

This module is a Unit test for Prolog built-ins that process threads.

@author	Jan Wielemaker
*/


test_threads :-
	run_tests([ thread_errors,
		    thread_property,
		    mutex,
		    mutex_property,
		    message_queue
		  ]).


:- begin_tests(thread_errors).

test(null, error(existence_error(thread, 0))) :-
	thread_send_message(0, foo).

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
