/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2007, University of Amsterdam

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
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

:- module(test_threads, [test_threads/0]).
:- use_module(library(plunit)).

/** <module> Test basic threading predicates

This module is a Unit test for Prolog built-ins that process threads.

@author	Jan Wielemaker
*/


test_threads :-
	run_tests([ thread_property
		  ]).


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
