/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2014, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
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


