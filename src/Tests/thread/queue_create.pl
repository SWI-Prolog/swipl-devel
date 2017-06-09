/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2002-2010, University of Amsterdam
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

:- module(queue_create,
	  [ queue_create/0,
	    queue_create/1
	  ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This simple test verifies creation  and   destruction  of message queues
associated with threads. There appears to be  a HUGE difference in speed
between platforms in the elapsed time it takes to get this test done, so
first we try to figure out how fast it is about.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

queue_create :-
	elapsed_time(queue_create(10), T),
	(   T > 0
	->  Times is min(1000, round(10*(5/T)))
	;   Times = 1000
	),
	queue_create(Times).

queue_create(Times) :-
	forall(between(1, Times, _), test).

elapsed_time(G, T) :-
	get_time(T0),
	G,
	get_time(T1),
	T is T1 - T0.

test :-
	thread_self(Me),
	thread_create(client, Id, []),
	catch(thread_send_message(Id, hello(Me)), E, true),
	(   var(E)
	->  thread_get_message(ok),
	    thread_join(Id, true)
	;   message_to_string(E, Msg),
	    format(user_error,
		   'Failed to send message to client: ~s~n', [Msg]),
	    thread_join(Id, Status),
	    format(user_error, 'Client terminated with ~q~n', [Status]),
	    fail
	).

client :-
	thread_get_message(hello(From)),
	thread_send_message(From, ok).
