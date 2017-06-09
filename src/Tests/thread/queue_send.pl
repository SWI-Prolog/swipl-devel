/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2003, University of Amsterdam
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

:- module(queue_send,
	  [ queue_send/0,
	    queue_send/2
	  ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This simple test verifies sending messages   between  threads. There are
two variations: one-by-one synchronous and   asynchronous. This was done
to get some timing information. Asynchronous   is way faster, especially
on Windows (tested on Windows 2000 Advanced Server).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

queue_send :-
	elapsed_time(queue_send(sync, 10), T),
	(   T > 0
	->  Times is min(1000, round(10*(5/T)))
	;   Times = 1000
	),
	queue_send(sync, Times),
	queue_send(async, Times).

queue_send(sync, Times) :-
	thread_self(Me),
	thread_create(client, Id, []),
	(   between(1, Times, _),
	    thread_send_message(Id, hello(Me)),
	    thread_get_message(ok),
	    fail
	;   thread_send_message(Id, done(Me))
	),
	thread_join(Id, true).
queue_send(async, Times) :-
	thread_self(Me),
	thread_create(client, Id, []),
	forall(between(1, Times, _),
	       thread_send_message(Id, hello(Me))),
	forall(between(1, Times, _),
	       thread_get_message(ok)),
	thread_send_message(Id, done(Me)),
	thread_join(Id, true).

client :-
	thread_get_message(Msg),
	(   Msg = hello(From)
	->  thread_send_message(From, ok),
	    client
	;   true
	).

elapsed_time(G, T) :-
	get_time(T0),
	G,
	get_time(T1),
	T is T1 - T0.

