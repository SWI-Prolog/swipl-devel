/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2026, SWI-Prolog Solutions b.v.
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

:- module(queue_wakeup,
	  [ queue_wakeup/0
	  ]).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(apply)).

/** <module> Woken goals when receiving a message

Unifying a message may wake goals, e.g., from freeze/2.  These goals
run before the message is removed and may use the queue.  They used to
run while holding the lock of the queue.
*/

queue_wakeup :-
	send_from_woken_goal,
	failing_woken_goal,
	concurrent.

%	A goal woken by receiving a message sends to the same queue.
%	This used to deadlock.

send_from_woken_goal :-
	message_queue_create(Q),
	thread_send_message(Q, msg(1)),
	freeze(X, thread_send_message(Q, got(X))),
	thread_get_message(Q, msg(X)),
	thread_get_message(Q, M, [timeout(5)]),
	assertion(M == got(1)),
	message_queue_destroy(Q).

%	If the woken goal fails, the message stays at its position.

failing_woken_goal :-
	message_queue_create(Q),
	forall(member(I, [1,7,3]), thread_send_message(Q, msg(I))),
	freeze(X, X > 5),
	thread_get_message(Q, msg(X)),
	assertion(X == 7),
	drain(Q, Rest),
	assertion(Rest == [msg(1), msg(3)]),
	message_queue_destroy(Q).

%	Multiple readers whose woken goals send to the queue while
%	writers add messages.

concurrent :-
	message_queue_create(Q),
	findall(T, ( between(1, 3, _),
		     thread_create(reader(Q, 500), T, [])
		   ), Readers),
	findall(T, ( between(1, 3, _),
		     thread_create(writer(Q, 500), T, [])
		   ), Writers),
	append(Readers, Writers, Threads),
	maplist(join, Threads),
	drain(Q, Echos),
	length(Echos, Len),
	assertion(Len == 1500),
	message_queue_destroy(Q).

reader(Q, N) :-
	forall(between(1, N, _),
	       ( freeze(X, thread_send_message(Q, echo(X))),
		 thread_get_message(Q, msg(X))
	       )).

writer(Q, N) :-
	forall(between(1, N, I), thread_send_message(Q, msg(I))).

join(T) :-
	thread_join(T, Status),
	assertion(Status == true).

drain(Q, [H|T]) :-
	thread_get_message(Q, H, [timeout(0)]),
	!,
	drain(Q, T).
drain(_, []).
