/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2007-2009, University of Amsterdam
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

:- module(queue_max_size,
	  [ queue_max_size/0
	  ]).
:- use_module(library(debug)).

queue_max_size :-
	queue_max_size(10000, 10).

queue_max_size(Max, MaxSize) :-
	thread_self(Me),
	message_queue_create(Q, [max_size(MaxSize)]),
	thread_create(reader(Q, Me), Reader, []),
	forall(between(0, Max, X), send(Q, X, MaxSize)),
	thread_send_message(Q, end_of_data),
	thread_get_message(ok(List)),
	numlist(0, Max, List),
	thread_join(Reader, true),
	message_queue_destroy(Q).

send(Q, Term, MaxSize) :-
	message_queue_property(Q, size(M)),
	assertion(M =< MaxSize),
	thread_send_message(Q, Term).

reader(Q, Sender) :-
	thread_get_message(Q, T0),
	collect(T0, Q, List),
	thread_send_message(Sender, ok(List)).

collect(end_of_data, _, []).
collect(T, Q, [T|L]) :-
	thread_get_message(Q, T1),
	collect(T1, Q, L).
