/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2002-2009, University of Amsterdam
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

:- module(pooltest,
	  [ pooltest/0
	  ]).

pooltest :-
	PoolSize = 5,
	thread_self(Me),
	create_pool(PoolSize, Threads),
	forall(between(0, 100, X),
	       thread_send_message(queue,
				   thread_send_message(Me, X))),
	forall(between(0, 100, X),
	       thread_get_message(X)),
	forall(between(1, PoolSize, X),
	       thread_send_message(queue,
				   thread_exit(done))),
	join_threads(Threads),
	message_queue_destroy(queue).


join_threads([]).
join_threads([H|T]) :-
	thread_join(H, exited(done)),
	join_threads(T).


create_pool(N, Threads) :-
	message_queue_create(queue),   % create the queue
	create_pool_threads(0, N, Threads).

create_pool_threads(N, N, []) :- !.
create_pool_threads(I, N, [Id|T]) :-
	thread_create(worker(queue), Id, []),
	NI is I + 1,
	create_pool_threads(NI, N, T).


worker(Queue) :-
	repeat,
	thread_get_message(Queue, Goal),
	call(Goal),
	fail.
