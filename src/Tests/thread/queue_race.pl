/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2003-2006, University of Amsterdam
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

:- module(queue_race,
	  [ queue_race/0,
	    queue_race/1
	  ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This test verifies multiple readers and writers  on the same queue. This
isn't a particulary nice design (it  is   better  to distinct queues for
instructing the workers and feedback to the   main thread), but it has a
high potential for race-conditions and therefore makes a good test. This
test is a generalisation of a test by Mike Elston.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- dynamic
	verbose/0.

verbose.

queue_race :-
	retractall(verbose),
	queue_race(10).			% concurrency level

queue_race(N) :-
	message_queue_create(WorkerMessageQueue),
	forall(between(1, N, I),
	       (   atom_concat(worker, I, Alias),
		   thread_create(worker(WorkerMessageQueue), _,
				 [ alias(Alias)
				 ])
	       )),
	workers_ready(0, N, WorkerMessageQueue),
	forall(between(1, N, I),
	       (   thread_send_message(WorkerMessageQueue, worker_task(done)),
		   thread_send_message(WorkerMessageQueue, worker_task(done))
	       )),
	wait_workers(0, N, WorkerMessageQueue),
	message_queue_destroy(WorkerMessageQueue).


workers_ready(N, N, _) :- !.
workers_ready(N,
	      NumberOfWorkers,
	      WorkerMessageQueue) :-
	thread_get_message(WorkerMessageQueue, worker_ready(Id)),
	verbose(ready(Id)),
	M is N + 1,
	workers_ready(M, NumberOfWorkers, WorkerMessageQueue).


wait_workers(N, N, _) :- !.
wait_workers(N, NumberOfWorkers, WorkerMessageQueue) :-
	thread_get_message(WorkerMessageQueue, worker_done(Id)),
	verbose(done(Id)),
	thread_join(Id, X),
	X == true,
	M is N + 1,
	wait_workers(M, NumberOfWorkers, WorkerMessageQueue).


worker(WorkerMessageQueue) :-
	sleep(0.1),
	thread_self(WorkerThreadId),
	verbose(send(ready(WorkerThreadId))),
	thread_send_message(WorkerMessageQueue, worker_ready(WorkerThreadId)),
	repeat,
	thread_get_message(WorkerMessageQueue, worker_task(WorkerTask)),
	(   WorkerTask == done
	->  verbose(send(done(WorkerThreadId))),
	    thread_send_message(WorkerMessageQueue,
				worker_done(WorkerThreadId))
	;   fail
	).

verbose(X) :-
	(   verbose
	->  writeln(X)
	;   true
	).
