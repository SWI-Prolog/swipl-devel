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

queue_race :-
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
	thread_get_message(WorkerMessageQueue, worker_ready(_)),
	M is N + 1,
	workers_ready(M, NumberOfWorkers, WorkerMessageQueue).


wait_workers(N, N, _) :- !.
wait_workers(N, NumberOfWorkers, WorkerMessageQueue) :-
	thread_get_message(WorkerMessageQueue, worker_done(Id)),
	thread_join(Id, X),
	X == true,
	M is N + 1,
	wait_workers(M, NumberOfWorkers, WorkerMessageQueue).


worker(WorkerMessageQueue) :-
	thread_self(WorkerThreadId),
	thread_send_message(WorkerMessageQueue, worker_ready(WorkerThreadId)),
	repeat,
	thread_get_message(WorkerMessageQueue, worker_task(WorkerTask)),
	(   WorkerTask == done
	->  thread_send_message(WorkerMessageQueue, worker_done(WorkerThreadId))
	;   fail
	).
