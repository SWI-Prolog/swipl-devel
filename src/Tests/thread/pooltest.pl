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
