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
	thread_send_message(Id, hello(Me)),
	thread_get_message(ok),
	thread_join(Id, true).

client :-
	thread_get_message(hello(From)),
	thread_send_message(From, ok).
