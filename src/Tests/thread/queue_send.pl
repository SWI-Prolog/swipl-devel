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

