:- module(queue_create,
	  [ queue_create/0
	  ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This simple test verifies creation and destruction of message queues
associated with threads.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

queue_create :-
	forall(between(0, 1000, _), test).

test :-
	thread_self(Me),
	thread_create(client, Id, []),
	thread_send_message(Id, hello(Me)),
	thread_get_message(ok),
	thread_join(Id, true).

client :-
	thread_get_message(hello(From)),
	thread_send_message(From, ok).
