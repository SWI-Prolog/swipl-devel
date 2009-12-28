:- module(qpattern,
	  [ qpattern/0,
	    qpattern/1
	  ]).
:- use_module(library(debug)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This tests tries to send messages to   a  queue of multiple readers with
readers joining and leaving. Some of   them  listen to specific messages
and the joining ones to arbitrary   messages. This tests the cooperation
of signalling and broadcasting the queue condition variable.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- dynamic
	seen/1,
	tid/1.

qpattern :-
	qpattern(100).

qpattern(N) :-
	message_queue_create(Queue),
	thread_create(do(Queue, a(_)), T1, []),
	thread_create(do(Queue, b(_)), T2, []),
	thread_create(do(Queue, c(_)), T3, []),
	(   between(1, N, X),
	    (	X mod 5 =:= 0,
		catch(thread_create(do1(Queue), Id, []),
		      _, fail)
	    ->	assert(tid(Id))
	    ;	true
	    ),
	    A is random(3),
	    g(A, X, T),
	    thread_send_message(Queue, T),
	    fail
	;   true
	),
	forall(tid(_),
	       thread_send_message(Queue, go_away)),
	forall(retract(tid(TID)), join(TID)),
	thread_send_message(Queue, a(done)),
	thread_send_message(Queue, b(done)),
	thread_send_message(Queue, c(done)),
	join(T1),
	join(T2),
	join(T3),
	message_queue_destroy(Queue),
	setof(X, retract(seen(X)), Xs),
	numlist(1, N, L),
	Xs == L.

join(TID) :-
	thread_join(TID, Status),
	assertion(Status == true).

g(0, X, a(X)).
g(1, X, b(X)).
g(2, X, c(X)).

do(Queue, T) :-
	repeat,
	thread_get_message(Queue, T),
	arg(1, T, N),
	(   N == done
	->  true
	;   assert(seen(N)),
	    fail
	).

do1(Queue) :-
	thread_get_message(Queue, T),
	(   T == go_away
	->  true
	;   arg(1, T, N),
%	    format('~w~n', [N]),
	    assert(seen(N))
	).
