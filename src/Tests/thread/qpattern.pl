:- module(qpattern,
	  [ qpattern/0,
	    qpattern/1
	  ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This tests tries to send messages to   a  queue of multiple readers with
readers joining and leaving. Some of   them  listen to specific messages
and the joining ones to arbitrary   messages. This tests the cooperation
of signalling and broadcasting the queue condition variable.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- dynamic
	seen/1.

qpattern :-
	qpattern(100).

qpattern(N) :-
	message_queue_create(Queue),
	thread_create(do(Queue, a(_)), T1, []),
	thread_create(do(Queue, b(_)), T2, []),
	thread_create(do(Queue, c(_)), T3, []),
	(   between(1, N, X),
	    (	X mod 5 =:= 0
	    ->	catch(thread_create(do1(Queue), _, [detached(true)]),
		      _, true)
	    ;	true
	    ),
	    A is random(3),
	    g(A, X, T),
	    thread_send_message(Queue, T),
	    fail
	;   true
	),
	(   repeat,
	    findall(Th, current_thread(Th, _), Ths),
	    (	length(Ths, 4)
	    ->	true
	    ;	thread_send_message(Queue, go_away),
	        sleep(0.1),
		fail
	    )
	->  true
	),
	thread_send_message(Queue, a(done)),
	thread_send_message(Queue, b(done)),
	thread_send_message(Queue, c(done)),
	thread_join(T1, true),
	thread_join(T2, true),
	thread_join(T3, true),
	setof(X, retract(seen(X)), Xs),
	numlist(1, N, L),
	Xs == L.
	

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
