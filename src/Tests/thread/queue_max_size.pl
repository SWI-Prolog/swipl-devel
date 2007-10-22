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
