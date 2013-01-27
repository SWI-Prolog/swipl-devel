:- module(queue_timeout,
	  [ queue_timeout/0
	  ]).
:- use_module(library(debug)).
:- use_module(library(plunit)).

queue_timeout :-
	run_tests([ queue_timeout
		  ]).

:- begin_tests(queue_timeout).

test(relative,
     [ setup(message_queue_create(Q)),
       cleanup(message_queue_destroy(Q))
     ]) :-
	get_time(T0),
	\+ thread_get_message(Q, _, [timeout(0.01)]),
	get_time(T1),
	T1 > T0.
test(abs,
     [ setup(message_queue_create(Q)),
       cleanup(message_queue_destroy(Q))
     ]) :-
	get_time(T0),
	Deadline is T0+0.01,
	\+ thread_get_message(Q, _, [deadline(Deadline)]),
	get_time(T1),
	T1 >= Deadline.
test(abs_rel,
     [ setup(message_queue_create(Q)),
       cleanup(message_queue_destroy(Q))
     ]) :-
	get_time(T0),
	Deadline is T0+0.01,
	\+ thread_get_message(Q, _, [ deadline(Deadline),
				      timeout(1)
				    ]),
	get_time(T1),
	T1 >= Deadline,
	T1 < T0+1.
test(abs_rel,
     [ setup(message_queue_create(Q)),
       cleanup(message_queue_destroy(Q))
     ]) :-
	get_time(T0),
	Deadline is T0+1,
	\+ thread_get_message(Q, _, [ deadline(Deadline),
				      timeout(0.01)
				    ]),
	get_time(T1),
	T1 > T0,
	T1 < Deadline.

:- end_tests(queue_timeout).

