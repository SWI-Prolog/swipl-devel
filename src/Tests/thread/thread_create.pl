:- module(thread_create,
	  [ thread_create/0,
	    thread_create/1
	  ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This simple test verifies creation  and   destruction  of threads. There
appears to be a HUGE  difference  in   speed  between  platforms  in the
elapsed time it takes to get this test   done, so first we try to figure
out how fast it is about.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

thread_create :-
	elapsed_time(thread_create(10), T),
	(   T > 0
	->  Times is min(1000, round(10*(5/T)))
	;   Times = 1000
	),
	thread_create(Times).

thread_create(Times) :-
	forall(between(1, Times, _), test).

elapsed_time(G, T) :-
	get_time(T0),
	G,
	get_time(T1),
	T is T1 - T0.

test :-
	thread_create(client, Id, []),
	thread_join(Id, true).

client.

