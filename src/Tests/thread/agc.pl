:- module(agc,
	  [ agc/0,
	    agc/1
	  ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This test validates the marking of atoms by the atom garbage collector.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- dynamic
	a/1.

agc_loop :-
	repeat,
	sleep(0.01),			% avoid eating CPU here
	garbage_collect_atoms,
	fail.


agc :-
	elapsed_time(agc(100), T),
	(   T > 0
	->  Times is min(50000, round(100*(5/T)))
	;   Times = 50000
	),
	agc(Times).


agc(Times) :-
	thread_create(agc_loop, Id, []),
	forall(between(1, Times, X),
	       (   atom_concat(xxxx, X, A),
		   assert(a(A))
	       )),
	thread_signal(Id, abort),
	thread_join(Id, _),
	retractall(a(_)).


elapsed_time(G, T) :-
	get_time(T0),
	G,
	get_time(T1),
	T is T1 - T0.
