:- module(agc,
	  [ agc/0
	  ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This test validates the marking of atoms by the atom garbage collector.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- dynamic
	a/1.

agc_loop :-
	repeat,
	garbage_collect_atoms,
	fail.

agc :-
	thread_create(agc_loop, Id, []),
	forall(between(0, 50000, X),
	       (   atom_concat(xxxx, X, A),
		   assert(a(A))
	       )),
	thread_signal(Id, abort),
	thread_join(Id, _),
	retractall(a(_)).
