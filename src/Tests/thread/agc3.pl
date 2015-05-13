:- module(agc3,
	  [ agc3/0
	  ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This test validates the marking of atoms by the atom garbage collector
during thread at-exit hooks.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

agc3 :-
	thread_create(exiting_thread, Id1, []),
	thread_join(Id1, _).

exiting_thread :-
	thread_at_exit(thread_cleanup).

thread_cleanup :-
	atom_concat(foobar, 1357986420, A),
	thread_create(garbage_collect_atoms, Id, []),
	thread_join(Id, _),
	atom(A),
	atom_concat(foobar, 1357986420, A).

