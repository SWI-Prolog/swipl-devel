:- module(agc2,
	  [ agc2/0
	  ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This test does intensive atom-manipulation with   a  thread doing AGC in
the background. It was used to spot a bug in the the interaction of atom
marking and atom-reference counting.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

agc :-
	repeat,
	garbage_collect_atoms,
	fail.

agc2 :-
	thread_create(agc, Id, []),
	go(5000),
	thread_signal(Id, abort),
	thread_join(Id, _),
	garbage_collect_atoms.

go(N) :-
	make_atoms(N, xxxx, L),
	check_atoms(N, xxxx, L).


make_atoms(0, _, []) :- !.
make_atoms(N, Prefix, [H|T]) :-
	atom_concat(Prefix, N, H),
	N2 is N - 1,
	make_atoms(N2, Prefix, T).

	
check_atoms(0, _, []) :- !.
check_atoms(N, Prefix, [H|T]) :-
	atom_concat(Prefix, N, H0),
	(   H0 == H
	->  true
	;   format('~q <-> ~q~n', [H0, H]),
	    fail
	),
	N2 is N - 1,
	check_atoms(N2, Prefix, T).


