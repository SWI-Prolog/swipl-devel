:- module(thread_agc_findall,
	  [ thread_agc_findall/0,
	    thread_agc_findall/2	% +Threads, +Count
	  ]).
:- use_module(library(debug)).

%%	thread_agc_findall
%
%	Test interaction of findall  without   locking  atoms in records
%	with AGC. Like thread_agc_queue, we create atoms in a thread and
%	verify we do not loose anything.

thread_agc_findall :-
	thread_agc_findall(4, 10000).

thread_agc_findall(Threads, Count) :-
	current_prolog_flag(agc_margin, Old),
	set_prolog_flag(agc_margin, 1000),
	call_cleanup(test(Threads, Count),
		     set_prolog_flag(agc_margin, Old)).

test(Threads, Count) :-
	numlist(1, Threads, Is),
	maplist(create_test(Count), Is, Ids),
	maplist(thread_join, Ids, States),
	maplist(==(true), States).

create_test(Count, I, Id) :-
	prefix(I, Prefix),
	thread_create(test_find(Prefix, Count), Id, []).

prefix(N, Prefix) :-
	A is 0'A+N,
	atom_codes(Prefix, [A,A,A]).

test_find(Prefix, N) :-
	findall(Atom, gen_atom(N, Prefix, Atom), Atoms),
	check_atoms(Atoms, 1, N, Prefix).

gen_atom(High, Prefix, Atom) :-
	between(1, High, N),
	atom_concat(Prefix, N, Atom).

check_atoms([], I, N, _Prefix) :-
	assertion(I=:=N+1).
check_atoms([H|T], I, N, Prefix) :-
	atom_concat(Prefix, Rest, H),
	atom_number(Rest, Num),
	assertion(I == Num),
	I2 is I + 1,
	check_atoms(T, I2, N, Prefix).
