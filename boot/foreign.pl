/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    jan@swi.psy.uva.nl

    Purpose: Foreign language loader front end
*/

:- module($foreign,
	[ load_foreign/5
	, load_foreign/2
	, foreign_file/1
	]).

:- module_transparent
	load_foreign/5,
	load_foreign_/5,
	load_foreign/2.

%	foreign_file(?File)
%	Is true if 'File' is loaded as a foreign file.

foreign_file(File) :-
	recorded($foreign_file, File).

%	load_foreign(+File, +Entry)
%	Load foreign file and initialise with specified entry.

load_foreign(File, Entry) :-
	load_foreign(File, Entry, '', '', 0).

%	load_foreign(+File, +Entry, +Options, +Libraries, +Size)
%	Load a (list of) .o file(s) as produced with  'cc  -c  ...'  and
%	install the foreign predicates defined in them. 

load_foreign(Files, Entry, Options, Libraries, Size) :-
	statistics(heapused, OldHeap),
	statistics(cputime, OldTime),

	(   load_foreign_(Files, Entry, Options, Libraries, Size)
	;   true
	), !,

	statistics(heapused, Heap),
	statistics(cputime, Time),
	HeapUsed is Heap - OldHeap,
	TimeUsed is Time - OldTime,

	confirm_files(Files, CFs),
	list_to_atom(CFs, CF),
	context_module(M),
	module_spec(M, ModSpec),
	$ttyformat('Foreign file(s) ~w loaded~w, ~2f seconds, ~D bytes~n',
					[CF, ModSpec, TimeUsed, HeapUsed]).
	
module_spec(user, '') :- !.
module_spec(M, S) :-
	sformat(S, ' into ~w', [M]).

confirm_files([], []) :- !.
confirm_files([H|T], [CH|CT]) :- !,
	confirm_file(H, CH),
	confirm_files(T, CT).
confirm_files(F, CF) :-
	confirm_file(F, CF).

confirm_file(library(File), Confirm) :- !,
	check_files(library(File), Confirm).
confirm_file(File, File).

load_foreign_(Files, Entry, Options, Libraries, Size) :-
	check_files(Files, Expanded),
	list_to_atom(Expanded, F),	
	list_to_atom(Options, O),
	list_to_atom(Libraries, L),

	$load_foreign(F, Entry, O, L, Size),
	record_foreigns(Expanded).

check_files(0, _) :- !, fail.		/* variable file name */
check_files([], []) :- !.
check_files([F|R], [A|T]) :- !,
	check_files(F, A),
	check_files(R, T).
check_files(F, A) :-
	$chk_file(F, ['.o', '.a', ''], A), !.
check_files(F, _) :-
	$warning('~w: No such foreign file', [F]),
	fail.

list_to_atom(Atom, Atom) :-
	atomic(Atom), !.	
list_to_atom(List, Atom) :-
	insert_spaces(List, SPList),	
	concat_atom(SPList, Atom).

insert_spaces([One], [One]) :- !.
insert_spaces([H|T], [H, ' '| R]) :-
	insert_spaces(T, R).

record_foreigns([]) :- !.
record_foreigns([H|T]) :- !,
	record_foreigns(H),
	record_foreigns(T).
record_foreigns(F) :-
	absolute_file_name(F, Path),
	(   recorded($foreign_file, Path)
	;   recordz($foreign_file, F)
	), !.
