/*  $Id$

    Copyright (c) 1991 Jan Wielemaker. All rights reserved.
    jan@swi.psy.uva.nl

    Purpose: Foreign interface for AIX version
*/

:- module($aix_foreign,
	[ load_foreign/5
	, load_foreign/2
	, load_foreign/1
	, foreign_file/1
	]).

:- module_transparent
	load_foreign/2,
	load_foreign/1.

%	load_foreign/5
%	Not available on AIX.  Just print an error

load_foreign(_, _, _, _, _) :-
	$warning('load_foreign/5: not on AIX; see load_foreign/[1,2]'),
	fail.


%	load_foreign(+Executable)
%	Load a foreign file and call its entry point.

load_foreign(File) :-
	statistics(heapused, OldHeap),
	statistics(cputime, OldTime),

	(   feature(arch, Arch),
	    $chk_file(File, Path, [Arch, ''], [''])
	->  true
	;   $warning('~w: No such foreign file', [File]),
	    fail
	),	
	$load_foreign(Path),
	record_foreigns(Path),

	statistics(heapused, Heap),
	statistics(cputime, Time),
	HeapUsed is Heap - OldHeap,
	TimeUsed is Time - OldTime,

	confirm_files(File, CFs),
	list_to_atom(CFs, CF),
	context_module(M),
	module_spec(M, ModSpec),
	$ttyformat('Foreign file `~w'' loaded~w, ~2f seconds, ~D bytes~n',
		   [CF, ModSpec, TimeUsed, HeapUsed]).


%	load_foreign(+Files, +Entry)

load_foreign(Files, Entry) :-
	statistics(heapused, OldHeap),
	statistics(cputime, OldTime),

	check_files(Files, Paths),
	list_to_atom(Paths, Atom),
	confirm_files(Files, CFs),
	list_to_atom(CFs, CF),
	tmp_file(ld, Object),
	import_file(Imports),
	concat_atom(['cc -bI:', Imports,
		     ' -e ', Entry,
		     ' -o ', Object,
		     ' ', Atom], Cmd),
	concat_atom(['cc -bI:', Imports,
		     ' -e ', Entry,
		     ' -o ', Object,
		     ' ', CF], UCmd),
	format(user_output, '~w~n', [UCmd]),
	(    shell(Cmd)
	->   $load_foreign(Object),
	     delete_file(Object)
	;    $warning('load_foreign/2: failed ~w', Cmd),
	     fail
	),
	record_foreigns(Paths),

	statistics(heapused, Heap),
	statistics(cputime, Time),
	HeapUsed is Heap - OldHeap,
	TimeUsed is Time - OldTime,

	context_module(M),
	module_spec(M, ModSpec),
	$ttyformat('Foreign file(s) ~w loaded~w, ~2f seconds, ~D bytes~n',
		   [CF, ModSpec, TimeUsed, HeapUsed]).
	

%	import_file(-File)
%	Return path-name to file with import declarations

import_file(File) :-
	feature(home, Home),
	concat(Home, '/include/SWI-Exports', File).

%	foreign_file(?File)
%	Is true if 'File' is loaded as a foreign file.

foreign_file(File) :-
	recorded($foreign_file, File).

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
	feature(arch, Arch),
	$chk_file(F, A, [Arch, ''], ['.o', '.a', '.c', '']), !.
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
