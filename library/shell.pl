/*  File:    shell.pl
    Purpose: Limited Unix Shell Emulation
    Author:  Jan Wielemaker
    Date:    Sep 16 1989
*/

:- module(shell,
	[ (ls)/0
	, (ls)/1
	, (cd)/0
	, (cd)/1
	, (pushd)/0
	, (pushd)/1
	, dirs/0
	, pwd/0
	, popd/0
	, mv/2
	, (rm)/1
	]).

% :- op(900, fy, [ls, cd, pushd, rm, grep]).

/*  Shell Emulation Library

    This library is meant for systems that do not allow us to get access
    to the operating system via shell/[0,1,2].  It is developed  on  the
    ST-MINIX version.  MINIX does not have a vfork() call, and thus only
    allows  shell/[0,1,2]  if  Prolog  uses less than half the amount of
    available memory.  This library offers a number  of  predicates  for
    listing, directory management, deleting, copying and renaming files.

 ** Sun Sep 17 12:04:54 1989  jan@swi.psy.uva.nl */

%	cd
%	cd(Dir)
%	Change working directory

(cd) :-
	cd(~).

cd(Dir) :-
	name_to_atom(Dir, Name),
	chdir(Name).

%	dirs	-- Print Directory Stack
%	pushd	-- Push Directory Stack
%	popd	-- Pop Directory Stack

:- dynamic
	stack/1.

(pushd) :-
	pushd(+1).

pushd(N) :-
	integer(N), !,
	findall(D, stack(D), Ds),
	(   nth1(N, Ds, Go),
	    retract(stack(Go))
	->  pushd(Go)
	;   warning('Directory stack not that deep', []),
	    fail
	).
pushd(Dir) :-
	name_to_atom(Dir, Name),
	absolute_file_name('', Old),
	chdir(Name),
	asserta(stack(Old)).

popd :-
	retract(stack(Dir)), !,
	chdir(Dir).
popd :-
	warning('Directory stack empty', []),
	fail.

dirs :-
	(   absolute_file_name('', D)
	;   stack(D)
	),
	dir_name(D, Name),
	format('~w ', [Name]),
	fail.
dirs :-
	nl.

pwd :-
	absolute_file_name('', D),
	dir_name(D, Name),
	format('~w~n', [Name]).

dir_name('/', '/') :- !.
dir_name(Path, Name) :-
	atom_concat(P, /, Path), !,
	dir_name(P, Name).
dir_name(Path, Name) :-
	current_prolog_flag(unix, true),
	absolute_file_name('~', Home0),
	(   atom_concat(Home, /, Home0)
	->  true
	;   Home = Home0
	),
	atom_concat(Home, FromHome, Path), !,
	sformat(Name, '~~~w', [FromHome]).
dir_name(Path, Path).

%	ls
%	ls(Dir|Files)
%	List a directory, flag directories with a '/'

(ls) :-
	ls('.').

ls(Spec) :-
	name_to_atom(Spec, Atom),
	expand_file_name(Atom, Matches),
	ls_(Matches).

ls_([Dir]) :-
	exists_directory(Dir), !,
	absolute_file_name('', Here),
	chdir(Dir),
	expand_file_name('*', Files),
	ls__(Files),
	chdir(Here).
ls_(Files) :-
	ls__(Files).

ls__([]) :- !,
	warning('No Match', []),
	fail.
ls__(Files) :-
	maplist(tag_file, Files, Tagged),
	list_atoms(Tagged, 72).

tag_file(File, Dir) :-
	exists_directory(File),	
	atom_concat(File, /, Dir).
tag_file(File, File).

%	mv(+From, +To)	--- Move (Rename) a file
%	rm(+File)	--- Remove (unlink) a file

mv(From, To) :-
	name_to_atom(From, A0),
	name_to_atom(To, A1),
	rename_file(A0, A1).

rm(File) :-
	name_to_atom(File, A),
	delete_file(A).

%	name_to_atom(Typed, Atom)
%	Convert a typed name into an atom

name_to_atom(Atom, Atom) :-
	atomic(Atom), !.
name_to_atom(Term, Atom) :-
	term_to_atom(Term, Raw),
	name(Raw, S0),
	sublist(non_blank, S0, S1),
	name(Atom, S1).

non_blank(C) :-
	between(0, 32, C), !,
	fail.
non_blank(_).


%	list_atoms(+List, +Width)
%	List a set of atoms multicolumn on a Width wide output device.

list_atoms(List, W) :-
	length(List, L),
	Term =.. [l|List],
	longest(List, Longest),
	Columns is W // (Longest + 3),
	Rows is integer(L / Columns + 0.49999),	% should be ceil/1
	ColumnWidth is W // Columns,
	Max is Columns * Rows - 1,
	between(0, Max, N),
	    Index is N // Columns + (N mod Columns) * Rows + 1,
	    (	(N+1) mod Columns =:= 0
	    ->	NL = nl
	    ;	NL = fail
	    ),
	    (	arg(Index, Term, Atom),
		atom_length(Atom, AL),
		write(Atom),
		(   NL == fail
		->  tab(ColumnWidth - AL)
		;   true
		)
	    ->  true
	    ;   true
	    ),
	    NL,
	fail.
list_atoms(_, _).

longest(List, Longest) :-
	longest(List, 0, Longest).

longest([], M, M) :- !.
longest([H|T], Sofar, M) :-
	atom_length(H, L),
	L >= Sofar, !,
	longest(T, L, M).
longest([_|T], S, M) :-
	longest(T, S, M).

%	warning(Fmt, [Args]).

warning(Fmt, Args) :-
	print_message(warning, format(Fmt, Args)).
