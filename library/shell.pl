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
	, (p)/0
	, (p)/1
	, d/0
	, pd/0
	, mv/2
	, (rm)/1
	, (grep)/1
	, (grep)/2
	, (tg)/1
	]).

:- op(900, fy, [ls, cd, p, rm, grep, tg]).

/*  Shell Emulation Library

    This library is meant for systems that do not allow us to get access
    to the operating system via shell/[0,1,2].  It is developed  on  the
    ST-MINIX version.  MINIX does not have a vfork() call, and thus only
    allows  shell/[0,1,2]  if  Prolog  uses less than half the amount of
    available memory.  This library offers a number  of  predicates  for
    listing, directory management, deleting, copying and renaming files.
    It should be combined with the linked-in version of Richard O'Keefes
    `thief' editor (via the $thief/1 predicate).

 ** Sun Sep 17 12:04:54 1989  jan@swi.psy.uva.nl */

%	cd
%	cd(Dir)
%	Change working directory

(cd) :-
	cd(~).

cd(Dir) :-
	name_to_atom(Dir, Name),
	chdir(Name).

%	d	-- Print Directory Stack
%	p	-- Push Directory Stack
%	pd	-- Pop Directory Stack

:- dynamic
	stack/1.

(p) :-
	p(+1).

p(N) :-
	integer(N), !,
	findall(D, stack(D), Ds),
	(   nth1(N, Ds, Go),
	    retract(stack(Go))
	->  p(Go)
	;   warning('Directory stack not that deep'),
	    fail
	).
p(Dir) :-
	name_to_atom(Dir, Name),
	absolute_file_name('', Old),
	chdir(Name),
	asserta(stack(Old)).

pd :-
	retract(stack(Dir)), !,
	chdir(Dir).
pd :-
	warning('Directory stack empty'),
	fail.

d :-
	(   absolute_file_name('', D)
	;   stack(D)
	),
	dir_name(D, Name),
	format('~w ', [Name]),
	fail.
d :-
	nl.

dir_name('/', '/') :- !.
dir_name(Path, Name) :-
	concat(P, /, Path), !,
	dir_name(P, Name).
dir_name(Path, Name) :-
	absolute_file_name('~', Home),
	concat(Home, FromHome, Path), !,
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
	(   Dir == '.'
	->  expand_file_name('*', Files)
	;   concat(Dir, '/*', Spec),
	    expand_file_name(Spec, Files)
	),
	ls__(Files).
ls_(Files) :-
	ls__(Files).

ls__([]) :- !,
	warning('No Match'),
	fail.
ls__(Files) :-
	maplist(tag_file, Files, Tagged),
	list_atoms(Tagged, 78).

tag_file(File, Dir) :-
	exists_directory(File),	
	concat(File, /, Dir).
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

%	grep(String)		--- grep through all source files
%	grep(File, String)	--- grep through specified files

grep(S) :-
	flag(grep_, _, 0),
	source_file(File),
	    File \== user,
	    grep_(File, S),
	fail.
grep(_) :-
	flag(grep_, 1, 1).

grep(File, S) :-
	flag(grep_, _, 0),
	name_to_atom(File, F),
	expand_file_name(F, Files),
	member(F2, Files),
	    grep_(F2, S),
	fail.
grep(_, _) :-
	flag(grep_, 1, 1).

grep_(File, S) :-
	'$file_base_name'(File, Base),
	'$grep'(File, S, Line),
	    flag(grep_, _, 1),
	    format('~w: ~w~n', [Base, Line]),
	fail.
grep_(_, _) :-
	flag(grep_, 1, 1).

tg(String) :-
	source_file(File),
	    File \== user,
	    (   '$grep'(File, String, _)
	    ->  '$confirm'('Edit ~w', [File]),
		concat('-', String, Search),
		\+ '$thief'(['-f', File, Search])	% succeed on ^C
	    ).
tg(_) :-
	make.

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
	    (	arg(Index, Term, Atom),
		atom_length(Atom, AL),
		write(Atom), tab(ColumnWidth - AL)
	    ->  true
	    ;   true
	    ),
	    (N+1) mod Columns =:= 0,
	    nl,
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

warning(Fmt) :-
	warning(Fmt, []).

warning(Fmt, Args) :-
	'$break'('$warning'(Fmt, Args)).
