/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2002, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
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
	working_directory(_, Name).

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
	working_directory(Old, Name),
	asserta(stack(Old)).

popd :-
	retract(stack(Dir)), !,
	working_directory(_, Dir).
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
	working_directory(Here, Dir),
	expand_file_name('*', Files),
	call_cleanup(ls__(Files), working_directory(_, Here)).
ls_(Files) :-
	ls__(Files).

ls__([]) :- !,
	warning('No Match', []),
	fail.
ls__(Files) :-
	maplist(tag_file, Files, Tagged),
	list_atoms(Tagged, 72).

tag_file(File, Dir) :-
	exists_directory(File),	!,
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
%
%	Convert a typed name into an atom

name_to_atom(Spec, File) :-
	(   atomic(Spec)
	->  S1 = Spec
	;   sformat(S1, '~w', [Spec])
	),
	expand_file_name(Spec, Expanded),
	(   Expanded = [File]
	->  true
	;   Expanded == []
	->  print_message(warning, format('No match: ~w', [Spec])),
	    fail
	;   print_message(warning, format('Ambiguous: ~w', [Spec])),
	    fail
	).


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
