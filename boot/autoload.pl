/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    jan@swi.psy.uva.nl

    Purpose: Automatic library loading
*/

:- module($autoload,
	[ $find_library/5
	, $in_library/2
	, $update_library_index/0
	, make_library_index/1
	]).

:- dynamic
	library_index/3.			% Head x Module x Path

%	$find_library(+Module, +Name, +Arity, -LoadModule, -Library)
%
%	Locate a predicate in the library.  Name and arity are the name
%	and arity of the predicate searched for.  `Module' is the
%	preferred target module.  The return values are the full path names
%	of the library and module declared in that file.

$find_library(Module, Name, Arity, LoadModule, Library) :-
	load_library_index,
	functor(Head, Name, Arity),
	(   library_index(Head, Module, Library),
	    LoadModule = Module
	;   library_index(Head, LoadModule, Library)
	), !.

%	$in_library(?Name, ?Arity)
%	Is true if Name/Arity is in the autoload libraries.

$in_library(Name, Arity) :-
	load_library_index,
	library_index(Head, _, _),
	functor(Head, Name, Arity).

		/********************************
		*          UPDATE INDEX		*
		********************************/

$update_library_index :-
	absolute_file_name('', CWD),
	user:library_directory(Dir),
	    update_library_index(CWD, 'INDEX.pl', Dir),
	fail.
$update_library_index.

update_library_index(CWD, Index, Dir) :-
	exists_directory(Dir),
	concat_atom([Dir, /, Index], IndexFile),
	access_file(IndexFile, write),
	chdir(Dir),
	time_file(Index, IndexTime),
	expand_file_name('*.pl', Files),
	(   member(X, ['.'|Files]),
	    X \== Index,
	    time_file(X, TX),
	    TX @> IndexTime
	->  chdir(CWD),
	    format(user, 'Rebuilding index for library ~w ... ', Dir),
	    ttyflush,
	    library_index(Dir, Index),
	    format(user, 'ok.~n', []),
	    clear_library_index
	;   chdir(CWD)
	).

clear_library_index :-
	retractall(library_index(_, _, _)).

		/********************************
		*           LOAD INDEX		*
		********************************/

load_library_index :-
	library_index(_, _, _), !.		% loaded
load_library_index :-
	user:library_directory(Dir),
	    concat_atom([Dir, '/', 'INDEX.pl'], Index),
	    exists_file(Index),
	    read_index(Index, Dir),
	fail.
load_library_index.
	
read_index(Index, Dir) :-
	seeing(Old), see(Index),
	repeat,
	    read(Term),
	    (   Term == end_of_file
	    ->  !
	    ;   assert_index(Term, Dir),
	        fail
	    ),
	seen, see(Old).

assert_index(index(Name, Arity, Module, File), Dir) :- !,
	functor(Head, Name, Arity),
	concat_atom([Dir, '/', File], Path),
	assertz(library_index(Head, Module, Path)).
assert_index(Term, Dir) :-
	$warning('Illegal term in INDEX.pl of directory ~w: ~w', [Dir, Term]).
	

		/********************************
		*       CREATE INDEX.pl		*
		********************************/

make_library_index(Dir) :-
	access_file(Dir, write), !,
	library_index(Dir, 'INDEX.pl').
make_library_index(Dir) :-
	$warning('make_library_index/1: Cannot write ~w', [Dir]).

library_index(Dir, Index) :-
	absolute_file_name('', OldDir),
	chdir(Dir),
	expand_file_name('*.pl', Files),
	delete(Files, Index, PrologFiles),
	open(Index, write, Fd),
	index_header(Fd),
	checklist(index_file(Fd), PrologFiles),
	close(Fd),
	chdir(OldDir).

index_file(Fd, File) :-
	open(File, read, In),
	read(In, Term),
	close(In),
	Term = (:- module(Module, Public)), !,
	forall( member(Name/Arity, Public),
		format(Fd, 'index((~k), ~k, ~k, ~k).~n',
		       [Name, Arity, Module, File])).
index_file(_, _).

index_header(Fd):-
	format(Fd, '/*  $Id$~n~n', []),
	format(Fd, '    Creator: make/0~n~n', []),
	format(Fd, '    Purpose: Provide index for autoload~n', []),
	format(Fd, '*/~n~n', []).
