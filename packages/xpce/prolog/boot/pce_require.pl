/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

:- module(require,
	  [ require/1,
	    make_library_index/1
	  ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This file defines the  Prolog  predicate   require/1  which  allows  for
portable loading of libraries on  systems   with  a  Quintus-like module
system.  require/1 are part of both  SWI-Prolog and SICStus Prolog.  For
other Prologs this module should be ported to the Prolog system and made
part of the module `pce', so any PCE/Prolog program can start with:

:- module(foobar,
	  [ foobar/1,
	    ...
	  ]).
:- use_module(library(pce)).
:- require([ member/1
	   , forall/2
	   , send_list/3
	   ]).

without having to worry about the available system predicates, autoload
libraries, library structure, etc.

This software was originally written for the SWI-Prolog autoloader.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/*				insert proper use_module's here
				or rewrite as necessary
:- require([ access_file/2
	   , chdir/1
	   , checklist/2
	   , concat_atom/2
	   , delete/3
	   , exists_file/1
	   , expand_file_name/2
	   , forall/2
	   , member/2
	   , strip_module/3
	   ]).
*/

:- meta_predicate
	require(:).

require(Spec) :-
	strip_module(Spec, Module, Predicates),
	require(Module, Predicates).


require(_, []) :- !.
require(Module, [H|T]) :- !,
	require(Module, H),
	require(Module, T).
require(Module, Name/Arity) :-		% already defined
	functor(Head, Name, Arity),
	current_predicate(Name, Module:Head), !.
require(Module, Name/Arity) :-		% load from library
	load_library_index,
	functor(Head, Name, Arity),
	library_index(Head, _, File),
	Module:use_module(File, [Name/Arity]).


		/********************************
		*           LOAD INDEX		*
		********************************/

:- dynamic
	library_index/3.			% Head x Module x Path

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
	format(user_error,
	       '[WARNING: Illegal term in INDEX.pl of directory ~w: ~w]~n',
	       [Dir, Term]).
	

		/********************************
		*       CREATE INDEX.pl		*
		********************************/

make_library_index(Dir) :-
	access_file(Dir, write), !,
	library_index(Dir, 'INDEX.pl').
make_library_index(Dir) :-
	format(user_error,
	       '[WARNING: make_library_index/1: Cannot write ~w]~n', [Dir]),
	fail.

%	Note: if your Prolog doesn't like expand_file_name('*.pl', Files,
%
%		...
%		get(directory('.'), files, '.*\.pl$', Chain),
%		send(Chain, delete, Index),
%		send(Chain, for_all,
%		     message(@prolog, index_file, Fd, @arg1)),
%		...
%
%	should provide you with an alternative that runs with XPCE.

library_index(Dir, Index) :-
	absolute_file_name('', OldDir),
	chdir(Dir),
	open(Index, write, Fd),
	index_header(Fd),
	expand_file_name('*.pl', Files),
	delete(Files, Index, PrologFiles),
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
	format(Fd, '/*  $Id', []),
	format(': $~n~n', []),
	format(Fd, '    Creator: make_library_index/1~n~n', []),
	format(Fd, '    Purpose: Provide index for require/1~n', []),
	format(Fd, '*/~n~n', []).

	
