/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    jan@swi.psy.uva.nl

    Purpose: Installation dependant parts of the prolog code
*/

:- module('$parms', []).

:- multifile
	user:library_directory/1.
:- dynamic
	user:library_directory/1.

:- dynamic
	library_directory_cache/2.
:- volatile
	library_directory_cache/2.

user:library_directory(Dir) :-
	cached_library_directory(local,
				 Dir0=lib,
				 Dir0),
	Dir = Dir0.
user:library_directory(Dir) :-
	cached_library_directory(user,
				 expand_file_name('~/lib/prolog', [Dir0]),
				 Dir0),
	Dir = Dir0.
user:library_directory(Dir) :-
	cached_library_directory(system,
				 absolute_file_name(swi(library), Dir0),
				 Dir0),
	Dir = Dir0.

cached_library_directory(CacheName, _, Dir) :-
	library_directory_cache(CacheName, Dir), !,
	Dir \== [].
cached_library_directory(CacheName, Goal, Dir) :-
	catch(Goal, _, fail),
	exists_directory(Dir), !,
	asserta(library_directory_cache(CacheName, Dir)).
cached_library_directory(CacheName, _, _) :-
	asserta(library_directory_cache(CacheName, [])),
	fail.
		
$default_editor(notepad) :-
	current_prolog_flag(windows, true), !.
$default_editor(vi).
