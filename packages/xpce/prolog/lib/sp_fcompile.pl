/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/

:- module(sp_fcompile,
	  [ pce_fcompile/1
	  , pce_fcompile_directory/1
	  , pce_frecompile_directory/1
	  , pce_fcompile_libraries/0
	  , pce_frecompile_libraries/0
	  ]).
:- use_module(library(pce)).
:- require([ chain_list/2
	   , forall/2
	   , member/2
	   ]).


pce_fcompile(File) :-
	pce_compile:push_compile_operators,
	fcompile(File),
	pce_compile:pop_compile_operators.


pce_fcompile(Dir, File) :-
	get(string('%s/%s', Dir, File), value, Path),
	pce_fcompile(Path).


pce_frecompile(Dir, File) :-
	get(string('%s/%s', Dir, File), value, PL),
	get(PL, delete_suffix, '.pl', Base),
	get(Base, ensure_suffix, '.ql', QL),
	(   (	\+ send(file(QL), exists)
	    |   send(file(PL)?time, after, file(QL)?time)
	    )
	->  pce_fcompile(PL)
	;   true
	).


pce_fcompile_directory(Dir) :-
	get(directory(Dir), files, '^.*\.pl$', Chain),
	chain_list(Chain, Files),
	forall(member(File, Files),
	       pce_fcompile(Dir, File)).


pce_frecompile_directory(Dir) :-
	get(directory(Dir), files, '^.*\.pl$', Chain),
	chain_list(Chain, Files),
	forall(member(File, Files),
	       pce_frecompile(Dir, File)).


pce_fcompile_libraries :-
	get(@pce, home, Home),
	library_directory(X),
	send(X, prefix, Home),
	pce_fcompile_directory(X),
	fail ; true.


pce_frecompile_libraries :-
	get(@pce, home, Home),
	user:library_directory(X),
	send(X, prefix, Home),
	pce_frecompile_directory(X),
	fail ; true.

