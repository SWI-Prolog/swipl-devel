/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/

:- module(sp_fcompile,
	  [ pce_fcompile/1
	  , pce_fcompile_boot_files/0
	  , pce_fcompile_directory/1
	  , pce_frecompile_directory/1
	  , pce_fcompile_libraries/0
	  , pce_frecompile_libraries/0
	  ]).
:- use_module(library(pce)).
:- require([ chain_list/2
	   , forall/2
	   , member/2
	   , absolute_file_name/3
	   ]).

:- prolog_flag(character_escapes, _, off).

		 /*******************************
		 *        TERM_EXPANSION	*
		 *******************************/

:- multifile
	user:term_expansion/2.
:- dynamic
	user:term_expansion/2.

user:term_expansion((:- List), (:- ensure_loaded(List))) :-
	List = [_|_].
user:term_expansion((:- consult(Files)), (:- ensure_loaded(Files))).
user:term_expansion((:- push_compile_operators), _) :-
	pce_expansion:push_compile_operators,
	fail.
user:term_expansion((:- pop_compile_operators), _) :-
	pce_expansion:pop_compile_operators,
	fail.

		 /*******************************
		 *	COMPILE STATEMENTS	*
		 *******************************/


pce_fcompile(File) :-
	use_module(user:File, []),
	fcompile(user:File).


dirpath(Dir, DirPath) :-
	absolute_file_name(Dir,
			   [ file_type(directory),
			     access(read)
			   ],
			   DirPath).


pce_fcompile(Dir, File) :-
	dirpath(Dir, DirPath),
	get(string('%s/%s', DirPath, File), value, Path),
	pce_fcompile(Path).


pce_frecompile(Dir, File) :-
	dirpath(Dir, DirPath),
	get(string('%s/%s', DirPath, File), value, PL),
	get(PL, delete_suffix, '.pl', Base),
	get(Base, ensure_suffix, '.ql', QL),
	(   (	\+ send(file(QL), exists)
	    |   send(file(PL)?time, after, file(QL)?time)
	    )
	->  pce_fcompile(PL)
	;   true
	).


pce_fcompile_directory(Dir) :-
	dirpath(Dir, DirPath),
	get(directory(DirPath), files, '^.*\.pl$', Chain),
	send(Chain, delete_all, 'INDEX.pl'),
	chain_list(Chain, Files),
	forall(member(File, Files),
	       pce_fcompile(DirPath, File)).


pce_frecompile_directory(Dir) :-
	dirpath(Dir, DirPath),
	get(directory(DirPath), files, '^.*\.pl$', Chain),
	send(Chain, delete_all, 'INDEX.pl'),
	chain_list(Chain, Files),
	forall(member(File, Files),
	       pce_frecompile(DirPath, File)).


pce_fcompile_libraries :-
	forall(pce_prolog_directory(LibDir),
	       pce_fcompile_directory(LibDir)).

pce_prolog_directory(pce(library)).
pce_prolog_directory(pce('library/draw')) :-
	ensure_loaded(user:library(pcedraw)).
pce_prolog_directory(pce('library/man')) :-
	ensure_loaded(user:library(pce_manual)).
pce_prolog_directory(pce('library/emacs')) :-
	ensure_loaded(user:library(pce_emacs)),
	user:start_emacs.
pce_prolog_directory(pce('library/dialog')) :-
	ensure_loaded(user:library(edit_dialog)).
pce_prolog_directory(pce('demo')).
pce_prolog_directory(pce('contrib')).

pce_frecompile_libraries :-
	forall(pce_prolog_directory(LibDir),
	       pce_frecompile_directory(LibDir)).

boot_file(pce_boot(pce_expand)).
boot_file(pce_boot(pce_sp)).
boot_file(pce_boot(pce_principal)).
boot_file(pce_boot(pce_error)).
boot_file(pce_boot(pce_operator)).
boot_file(pce_boot(pce_global)).
boot_file(pce_boot(pce_expansion)).
boot_file(pce_boot(pce_realise)).
boot_file(pce_boot(pce_autoload)).
boot_file(pce_boot(pce_editor)).
boot_file(library(sp_compatibility)).
boot_file(library(pce)).

pce_fcompile_boot_files :-
	forall(boot_file(BootFile),
	       fcompile(user:BootFile)).
