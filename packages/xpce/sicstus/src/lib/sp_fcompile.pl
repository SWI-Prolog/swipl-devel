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
		 *	    ORDERING		*
		 *******************************/

before(pce('prolog/lib/man'),
       [ before(v_visual, pce_op)	% do v_viual before pce_op
       ]).
	
order(Dir, Files0, Files) :-
	before(Dir, Ordering), !,
	order2(Ordering, Files0, Files).
order(_, Files, Files).

order2([], Files, Files).
order2([before(First, Last)|T], Files0, Files) :-
	is_before(First, Last, Files0), !,
	order2(T, Files0, Files).
order2([before(First, Last)|T], Files0, Files) :-
	swap(First, Last, Files0, Files1),
	order2(T, Files1, Files).

is_before(A, _, [A|_]) :- !.
is_before(_, B,	[B|_]) :- !, fail.
is_before(A, B, [_|T]) :-
	is_before(A, B, T).

swap(A, B, [A|T0], [B|T1]) :- !,
	swap(A, B, T0, T1).
swap(A, B, [B|T0], [A|T1]) :- !,
	swap(A, B, T0, T1).
swap(A, B, [H|T0], [H|T1]) :- !,
	swap(A, B, T0, T1).
swap(_, _, [], []).

		 /*******************************
		 *	COMPILE STATEMENTS	*
		 *******************************/

pce_fcompile(File) :-
	absolute_file_name(File, Path),
	(   source_file(Path)
	->  true
	;   use_module(user:File, [])
	),
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


ignore_by_fcompile('pce.pl').
ignore_by_fcompile('INDEX.pl').

pce_fcompile_directory(Dir) :-
	dirpath(Dir, DirPath),
	format('~n************************************************~n', []),
	format('** Directory ~w~n', [DirPath]),
	format('************************************************~n~n', []),
	get(directory(DirPath), files, '^.*\.pl$', Chain),
	chain_list(Chain, Files0),
	order(Dir, Files0, Files),
	forall(member(File, Files),
	      (	  ignore_by_fcompile(File)
	      ->  format('**** ignored ~w~n', [File])
	      ;   pce_fcompile(DirPath, File))
	      ).

pce_frecompile_directory(Dir) :-
	dirpath(Dir, DirPath),
	get(directory(DirPath), files, '^.*\.pl$', Chain),
	send(Chain, delete_all, 'INDEX.pl'),
	chain_list(Chain, Files),
	forall(member(File, Files),
	       pce_frecompile(DirPath, File)).


pce_fcompile_libraries :-
	findall(Lib, pce_prolog_directory(Lib), Libs),
	fcompile_libs(Libs).

fcompile_libs([]).
fcompile_libs([H|T]) :-
	pce_fcompile_directory(H),
	fcompile_libs(T).

pce_prolog_directory(pce('prolog/lib')).
pce_prolog_directory(pce('prolog/lib/draw')) :-
	use_module(user:library(pcedraw)).
pce_prolog_directory(pce('prolog/lib/man')) :-
	use_module(user:library(pce_manual)).
%	use_module(user:library('man/pce_op')).
pce_prolog_directory(pce('prolog/lib/emacs')) :-
	use_module(user:library(pce_emacs)),
	user:start_emacs.
pce_prolog_directory(pce('prolog/lib/dialog')) :-
	use_module(user:library(edit_dialog)),
	use_module(user:library('dialog/dialog')).
pce_prolog_directory(pce('prolog/demo')).
pce_prolog_directory(pce('prolog/contrib')).

pce_frecompile_libraries :-
	forall(pce_prolog_directory(LibDir),
	       pce_frecompile_directory(LibDir)).

boot_file(pce_boot(pce_expand)).
boot_file(pce_boot(pce_sicstus)).
boot_file(pce_boot(pce_principal)).
boot_file(pce_boot(pce_error)).
boot_file(pce_boot(pce_operator)).
boot_file(pce_boot(pce_global)).
boot_file(pce_boot(pce_expansion)).
boot_file(pce_boot(pce_goal_expansion)).
boot_file(pce_boot(pce_realise)).
boot_file(pce_boot(pce_autoload)).
boot_file(pce_boot(pce_editor)).
boot_file(pce_boot(pce_portray)).
boot_file(library(sp_report)).
boot_file(library(sp_compat)).
boot_file(library(pce)).
boot_file(library('english/pce_messages')).

pce_fcompile_boot_files :-
	forall(boot_file(BootFile),
	       fcompile(user:BootFile)).
