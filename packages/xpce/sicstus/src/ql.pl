/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1999 University of Amsterdam. All rights reserved.
*/

:- prolog_flag(character_escapes, _, off).

:- absolute_file_name('$SP_PATH/xpce/prolog/lib', PceLibDir),
   assert(user:library_directory(PceLibDir)),
   use_module(library(pce)).

main :-
	prolog_flag(argv, Argv),
	mkql(Argv),
	halt(0).

mkql([boot]) :-
	require(pce_fcompile_boot_files/0),
	pce_fcompile_boot_files.
mkql([library]) :-
	require(pce_fcompile_libraries/0),
	pce_fcompile_libraries.

:- main.
:- halt(1).
