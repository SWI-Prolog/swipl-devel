/*  pl_pce.pl,v 1.2 1992/09/03 08:48:29 jan Exp

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/


		/********************************
		*      LOAD COMMON PLATFORM	*
		********************************/

:- user:retract(library_directory('/usr/local/lib/pl/library')) ; true.

:- ['../prolog/boot/pce_expand',
    '../prolog/boot/pce_pl'
   ].

:- prolog_load_context(directory, Here),
   concat(Here, '../prolog/boot', Dir),
   absolute_file_name(Dir, CanonocalDir),
   assert(library_directory(CanonocalDir), Ref),
   consult('../prolog/lib/pce'),
   erase(Ref).


:- feature(version, PlVersion),
%  send(@pce, catch_error_signals, @on),
   concat('SWI-Prolog version ', PlVersion, PlId),
   send(@prolog, system, PlId).
   

		/********************************
		*           SET PCE HOME	*
		********************************/

:- prolog_load_context(directory, Dir),
   '$file_dir_name'(Dir, Home),
   send(@pce, home, Home).


		 /*******************************
		 *	     CONSOLE		*
		 *******************************/

:- send(@pce, console_label, 'XPCE/SWI-Prolog').


		/********************************
		*       PROLOG LIBRARIES	*
		********************************/

library_directory(PceLib) :-
	get(@pce, home, Home),
	concat(Home, '/prolog/lib', RawPceLib),
	absolute_file_name(RawPceLib, PceLib).

%:- ensure_loaded(library(pce_manual)).


		/********************************
		*        LOCK LOADED FILES	*
		********************************/

:- '$make_system_source_files'.
:- '$autoload':clear_library_index.
