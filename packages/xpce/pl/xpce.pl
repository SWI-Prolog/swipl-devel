/*  pl_pce.pl,v 1.2 1992/09/03 08:48:29 jan Exp

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/


		/********************************
		*      LOAD COMMON PLATFORM	*
		********************************/

:- ['../prolog/boot/pce_expand',
    '../prolog/boot/pce_pl'
   ].

:- prolog_load_context(directory, Here),
   concat(Here, '/../prolog/lib/', RawLib),
   absolute_file_name(RawLib, Lib),
   asserta(library_directory(Lib)).
:- use_module(library(pce)).
:- retract(library_directory(_)).

:- feature(version, PlVersion),
   send(@pce, catch_error_signals, @on),
   concat('SWI-Prolog version ', PlVersion, PlId),
   send(@prolog, system, PlId).
   

		/********************************
		*           SET PCE HOME	*
		********************************/

:- prolog_load_context(directory, Dir),
   '$file_dir_name'(Dir, PceHome),
   send(@pce, home, PceHome).


		 /*******************************
		 *	     CONSOLE		*
		 *******************************/

%:- send(@pce, console_label, 'XPCE/SWI-Prolog').


		/********************************
		*       PROLOG LIBRARIES	*
		********************************/

library_directory(PceLib) :-
	get(@pce, home, Home),
	concat(Home, '/prolog/lib', RawLib),
	absolute_file_name(RawLib, PceLib).

file_search_path(demo, DemoDir) :-
	get(@pce, home, Home),
	concat(Home, '/prolog/demo', Raw),
	absolute_file_name(Raw, DemoDir).

file_search_path(contrib, ContribDir) :-
	get(@pce, home, Home),
	concat(Home, '/prolog/contrib', Raw),
	absolute_file_name(Raw, ContribDir).

%:- '$autoload':clear_library_index.
%:- ensure_loaded(library(pce_manual)).


		/********************************
		*        LOCK LOADED FILES	*
		********************************/

:- '$make_system_source_files'.
:- '$autoload':clear_library_index.






