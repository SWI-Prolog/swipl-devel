/*  pl_pce.pl,v 1.2 1992/09/03 08:48:29 jan Exp

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/


		/********************************
		*      LOAD COMMON PLATFORM	*
		********************************/

:- user:retract(library_directory('/usr/local/lib/pl/library')).

:- (   getenv('PCEHOME', Home)
   ->  true
   ;   absolute_file_name('..', Home)
   ),
   concat(Home, '/prolog/boot/pce_expand', PceExpand),
   concat(Home, '/prolog/boot/pce_pl', PcePl),
   concat(Home, '/prolog/lib/pce', Pce),
   consult(PceExpand),
   use_module(PcePl),
   use_module(Pce).


:- '$version'(PlVersion),
   send(@pce, catch_error_signals, @on),
   concat('SWI-Prolog version ', PlVersion, PlId),
   send(@prolog, system, PlId).
   

		/********************************
		*           SET PCE HOME	*
		********************************/

:- (   getenv('PCEHOME', Home)
   ->  true
   ;   absolute_file_name('..', Home)
   ),
   format('PCE home directory = ~w~n', Home),
   send(@pce, home, Home).


		/********************************
		*       PROLOG LIBRARIES	*
		********************************/

:- user:assert((library_directory(PceLib) :-
	     		get(@pce, home, Home),
			concat(Home, '/prolog/lib', PceLib))).

:- ensure_loaded(library(pce_manual)).


		/********************************
		*        LOCK LOADED FILES	*
		********************************/

:- '$make_system_source_files'.
:- '$autoload':clear_library_index.
