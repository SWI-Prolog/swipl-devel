/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1999 University of Amsterdam. All rights reserved.
*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Load XPCE from the current directory into SICStus prolog.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

user:xpce_trial_run.

		 /*******************************
		 *	  DEBUGGING STUFF	*
		 *******************************/

:- multifile
	portray_message/2.

portray_message(error, _) :-
	prolog_load_context(file, File),
	(   prolog_load_context(term_position,
				'$stream_position'(_,Line,_,_))
	->  true
	;   Line = '?'
	),
	format('[ERROR Loading from ~w:~w]~n    ', [File, Line]),
	fail.

		 /*******************************
		 *	      SETTINGS		*
		 *******************************/

:- prolog_flag(character_escapes, _, off).

		 /*******************************
		 *	  DEBUG ON ERROR	*
		 *******************************/

error_exception(_).

		 /*******************************
		 *	     LOAD XPCE		*
		 *******************************/

:- ['lib/pce'].
%:- [library(pce_manual)].	


		 /*******************************
		 *	    AUTOLOADING		*
		 *******************************/

user:unknown_predicate_handler(Goal, Module, Goal) :-
        functor(Goal, Name, Arity),
        require(Module:(Name/Arity)).

