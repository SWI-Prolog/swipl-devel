/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1999 University of Amsterdam. All rights reserved.
*/

%	This must be `off' for loading part of the XPCE Prolog files.  If you
%	have compiled these to .ql files using `make ql', it is your choice.

:- prolog_flag(character_escapes, _, off).

:- absolute_file_name('$SP_PATH/xpce/prolog/lib', PceLibDir),
   assert(user:library_directory(PceLibDir)),
   use_module(library(pce)).

		 /*******************************
		 *     DEVELOPMENT MODULES	*
		 *******************************/

%	Define the predicates of the development environment you'd like
%	to have included into the saved-state.

:- require([ manpce/0,
	     emacs/0,
	     editpce/1,
	     tracepce/1,
	     spypce/1
	   ]).

		 /*******************************
		 *	    AUTOLOADING		*
		 *******************************/

%	Define this to provide autoloading.  Autoloading is handy during
%	program development.

user:unknown_predicate_handler(Goal, Module, Goal) :-
        functor(Goal, Name, Arity),
        require(Module:(Name/Arity)).


