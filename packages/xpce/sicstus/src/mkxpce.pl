/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1999 University of Amsterdam. All rights reserved.
*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
mkxpce.pl

This file specifies the contents of the xpce saved-state.  Please edit
this file to suit your personal needs and issue:

    % sicstus -l mkxpce.pl -a <PCEHOME>/prolog/lib <state>

Where <PCEHOME> refers to the directory in   which  the XPCE library was
installed and <state> is name of the file for creating the staved state.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


%	This must be `off' for loading part of the XPCE Prolog files.  If you
%	have compiled these to .ql files using `make ql', it is your choice.

% :- prolog_flag(character_escapes, _, off).

:- prolog_flag(argv, [PceLibDir, _State]),
   assert(user:library_directory(PceLibDir)),
   use_module(library(pce)).

		 /*******************************
		 *     DEVELOPMENT MODULES	*
		 *******************************/

%	Define the predicates of the development environment you'd like
%	to have included into the saved-state.

:- require([ manpce/0,			% base of manual system
	     emacs/0,			% PceEmacs
	     editpce/1,			% Edit from class/method specification
	     tracepce/1,		% Set tracepoints
	     spypce/1			% Set breakpoints
	   ]).

		 /*******************************
		 *	    AUTOLOADING		*
		 *******************************/

%	Define this to provide autoloading.  Autoloading is handy during
%	program development.

user:unknown_predicate_handler(Goal, Module, Goal) :-
        functor(Goal, Name, Arity),
        require(Module:(Name/Arity)).

		 /*******************************
		 *	       SAVE		*
		 *******************************/

:- prolog_flag(argv, [_PceLibDir, State]),
   save_program(State),
   halt(0).

:- halt(1).
