/*  $Id$

    Part of SWI-Prolog
    Designed and implemented by Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1999 University of Amsterdam. All rights reserved.
*/

:- module(unix,
	  [ fork/1,			% -'client'|pid
	    exec/1,			% +Command(...Args...)
	    wait/2,			% -Pid, -Reason
	    kill/2,			% +Pid. +Signal
	    pipe/2,			% +Read, +Write
	    dup/2,			% +From, +To
	    detach_IO/0,
	    environ/1			% -[Name=Value]
	  ]).
:- use_module(library(shlib)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
These predicates are documented in the source-distribution of the package
`clib'.  See also the SWI-Prolog home-page at

	http://www.swi.psy.uva.nl/projects/SWI-Prolog/
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- initialization
   load_foreign_library(foreign(unix), install_unix).
