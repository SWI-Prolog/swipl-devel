/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

:- module(emacs_script_mode, []).
:- use_module(library(pce)).

:- emacs_begin_mode(script, language,
		  "Edit arbitrary scripts with # line comment",
		  [
		  ],
		  [ '"'  = string_quote('\'),
		    '''' = string_quote('\'),
		    '#'  = comment_start,
		    '\n' + comment_end
		  ]).

:- emacs_end_mode.

