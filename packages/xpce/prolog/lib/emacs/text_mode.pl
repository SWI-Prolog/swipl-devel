/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    M-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

:- module(emacs_text_mode, []).
:- use_module(library(pce)).
:- set_prolog_flag(character_escapes, false).

:- emacs_begin_mode(text, fundamental,
		    "Edit plain text (sets fillmode",
	[],
	[]).

setup_mode(E) :->
	"Switch editor into fill-mode"::
	send(E, fill_mode, @on).

:- emacs_end_mode.

