/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
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

