/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    M-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

:- module(emacs_text_mode, []).
:- use_module(library(pce)).

:-  new(_KB, emacs_key_binding(emacs_text, fundamental)).

:- new(_, syntax_table(text)).

:- pce_begin_class(emacs_text_mode, emacs_fundamental_mode).

setup_mode(E) :->
	"Switch editor into fill-mode"::
	send(E, fill_mode, @on).

:- pce_end_class.

