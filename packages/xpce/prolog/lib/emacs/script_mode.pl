/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    M-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

:- module(emacs_script_mode, []).
:- use_module(library(pce)).


:- initialization
	new(_KB, emacs_key_binding(script, language)).

:- initialization
	new(X, syntax_table(script)),
	send(X, syntax, '"',  string_quote, '\'),
	send(X, syntax, '''', string_quote, '\'),

	send(X, syntax,     '#',  comment_start),
	send(X, add_syntax, '\n', comment_end).

:- pce_begin_class(emacs_script_mode, emacs_language_mode).

:- pce_end_class.

