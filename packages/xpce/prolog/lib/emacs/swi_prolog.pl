/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/


:- module(swi_prolog_emacs_binding, []).
:- use_module(library(pce)).
:- require([ start_emacs/0
	   ]).

:- multifile
	user:edit_source/1,
	user:exception/3.

		 /*******************************
		 *       EDITOR INTERFACE	*
		 *******************************/

user:edit_source(File:_Line:Name/Arity) :-
	start_emacs,
	new(X, emacs_buffer(File)),
	send(X, open),
	get(X?editors, head, Editor),
	send(Editor, locate, Name, Arity).
user:edit_source(File) :-
	\+ File = _:_,
	start_emacs,
	new(X, emacs_buffer(File)),
	send(X, open).
user:edit_source(Spec) :-
	format('Failed to start PCE/Emacs from ~w~n', [Spec]).


		 /*******************************
		 *          WARNINGS		*
		 *******************************/

:- pce_global(@prolog_warnings, make_prolog_warning_list).

make_prolog_warning_list(L) :-
	new(L, emacs_hit_list('SWI-Prolog warnings')),
	send(L, clear_on_append, @on),
	send(L, expose_on_append, @on),
	send(L, message, error_at_location).

user:exception(warning, warning(Path, Line, Message), _) :-
	Path \== user,
	\+ object(@loading_emacs),
	start_emacs,
	new(Buffer, emacs_buffer(Path)),
	get(Buffer, scan, 0, line, Line-1, start, SOL),
	send(@prolog_warnings, append_hit, Buffer, SOL, @default, Message),
	fail.					% give normal message too








