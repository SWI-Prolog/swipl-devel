/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/


:- module(swi_prolog_emacs_binding, []).
:- use_module(library(pce)).
:- require([ start_emacs/0
	   ]).

:- multifile
	user:message_hook/3.
:- dynamic
	user:message_hook/3.


		 /*******************************
		 *          WARNINGS		*
		 *******************************/

:- pce_global(@prolog_warnings, make_prolog_warning_list).

make_prolog_warning_list(L) :-
	new(L, emacs_hit_list('SWI-Prolog warnings')),
	send(L, clear_on_append, @on),
	send(L, expose_on_append, @on),
	send(L, message, error_at_location).

user:message_hook(Term, Level, Lines) :-
	accept_level(Level),
	(   Term = error(syntax_error(Error), file(Path, Line))
	->  new(Message, string('Syntax error: %s', Error))
	;   source_location(Path, Line),
	    make_message(Lines, Message)
	),
	\+ object(@loading_emacs),
	start_emacs,
	new(Buffer, emacs_buffer(Path)),
	get(Buffer, scan, 0, line, Line-1, start, SOL),
	send(@prolog_warnings, append_hit, Buffer, SOL, @default, Message),
	fail.					% give normal message too

accept_level(warning).
accept_level(error).

make_message(Lines, String) :-
	phrase(make_message(Lines), Chars), !,
	new(String, string(Chars)).

make_message([]) -->
	[].
make_message([nl|T]) -->
	" ",
	make_message(T).
make_message([Fmt-Args|T]) --> !,
	{ sformat(S, Fmt, Args),
	  string_to_list(S, Chars)
	},
	Chars,
	make_message(T).
make_message([Fmt|T]) -->
	make_message([Fmt-[]|T]).

.(H, T, L, R) :- append([H|T], R, L).


