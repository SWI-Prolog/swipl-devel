/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

:- module(emacs_outline_mode, []).
:- use_module(library(pce)).
:- set_prolog_flag(character_escapes, false).

:- emacs_begin_mode(outline, language,
		    "Mode for viewing outlines",
					% BINDINGS
	[ toggle_outline	       = key('\C-co') + button(outline),
	  update_outline	       = key('\C-c\C-l') + button(outline),
	  toggle_open_outline	       = key('\e\e') + button(outline),
	  open_all_outlines	       = button(outline),
	  close_all_outlines	       = button(outline)
	],
					% SYNTAX TABLE
	[ 
	]).
		 
variable(outlined,	bool := @off,	get,	"Is in outline mode?").
variable(outline_regex_list, [chain],	send,	"Associated regexes").

class_variable(header_style, style,
	       when(@colour_display,
		    style(colour := blue),
		    style(bold := @on)),
	       "Style for outline-headers").

initialise(M) :->
	"Associate class <-outline_regex_list or empty"::
	send(M, send_super, initialise),
	send(M, slot, outline_regex_list, @default).


outline_regex_list(M, Ch:chain) :<-
	"Fetch from <-class"::
	get(M, slot, outline_regex_list, C),
	(   C == @default
	->  get(M?class, attribute, outline_regex_list, Ch)
	;   Ch = C
	).


toggle_outline(M) :->
	"Switch outline mode on/off"::
	get(M, outlined, Current),
	(   Current == @off
	->  send(M, outline_on)
	;   send(M, outline_off)
	),
	send(M, report, status, 'Outline mode switched %N', M?outlined).


outline_on(M) :->
	"Switch outline mode on"::
	(   get(M, outline_regex_list, Ch),
	    send(Ch, empty)
	->  send(M, report, warning, 'No outline definition'),
	    fail
	;   get(M, header_style, Header),
	    send(M, style, outline_header, Header),
	    send(M, style, outline_hidden, style(hidden := @on)),
	    send(M, slot, outlined, @on),
	    send(M, update_outline)
	).


outline_off(M) :->
	"Switch outline mode off"::
	send(M, delete_outline_fragments),
	send(M, slot, outlined, @off).


delete_outline_fragments(M) :->
	"Delete all outline fragments"::
	get(M, text_buffer, TB),
	send(TB, for_all_fragments,
	     if(or(@arg1?style == outline_header,
		   @arg1?style == outline_hidden,
		   @arg1?style == outline_body),
		message(@arg1, free))).


make_outline_fragment(M, Re:regex) :->
	"Make outline fragments for this outline"::
	get(M, text_buffer, TB),
	send(Re, for_all, TB,
	     message(@prolog, call, do_outline, @arg1, @arg2)).


do_outline(Regex, TB) :-
	get(Regex, register_start, 1, SH),
	get(Regex, register_end,   1, EH),
	get(Regex, register_start, 2, SB),
	get(Regex, register_end,   2, EB),
	new(_H, fragment(TB, SH, EH-SH, outline_header)),
	new(_B, fragment(TB, SB, EB-SB, outline_hidden)).

update_outline(M) :->
	"Update the outline definition"::
	(   get(M, outlined, @on)
	->  send(M, report, progress, 'Updating outline '),
	    send(M, delete_outline_fragments),
	    get(M, outline_regex_list, Chain),
	    send(Chain, for_all,
		 message(M, make_outline_fragment, @arg1)),
	    send(M, report, done)
	;   true
	).

recenter(M) :->
	"Update outline and recenter"::
	send(M, update_outline),
	send(M, send_super, recenter).

toggle_open_outline(M) :->
	"Open/Close outline around caret"::
	get(M, caret, Caret),
	(   get(M?text_buffer, find_fragment,
		and(message(@arg1, overlap, Caret),
		    @arg1?style == outline_header),
		Header)
	->  body_fragment(Header, Body), !,
	    (   get(Body, style, outline_hidden)
	    ->  send(M, open_body, Body)
	    ;   send(M, close_body, Body)
	    )
	;   get(M?text_buffer, find_fragment,
		and(message(@arg1, overlap, Caret),
		    @arg1?style == outline_hidden),
		Body)
	->  send(M, open_body, Body)
	;   get(M?text_buffer, find_fragment,
		and(message(@arg1, overlap, Caret),
		    @arg1?style == outline_body),
		Body)
	->  send(M, close_body, Body)
	;   send(M, report, warning, 'Not in outlined fragment')
	).

open_body(_M, Body:fragment) :->
	"Make hidden body visible and try to normalise on it"::
	send(Body, style, outline_body).
/*	doesn't feel very natural after all
	head_fragment(Body, Head),
	get(Head, start, Start),
	get(Body, end, End),
	send(M, normalise, Start, End).
*/

close_body(_M, Body:fragment) :->
	"Make body invisible"::
	send(Body, style, outline_hidden).

body_fragment(Body, Body) :-
	get(Body, style, outline_body), !.
body_fragment(Body, Body) :-
	get(Body, style, outline_hidden), !.
body_fragment(Header, Body) :-
	get(Header, next, F),
	body_fragment(F, Body).

head_fragment(Body, Body) :-
	get(Body, style, outline_header), !.
head_fragment(Header, Body) :-
	get(Header, previous, F),
	head_fragment(F, Body).


open_all_outlines(M) :->
	"Open all outlined fragments"::
	send(M?text_buffer, for_all_fragments,
	     if(@arg1?style == outline_hidden,
		message(@arg1, style, outline_body))).


close_all_outlines(M) :->
	"Close all outlined fragments"::
	send(M?text_buffer, for_all_fragments,
	     if(@arg1?style == outline_body,
		message(@arg1, style, outline_hidden))).

:- emacs_end_mode.

