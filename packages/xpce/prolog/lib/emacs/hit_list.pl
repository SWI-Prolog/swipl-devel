/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

:- module(emacs_hit_list, []).
:- use_module(library(pce)).
:- require([ default/3
	   ]).


:- pce_begin_class(emacs_hit_list, frame).

variable(expose_on_append, bool := @off, both,
	 "->expose on ->append_hit").
variable(clear_on_append,  bool := @off, both,
	 "Clear on ->append_hit after usage").
variable(used, 		   bool := @off, both,
	 "->goto has been used").
variable(message,	   name := caret, both,
	 "Method to apply").

class_variable(confirm_done, bool, @off).

initialise(L, Label:[string]) :->
	"Create from label"::
	default(Label, 'Compilation errors', FrameLabel),
	send(L, send_super, initialise, FrameLabel),
	send(L, append, new(B, browser('', size(60, 6)))),
	send(B, open_message, message(L, goto, @arg1?object)),
	send(new(D, dialog), below, B),
	send(D, pen, 0),
	send(D, gap, size(10, 5)),
	send(D, append, button(quit, message(L, destroy))),
	send(D, append, label(reporter), right),
	send(L, open).


unlink(L) :->
	"Remove fragments from the buffers"::
	send(L, clear),
	send(L, send_super, unlink).


browser(L, Browser:list_browser) :<-
	get(L, member, browser, B),
	get(B, list_browser, Browser).


clear(L) :->
	"Clear browser and delete fragments"::
	get(L?browser, dict, Dict),
	send(Dict, for_all, message(@arg1?object, free)),
	send(Dict, clear).


append_hit(L, Buffer:emacs_buffer, Start:int, Len:[int], Msg:[char_array]) :->
	"Append a hit to the hit-list"::
	(   get(L, expose_on_append, @on)
	->  send(L, expose)
	;   true
	),
	(   get(L, clear_on_append, @on),
	    get(L, used, @on)
	->  send(L, clear)
	;   true
	),
	(   Len == @default
	->  get(Buffer, scan, Start, line, 0, end, EOL),
	    FragLength is EOL - Start
	;   FragLength = Len
	),
	get(Buffer, line_number, Start, LineNo),
	(   Msg == @default
	->  get(Buffer, contents, Start, FragLength, String)
	;   String = Msg
	),
	get(Buffer, name, BufName),
	get(L, browser, ListBrowser),
	send(ListBrowser, append,
	     new(DI, dict_item('',
			       string('%s:%d: %s',
				      BufName, LineNo, String),
			       new(F, fragment(Buffer, Start, FragLength))))),
	new(_, emacs_mark_hyper(DI, F, dict_item, fragment)),
	send(ListBrowser, normalise, DI).

goto(L, Fragment:fragment) :->
	"Indicate the fragment"::
	send(L, used, @on),
	get(Fragment, text_buffer, TB),
	send(TB, open),
	get(TB?editors, head, Editor),
	get(L, message, Method),
	send(Editor, Method, Fragment?start).

:- pce_end_class.

:- pce_begin_class(emacs_mark_hyper, hyper).

unlink_to(H) :->
	get(H, from, From),
	free(From),
	free(H).
unlink_from(H) :->
	get(H, to, To),
	free(To),
	free(H).

:- pce_end_class.
