/*  File:    find.pl
    Author:  unknown
    Created: Jan 11 2002
    Purpose: 
*/

:- module(emacs_find,
	  [
	  ]).
:- use_module(library(pce)).
:- use_module(library(pce_tick_box)).
:- use_module(library(hyper)).

:- pce_begin_class(emacs_find_dialog, dialog, "Find in XPCE editor").

variable(search_status, {start,next,wrapped}, get, "Current search status").

initialise(D, E:editor) :->
	"Create for editor"::
	send_super(D, initialise, 'Find in text'),
	send(D, append, text_item(find)),
	send(D, append, new(M, menu(mode, choice))),
	send_list(M, append,
		  [ regex,
		    plain,
		    word
		  ]),
	send(D, append, tick_box(case_sensitive, @on)),
	send(D, append, button(find)),
	send(D, append, button(cancel)),
	send(D, slot, search_status, start),
	new(_, partof_hyper(E, D, search, editor)).

editor(D, E:editor) :<-
	"Get associated editor"::
	get(D, hypered, editor, E).

open(D) :->
	"Open for attached editor"::
	get(D, editor, E),
	get(E, frame, Frame),
	send(D, transient_for, Frame),
	get(E, display_position, point(DX, DY)),
	get(E?area, size, size(EW, EH)),
	CX is DX+EW//2,
	CY is DY+EH//2,
	send(D, open_centered, point(CX, CY)).
	
cancel(D) :->
	"Cancel search dialog"::
	send(D, destroy).

find(D) :->
	"Execute search"::
	
	



:- pce_end_class(emacs_find_dialog).

