/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

:- module(doc_window, []).
:- use_module(library(pce)).
:- use_module(doc(html)).
:- use_module(doc(url_fetch)).
:- use_module(doc(util)).

:- pce_begin_class(doc_window, picture, "Represent a document").

variable(url,		name*, get, "Represented URL").
variable(location,	name*, get, "Label last jumped too").
variable(backward_list,	chain, get, "->back list of URL's").
variable(forward_list,	chain, get, "->back list of URL's").
variable(page_source,   file*, get, "(Cached) source-page").

class_variable(size, size, size(600, 300)).

initialise(DW, URL:[name]*) :->
	send_super(DW, initialise),
	send(DW, slot, forward_list, new(chain)),
	send(DW, slot, backward_list, new(chain)),
	send(DW, display, new(PB, pbox(550, justify)), point(25,0)),
	send(DW, resize_message,
	     message(PB, line_width, @arg1?width-50)),
	send(DW, recogniser, @scroll_recogniser),
	(   atom(URL)
	->  send(DW, url, URL)
	;   true
	).


:- pce_global(@scroll_recogniser, make_scroll_recogniser).


make_scroll_recogniser(G) :-
	new(G, key_binding),
	send_list(G,
		  [ function('SPC',
			     message(@event?receiver, scroll_vertical,
				     forwards, page, 700)),
		    function('\\ef',
			     message(@event?receiver?window, find)),
		    function(page_down,
			     message(@receiver, scroll_vertical,
				     forwards, page, 1000)),
		    function(page_up,
			     message(@receiver, scroll_vertical,
				     backwards, page, 1000)),
		    function(cursor_home,
			     message(@receiver, scroll_vertical,
				     goto, file, 0)),
		    function(end,
			     message(@receiver, scroll_vertical,
				     goto, file, 1000))
		  ]).

parbox(DW, PB:pbox) :<-
	"Get the parbox representing the whole document"::
	get(DW, member, pbox, PB).

:- pce_group(navigate).

goto_label(DW, Label:name) :->
	"Goto named label in current document"::
	send(DW?decoration, compute),		% so layout is fine
	get(DW, parbox, PB),
	(   get(PB, anchor, Label, tuple(PB2, Index))
%	    get(PB2, box, Index, OpenAnchor),
%	    get(OpenAnchor, hypered, close, CloseAnchor)
	->  get(PB2, window, Window),
	    get(PB2, box_area, Index, Window, Area),
	    send(Window, scroll_to, point(0, Area?y-20))
	;   send(DW, report, error, 'Label %s not found', Label)
	).

clear(DW) :->
	"Remove current page"::
	get(DW, parbox, PB),
	send(PB, clear),
	send(PB, append, hbox(0, 10)),
	send(DW, scroll_to, point(0, 0)),
	send(DW, flush).

show(DW, Tokens:prolog, Mode:[doc_mode]) :->
	"Show parsed document"::
	send(DW, clear),
	get(DW, parbox, PB),
	send(PB, show, Tokens, Mode).

url(DW, URLSpec:name*) :->
	"Switch to indicated url"::
	(   URLSpec == @nil
	->  send(DW, slot, page_source, @nil),
	    send(DW, clear),
	    send(DW, slot, url, @nil)
	;   labeled_url(URLSpec, URL, Label),
	    get(DW, frame, Frame),
	    catch(send(Frame, add_history, URL), _, true),
	    (   get(DW, url, URL)
	    ->  true
	    ;   send(Frame, busy_cursor),
	        get_url_to_file(URL, Path),
		send(DW, slot, page_source, Path),
		send(DW, clear),
		send(Frame, label, URL),	% Dubious
		send(DW, slot, url, URL),
		get(DW, parbox, PB),
		load_html_file(Path, Tokens),
		new(Mode, doc_mode),
		send(Mode, base_url, URL),
		send(PB, show, Tokens, Mode),
		send(Frame, busy_cursor, @nil)
	    ),
	    (	Label \== ''
	    ->  send(DW, goto_label, Label)
	    ;   true
	    )
	).

goto_url(DW, URLSpec:name, Dir:[{forward,backward}]) :->
	"Switch to label or URL with label"::
	debug(browser, 'Request for URL \'~w\'~n', [URLSpec]),
	send(DW, push_location, Dir),
	labeled_url(URLSpec, URL, Label),
	(   URL \== ''
	->  get(DW, url, Base),
	    global_url(URL, Base, GlobalURL),
	    send(DW, url, GlobalURL)
	;   true
	),
	(   Label \== ''
	->  send(DW, goto_label, Label)
	;   true
	).

push_location(DW, Dir:[{forward,backward}]) :->
	"Push current location on navigation list"::
	get(DW, url, URL),
	get(DW, location, Label),
	(   Label == @nil
	->  Full = URL
	;   concat_atom([URL, #, Label], Full)
	),
	(   Dir == backward
	->  send(DW?forward_list, prepend, Full)
	;   send(DW?backward_list, prepend, Full)
	).

backward(DW) :->
	"Go to previous location"::
	get(DW, backward_list, L),
	get(L, delete_head, To),
	send(DW, goto_url, To, backward).

forward(DW) :->
	"Got the next location"::
	get(DW, forward_list, L),
	get(L, delete_head, To),
	send(DW, goto_url, To, forward).

find(DW) :->
	"Open find dialog"::
	new(D, dialog('Find in page')),
	send(D, append, new(TI, text_item(search_for))),
	send(D, append, button(cancel, message(D, destroy))),
	send(D, append, button(find, message(DW, do_find, TI?selection))),
	send(D, default_button, find),
	get(DW, frame, Frame),
	send(D, transient_for, Frame),
	send(D, open_centered, Frame?area?center).

do_find(DW, Text:name) :->
	"Find named text and jump to it"::
	get(DW, parbox, PB),
	(   get(PB, find, and(message(@arg1, instance_of, tbox),
			      @arg1?text == Text),
		tuple(PB2, Index))
	->  get(PB2, window, Window),
	    get(PB2, box_area, Index, Window, Area),
	    send(Window, normalise, Area),
	    object(Area, area(X, Y, W, H)),
	    send(Window, display, box(W,H), point(X, Y))
	;   send(DW, report, error, 'Not found')
	).

labeled_url(Spec, URL, Label) :-
	sub_atom(Spec, B, _, A, #), !,
	sub_atom(Spec, 0, B, _, URL),
	sub_atom(Spec, _, A, 0, Label).
labeled_url(Spec, Spec, '').

:- pce_end_class.

