/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 2000 University of Amsterdam. All rights reserved.
*/

:- module(doc_browser, []).
:- use_module(library(pce)).
:- use_module(library(toolbar)).
:- use_module(library(pce_report)).
:- use_module(library(url)).
:- use_module(doc(window)).

:- pce_image_directory(icons).

resource(backward, image, image('back.xpm')).
resource(forward,  image, image('forward.xpm')).
resource(reload,   image, image('reload.xpm')).
resource(source,   image, image('source.xpm')).


		 /*******************************
		 *	    THE BROWSER		*
		 *******************************/

:- pce_begin_class(doc_browser, frame, "Document browser").

initialise(TB) :->
	send_super(TB, initialise),
	send(TB, append, new(D, dialog)),
	send(D, name, top_dialog),
        send(D, pen, 0),
        send(D, gap, size(0, 5)),
        send(D, append, tool_bar(TB)),
	send(D, append, graphical(0,0,20,1), right),
	send(D, append,
	     new(TI, text_item(url, '', message(TB, url, @arg1))),
	     right),
	send(TI, hor_stretch, 100),
	send(D, resize_message, message(D, layout, @arg2)),
	get(TI, height, H),
	send(TI, reference, point(0, H+5)),
	send(TI, value_set, new(chain)),
	send(TB, fill_toolbar),
	send(new(DW, doc_window), below, D),
	send(new(report_dialog), below, DW).

:- pce_group(parts).

document_window(TB, DW:doc_window) :<-
	"Window representing current document"::
	get(TB, member, doc_window, DW).

tool_bar(TB, Toolbar:tool_bar) :<-
	"Find the toolbar"::
	get(TB, member, top_dialog, D),
	get(D, member, tool_bar, Toolbar).

:- pce_group(menu).

fill_toolbar(F) :->
	get(F, tool_bar, TB),
	send_list(TB, append,
		  [ tool_button(backward,
				resource(backward),
				backward),
		    tool_button(forward,
				resource(forward),
				forward),
		    gap,
		    tool_button(reload,
				resource(reload),
				reload),
		    gap,
		    tool_button(view_source,
				resource(source),
				view_source)
		  ]).

reload(TB) :->
	"Reload current page"::
	get(TB, document_window, DW),
	get(DW, url, URL),
	send(DW, url, @nil),
	send(DW, url, URL).


backward(TB) :->
	"Go back to previous location"::
	get(TB, document_window, DW),
	send(DW, backward).


forward(TB) :->
	"Go forwards in history"::
	get(TB, document_window, DW),
	send(DW, forward).


view_source(TB) :->
	"Go forwards in history"::
	get(TB, document_window, DW),
	get(DW, page_source, File),
	start_emacs,
	send(@emacs, open_file, File).


add_history(TB, URL:name) :->
	"Add URL to history"::
	get(TB, member, top_dialog, D),
	get(D, member, url, TI),
	send(TI, selection, URL),
	get(TI, value_set, Chain),
	send(Chain, delete_all, URL),
	send(Chain, prepend, URL),
	(   get(Chain, size, Size),
	    Size > 20
	->  send(Chain, delete_tail)
	;   true
	).

:- pce_group(navigate).

url(TB, URL:name*) :->
	"Jump to indicated URL"::
	get(TB, document_window, DW),
	parse_url(URL, Parts),
	parse_url(Cannonical, Parts),
	send(DW, url, Cannonical).

:- pce_end_class.
