/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

:- module(xml_browse, []).
:- use_module(library(pce)).
:- use_module(library('doc/load')).
:- use_module(library('doc/xml_hierarchy')).

:- multifile
	doc:caption/2.

:- pce_begin_class(xml_browser, frame,
		   "Browse XML document").

initialise(B, XML:prolog) :->
	send_super(B, initialise, 'XML structure browser'),
	send(B, append, new(D, dialog)),
	send(D, append, new(menu_bar)),
	send(D, pen, 0),
	send(D, gap, size(0, 5)),
	send(B, fill_menu),
	new(H, xml_browse_hierarchy(XML)),
	send(new(doc_window), right, H),
	send(H, below, D).
	
fill_menu(B) :->
	get(B, member, dialog, D),
	get(D, member, menu_bar, MB),
	send(MB, append, new(F, popup(view))),
	send_list(F, append,
		  [ menu_item(dom,
			      message(B, view_dom),
			      'DOM')
		  ]).


xml(B, XML:prolog) :->
	get(B, member, doc_window, DW),
	send(DW, clear),
	get(B, member, xml_browse_hierarchy, H),
	send(H, xml, XML).

show_xml(B, Tokens:prolog) :->
	"Show parsed XML in browser"::
	get(B, member, doc_window, DW),
	send(DW, show, Tokens).
	
view_dom(B) :->
	get(B, member, xml_browse_hierarchy, H),
	get_chain(H, selection, Selection),
	new(V, view),
	pce_open(V, write, Fd),
	forall(member(Node, Selection),
	       (   get(Node, xml, DOM),
		   pretty_print(Fd, DOM)
	       )),
	close(Fd),
	send(V, caret, 0),
	send(V, open).

:- pce_end_class.

		 /*******************************
		 *	      HIERARCHY		*
		 *******************************/

:- pce_begin_class(xml_browse_hierarchy, xml_hierarchy,
		   "Browse an XML hierarchy").

select_node(H, Node:xml_node) :->
	"Show content of selected node"::
	get(Node, xml, Tokens),
	send(H?frame, show_xml, Tokens).

caption(H, XML:prolog, Caption:name) :<-
	"Provide caption"::
	(   doc:caption(XML, Caption)
	->  true
	;   get_super(H, caption, XML, Caption)
	->  true
	;   XML=[_|_]
	->  Caption = '<Elements>'
	;   Caption = '??'
	).

:- pce_end_class.

