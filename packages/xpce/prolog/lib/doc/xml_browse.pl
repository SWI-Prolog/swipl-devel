/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 2000 University of Amsterdam. All rights reserved.
*/

:- module(xml_browse, []).
:- use_module(library(pce)).
:- use_module(library('doc/load')).
:- use_module(library('doc/xml_hierarchy')).

:- pce_begin_class(xml_browser, frame,
		   "Browse XML document").

initialise(B, XML:prolog) :->
	send_super(B, initialise, 'XML structure browser'),
	send(B, append, new(H, xml_browse_hierarchy(XML))),
	send(new(doc_window), right, H).
	
xml(B, XML:prolog) :->
	get(B, member, doc_window, DW),
	send(DW, clear),
	get(B, member, xml_browse_hierarchy, H),
	send(H, xml, XML).

show_xml(B, Tokens:prolog) :->
	"Show parsed XML in browser"::
	get(B, member, doc_window, DW),
	send(DW, show, Tokens).
	
:- pce_end_class.

:- pce_begin_class(xml_browse_hierarchy, xml_hierarchy,
		   "Browse an XML hierarchy").

select_node(H, Node:xml_node) :->
	"Show content of selected node"::
	get(Node, xml, Tokens),
	send(H?frame, show_xml, Tokens).

:- pce_end_class.
