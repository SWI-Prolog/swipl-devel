/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 2000 University of Amsterdam. All rights reserved.
*/

:- module(xml_hierarchy, []).
:- use_module(library(pce)).
:- use_module(library(pce_toc)).
:- require([ memberchk/2
	   , append/3
	   , nth1/3
	   ]).

resource(open,	image, image('16x16/book2.xpm')).
resource(close,	image, image('16x16/manual.xpm')).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Show hierarchy of elements as produced by  xml2pl. This class is kept as
simple as possible. In general it  requires subclassing and defining the
select_node message to do anything sensible.   See  class toc_window for
details on programming this library.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(xml_hierarchy, toc_window,
		   "Browse XML document hierarchy").

variable(xml,	prolog,	get, "Represented XML term").

initialise(H, XML:prolog) :->
	send_super(H, initialise),
	send(H, xml, XML).

xml(H, XML:prolog) :->			% list of elements
	"Visualise an XML structure"::
	content(XML, Content),
	send(H, slot, xml, Content),
	get(H, icon, Content, @on, Icons),
	get(H, caption, Content, Caption),
	send(H, root, xml_node(Caption, [], Icons, @on)),
	send(H, expand_root).

content(document(_Type, [Content]), Content).
content([Content], Content).
content(element(A,B,C), element(A,B,C)).

expand_node(H, Node:xml_node) :->
	"Expand clicked node"::
	get(Node, xml, element(_Name, _Attributes, Content)),
	get(Node, path, Loc0),
	append(Loc0, [Index], Loc),
	(   nth1(Index, Content, Esub),
	    get(H, caption, Esub, Name),
	    arg(3, Esub, SubContent),
	    (	memberchk(element(_,_,_), SubContent)
	    ->	get(H, icon, Esub, @on, Icons),
		send(H, son, Node, xml_node(Name, Loc, Icons, @on))
	    ;   get(H, icon, Esub, @on, Icons),
		send(H, son, Node, xml_node(Name, Loc, Icons, @off))
	    ),
	    fail
	;   true
	).

:- pce_group(refine).

caption(_, XML:prolog, Title:name) :<-
	"Get title for a node"::
	XML = element(Title, _, _).

icon(_H, _XML:prolog, _HasSub:bool, Tuple:tuple) :<-
	"Return open/close icon"::
	new(Tuple, tuple(image(resource(open)),
			 image(resource(close)))).

:- pce_end_class.


:- pce_begin_class(xml_node, toc_folder,
		   "Show XML node with sub-nodes").

variable(path, prolog, get, "Path from root").

initialise(Node, Name:name, Path:prolog, Icons:tuple, CanExpand:bool) :->
	get(Icons, first, Open),
	get(Icons, second, Close),
	send_super(Node, initialise, Name, @default, Close, Open, CanExpand),
	send(Node, slot, path, Path).

xml(Node, XML:prolog) :<-
	"Get XML as Prolog term"::
	get(Node?tree, window, Window),
	get(Window, xml, Content),
	get(Node, path, Path),
	find_xml(Path, Content, XML).

find_xml([], XML, XML) :- !.
find_xml([H|T], element(_, _, Content), XML) :-
	nth1(H, Content, XML0),
	find_xml(T, XML0, XML).

:- pce_end_class.


