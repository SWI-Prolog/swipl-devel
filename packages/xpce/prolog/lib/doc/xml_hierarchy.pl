/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (C): 1985-2002, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
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
content([pi(_XML), Content], Content).	% bit of a hack to handle XML files

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


