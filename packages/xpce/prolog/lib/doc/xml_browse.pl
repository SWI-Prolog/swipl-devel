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

:- module(xml_browse, []).
:- use_module(library(pce)).
:- use_module(library('doc/load')).
:- use_module(library('doc/xml_hierarchy')).
:- use_module(library('trace/pprint')).
:- use_module(library(toolbar)).
:- use_module(library(pce_report)).
:- use_module(library(find_file)).

:- pce_global(@finder, new(finder)).
 
:- multifile
	doc:caption/2.

:- pce_begin_class(xml_browser, frame,
		   "Browse XML document").

initialise(B, XML:[prolog]) :->
	send_super(B, initialise, 'XML structure browser'),
	send(B, append, new(D, tool_dialog(B))),
	send(B, fill_dialog(D)),
	new(H, xml_browse_hierarchy(XML)),
	send(new(doc_window), right, H),
	send(H, below, D),
	send(new(report_dialog), below, H).
	
fill_dialog(B, D:tool_dialog) :->
	send_list(D, append,
		  [ new(F, popup(file)),
		    new(V, popup(view)),
		    new(S, popup(search)),
		    new(H, popup(help))
		  ]),
	send_list(F, append,
		  [ menu_item(open_file,
			      message(B, open_file)),
		    gap,
		    menu_item(exit,
			      message(B, destroy))
		  ]),
	send_list(V, append,
		  [ menu_item(dom,
			      message(B, view_dom),
			      'DOM')
		  ]),
	send_list(S, append,
		  [ menu_item(find_elements,
			      message(B, search))
		  ]),
	send_list(H, append,
		  [ menu_item(about,
			      message(B, about))
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
	
:- pce_group(action).

open_file(B) :->
	"Open SGML/XML/HTML file"::
	get(@finder, file, open,
	    chain(tuple('HTML files', chain(html,htm)),
		  tuple('XML files', xml),
		  tuple('SGML files', chain(sgml,sgm)),
		  tuple('All files', '*')),
	    FileName),
	file_name_extension(_, Ext, FileName),
	dialect(Ext, Dialect),
	(   Dialect == html
	->  load_html_file(FileName, DOM)
	;   load_structure(FileName, DOM,
			   [ dialect(Dialect)
			   ])
	),
	send(B, xml, DOM).
	    
dialect(html, html).
dialect(htm,  html).
dialect(xml,  xml).
dialect(sgml, sgml).
dialect(sgm,  sgml).
	    
about(_B) :->
	send(@display, inform,
	     'Visualise HTML/XML/SGML DOM structure\n\
	      as produced by library(sgml)\n\n\
	      By Jan Wielemaker\n\
	      jan@swi.psy.uva.nl').

view_dom(B) :->
	get(B, member, xml_browse_hierarchy, H),
	get_chain(H, selection, Selection),
	(   Selection == []
	->  send(B, report, warning, 'No selection')
	;   new(V, view),
	    pce_open(V, write, Fd),
	    forall(member(Node, Selection),
		   (   get(Node, xml, DOM),
		       print_term(DOM,
				  [ output(Fd),
				    right_margin(78)
				  ])
		   )),
	    close(Fd),
	    send(V, caret, 0),
	    send(V, open)
	).

search(B) :->
	"Search the hierarchy"::
	new(D, dialog('Search DOM')),
	send(D, append, new(T, text_item(text))),
	send(D, append, new(E, text_item(element))),
	send(D, append, new(A, text_item(attribute))),
	send(D, append, new(V, text_item(value))),
	send(D, append, button(search, message(B, search_for,
					       T?selection,
					       E?selection,
					       A?selection,
					       V?selection))),
	send(D, append, button(done, message(D, destroy))),
	send(D, default_button(search)),
	send(D, transient_for, B),
	send(D, open_centered, B?area?center).

search_for(B,
	   Text0:name,
	   Element0:name,
	   Attribute0:name,
	   Value0:name) :->
	"Execute search"::
	mkvar(Text0, Text),
	mkvar(Element0, Element),
	mkvar(Attribute0, Attribute),
	mkvar(Value0, Value),
	(   var(Attribute),
	    var(Value)
	->  true
	;   A = (Attribute = Value)
	),
	get(B, member, xml_browse_hierarchy, H),
	get(H, root, Root),
	send(Root, collapsed, @on),
	send(H, selection, @nil),
	get(H, xml, DOM),
	findall(Node, find_node(B, DOM, Element, A, Text, Node), Nodes),
	(   Nodes == []
	->  send(B, report, warning, 'No match')
	;   length(Nodes, Len),
	    send(B, report, status, 'Found %d matching elements', Len),
	    send(H, normalise, Nodes)
	).

mkvar('', _) :- !.
mkvar(X, X).

find_node(B, DOM, E, A, T, Node) :-
	find(DOM, E, A, T, Path),
	get(B, member, xml_browse_hierarchy, H),
	get(H, node_from_path, Path, Node),
	send(Node, selected, @on).

find(element(E, AL, Content), E, A, Text, []) :-
	(   var(A)
	->  true
	;   memberchk(A, AL)
	),
	(   var(Text)
	->  true
	;   member(Atom, Content),
	    atom(Atom),
	    sub_atom(Atom, _, _, _, Text)
	->  true
	).
find(element(_, _, Content), E, A, Text, [N|T]) :-
	nth_element(N, Content, Sub),
	find(Sub, E, A, Text, T).

%	nth_element(-N, +List, -Element)
%	
%	As nth1/3, but only counts the element(_,_,_) terms in the
%	list.

nth_element(N, Content, E) :-
	nth_element(Content, E, 1, N).

nth_element([Elem|_], Elem, Base, Base) :-
	Elem = element(_,_,_).
nth_element([H|Tail], Elem, N, Base) :-
	(   H = element(_,_,_)
	->  succ(N, M)
	;   M = N
	),
        nth_element(Tail, Elem, M, Base).

:- pce_end_class(xml_browser).

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

