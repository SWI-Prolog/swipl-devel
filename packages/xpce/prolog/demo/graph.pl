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

:- module(graph_viewer,
	  [ graph_viewer/0
	  ]).

:- use_module(library(pce)).
:- require([ forall/2
	   , free_variables/2
	   , random/3
	   , term_to_atom/2
	   ]).


graph_viewer :-
	new(GV, graph_viewer),
	send(GV, open).

:- pce_begin_class(graph_viewer, frame).


initialise(GV) :->
	"Create graph-viewer"::
	send(GV, send_super, initialise, 'Graph Viewer'),
	send(GV, append, new(P, picture)),
	send(new(D, dialog), below, P),
	fill_dialog(D).

fill_dialog(D) :-
	new(Frame, D?frame),

	send(D, append, label(reporter)),

	send(D, append, button(generate, message(Frame, generate,
					       D?generator_member?selection))),
	send(D, append, text_item(generator, 'graph_viewer:test(A, B)',
				  message(D?generate_member, execute)), right),

	send(D, append, button(quit, message(Frame, destroy))),
	send(D, append, button(clear, message(Frame, clear))),
	send(D, append, button(postscript, message(Frame, postscript))),
	send(D, append, button(layout, message(Frame, layout))).


clear(F) :->
	"Clear the diagram"::
	get(F, member, picture, P),
	send(P, clear).


layout(F) :->
	"Run graph layout"::
	get(F, member, picture, P),
	(   get(P?graphicals, head, Head)
	->  send(Head, layout)
	;   send(F, report, error, 'No graph to layout')
	).


:- pce_autoload(finder, library(find_file)).
:- pce_global(@finder, new(finder)).

postscript(F) :->
	"Create PostScript in file"::
	get(@finder, file, @off, '.eps', FileName),
	get(F, member, picture, Pict),
	new(File, file(FileName)),
	send(File, open, write),
	send(File, append, Pict?postscript),
	send(File, close),
	send(File, done),
	send(F, report, status, 'Saved PostScript in %s', FileName).


generate(F, Generator:name) :->
	"Create graph using generator"::
	(   term_to_atom(Term, Generator),
	    free_variables(Term, [From, To])
	->  send(F, clear),
	    forall(user:Term,
		   send(F, display_arc, From, To)),
	    send(F, layout),
	    send(F, label, Generator)
	;   send(F, report, error,
		 'Generator should be a Prolog goal with two variables')
	).


:- pce_global(@graph_link, new(link(link, link, line(0,0,0,0,second)))).

display_arc(F, From:name, To:name) :->
	"Display arc From -> To"::
	get(F, node, From, NF),
	get(F, node, To, TF),
	send(NF, connect, TF, @graph_link).


node(F, Name:name, Node:graph_node) :<-
	"Get (create) node with specified name"::
	get(F, member, picture, Picture),
	(   get(Picture, member, Name, Node)
	->  true
	;   get(Picture, visible, area(X, Y, W, H)),
	    MX is X + W,
	    MY is Y + H,
	    random(X, MX, NX),
	    random(Y, MY, NY),
	    send(Picture, display, new(Node, graph_node(Name)), point(NX, NY))
	).


:- pce_end_class.


:- pce_begin_class(graph_node(name), device).

handle(w/2, 0, link, link).

initialise(Node, Name:name) :->
	"Create from name"::
	send(Node, send_super, initialise),
	send(Node, display, circle(5), point(-2, -2)),
	send(Node, display, new(T, text(Name, center))),
	send(T, center, point(0, 10)),
	send(Node, send_super, name, Name).

name(Node, Name:name) :->
	"Change name of a node"::
	get(Node, member, text, Text),
	send(Text, string, Name),
	send(Node, send_super, name, Name).

:- pce_global(@graph_node_recogniser, make_graph_node_recogniser).

make_graph_node_recogniser(R) :-
	new(R, move_gesture(left)),
	send(R, condition,
	     ?(@event?position, distance, point(0,0)) < 5).


event(Node, Ev:event) :->
	"Make it movable"::
	(   send(@graph_node_recogniser, event, Ev)
	->  true
	;   send(Node, send_super, event, Ev)
	).

:- pce_end_class.


test(a,b).
test(a,c).
test(a,d).
test(c,d).
