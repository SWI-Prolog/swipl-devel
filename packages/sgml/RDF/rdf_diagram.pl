/*  $Id$

    Part of SWI-Prolog SGML/XML parser

    Author:  Jan Wielemaker
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/SWI-Prolog/
    Copying: LGPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2002 SWI, University of Amsterdam. All rights reserved.
*/

:- module(rdf_diagram,
	  [ rdf_diagram_from_file/1	% +File
	  ]).
:- use_module(library(pce)).
:- use_module(library(pce_tagged_connection)).
:- use_module(library(autowin)).
:- use_module(library(pce_report)).
:- use_module(library(print_graphics)).
:- use_module(library(rdf_parser)).	% get access to declared namespaces


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This file defines the class rdf_diagram, a   window capable of showing a
set of triples.

The predicate rdf_diagram_from_file(+File) is a   simple demo and useful
tool to show RDF from simple RDF files.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


		 /*******************************
		 *	    SIMPLE ENTRY	*
		 *******************************/

%	rdf_diagram_from_file(+File)
%	
%	Show the triples from File in a window.

rdf_diagram_from_file(File) :-
	absolute_file_name(File,
			   [ access(read),
			     extensions([rdf,rdfs,owl,''])
			   ], AbsFile),
	load_rdf(AbsFile, Triples,
		 [ expand_foreach(true)
		 ]),
	new(D, rdf_diagram(string('RDF diagram for %s', File))),
	send(new(report_dialog), below, D),
	send(D, triples, Triples),
	send(D, open).


		 /*******************************
		 *	CLASS RDF-DIAGRAM	*
		 *******************************/

:- pce_begin_class(rdf_diagram, auto_sized_picture,
		   "Show set of RDF triples in a window").
:- use_class_template(print_graphics).

variable(auto_layout,	bool := @on, both, "Automatically layout on resize").

initialise(D, Label:[name]) :->
	send_super(D, initialise, Label),
	send(D, scrollbars, both),
	send(D, fill_popup),
	send(D, resize_message,
	     if(and(D?auto_layout == @on,
		    D?focus_recogniser == @nil),
		message(D, layout))).

fill_popup(D) :->
	send(D, popup, new(P, popup)),
	send_list(P, append,
		  [ menu_item(layout, message(D, layout)),
		    gap,
		    menu_item(print, message(D, print))
		  ]).

:- pce_group(triples).

append(D, Triple:prolog) :->
	"Append and rdf(Subject, Predicate, Object) triple"::
	subject_name(Triple, SubjectName),
	get(D, resource, SubjectName, Subject),
	(   is_type(Triple)
	->  object_resource(Triple, ObjectName),
	    send(Subject, type, ObjectName)
	;   predicate_name(Triple, PredName),
	    (   object_resource(Triple, ObjectName)
	    ->  get(D, resource, ObjectName, Object)
	    ;   object_literal(Triple, Literal)
	    ->  get(D, literal, Literal, Object)
	    ),
	    send(Subject, connect, PredName, Object)
	).
append(D, Triple:prolog) :->
	term_to_atom(Triple, Atom),
	ignore(send(D, report, error,
		    'Failed to display triple: %s', Atom)).
	
triples(D, Triples:prolog) :->
	"Show disgram from Prolog triples"::
	send(D, clear),
	forall(member(T, Triples),
	       send(D, append, T)),
	send(D, layout).

resource(D, Resource:name, Create:[bool], Subject:rdf_resource) :<-
	"Get reference for a subject or create one"::
	(   get(D, member, Resource, Subject)
	->  true
	;   Create \== @off,
	    send(D, display, new(Subject, rdf_resource(Resource)),
		 D?visible?center)
	).

literal(D, Value:prolog, Gr:rdf_literal) :<-
	"Display a literal.  Don't try to re-use"::
	(   literal_name(Value, Name),
	    get(D, member, Name, Gr)
	->  true
	;   send(D, display, new(Gr, rdf_literal(Value)),
		 D?visible?center)
	).


:- pce_group(layout).

layout(D) :->
	"Produce automatic layout"::
	new(Nodes, chain),
	send(D?graphicals, for_all,
	     if(message(@arg1, instance_of, rdf_any),
		message(Nodes, append, @arg1))),
	send(Nodes?head, layout, 2, 40,
	     iterations := 200,
	     area := D?visible,
	     network := Nodes).

copy_layout(D, From:rdf_diagram, Subst:prolog) :->
	"Copy the layout from another windows"::
	send(D?graphicals, for_some,
	     message(D, copy_location, @arg1, From, prolog(Subst))).

copy_location(_D, Obj:graphical, From:rdf_diagram, Subst:prolog) :->
	"Copy location of a single RDF object"::
	(   send(Obj, instance_of, rdf_any)
	->  (   get(Obj, name, Name),
	        find(From, Name, Subst, FromObj)
	    ->  format('Copied location of ~p from ~p~n', [Obj, FromObj]),
		get(FromObj, center, Center),
		send(Obj, center, Center)
	    )
	;   true
	).
	
find(D, Name, _Subst, Obj) :-
	get(D, member, Name, Obj).
find(D, Name, Subst, Obj) :-
	member(Name=AltName, Subst),
	atom_concat('_:', AltName, FullAltName),
	get(D, member, FullAltName, Obj).
find(D, Name, Subst, _) :-
	format('Cannot find ~w in ~p, Subst =~n', [Name, D]),
	pp(Subst),
	fail.


:- pce_end_class(rdf_diagram).


		 /*******************************
		 *	       SHAPES		*
		 *******************************/

:- pce_begin_class(rdf_connection, tagged_connection,
		   "Represents a triple").

:- pce_global(@rdf_link, new(link(link, link,
				  line(0,0,0,0,second)))).

initialise(C, Gr1:graphical, Gr2:graphical, Pred:name) :->
	"Create from predicate"::
	send_super(C, initialise, Gr1, Gr2, @rdf_link),
	send(C, tag, rdf_label(Pred, italic)).

ideal_length(C, Len:int) :<-
	"Layout: compute the desired length"::
	get(C, height, H),
	(   H < 40
	->  get(C, tag, Tag),
	    get(Tag, width, W),
	    Len is W + 30
	;   Len = 40
	).

:- pce_end_class(rdf_connection).

:- pce_begin_class(rdf_any(name), figure,
		   "Represent an RDF resource or literal").

handle(w/2, 0,	 link, north).
handle(w,   h/2, link, east).
handle(w/2, h,	 link, south).
handle(0,   h/2, link, west).

initialise(F, Ref:name) :->
	"Create visualisation"::
	send_super(F, initialise),
	send(F, name, Ref).
	
connect(F, Pred:name, Object:graphical) :->
        new(_C, rdf_connection(F, Object, Pred)).

:- pce_global(@rdf_any_recogniser,
	      make_rdf_any_recogniser).

make_rdf_any_recogniser(G) :-
	new(M1, move_gesture(left)),
	new(M2, move_network_gesture(left, c)),
	new(P, popup_gesture(new(Popup, popup))),
	Gr = @arg1,
	send(Popup, append,
	     menu_item(layout, message(Gr, layout))),
	new(G, handler_group(M1, M2, P)).

event(F, Ev:event) :->
	(   send_super(F, event, Ev)
	->  true
	;   send(@rdf_any_recogniser, event, Ev)
	).

:- pce_end_class(rdf_any).


:- pce_begin_class(move_network_gesture, move_gesture,
		   "Move network of connected graphicals").

variable(outline,	box,	get,
	 "Box used to indicate move").
variable(network,	chain*, both,
	 "Stored value of the network").
variable(origin,	point,  get,
	 "Start origin of network").

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The  gesture maintains  an outline, the selection to  be moved and the
positon  where  the move orginiated.    The outline  itself is given a
normal  move_gesture to make  it move on  dragging.  This move_gesture
should operate on the same button and modifier.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

initialise(G, B:[button_name], M:[modifier]) :->
	send(G, send_super, initialise, B, M),
	send(G, slot, outline, new(Box, box(0,0))),
	send(G, slot, origin, point(0,0)),
	send(Box, texture, dotted),
	send(Box, recogniser, move_gesture(G?button, G?modifier)).

initiate(G, Ev:event) :->
	get(Ev, receiver, Gr),
	get(Gr, device, Dev),
	get(G, outline, Outline),
	get(Gr, network, Network),
	send(G, network, Network),
	new(Union, area(0,0,0,0)),
	send(Network, for_all, message(Union, union, @arg1?area)),
	send(G?origin, copy, Union?position),
	send(Outline, area, Union),
	send(Union, done),
	send(Dev, display, Outline),
	ignore(send(Ev, post, Outline)).

drag(G, Ev) :->
	send(Ev, post, G?outline).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Terminate.   First undisplay the outline.  Next  calculate by how much
the outline has been dragged and move all objects  of the selection by
this amount.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

terminate(G, Ev:event) :->
	ignore(send(G, drag, Ev)),
	get(G, outline, Outline),
	send(Outline, device, @nil),
	get(Outline?area?position, difference, G?origin, Offset),	
	get(G, network, Network),
	send(Network, for_all, message(@arg1, relative_move, Offset)),
	send(G, network, @nil).

:- pce_end_class(move_network_gesture).



:- pce_begin_class(rdf_label, text,
		   "Label for an RDF relation").

variable(resource, name, get, "Represented predicate").

initialise(L, Pred:name, Font:font) :->
	local_name(Pred, Label),
	send_super(L, initialise, Label, center, Font),
	send(L, slot, resource, Pred),
	send(L, background, @default).

:- pce_global(@rdf_label_recogniser,
	      make_rdf_label_recogniser).

make_rdf_label_recogniser(G) :-
	new(G, handler_group),
	send(G, append,
	     handler(area_enter, message(@receiver, identify))),
	send(G, append,
	     handler(area_exit, message(@receiver, report, status, ''))),
	send(G, append, popup_gesture(new(P, popup))),
	send_list(P, append,
		  [ menu_item(copy,
			      message(@display, copy, @arg1?resource))
		  ]).

event(F, Ev:event) :->
	(   send_super(F, event, Ev)
	->  true
	;   send(@rdf_label_recogniser, event, Ev)
	).

identify(L) :->
	send(L, report, status, '%s', L?resource).
	     
:- pce_end_class.



:- pce_begin_class(rdf_resource, rdf_any,
		   "Represent an RDF resource").

initialise(F, Ref:name) :->
	"Create visualisation"::
	send_super(F, initialise, Ref),
	send(F, display, ellipse(100, 50), point(-50,-25)),
	send(F, display, new(T, rdf_label(Ref, normal))),
	send(T, center, point(0,0)).

type(F, Type:name) :->
	send(F, display, new(TL, rdf_label(Type, small))),
	send(TL, center, point(0,12)),
	get(F, member, ellipse, E),
	send(E, shadow, 2).

identify(F) :->
	send(F, report, status, 'Resource %s', F?name).

:- pce_end_class(rdf_resource).


:- pce_begin_class(rdf_literal, rdf_any,
		   "Represent an RDF literal value").

initialise(F, Value:prolog) :->
	"Create visualisation"::
	literal_label(Value, Label),
	atom_concat('__lit:', Label, Id),
	send_super(F, initialise, Id),
	send(F, display, new(B, box)),
	send(B, fill_pattern, colour(grey80)),
	send(B, pen, 0),
	send(F, display, new(T, text(Label, center))),
	send(T, center, point(0,0)),
	send(F, fit).

literal_label(literal(Value0), Value) :- !,
	literal_label(Value0, Value).
literal_label(xml(Value0), Value) :- !,
	literal_label(Value0, Value).
literal_label(Value, Value) :-
	atomic(Value), !.
literal_label(Value, Label) :-
	term_to_atom(Value, Label).

literal_name(Value, Name) :-
	literal_label(Value, Label),
	atom_concat('__lit:', Label, Name).

fit(F) :->
	"Make box fit contents"::
	get(F, member, text, Text),
	get(Text?area, clone, Area),
	send(Area, increase, 3),
	get(F, member, box, Box),
	send(Box, area, Area).

:- pce_end_class(rdf_literal).






		 /*******************************
		 *	    PRIMITIVES		*
		 *******************************/

subject_name(rdf(Name0, _, _), Name) :-
	resource_name(Name0, Name).
predicate_name(rdf(_, Name0, _), Name) :-
	resource_name(Name0, Name).
object_resource(rdf(_, _, Name0), Name) :-
	resource_name(Name0, Name).
object_literal(rdf(_,_,Literal), Literal).


resource_name(Name, Name) :-
	atom(Name), !.
resource_name(rdf:Local, Name) :- !,	% known namespaces
	concat_atom([rdf, :, Local], Name).
resource_name(NS:Local, Name) :- !,
	atom_concat(NS, Local, Name).
resource_name(node(Anon), Name) :-	% not for predicates
	atom_concat('_:', Anon, Name).

is_type(rdf(_, rdf:type, _)) :- !.	% our parser
is_type(rdf(_, Pred, _)) :-		% our parser
	atom(Pred),
	rdf_name_space(NS),
	atom_concat(NS, type, Pred), !.

%	local_name(+Resource, -Label)
%	
%	Return easy readable local name

local_name(Resource, Local) :-
	sub_atom(Resource, _, _, A, #),
	sub_atom(Resource, _, A, 0, Local),
	\+ sub_atom(Local, _, _, _, #), !.
local_name(Resource, Local) :-
	atom_concat('rdf:', Local, Resource), !.
local_name(Resource, Local) :-
	file_base_name(Resource, Local),
	Local \== ''.
local_name(Resource, Resource).
	
