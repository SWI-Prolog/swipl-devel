/*  $Id$

    Part of SWI-Prolog SGML/XML parser

    Author:  Jan Wielemaker
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/SWI-Prolog/
    Copying: LGPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2002 SWI, University of Amsterdam. All rights reserved.
*/

:- module(rdf_diagram,
	  [
	  ]).
:- use_module(library(pce)).
:- use_module(library(pce_tagged_connection)).
:- use_module(library(autowin)).
:- use_module(library(print_graphics)).
:- use_module(rdf_parser).		% get access to declared namespaces

:- pce_begin_class(rdf_diagram, auto_sized_picture,
		   "Show set of RDF triples in a window").
:- use_class_template(print_graphics).

initialise(D, Label:[name]) :->
	send_super(D, initialise, Label),
	send(D, scrollbars, both),
	send(D, fill_popup).

fill_popup(D) :->
	send(D, popup, new(P, popup)),
	send_list(P, append,
		  [ menu_item(layout, message(D, layout)),
		    gap,
		    menu_item(print, message(D, print))
		  ]).

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
	send(D, display, new(Gr, rdf_literal(Value)),
	     D?visible?center).


triples(D, Triples:prolog) :->
	"Show disgram from Prolog triples"::
	send(D, clear),
	forall(member(T, Triples),
	       send(D, append, T)),
	send(D, layout).


:- pce_group(layout).

layout(D) :->
	"Produce automatic layout"::
	(   get(D?graphicals, head, First)
	->  send(First, layout)
	;   true
	).

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

:- pce_begin_class(rdf_any(name), figure,
		   "Represent an RDF resource or literal").

:- pce_global(@rdf_link, new(link(link, link,
				  line(0,0,0,0,second)))).

handle(w/2, 0,	 link, north).
handle(w,   h/2, link, east).
handle(w/2, h,	 link, south).
handle(0,   h/2, link, west).

initialise(F, Ref:name) :->
	"Create visualisation"::
	send_super(F, initialise),
	send(F, name, Ref).
	
connect(F, Pred:name, Object:graphical) :->
	new(C, tagged_connection(F, Object, @rdf_link)),
	send(C, tag, rdf_label(Pred, italic)).

:- pce_global(@rdf_any_recogniser,
	      new(move_gesture(left))).

event(F, Ev:event) :->
	(   send_super(F, event, Ev)
	->  true
	;   send(@rdf_any_recogniser, event, Ev)
	).

:- pce_end_class(rdf_any).


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
	
