/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

:- module(draw_importpl,
	  [ realise_drawing/2		% +Device, +Term
	  ]).
:- use_module(library(pce)).
:- require([ maplist/3
	   ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Support library to import drawings from   PceDraw  into applications. To
use this library, make a drawing in PceDraw,   and export it as a Prolog
term, either by dragging the icon  at   the  top-right corner of PceDraw
onto a PceEmacs window running in Prolog mode, or by selecting the `Copy
To ClipBoard' option from the popup  on   this  image and pasting in any
text editor.

The  term  returned  is  what  is  needed  as  the  second  argument  to
realise_drawing/2. The drawing is placed at  the same coordinates as the
initial drawing. If realised the drawing is  realised on a device object
(or figure object), the method ->reference  may   be  used  to place the
reference point of the device at the top-left corner of the bounding box
of the drawing.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%	realise_drawing(Device, Drawing)
%
%	Recreates the drawing at the indicates device.  Drawing is a term
%	as created by the PceDraw package library('draw/exportpl.pl').  This
%	file contains a definition of the format of the Drawing term.

realise_drawing(Device, drawing(Objects)) :-
	draw(Objects, Device).

draw([], _).
draw([display(Term, Point)|T], Device) :-
	term_to_object(Term, Object),
	send(Device, display, Object, Point),
	draw(T, Device).
draw([display(Term)|T], Device) :-
	term_to_object(Term, Object),
	send(Device, display, Object),
	draw(T, Device).
draw([connect(Term)|T], Device) :-
	term_to_connection(Term, _Connection),
	draw(T, Device).
draw([compound(Term, Contents, Point)|T], Device) :-
	term_to_object(Term, SubDev),
	realise_drawing(SubDev, Contents),
	send(Device, display, SubDev, Point),
	draw(T, Device).

	
term_to_object(Term+Attribute, Object) :- !,
	term_to_object(Term, Object),
	Attribute =.. [Selector|PlArgs],
	maplist(term_to_object, PlArgs, Args),
	Goal =.. [send, Object, Selector | Args],
	Goal.
term_to_object(Atomic, Atomic) :-
	atomic(Atomic), !.
term_to_object(new(Object, Term), Object) :- !,
	new(Object, Term).
term_to_object(Term, Object) :-
	new(Object, Term).


term_to_connection(Term+Attribute, Connection) :- !,
	term_to_connection(Term, Connection),
	Attribute =.. [Selector|PlArgs],
	maplist(term_to_object, PlArgs, Args),
	Goal =.. [send, Connection, Selector | Args],
	Goal.
term_to_connection(Term, Connection) :-
	Term =.. [Class, From, To, FH, TH],
	make_link(FH, TH, Link),
	NewTerm =.. [Class, From, To, Link],
	new(Connection, NewTerm),
	attach_handle(From, FH),
	attach_handle(To, TH).

attach_handle(Graphical, handle(_, _, _, Name)) :-
	get(Graphical, handle, Name, _), !.
attach_handle(Graphical, Handle) :-
	send(Graphical, handle, Handle).

:- dynamic
	link_store/3.

link_store(link, link, @default).

make_link(FH, TH, Link) :-
	handle_kind(FH, FName),
	handle_kind(TH, TName),
	(   link_store(FName, TName, Link)
	->  true
	;   new(Link, link(FName, TName)),
	    send(Link, lock_object, @on),
	    asserta(link_store(FName, TName, Link))
	).
	

handle_kind(handle(_X, _Y, Kind, _Name), Kind).

