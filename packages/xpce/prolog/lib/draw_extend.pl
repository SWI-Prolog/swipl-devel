/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/

:- module(draw_extend,
	  [ draw_begin_shape/3,
	    draw_end_shape/0
	  ]).
:- use_module(library(pce)).


:- pce_begin_class(draw_shape_class, class, "Handle class-level stuff").

variable(hidden_attributes, chain*, get, "Masked attributes").
variable(recognisers,	    chain*, get, "Event-handling recognisers").

initialise(Class, Name, Super) :->
	send(Class, send_super, initialise, Name, Super),
	(   get(Class, super_class, SuperClass),
	    send(SuperClass, instance_of, draw_shape_class)
	->  send(Class, slot, hidden_attributes,
		 SuperClass?hidden_attributes?copy),
	    send(Class, slot, recognisers,
		 SuperClass?recognisers)
	;   send(Class, slot, hidden_attributes, new(chain)),
	    send(Class, slot, recognisers, new(chain))
	).


hidden_attribute(Class, Attr:name) :->
	"Register a hidden attribute"::
	get(Class, hidden_attributes, Hidden),
	send(Hidden, add, Attr).


recogniser(Class, Recogniser:recogniser) :->
	"Register (prepend) a recogniser"::
	get(Class, recognisers, Recognisers),
	send(Recognisers, add, Recogniser).

:- pce_end_class.


draw_begin_shape(Name, Super, Summary, Recognisers) :-
	ensure_prefix(Name, PceName),
	ensure_prefix(Super, PceSuper),
	make_pce_super(PceSuper),
	pce_begin_class(PceName, PceSuper, Summary),
	forall(member(R, Recognisers),
	       send(@class, recogniser, R)).

make_pce_super(DrawClass) :-
	get(@pce, convert, DrawClass, class, _), !.
make_pce_super(DrawClass) :-
	concat(draw_, PceClass, DrawClass),
	get(@pce, convert, PceClass, class, _),
	new(NewClass, draw_shape_class(DrawClass, PceClass)).
