/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

:- module(pce_edit,
	  [ editpce/1
	  ]).
:- use_module(library(pce)).


editpce(Spec) :-
	method(Spec, Obj),
	(   get(Obj, source, Location),
	    Location \== @nil
	->  use_module(library(pce_emacs)),
	    Goal = start_emacs, Goal,	% fool xref
	    send(@emacs, goto_source_location, Location)
	;   send(Obj, report, warning, 'Can''t find source')
	).


method(Object, Object) :-
	object(Object),
	send(Object, has_get_method, source), !.
method(Object, Class) :-
	object(Object), !,
	get(Object, class, Class).
method(ClassName, Class) :-
	atom(ClassName), !,
	get(@pce, convert, ClassName, class, Class).
method(->(Receiver, Selector), Method) :- !,
	(   atom(Receiver)
	->  get(@pce, convert, Receiver, class, Class),
	    get(Class, send_method, Selector, Method)
	;   object(Receiver)
	->  get(Receiver, send_method, Selector, tuple(_, Method))
	).
method(<-(Receiver, Selector), Method) :- !,
	(   atom(Receiver)
	->  get(@pce, convert, Receiver, class, Class),
	    get(Class, get_method, Selector, Method)
	;   object(Receiver)
	->  get(Receiver, get_method, Selector, tuple(_, Method))
	).

	
