/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
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

	
