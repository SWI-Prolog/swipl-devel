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
	(   get(Obj, source, Location)
	->  use_module(library(pce_emacs)),
	    Goal = start_emacs, Goal,	% fool xref
	    send(@emacs, goto_source_location, Location)
	;   send(Obj, report, warning, 'Can''t find source')
	).


method(Method, Method) :-
	object(Method), !.
method(->(ClassName, Selector), Method) :- !,
	get(@pce, convert, ClassName, class, Class),
	get(Class, send_method, Selector, Method).
method(<-(ClassName, Selector), Method) :- !,
	get(@pce, convert, ClassName, class, Class),
	get(Class, get_method, Selector, Method).
method(ClassName, Class) :-
	atom(ClassName),
	get(@pce, convert, ClassName, class, Class).

	
