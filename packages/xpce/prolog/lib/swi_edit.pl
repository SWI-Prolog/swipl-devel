/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1998 University of Amsterdam. All rights reserved.
*/

:- module(pce_nedit, []).
:- use_module(library(pce_meta)).

:- multifile
	prolog_edit:locate/3,		% +Partial, -FullSpec, -Location
	prolog_edit:locate/2,
	prolog_edit:edit_source/1.
	  

		 /*******************************
		 *      FINDING LOCATIONS	*
		 *******************************/

prolog_edit:(locate(ClassName, class(ClassName), Location) :-
	locate(class(ClassName), Location)).

prolog_edit:locate(class(ClassName), Location) :-	% class(Name)
	atom(ClassName),
	get(@pce, convert, ClassName, class, Class),
	\+ get(Class, creator, built_in),
	source(Class, Location).
prolog_edit:locate(Object, Location) :-			% @reference
	source(Object, Location).
prolog_edit:locate(Object, Location) :-			% @reference
	object(Object),
	get(Object, class, Class),
	source(Class, Location).
prolog_edit:locate(send(Receiver, Selector), Location) :-
	receiver_class(Receiver, Class),
	(   implements(Class, send(Selector), Method),
	    source(Method, Location)
	;   method_source(Class, send(Selector), Location)
	).
prolog_edit:locate(get(Receiver, Selector), Location) :-
	receiver_class(Receiver, Class),
	(   implements(Class, get(Selector), Method),
	    source(Method, Location)
	;   method_source(Class, get(Selector), Location)
	).
prolog_edit:(locate(->(Receiver, Selector), Location) :- !,
	locate(send(Receiver, Selector), Location)).
prolog_edit:(locate(<-(Receiver, Selector), Location) :- !,
	locate(get(Receiver, Selector), Location)).


source(Object, [file(Path)|T]) :-
	object(Object),
	send(Object, has_get_method, source),
	get(Object, source, Loc),
	Loc \== @nil,
	get(Loc, file_name, FileName),
	absolute_file_name(FileName, Path),
	get(Loc, line_no, Line),
	(   integer(Line)
	->  T = [line(Line)]
	;   T = []
	).
	     
receiver_class(Object, Class) :-
	object(Object), !,
	get(Object, class_name, Class).
receiver_class(Class, Class).

method_source(ClassName, send(Selector), [file(File),line(Line)]) :-
	var(ClassName),
	pce_principal:pce_lazy_send_method(Selector, ClassName, Binder),
	arg(4, Binder, source_location(File, Line)),
	\+ get(@classes, member, ClassName, _).
method_source(ClassName, get(Selector), [file(File),line(Line)]) :-
	var(ClassName),
	pce_principal:pce_lazy_get_method(Selector, ClassName, Binder),
	arg(4, Binder, source_location(File, Line)),
	\+ get(@classes, member, ClassName, _).
					   

		 /*******************************
		 *	     EDIT HOOK		*
		 *******************************/

prolog_edit:edit_source(Location) :-
	memberchk(file(File), Location),
	memberchk(line(Line), Location), !,
	Goal = start_emacs, Goal,	% fool xref
	send(@emacs, goto_source_location, source_location(File, Line)).
prolog_edit:edit_source(Location) :-
	memberchk(file(File), Location),
	Goal = start_emacs, Goal,	% fool xref
	send(@emacs, goto_source_location, source_location(File)).




