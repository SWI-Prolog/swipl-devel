/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/

:- module(dia_meta, [class_of_type/2]).
:- use_module(library(pce)).
:- require([ chain_list/2
	   , member/2
	   ]).

:- pce_extend_class(object).

dia_argument_type(Obj, Selector:name, Type:type) :<-
	get(Obj, get_method, Selector, tuple(_, Implementation)),
	get(Implementation, return_type, Type).

:- pce_end_class.


class_of_type(Type, Class) :-
	get(Type, kind, class),
	get(Type, context, Class).
class_of_type(Type, Class) :-
	get(Type, kind, class_object),
	get(Type, context, Class).
class_of_type(Type, @object_class) :-
	get(Type, kind, any).
class_of_type(Type, Class) :-
	get(Type, kind, alias),
	get(Type, context, Type2),
	class_of_type(Type2, Class).
class_of_type(Type, Class) :-
	get(Type, kind, member),
	get(Type, context, Type2),
	class_of_type(Type2, Class).
class_of_type(Type, Class) :-
	get(Type, supers, Supers),
	Supers \== @nil,
	chain_list(Supers, List),
	member(T, List),
	class_of_type(T, Class).
