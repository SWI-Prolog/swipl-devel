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

:- module(pce_meta,
	  [ pce_to_method/2,		% +Spec, -BehaviourObject
	    isa_class/2,		% ?SubClassName, ?SuperClassName
	    current_class/2,		% ?ClassName, ?ClassObject
	    to_class_name/2,		% +NameOrClass, -ClassName
	    implements/2,		% ?Class, ?SendOrGet(?Name)
	    implements/3,		% idem, -Method
	    pce_to_pl_type/2,		% +PceType, -PrologType
	    type_accepts_function/1	% +Type
	  ]).
:- use_module(library(pce)).
:- require([ pce_error/1
	   , chain_list/2
	   , get_chain/3
	   , maplist/3
	   ]).


		 /*******************************
		 *	INTERACTION SUPPORT	*
		 *******************************/

pce_to_method(->(Receiver, Selector), Method) :- !,
	(   atom(Receiver)
	->  get(@pce, convert, Receiver, class, Class),
	    get(Class, send_method, Selector, Method)
	;   object(Receiver)
	->  get(Receiver, send_method, Selector, tuple(_, Method))
	).
pce_to_method(<-(Receiver, Selector), Method) :- !,
	(   atom(Receiver)
	->  get(@pce, convert, Receiver, class, Class),
	    get(Class, get_method, Selector, Method)
	;   object(Receiver)
	->  get(Receiver, get_method, Selector, tuple(_, Method))
	).
pce_to_method((Receiver-Selector), Method) :- !,
	(   atom(Receiver)
	->  get(@pce, convert, Receiver, class, Class),
	    get(Class, instance_variable, Selector, Method)
	;   object(Receiver),
	    get(Receiver, attribute, Method)
	->  true
	;   object(Receiver),
	    get(Receiver, class, Class),
	    get(Class, instance_variable, Selector, Method)
	).
pce_to_method(ClassName, Class) :-
	atom(ClassName),
	get(@pce, convert, ClassName, class, Class), !.
pce_to_method(Method, Method) :-
	object(Method).



		 /*******************************
		 *	     BASE-STUFF		*
		 *******************************/

%	isa_class(+Sub, +Super)
%
%	Succeeds if Sub is Super or below Super.  Can be used with any
%	instantiation.  If class is instantiated the super-chain is
%	followed.

isa_class(Class, Super) :-
	ground(Class), !,
	gen_super(Class, Super).
isa_class(Class, Super) :-
	current_class(Class, ClassObject),
	current_class(Super, SuperObject),
	send(ClassObject, is_a, SuperObject).

gen_super(Class, Class).
gen_super(Class, Super) :-
	current_class(Class, ClassObject),
	get(ClassObject, super_class, SuperObject),
	current_class(Super0, SuperObject),
	gen_super(Super0, Super).


%	current_class(?Name, ?Class)
%
%	Convert between name and class object.  Insufficient instantation
%	enumerates the classes.

:- dynamic
	current_class_cache/2.


make_current_class :-
	retractall(current_class_cache(_,_)),
	send(@classes, for_all,
	     message(@prolog, assert_class, @arg1, @arg2)),
	send(class(class), created_message,
	     message(@prolog, assert_class, @arg2?name, @arg2)).

assert_class(Name, Object) :-
	assert(current_class_cache(Name, Object)).

:- initialization
	make_current_class.

current_class(Class, ClassObject) :-
	current_class_cache(Class, ClassObject).
current_class(Class, ClassObject) :-
	pce_prolog_class(Class),
	\+ current_class_cache(Class, _),
	get(@pce, convert, Class, class, ClassObject).


%	to_class_name(+AtomOrClass, -ClassName)
%
%	Convert a name or class-object into a class name

to_class_name(Name0, Name) :-
	atom(Name0), !,
	(   current_class(Name0, _)
	->  Name = Name0
	;   pce_error(no_class(Name0))
	).
to_class_name(ClassObj, Name) :-
	object(ClassObj),
	send(ClassObj, instance_of, class), !,
	get(ClassObj, name, Name).

%	implements(?Class, SendOrGet(?Method), [Method])
%	
%	True if Class implements the method.  If class is a variable,
%	backtracking yields all classes
%
%	`What' may be wrapped in self/1 or root/1.  Self/1 returns only
%	those classes that have a non-inherited implementation of the
%	method, while root/1 returns only those classes for which there
%	is no super-class implementing the requested method.

implements(Class, What) :-
	implements(Class, What, _).

implements(Class, self(What), Method) :-
	implements(Class, What, Method),
	get(Method, context, ClassObject),
	get(ClassObject, name, Class).
implements(Class, root(What), Method) :-
	implements(Class, self(What), Method),
	(   send(Method, has_get_method, inherited_from)
	->  \+ get(Method, inherited_from, _)
	;   true
	).
implements(Class, send(Name), Method) :-
	current_class(Class, ClassObject),
	(   atom(Name)
	->  get(ClassObject, send_method, Name, Method)
	;   isa_class(Class, Super),
	    current_class(Super, SuperObject),
	    (	get_chain(SuperObject, send_methods, Methods)
	    ;	get_chain(SuperObject, instance_variables, Methods)
	    ),
	    member(Method, Methods),
	    get(Method, name, Name),
	    get(ClassObject, send_method, Name, Method) 	% not overruled
	).
implements(Class, get(Name), Method) :-
	current_class(Class, ClassObject),
	(   atom(Name)
	->  get(ClassObject, get_method, Name, Method)
	;   isa_class(Class, Super),
	    current_class(Super, SuperObject),
	    (	get_chain(SuperObject, get_methods, Methods)
	    ;	get_chain(SuperObject, instance_variables, Methods)
	    ),
	    member(Method, Methods),
	    get(Method, name, Name),
	    get(ClassObject, get_method, Name, Method)
	).


		 /*******************************
		 *	       TYPES		*
		 *******************************/

%	pce_to_pl_type(+PceType, -PrologType)
%	Convert an XPCE Type object to our type-checkers type-logic.
%

pce_to_pl_type(Type, Pl) :-
	get(Type, kind, Kind),
	pce_to_pl_type(Kind, Type, Pl0),
	type_supers(Pl0, Type, Pl).

type_supers(Pl0, Type, Pl) :-
	get(Type, supers, Supers),
	Supers \== @nil, !,
	chain_list(Supers, SuperList),
	maplist(pce_to_pl_type, SuperList, PlSupers),
	list_to_or([Pl0|PlSupers], Pl).
type_supers(Pl, _, Pl).

pce_to_pl_type(class, Type, Pl) :-
	get(Type, context, Context),
	(   atom(Context)
	->  Class = Context
	;   get(Context, name, Class)
	),
	class_type(Class, Pl).
pce_to_pl_type(class_object, _, and(sub(object), not(sub(function)))).
pce_to_pl_type(unchecked,    _, or(sub(object), integer)).
pce_to_pl_type(any,          _, and(or(sub(object), integer),
				    not(sub(function)))).
pce_to_pl_type(int,	     _, integer).
pce_to_pl_type(char,	     _, integer(0,255)).
pce_to_pl_type(int_range,    T, integer(Low, High)) :-
	get(T, context, tuple(Low0, High0)),
	to_range_boundary(Low0, Low),
	to_range_boundary(High0, High).
pce_to_pl_type(real_range,   T, float(Low, High)) :-
	get(T, context, tuple(Low0, High0)),
	to_range_boundary(Low0, Low),
	to_range_boundary(High0, High).
pce_to_pl_type(event_id,     _, or(integer, atom)).
pce_to_pl_type(value,	     T, value(V)) :-
	get(T, context, V).
pce_to_pl_type(name_of,	     T, Pl) :-
	get_chain(T, context, Atoms),
	list_to_value_or(Atoms, Pl).
pce_to_pl_type(member,	     T, PlType) :-
	get(T, context, T2),
	pce_to_pl_type(T2, PlType).
pce_to_pl_type(value_set,    T, Pl) :-
	get_chain(T, context, Elements),
	list_to_value_or(Elements, Pl).
pce_to_pl_type(compound,     T, PlType) :-
	get_chain(T, context, Supers),
	maplist(pce_to_pl_type, Supers, PlSupers),
	list_to_or(PlSupers, PlType).
pce_to_pl_type(alias,	     T, PlType) :-
	get(T, context, T2),
	pce_to_pl_type(T2, PlType).
pce_to_pl_type(alien,	     _, integer).

class_type(name,   atom) :- !.
class_type(number, integer) :- !.
class_type(real,   float) :- !.
class_type(Class,  sub(Class)).

to_range_boundary(N, unbound) :-
	unbound(N), !.
to_range_boundary(N, N).

unbound(@nil).
unbound(1073741823).
unbound(-1073741824).


list_to_or([X], X) :- !.
list_to_or([A|B], or(A, C)) :-
	   list_to_or(B, C).

list_to_value_or([X], value(X)) :- !.
list_to_value_or([A|B], or(value(A), T)) :-
	list_to_value_or(B, T).

%	type_accepts_function(+Type)
%
%	Succeeds if Type accepts function arguments

type_accepts_function(Type) :-
	send(type(function), specialised, Type).
