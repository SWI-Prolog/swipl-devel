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

:- module(pce_realise,
	  [ pce_register_class/1,	% +ClassName
	    pce_extended_class/1,	% +ClassName
	    pce_realise_class/1,	% +ClassName
	    pce_prolog_class/1,		% ?ClassName
	    pce_prolog_class/2,		% ?ClassName, ?SuperName
	    pce_bind_send_method/6,
	    pce_bind_get_method/7
	  ]).

:- use_module(pce_principal).
:- use_module(pce_global).
:- require([ pce_error/1
	   , call/3
	   , ignore/1
	   , strip_module/3
	   , notrace/1
	   ]).

:- pce_global(@class, new(var(class, class, @nil))).

		 /*******************************
		 *	      REGISTER		*
		 *******************************/

pce_register_class(ClassName) :-
	check_loaded_class(ClassName).


		 /*******************************
		 *	 EXTENDING CLASSES	*
		 *******************************/

pce_extended_class(ClassName) :-
	(   get(@classes, member, ClassName, Class),
	    send(Class, instance_of, class)
	->  attach_class_attributes(ClassName),
	    send(Class, clear_cache),
	    resolve_method_message(Msg),
	    send(Class, resolve_method_message, Msg)
	;   true
	).


		 /*******************************
		 *	      RELOAD		*
		 *******************************/

%	check_loaded_class(+ClassName)
%
%	If the class is already defined, we are dealing with redefinition
%	and have to take action immediately.

check_loaded_class(ClassName) :-
	get(@classes, member, ClassName, Class),
	send(Class, instance_of, class), !,
	pce_realise_class(ClassName).
check_loaded_class(_).
		

		 /*******************************
		 *	  REALISE-CLASS		*
		 *******************************/

%	pce_realise_class(+ClassName)
%	Creates `ClassName' from the compiled representation.

pce_realise_class(ClassName) :-
	pce_class(ClassName, MetaClassName, SuperName, _, _, _),
	MetaClassName \== -,
	create_class(ClassName, MetaClassName, SuperName, Class), !,
	resolve_method_message(Msg),
	send(Class, resolve_method_message, Msg),
	attach_class_attributes(ClassName),
	(   cache_table(TableName),
	    get(Class, slot, TableName, Table),
	    get(Table, size, Size),
	    Size > 0
	->  delete_prolog_methods(Class)
	;   true
	),
	ignore(get(Class, send_method, in_event_area, _)). % HACK!

cache_table(send_table).
cache_table(get_table).
cache_table(send_methods).
cache_table(get_methods).

attach_class_attributes(ClassName) :-
	get(@classes, member, ClassName, Class),
	pce_class(ClassName, _, _,
		  Variables,
		  ClassVariables,
		  Directives),
	attach_variables(Variables, Class),
	attach_class_variables(ClassVariables, Class),
	run_directives(Directives, Class),
	fail ; true.


%	pce_prolog_class(?ClassName, [?SuperName])
%
%	Is true if ClassName refers to a class defined in Prolog

pce_prolog_class(ClassName) :-
	pce_prolog_class(ClassName, _SuperName).
pce_prolog_class(ClassName, SuperName) :-
	pce_class(ClassName, _MetaClassName, SuperName,
		  _Variables,
		  _ClassVariables,
		  _Directives),
	SuperName \== '-'.		% extended class


%	create_class(+ClassName, +MetaClassName, +SuperName, -Class)
%
%	Creates class `ClassName' below `SuperName'.  Succeeds
%	(for redefinition) if the class already existed with the
%	same super-class.

create_class(ClassName, MetaClassName, Super, Class) :-
	get(@classes, member, ClassName, Class),
	send(Class, instance_of, class), !,
	get(Class, super_class, SuperClass),
	(   (   Super == @nil,
		SuperClass == @nil
	    ;   SuperClass \== @nil,
		get(SuperClass, name, Super)
	    )
	->  true
        ;   pce_error(superclass_not_changed(ClassName))
    	),
	(   (   MetaClassName == @default
	    ;	get(Class, class, MetaClass),
		get(MetaClass, name, MetaClassName)
	    )
	->  true
	;   pce_error(metaclass_not_changed(ClassName))
	),
	send(Class, clear_cache).
create_class(ClassName, MetaClassName, SuperName, Class) :-
	(   get(@pce, convert, SuperName, class, Super)
	->  true
	;   pce_error(superclass_not_exist(SuperName, ClassName))
	),
	(   MetaClassName == @default
	->  get(Super, sub_class, ClassName, Class)
	;   Term =.. [MetaClassName, ClassName, Super],
	    new(Class, Term)
	).


%	attach_variables(+VariableList, +Class)
%	
%	Attach the instance variables.  Error checking is done by the
%	XPCE kernel.

attach_variables([], _).
attach_variables([V|T], Class) :-
	send(Class, instance_variable, V),
	attach_variables(T, Class).

%	attach_class_variables(+ClassVarList, +Class)
%	
%	Attach the class variables

attach_class_variables([], _).
attach_class_variables([R|T], Class) :-
	attach_class_variable(Class, R),
	attach_class_variables(T, Class).

attach_class_variable(Class, class_variable(Name, Default, Type, Summary)) :-
	new(_, class_variable(Class, Name, Default, Type, Summary)).

run_directives([], _).
run_directives(Directives, Class) :-
	send(@class, assign, Class),
	run_directives(Directives).

run_directives([]).
run_directives([H|T]) :-
	H,
	run_directives(T).
	
delete_prolog_methods(Class) :-
	get(Class, name, ClassName),
	(   pce_lazy_send_method(Selector, ClassName, _Binder),
	    send(Class, delete_send_method, Selector),
	    fail
	;   pce_lazy_get_method(Selector, ClassName, _Binder),
	    send(Class, delete_get_method, Selector),
	    fail
	;   true
	).


		 /*******************************
		 *    LAZY METHOD RESOLUTION	*
		 *******************************/

%	resolve_method_message(-Message)
%
%	Create the @pce_resolve_method_message that is called by XPCE
%	whenever there is a method to be resolved.

resolve_method_message(X) :-
	X = @pce_resolve_method_message,
	(   object(X)
	->  true
	;   new(X, message(@prolog, call, '_bind_lazy', @arg1, @arg2, @arg3))
	).

pce_ifhostproperty(prolog(swi),
		   (:- '$hide'('_bind_lazy', 3))).

'_bind_lazy'(Type, ClassName, Selector) :-
%	format('bind_lazy(~p, ~p, ~p)~n', [Type, ClassName, Selector]),
	notrace(do_bind_lazy(Type, ClassName, Selector)).

do_bind_lazy(send, ClassName, @default) :- !,
	get(@pce, convert, ClassName, class, Class),
	(   pce_lazy_send_method(Selector, ClassName, Binder),
	    \+ send(Class, bound_send_method, Selector),
	    call_binder(ClassName, Selector, Binder),
	    fail ; true
	).
do_bind_lazy(send, ClassName, Selector) :-
	pce_lazy_send_method(Selector, ClassName, Binder),
	call_binder(ClassName, Selector, Binder).
do_bind_lazy(get, ClassName, @default) :- !,
	get(@pce, convert, ClassName, class, Class),
	(   pce_lazy_get_method(Selector, ClassName, Binder),
	    \+ send(Class, bound_get_method, Selector),
	    call_binder(ClassName, Selector, Binder),
	    fail ; true
	).
do_bind_lazy(get, ClassName, Selector) :-
	pce_lazy_get_method(Selector, ClassName, Binder),
	call_binder(ClassName, Selector, Binder).


call_binder(ClassName, Selector, Binder) :-
	build_in_binder(Binder, ClassName, Selector), !.
call_binder(ClassName, Selector, Binder) :-
	call(Binder, ClassName, Selector).

build_in_binder(bind_send_method(T, D, L, G), C, S) :- !,
	pce_bind_send_method(T, D, L, G, C, S).
build_in_binder(bind_send_method(T, D, L), C, S) :- !,
	pce_bind_send_method(T, D, L, @default, C, S).
build_in_binder(bind_send_method(T, D), C, S) :- !,
	pce_bind_send_method(T, D, @default, @default, C, S).
build_in_binder(bind_send_method(T), C, S) :- !,
	pce_bind_send_method(T, @default, @default, @default, C, S).

build_in_binder(bind_get_method(R, T, D, L, G), C, S) :- !,
	pce_bind_get_method(R, T, D, L, G, C, S).
build_in_binder(bind_get_method(R, T, D, L), C, S) :- !,
	pce_bind_get_method(R, T, D, L, @default, C, S).
build_in_binder(bind_get_method(R, T, D), C, S) :- !,
	pce_bind_get_method(R, T, D, @default, @default, C, S).
build_in_binder(bind_get_method(R, T), C, S) :- !,
	pce_bind_get_method(R, T, @default, @default, @default, C, S).


pce_bind_send_method(Types, Doc, Loc, Group, ClassName, Selector) :-
	get(@pce, convert, ClassName, class, Class),
	pce_method_implementation('->', ClassName, Selector, Message),
	send(Class, send_method,
	     send_method(Selector, Types, Message, Doc, Loc, Group)).
	
pce_bind_get_method(RType, Types, Doc, Loc, Group, ClassName, Selector) :-
	get(@pce, convert, ClassName, class, Class),
	pce_method_implementation('<-', ClassName, Selector, Message),
	send(Class, get_method,
	     get_method(Selector, RType, Types, Message, Doc, Loc, Group)).
