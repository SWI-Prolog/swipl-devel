/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

:- module(pce_realise,
	  [ pce_register_class/1,	% +ClassName
	    pce_extended_class/1,	% +ClassName
	    pce_realise_class/1,	% +ClassName
	    pce_prolog_class/1,		% ?ClassName
	    pce_prolog_class/2		% ?ClassName, ?SuperName
	  ]).

:- use_module(pce_boot(pce_principal)).
:- use_module(pce_boot(pce_global)).
:- require([ ignore/1
	   , pce_error/1
	   , call/3
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
	get(@classes, member, ClassName, Class), !,
	attach_class_attributes(ClassName),
	send(Class, clear_cache),
	resolve_method_message(Msg),
	send(Class, resolve_method_message, Msg).
pce_extended_class(_).


		 /*******************************
		 *	      RELOAD		*
		 *******************************/

%	check_loaded_class(+ClassName)
%
%	If the class is already defined, we are dealing with redefinition
%	and have to take action immediately.

check_loaded_class(ClassName) :-
	get(@classes, member, ClassName, _), !,
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

attach_class_variable(Class, M:class_variable(Name, Def, Type, Summary)) :- !,
	classvar_default(Def, PceDef),
	new(_, M:class_variable(Class, Name, PceDef, Type, Summary)).
attach_class_variable(Class, ClassVar) :-
	attach_class_variable(Class, user:ClassVar).

%	Allow the default to be specified as below to deal with different
%	window systems.
%
%	[ windows(foo), 'X'(bar) ]

classvar_default(List, Default) :-
	is_list(List), !,
	(   get(@pce, window_system, WS),
	    Term =.. [WS,Default],
	    memberchk(Term, List)
	->  true
	;   List = [H|_],
	    compound(H),
	    H =.. [_, Default]
	->  true
	;   throw(error(type_error(class_variable_default, List), _))
	).
classvar_default(Default, Default).
		 

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

pce_ifhostproperty(prolog(swi),
('_bind_lazy'(Type, ClassName, Selector) :-
%	format('bind_lazy(~p, ~p, ~p)~n', [Type, ClassName, Selector]),
	notrace(do_bind_lazy(Type, ClassName, Selector))),
('_bind_lazy'(Type, ClassName, Selector) :-
	do_bind_lazy(Type, ClassName, Selector))).

do_bind_lazy(send, ClassName, @default) :- !,
	get(@pce, convert, ClassName, class, Class),
	(   send_binder(Selector, ClassName, Binder),
	    \+ send(Class, bound_send_method, Selector),
	    call_binder(ClassName, Selector, Binder),
	    fail ; true
	).
do_bind_lazy(send, ClassName, Selector) :-
	send_binder(Selector, ClassName, Binder),
	call_binder(ClassName, Selector, Binder).
do_bind_lazy(get, ClassName, @default) :- !,
	get(@pce, convert, ClassName, class, Class),
	(   get_binder(Selector, ClassName, Binder),
	    \+ send(Class, bound_get_method, Selector),
	    call_binder(ClassName, Selector, Binder),
	    fail ; true
	).
do_bind_lazy(get, ClassName, Selector) :-
	get_binder(Selector, ClassName, Binder),
	call_binder(ClassName, Selector, Binder).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This deals with possible redefined methods.  We distinguish two types of
`legal' method redefinition: using pce_extend_class/1  and redefining an
implementation inherited from a template.

Other cases are reported by checkpce/0.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

send_binder(Class, Sel, Binder) :-
	bagof(B, pce_lazy_send_method(Class, Sel, B), Binders),
	last(Binder, Binders).
get_binder(Class, Sel, Binder) :-
	bagof(B, pce_lazy_get_method(Class, Sel, B), Binders),
	last(Binder, Binders).

call_binder(ClassName, Selector, Binder) :-
	build_in_binder(Binder, ClassName, Selector), !.
call_binder(ClassName, Selector, Binder) :-
	call(Binder, ClassName, Selector).

build_in_binder(bind_send(Id, T, D, L, G), C, S) :- !,
	pce_bind_send(Id, T, D, L, G, C, S).
build_in_binder(bind_send(Id, T, D, L), C, S) :- !,
	pce_bind_send(Id, T, D, L, @default, C, S).
build_in_binder(bind_send(Id, T, D), C, S) :- !,
	pce_bind_send(Id, T, D, @default, @default, C, S).
build_in_binder(bind_send(Id, T), C, S) :- !,
	pce_bind_send(Id, T, @default, @default, @default, C, S).

build_in_binder(bind_get(Id, R, T, D, L, G), C, S) :- !,
	pce_bind_get(Id, R, T, D, L, G, C, S).
build_in_binder(bind_get(Id, R, T, D, L), C, S) :- !,
	pce_bind_get(Id, R, T, D, L, @default, C, S).
build_in_binder(bind_get(Id, R, T, D), C, S) :- !,
	pce_bind_get(Id, R, T, D, @default, @default, C, S).
build_in_binder(bind_get(Id, R, T), C, S) :- !,
	pce_bind_get(Id, R, T, @default, @default, @default, C, S).


pce_bind_send(Id, Types, Doc, Loc, Group, ClassName, Selector) :-
	get(@pce, convert, ClassName, class, Class),
	pce_method_implementation(Id, Message),
	send(Class, send_method,
	     send_method(Selector, Types, Message, Doc, Loc, Group)).
	
pce_bind_get(Id, RType, Types, Doc, Loc, Group, ClassName, Selector) :-
	get(@pce, convert, ClassName, class, Class),
	pce_method_implementation(Id, Message),
	send(Class, get_method,
	     get_method(Selector, RType, Types, Message, Doc, Loc, Group)).

		 /*******************************
		 *	       UTIL		*
		 *******************************/

%	SICStus got the arguments of last/2 the wrong way around!

pce_ifhostproperty(prolog(sicstus), [
(last(X, [X])),
(last(X, [_|T]) :-
	last(X, T))
				    ]).
