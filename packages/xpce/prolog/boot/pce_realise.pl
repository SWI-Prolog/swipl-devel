/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1995 University of Amsterdam. All rights reserved.
*/

:- module(pce_realise,
	  [ pce_register_class/1,	% +ClassName
	    pce_extended_class/1,	% +ClassName
	    pce_realise_class/1,	% +ClassName
	    pce_prolog_class/1,		% ?ClassName
	    pce_prolog_class/2,		% ?ClassName, ?SuperName
	    pce_send_method_message/2,	% +Module:PlHead, -Message
	    pce_get_method_message/2,	% +Module:PlHead, -Message
	    pce_bind_send_method/8,
	    pce_bind_get_method/9
	  ]).

:- meta_predicate(pce_register_class(:)).
:- meta_predicate(pce_extended_class(:)).
:- use_module(pce_principal).
:- use_module(pce_global).
:- require([ pce_error/1
	   , call/3
	   , ignore/1
	   , strip_module/3
	   , notrace/1
	   ]).

:- dynamic
	class_module/2.			% ClassName, Module

:- pce_global(@class, new(var(class, class, @nil))).

		 /*******************************
		 *	      REGISTER		*
		 *******************************/

pce_register_class(Spec) :-
	strip_module(Spec, Module, ClassName),
	register_class(ClassName, Module),
	check_loaded_class(ClassName).

register_class(ClassName, Module) :-
	(   class_module(ClassName, Module)
	->  true
	;   asserta(class_module(ClassName, Module))
	).


		 /*******************************
		 *	 EXTENDING CLASSES	*
		 *******************************/

pce_extended_class(Spec) :-
	strip_module(Spec, Module, ClassName),
	register_class(ClassName, Module),
	(   get(@classes, member, ClassName, Class),
	    send(Class, instance_of, class)
	->  attach_class_attributes(ClassName, Module),
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
	class_module(ClassName, Module),
	Module:class(ClassName, MetaClassName, SuperName, _, _, _),
	MetaClassName \== -,
	create_class(ClassName, MetaClassName, SuperName, Class), !,
	resolve_method_message(Msg),
	send(Class, resolve_method_message, Msg),
	attach_class_attributes(ClassName, _Module),
	(   cache_table(TableName),
	    get(Class, slot, TableName, Table),
	    get(Table, size, Size),
	    Size > 0
	->  delete_prolog_methods(Class, Module)
	;   true
	),
%	delete_prolog_methods(Class, Module),
	ignore(get(Class, send_method, in_event_area, _)). % HACK!

cache_table(send_table).
cache_table(get_table).
cache_table(send_methods).
cache_table(get_methods).

attach_class_attributes(ClassName, Module) :-
	get(@classes, member, ClassName, Class),
	class_module(ClassName, Module),
	Module:class(ClassName, _, _,
		     Variables,
		     Resources,
		     Directives),
	attach_variables(Variables, Class),
	attach_resources(Resources, Class),
	run_directives(Directives, Class, Module),
	fail ; true.


%	pce_prolog_class(?ClassName, [?SuperName])
%
%	Is true if ClassName refers to a class defined in Prolog

pce_prolog_class(ClassName) :-
	pce_prolog_class(ClassName, _SuperName).
pce_prolog_class(ClassName, SuperName) :-
	class_module(ClassName, Module),
	Module:class(ClassName, _MetaClassName, SuperName,
		     _Variables,
		     _Resources,
		     _Directives).


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

%	attach_resources(+ResourceList, +Class)
%	
%	Attach the class resources.

attach_resources([], _).
attach_resources([R|T], Class) :-
	attach_resource(Class, R),
	attach_resources(T, Class).

attach_resource(Class, resource(Name, Type, Default)) :- !,
	attach_resource(Class, resource(Name, Type, Default, @default)).
attach_resource(Class, resource(Name, Type, Default, Summary)) :-
	send(Class, resource,
	     resource(Name, @default, Type, Default, Class, Summary)).

run_directives([], _, _).
run_directives(Directives, Class, Module) :-
	send(@class, assign, Class),
	run_directives(Directives, Module).

run_directives([], _).
run_directives([H|T], Module) :-
	call(Module:H),
	run_directives(T, Module).
	
delete_prolog_methods(Class, Module) :-
	get(Class, name, ClassName),
	(   find_data(Module:lazy_send_method(Selector, ClassName, Binder)),
	    send(Class, delete_send_method, Selector),
	    fail
	;   find_data(Module:lazy_get_method(Selector, ClassName, Binder)),
	    send(Class, delete_get_method, Selector),
	    fail
	;   true
	).


		 /*******************************
		 *	METHOD REGISTRATION	*
		 *******************************/

pce_ifhostproperty(use_predicate_references,
[
(pce_send_method_message(Head, PceRef) :-
	pce_predicate_reference(Head, PceRef)),
(pce_get_method_message(Head, PceRef) :-
	pce_predicate_reference(Head, PceRef))
],
[
(:- dynamic
	fwd_arg_cache/2),

(fwd_args(E, N, []) :-
	E > N, !),
(fwd_args(I, N, [@A|T]) :-
	concat(arg, I, A),
	NI is I + 1,
	fwd_args(NI, N, T)),
(forward_arguments(N, Args) :-
	fwd_arg_cache(N, Args), !),
(forward_arguments(N, Args) :-
	fwd_args(1, N, Args),
	assert(fwd_arg_cache(N, Args))),

(message_parms(Head, Selector, Sub, FwdArgs) :-
	functor(Head, Selector, Arity),
	FwdNArgs is Arity - Sub,
	forward_arguments(FwdNArgs, FwdArgs)),

(pce_send_method_message(Head0, Message) :-
	strip_module(Head0, M, Head),
	message_parms(Head, PredName, 1, FwdArgs),
	Message =.. [message, @prolog, call, M:PredName, @receiver|FwdArgs]),
(pce_get_method_message(Head0, Message) :-
	strip_module(Head0, M, Head),
	message_parms(Head, PredName, 2, FwdArgs),
 	Message =.. [?, @prolog, call, M:PredName, @receiver|FwdArgs])
]).


pce_send_method(Class, Selector, Head, Types, Doc, Loc, Group) :-
	pce_send_method_message(Head, Message),
	send(Class, send_method,
	     send_method(Selector, Types, Message, Doc, Loc, Group)).

pce_get_method(Class, Selector, RType, Head, Types, Doc, Loc, Group) :-
	pce_get_method_message(Head, Message),
	send(Class, get_method,
	     get_method(Selector, RType, Types, Message, Doc, Loc, Group)).


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
%	;   new(X, message(@prolog, '_bind_lazy', @arg1, @arg2, @arg3))
	;   pce_predicate_reference('_bind_lazy'(_,_,_,_), CPtr),
	    send(CPtr, name_reference, pce_resolve_method_message)
	).

find_data(Goal) :-
	current_predicate(_, Goal),
	Goal.

pce_ifhostproperty(prolog(swi),
		   (:- '$hide'('_bind_lazy', 4))).
pce_ifhostproperty(need_extern_declaration,
		   (:- extern('_bind_lazy'(+term, +term, +term, +term)))).

'_bind_lazy'(Type, _Class, ClassName, Selector) :-
%	format('bind_lazy(~p, ~p, ~p)~n', [Type, ClassName, Selector]),
	notrace(do_bind_lazy(Type, ClassName, Selector)).

do_bind_lazy(send, ClassName, @default) :- !,
	get(@pce, convert, ClassName, class, Class),
	(   class_module(ClassName, Module),
	    find_data(Module:lazy_send_method(Selector, ClassName, Binder)),
	    \+ send(Class, bound_send_method, Selector),
	    call_binder(Module, ClassName, Selector, Binder),
	    fail ; true
	).
do_bind_lazy(send, ClassName, Selector) :-
	class_module(ClassName, Module),
	find_data(Module:lazy_send_method(Selector, ClassName, Binder)),
	call_binder(Module, ClassName, Selector, Binder).
do_bind_lazy(get, ClassName, @default) :- !,
	get(@pce, convert, ClassName, class, Class),
	(   class_module(ClassName, Module),
	    find_data(Module:lazy_get_method(Selector, ClassName, Binder)),
	    \+ send(Class, bound_get_method, Selector),
	    call_binder(Module, ClassName, Selector, Binder),
	    fail ; true
	).
do_bind_lazy(get, ClassName, Selector) :-
	class_module(ClassName, Module),
	find_data(Module:lazy_get_method(Selector, ClassName, Binder)),
	call_binder(Module, ClassName, Selector, Binder).


call_binder(Module, ClassName, Selector, Binder) :-
	build_in_binder(Binder, Module, ClassName, Selector), !.
call_binder(Module, ClassName, Selector, Binder) :-
	call(Module:Binder, ClassName, Selector).

build_in_binder(bind_send_method(H, T, D, L, G), M, C, S) :- !,
	pce_bind_send_method(H, T, D, L, G, M, C, S).
build_in_binder(bind_send_method(H, T, D, L), M, C, S) :- !,
	pce_bind_send_method(H, T, D, L, @default, M, C, S).
build_in_binder(bind_send_method(H, T, D), M, C, S) :- !,
	pce_bind_send_method(H, T, D, @default, @default, M, C, S).
build_in_binder(bind_send_method(H, T), M, C, S) :- !,
	pce_bind_send_method(H, T, @default, @default, @default, M, C, S).

build_in_binder(bind_get_method(R, H, T, D, L, G), M, C, S) :- !,
	pce_bind_get_method(R, H, T, D, L, G, M, C, S).
build_in_binder(bind_get_method(R, H, T, D, L), M, C, S) :- !,
	pce_bind_get_method(R, H, T, D, L, @default, M, C, S).
build_in_binder(bind_get_method(R, H, T, D), M, C, S) :- !,
	pce_bind_get_method(R, H, T, D, @default, @default, M, C, S).
build_in_binder(bind_get_method(R, H, T), M, C, S) :- !,
	pce_bind_get_method(R, H, T, @default, @default, @default, M, C, S).


pce_bind_send_method(PredName, Types, Doc, Loc, Group,
		     Module, ClassName, Selector) :-
	arity_from_types(Types, send, Arity),
	functor(Head, PredName, Arity),
	pce_send_method_message(Module:Head, Message),
	get(@pce, convert, ClassName, class, Class),
	send(Class, send_method,
	     send_method(Selector, Types, Message, Doc, Loc, Group)).
	
pce_bind_get_method(RType, PredName, Types, Doc, Loc, Group,
		    Module, ClassName, Selector) :-
	arity_from_types(Types, get, Arity),
	functor(Head, PredName, Arity),
	pce_get_method_message(Module:Head, Message),
	get(@pce, convert, ClassName, class, Class),
	send(Class, get_method,
	     get_method(Selector, RType, Types, Message, Doc, Loc, Group)).

excess_arguments(send, 2).
excess_arguments(get,  3).

arity_from_types(@default, Kind, Args) :- !,
	excess_arguments(Kind, Args).
arity_from_types(Term, Kind, N) :-
	functor(Term, _, Arity),	% +1 for the receiver
	excess_arguments(Kind, Args),
	N is Arity + Args.
