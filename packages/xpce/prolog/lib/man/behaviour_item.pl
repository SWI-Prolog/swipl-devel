/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/

:- module(pce_behaviour_item, []).
:- use_module(library(pce)).
:- require([ forall/2
	   , get_chain/3
	   , member/2
	   ]).

:- pce_begin_class(behaviour_item, text_item,
		   "Text item for entering XPCE behaviour or class").

completions(_MI, Spec:'char_array|tuple', Matches:chain) :<-
	new(Matches, chain),
	(   send(Spec, instance_of, char_array)
	->  send(@classes, for_all,
		 if(message(@arg1, prefix, Spec),
		    message(Matches, append, @arg1)))
	;   get(Spec, first, ClassPart),
	    get(Spec, second, SelPart),
	    new(Re, regex('\s *\(\w+\)\s *\(->\|<-\|-\)')),
	    send(Re, match, ClassPart),
	    get(Re, register_value, ClassPart, 1, name, ClassName),
	    get(Re, register_value, ClassPart, 2, name, Arrow),
	    get(@pce, convert, ClassName, class, Class),
	    new(Code, if(message(@arg1?name, prefix, SelPart),
			 message(Matches, append, @arg1?name))),
	    (	Arrow == (-)
	    ->	send(Class?instance_variables, for_all, Code)
	    ;	Arrow == (->)
	    ->	forall(super_class(Class, TheClass),
		       (send(TheClass?send_methods, for_all, Code),
			send(TheClass?instance_variables, for_all,
			     if(message(@arg1, send_access), Code))))
	    ;	forall(super_class(Class, TheClass),
		       (send(TheClass?get_methods, for_all, Code),
			send(TheClass?instance_variables, for_all,
			     if(message(@arg1, get_access), Code))))
	    )
	),
	send(Matches, unique),
	send(Matches, sort).

:- dynamic done_class/1.
	
super_class(Class, Super) :-
	retractall(done_class(_)),
	super_class_(Class, Super).
super_class(_, _) :-
	retractall(done_class(_)),
	fail.

super_class_(Class, _) :-
	done_class(Class), !,
	fail.
super_class_(Class, Class) :-
	asserta(done_class(Class)).
super_class_(Class, Super) :-
	get(Class, super_class, ThisSuper),
	ThisSuper \== @nil,
	super_class_(ThisSuper, Super).
super_class_(Class, Delegate) :-
	get_chain(Class, delegate, List),
	member(Var, List),
	get(Var, type, Type),
	class_of_type(Type, DelClass),
	\+ done_class(DelClass),
	super_class_(DelClass, Delegate).

class_of_type(Type, Class) :-
	get(Type, kind, class),
	get(Type, context, Class).
class_of_type(Type, Class) :-
	get_chain(Type, supers, Supers),
	member(Super, Supers),
	class_of_type(Super, Class).


split_completion(_MI, Value:char_array, RVal:'char_array|tuple') :<-
	new(Re, regex('\s *\(\w+\s *\(->\|<-\|-\)\)\(\w*\)')),
	(   send(Re, match, Value)
	->  get(Re, register_value, Value, 1, Class),
	    get(Re, register_value, Value, 3, Selector),
	    new(RVal, tuple(Class, Selector))
	;   RVal = Value
	).


selection(MI, S:'behaviour|class*') :<-
	"Get selection as behaviour or class"::
	get(MI, get_super, selection, Text),
	(   get(Text, size, 0)
	->  S = @nil
	;   new(Re, regex('\s *\(\w+\)\s *\(->\|<-\|-\)\s *\(\w+\)')),
	    (   (   send(Re, match, Text)
		->  get(Re, register_value, Text, 1, class, Class),
		    get(Re, register_value, Text, 2, name, Access),
		    get(Re, register_value, Text, 3, name, Selector),
		    super_class(Class, TheClass),
		    (	Access == (-)
		    ->	get(TheClass, instance_variable, Selector, S)
		    ;	Access == (->)
		    ->	get(TheClass, send_method, Selector, S)
		    ;	get(TheClass, get_method, Selector, S)
		    )
		;   get(@pce, convert, Text, class, S)
		)
	    ;   send(MI, error, cannot_convert_text, Text, 'behaviour|class'),
		fail
	    )
	).


:- pce_end_class.

/*
test :-
	send(behaviour_item(manpce, '',
			 message(@prolog, manpce, @arg1)),
	     open).
*/
