/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/

:- module(msg_load,
	  [ load_dialog/2
	  ]).
:- meta_predicate load_dialog(-, :).

:- use_module(library(pce)).
:- use_module(proto).
:- use_module(behaviour).
:- require([ checklist/2
	   , concat/3
	   , forall/2
	   , ignore/1
	   , is_list/1
	   , member/2
	   , memberchk/2
	   , random/3
	   , send_list/3
	   , strip_module/3
	   , term_to_atom/2
	   ]).


load_dialog(Dialog, Id) :-
	strip_module(Id, Module, TheId),
	(   current_predicate(dialog, Module:dialog(_,_))
	->  TheModule = Module
	;   current_predicate(dialog, TheModule:dialog(_,_)) % tbd: setof
	),
	make_dialog(Dialog, TheModule, TheId).

make_dialog(Dialog, Module, TheId) :-
	Module:dialog(TheId, Attributes),
	memberchk(object := Dialog, Attributes),
	do(make_dialog_item,    parts,         Attributes),
	do(modify,              modifications, Attributes),
	do(popups,	        popups,	       Attributes),
	do(layout(Dialog),      layout,        Attributes),
	send(Dialog?overlay, expose),
	send(Dialog, layout),
	send(Dialog, fit_size),
	(   memberchk(behaviour := Behaviour, Attributes),
	    Behaviour \== []
	->  new(Model, msg_model_editor(Dialog)),
	    send(Model, open),
	    behaviour(Module, Model, Behaviour)
	;   true
	).

do(Goal, Attribute, List) :-
	memberchk(Attribute := Value, List), !,
	(   is_list(Value)
	->  checklist(Goal, Value)
	;   call(Goal, Value)
	).
do(_, _, _).

		 /*******************************
		 *	     DIALOG		*
		 *******************************/

make_dialog(Dialog, Term) :-
	Term =.. [Class|Args],
	atom_concat(dia_target_, Class, ProtoClass),
	NewTerm =.. [ProtoClass|Args],
	new(Dialog, NewTerm).


		 /*******************************
		 *	      ITEMS		*
		 *******************************/

proto_class(dialog, dia_target_dialog) :- !.
proto_class(Class, ProtoClass) :-
	atom_concat('dia_proto_', Class, ProtoClass).

proto_name(label(reporter, _),	reporter) :- !.
proto_name(label(_, image(_)),	image) :- !.
proto_name(label(_, _), label) :- !.	% Lourens van de Meij.  Thanks
proto_name(menu(_, choice),	choice) :- !.
proto_name(menu(_, toggle),	toggle) :- !.
proto_name(menu(_, cycle),	cycle) :- !.
proto_name(Term, Proto) :-
	proto_term(Proto, Class, Args),
	functor(Term, Class, Arity),
	length(Args, Arity).

resizable(Proto) :-
	proto(Proto, _, Atts, _),
	member(can_resize := @on, Atts), !.

make_dialog_item(Var := NewTerm) :-
	NewTerm =.. [Class|Args],
	proto_class(Class, ProtoClass),
	ProtoTerm =.. [ProtoClass|Args],
	new(Var, ProtoTerm),
	proto_name(NewTerm, ProtoName),
	(   send(Var, has_send_method, proto)
	->  send(Var, proto, ProtoName)
	;   true
	),
	(   resizable(ProtoName)
	->  send(Var, can_resize, @on)
	;   true
	).


		 /*******************************
		 *	  MODIFICATIONS		*
		 *******************************/

modify(Ref := List) :-
	modify(List, Ref).

modify([], _).
modify([Attr := Value|T], Ref) :-
	send_list(Ref, Attr, Value),
	modify(T, Ref).

		 /*******************************
		 *	      POPUPS		*
		 *******************************/

popups(Ref := [ PopupSelector := NewTerm, Attributes ]) :-
	new(Popup, NewTerm),
	modify(Popup := Attributes),
	send(Ref, PopupSelector, Popup).


		 /*******************************
		 *	      LAYOUT		*
		 *******************************/

layout(Dialog, below(I1, I2)) :- !,
	attach(Dialog, I1, I2),
	send(I1, below, I2).
layout(Dialog, right(I1, I2)) :- !,
	attach(Dialog, I1, I2),
	send(I1, right, I2).
layout(Dialog, position(I1, Pos)) :-
	send(Dialog, display, I1, Pos).
layout(Dialog, area(I1, area(X,Y,W,H))) :-
	send(I1, auto_align, @off),
	send(I1, do_set, X, Y, W, H),
	send(Dialog, display, I1).

attach(Dialog, I1, _I2) :-
	get(I1, device, Dialog), !.
attach(Dialog, _I1, I2) :-
	get(I2, device, Dialog), !.
attach(Dialog, _I1, I2) :-
	send(Dialog, append, I2).


		 /*******************************
		 *	  BEHAVIOUR MODEL	*
		 *******************************/

behaviour(_Module, E, List) :-
	get(E, member, model, Model),
	forall(member(Item := _, List),
	       add_model_item(Model, Item)),
	forall(member(Item := EventPorts, List),
	       add_model_item(Model, Item, EventPorts)).


random_position(Obj, point(PX, PY)) :-
	get(Obj, area, area(X, Y, W, H)),
	MX is X + W,
	MY is Y + H,
	random(X, MX, PX),
	random(Y, MY, PY).


add_model_item(Model, Item) :-
	object_name(Item, Name), 
	random_position(Model, Pos),
	send(Model, display, new(Object, msg_object(Name)), Pos),
	send(Object, ui_object, Item).

object_name(Item, Name) :-
	send(Item, has_get_method, name), !,
	get(Item, name, Name).
object_name(@Ref, Name) :-
	atom_concat(@, Ref, Name).


add_model_item(Model, Item, EventPorts) :-
	checklist(add_event_port(Model, Item), EventPorts).


add_event_port(Model, Item, PortName := Message) :-
	model_item(Model, Item, Object),
	send(Object, add_port, event, PortName, point(0,0)), % TBD: position
	get(Object, member, PortName, Port),
	add_message(Model, Port, Message).


add_message(Model, Port, Sequence) :-
	Sequence =.. [and | Messages], !,
	checklist(add_message(Model, Port), Messages).
add_message(Model, Port, if(Cond, Message)) :- !,
	add_message(Model, Port, Message, Connection),
	add_message(Model, Connection, Cond).
add_message(Model, Port, Message) :-
	add_message(Model, Port, Message, _Connection).


add_message(Model, Port, Message, Connection) :-
	Message =.. [message, Receiver, Selector | Args], !,
	model_item(Model, Receiver, ReceiverObject),
	send(ReceiverObject, add_port, send, Selector, point(0,0)), %TBD
	get(ReceiverObject, member, Selector, CallPort),
	make_relation(Connection, Port, CallPort),
	checklist(add_argument(Model, Connection), Args).
add_message(Model, Port, Message, Connection) :- % to be completed
	term_to_atom(Message, Atom),
	random_position(Model, Pos),
	send(Model, display, new(Obj, msg_object(Atom)), Pos),
	ignore(send(Obj, relink)),
	send(Obj, add_port, send, forward, point(0,0)),
	get(Obj?graphicals, find, @arg1?name == forward, ForwardPort),
	make_relation(Connection, Port, ForwardPort).


add_argument(Model, Connection, Obtainer) :-
	Obtainer =.. [?, Rec, Selector | Args], !,
	model_item(Model, Rec, RecObj),
	send(RecObj, add_port, get, Selector, point(0,0)), % TBD
	get(RecObj, member, Selector, ValuePort),
	make_relation(ArgRelation, ValuePort, Connection),
	checklist(add_argument(Model, ArgRelation), Args).
add_argument(Model, Connection, Item) :-
	get(Item, hypered, behaviour_model, _), !,
	add_argument(Model, Connection, Item?self).
add_argument(Model, Connection, Constant) :-
	(   get(Model?graphicals, find,
		and(message(@arg1, instance_of, msg_constant_port),
		    @arg1?value == Constant),
		ConstantPort)
	->  true
	;   term_to_atom(Constant, Atom),
	    send(Model, display,
		 new(ConstantPort, msg_constant_port(Atom)),
		 point(0,0)),
	    send(ConstantPort, value, Constant)
	),
	make_relation(_ArgRelation, ConstantPort, Connection).


model_item(_Model, Item, Object) :-
	get(Item, hypered, behaviour_model, Object), !.
model_item(Model, Item?Selector, Expansion) :- !,
	model_item(Model, Item, RecObj),
	send(RecObj, add_port, get, Selector, point(0,0)), % TBD
	get(RecObj, member, Selector, ValuePort),
	get(ValuePort, expand, Expansion).
model_item(Model, @Global, Object) :-
	atom(Global), !,
	send(Model, display, new(Object, msg_object('')), point(0,0)),
	get(Object, member, text, Text),
	send(Text, string, string('@%s', Global)),
	send(Object, ui_object, @Global).
model_item(Model, Item, Object) :-
	send(Item?device, instance_of, dia_target_dialog), !,
	add_model_item(Model, Item),
	get(Item, hypered, behaviour_model, Object).


make_relation(Connection, From, To) :-
	new(Connection, msg_connection(From, To)).
