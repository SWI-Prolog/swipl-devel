/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/

:- module(dia_generate, [source/2, new_term/2]).
:- use_module(library(pce)).
:- use_module(proto).
:- require([ append/3
	   , between/3
	   , chain_list/2
	   , concat/3
	   , delete/3
	   , forall/2
	   , genarg/3
	   , get_chain/3
	   , list_to_set/2
	   , maplist/3
	   , member/2
	   , memberchk/2
	   , portray_object/2
	   , subtract/3
	   , term_to_atom/2
	   ]).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Format description:

dialog(<ref>,
       [ <attribute> := <value>,
	 ...
       ]).

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- dynamic
	reference/2.

source(Dialog, Source) :-
	retractall(reference(_,_)),
	findall(Attr := Value, source_attribute(Dialog, Attr, Value), List),
	reference(ObjectRef, Dialog),
	get(Dialog, name, Id),
	Source =.. [dialog, Id, [object := ObjectRef | List]],
	retractall(reference(_,_)).

:- discontiguous
	source_attribute/3.

		 /*******************************
		 *	       PARTS		*
		 *******************************/

source_attribute(Dialog, parts, TheItems) :-
	get_chain(Dialog, graphicals, Grs),
	findall('$aref'(RefName) := NewTerm,
		(member(Gr, [Dialog|Grs]),
		 \+ get(Dialog, overlay, Gr),
		 new_term(Gr, NewTerm),
		 get(Gr, name, GrName),
		 prolog_variable_name(GrName, RefName),
		 asserta(reference('$aref'(RefName), Gr))),
		Items),
	generate_unique_variable_names(Items, [], TheItems).

generate_unique_variable_names([], _, []).
generate_unique_variable_names([Part|Parts], Done, [NewPart|NewParts]) :-
	varname(Part, Ref),
	(   occurs(Done, Ref, DoneTimes),
	    DoneTimes > 0
	->  Id is DoneTimes + 1,
	    concat(Ref, Id, NewRef),
	    set_varname(Part, NewRef, NewPart),
	    generate_unique_variable_names(Parts, [Part|Done], NewParts)
	;   occurs(Parts, Ref, Times),
	    (   Times == 0
	    ->  NewPart = Part,
		generate_unique_variable_names(Parts, [Part|Done], NewParts)
	    ;   occurs(Done, Ref, DoneTimes),
		Id is DoneTimes + 1,
		concat(Ref, Id, NewRef),
		set_varname(Part, NewRef, NewPart),
		generate_unique_variable_names(Parts, [Part|Done], NewParts)
	    )
	).
	    
varname('$aref'(Name) := _, Name).

set_varname('$aref'(Old) := Value, Var, '$aref'(Var) := Value) :-
	retract(reference('$aref'(Old), Gr)),
	assert(reference('$aref'(Var), Gr)).

occurs([], _, 0).
occurs([H|T], Var, Times) :-
	varname(H, Var), !,
	occurs(T, Var, T0),
	Times is T0 + 1.
occurs([_H|T], Var, Times) :-
	occurs(T, Var, Times).


		 /*******************************
		 *	  MODIFICATIONS		*
		 *******************************/

source_attribute(Dialog, modifications, Modifications) :-
	get_chain(Dialog, graphicals, Grs),
	findall(Ref := Atts,
		(member(Gr, Grs),
		 \+ get(Dialog, overlay, Gr),
		 object_attributes(Gr, Atts),
		 Atts \== [],
		 reference(Ref, Gr)),
		Modifications).


		 /*******************************
		 *	   LAYOUT INFO		*
		 *******************************/

source_attribute(Dialog, layout, Layout) :-
	get_chain(Dialog, graphicals, Grs),
	get(Dialog, overlay, Overlay),
	delete(Grs, Overlay, Items),
	findall(Alignment, alignment(Items, Alignment), P0),
	maplist(canonise_alignment, P0, P1),
	list_to_set(P1, P2),
	maplist(symbolic_pair, P2, Pairs),
	aligned(P2, Aligned),
	subtract(Items, Aligned, Explicit),
	findall(position(Ref, Point),
		(member(I, Explicit),
		 reference(Ref, I),
		 get_argument(I, position, Point)),
		Positions),
	append(Pairs, Positions, Layout).

alignment(Items, Alignment) :-
	pair(I1, I2, Items),
	alignment(I1, I2, Alignment).

alignment(I1, I2, below(I1, I2)) :- get(I2, below, I1).
alignment(I1, I2, above(I1, I2)) :- get(I2, above, I1).
alignment(I1, I2, left(I1, I2))  :- get(I2, left,  I1).
alignment(I1, I2, right(I1, I2)) :- get(I2, right, I1).
	
pair(I1, I2, [I1|L]) :-
	member(I2, L).
pair(I1, I2, [_|T]) :-
	pair(I1, I2, T).

canonise_alignment(left(I1, I2), right(I2, I1)) :- !.
canonise_alignment(above(I1, I2), below(I2, I1)) :- !.
canonise_alignment(A, A).

aligned(Pairs, Items) :-
	aligned_(Pairs, I0),
	sort(I0, Items).		% unqiue

aligned_([], []).
aligned_([H|T], [I1,I2|R]) :-
	H =.. [_, I1, I2],
	aligned_(T, R).

symbolic_pair(Term, Refs) :-
	Term =.. L,
	maplist(to_reference, R, L),
	Refs =.. R.

to_reference(Symbol, Ref) :-
	reference(Symbol, Ref), !.
to_reference(X, X).


		 /*******************************
		 *	      POPUPS		*
		 *******************************/

source_attribute(Dialog, popups, Popups) :-
	get_chain(Dialog, graphicals, Grs),
	findall(Ref := Popup,
		(member(Gr, Grs),
		 \+ get(Dialog, overlay, Gr),
		 popup(Gr, Popup),
		 reference(Ref, Gr)),
		Popups),
	Popups \== [].

popup(Gr, [ popup := NewTerm, Attributes ]) :-
	(   send(Gr, instance_of, menu),
	    get(Gr, kind, cycle)
	->  fail
	;   get(Gr, popup, Popup),
	    Popup \== @nil,
	    new_term(Popup, NewTerm),
	    object_attributes(Popup, Attributes)
	).


		 /*******************************
		 *	      BEHAVIOUR		*
		 *******************************/

source_attribute(Dialog, behaviour, Behaviour) :-
	get_chain(Dialog, graphicals, Grs),
	findall(Ref := Dyns,
		(member(Gr, Grs),
		 \+ get(Dialog, overlay, Gr),
		 behaviour(Gr, Dyns),
		 Dyns \== [],
		 reference(Ref, Gr)),
		Behaviour),
	Behaviour \== [].


source_attribute(Dialog, initialise, Initialise) :-
	get(Dialog, behaviour_model, Model),
	get(Model?graphicals, find_all,
	    message(@arg1, instance_of, msg_init_port), Chain),
	chain_list(Chain, Ports),
	Ports \== [],
	findall(Name := Message,
		(member(Port, Ports),
		 port_message(Port, Message),
		 get(Port, name, Name)),
		Initialise).


behaviour(I, Dyns) :-
	get(I, behaviour_model, Object),
	get(Object?graphicals, find_all,
	    message(@arg1, instance_of, msg_event_port),
	    Chain),
	chain_list(Chain, EventPorts),
	findall(Name := Message,
		(member(Port, EventPorts),
		 get(Port, name, Name),
		 port_message(Port, Message)),
		Dyns).


port_message(Port, Message) :-
	get(Port, connections, Cs),
	get(Cs, find_all, @arg1?type == activate, Activations),
	chain_list(Activations, List),
	maplist(activation_message, List, Messages),
	(   Messages = [Message]
	->  true
	;   Message =.. [and|Messages]	% order?
	).
	

activation_message(C, Message) :-
	get(C, to, CallPort),
	get(CallPort, name, Selector),
	get(CallPort, object, Object),
	receiver(Object, Receiver),
	activation_arguments(C, Args),
	TheMessage =.. [message, Receiver, Selector | Args],
	activation_conditions(C, Conditions),
	(   Conditions == []
	->  Message = TheMessage
	;   RawCond =.. [and | Conditions],
	    simplify(RawCond, Cond),
	    Message = if(Cond, TheMessage)
	).


activation_conditions(C, Conditions) :-
	(   get(C, connections, Cs)
	->  get(Cs, find_all, @arg1?type == condition, Cds),
	    chain_list(Cds, List),
	    maplist(condition, List, Conditions)
	;   Conditions = []
	).


condition(C, Cond) :-
	activation_message(C, Cond).


receiver(Object, Receiver) :-
	get(Object, ui_object, Self), !,
	(   reference(Receiver, Self)
	->  true
	;   Self = @Atom,
	    atom(Atom)
	->  Receiver = Self
	;   pce_to_prolog(Self, Receiver)
	).
receiver(Object, Receiver) :-
	get(Object, connections, Cs),
	get(Cs, find, @arg1?type == expansion, E), !,
	get(E, from, Port),
	get(Port, object, O2),
	get(Port, name, Selector),
	receiver(O2, R2),
	activation_arguments(E, Args),
	Obtainer =.. [?, R2, Selector | Args],
	simplify(Obtainer, Receiver).


activation_arguments(C, Args) :-
	(   get(C, connections, Cs)
	->  get(Cs, find_all, @arg1?type == argument, A0),
	    chain_list(A0, L0),
	    maplist(activation_argument, L0, ParmArgs),
	    sort(ParmArgs, Sorted),
	    argument_list(Sorted, PositionArgs, NamedArgs),
	    append(PositionArgs, NamedArgs, Args)
	;   Args = []
	).
	
argument_list([], [], []).
argument_list([N := A|T], PosArgs, [N := A|R]) :-
	atom(N), !,
	argument_list(T, PosArgs, R).
argument_list([_ := A|T], [A|R], NamedArgs) :-
	argument_list(T, R, NamedArgs).


activation_argument(A, Parm := Arg) :-
	get(A, from, Port),
	get(A, parameter, Parm),
	(   send(Port, instance_of, msg_constant_port)
	->  get(Port, name, TermAtom),
	    term_to_atom(Arg, TermAtom)
	;   send(Port, instance_of, msg_get_port)
	->  get(Port, object, Object),
	    get(Port, name, Selector),
	    receiver(Object, Receiver),
	    activation_arguments(A, Args),
	    Obtainer =.. [?, Receiver, Selector | Args],
	    simplify(Obtainer, Arg)
	).


simplify(X?self, S) :- !,
	simplify(X, S).
simplify(and(X), S) :- !,
	simplify(X, S).
simplify(X, S) :-
	simplify_forward(X, S), !.
simplify(X, X).


%	map message(X, forward, a, b, c).  Is this ok?  What if not
%	all arguments are handled?

simplify_forward(Message, Simple) :-
	Message =.. [message, _R, forward | Args],
	make_mapping(Args, Mapping),
	map_term(Mapping, Message, Mapped),
	Mapped =.. [message, Code, forward | Args],
	\+ argleft(Mapped), !,
	Simple = Code.


argleft(@Ref) :-
	concat(arg, N, Ref),
	get(@pce, convert, N, int, _), !.
argleft(Atomic) :-
	atomic(Atomic), !,
	fail.
argleft(Term) :-
	genarg(_, Term, Arg),
	argleft(Arg), !.


make_mapping(Args, Mapping) :-
	make_mapping(Args, 1, Mapping).

make_mapping([], _, []).
make_mapping([H|T], N, [@ArgN = H|R]) :-
	concat(arg, N, ArgN),
	NN is N + 1,
	make_mapping(T, NN, R).


%	map_term(+[From = To, ...], +Term, -MappedTerm).

map_term(Mapping, Term, NewTerm) :-
	memberchk(Term = NewTerm, Mapping), !.
map_term(Mapping, Term, NewTerm) :-
	functor(Term, Name, Arity),
	functor(NewTerm, Name, Arity),
	map_argument(1, Arity, Mapping, Term, NewTerm).


map_argument(N, M, Mapping, Term, NewTerm) :-
	arg(N, Term, A1), !,
	arg(N, NewTerm, A2),
	map_term(Mapping, A1, A2),
	NN is N + 1,
	map_argument(NN, M, Mapping, Term, NewTerm).
map_argument(_, _, _, _, _).


		 /*******************************
		 *	     NEW-TERM		*
		 *******************************/

new_term(Object, Term) :-
	proto(Object, Proto),
	proto_term(Proto, Functor, ProtoArgs), !,
	new_proto_term(Proto, Functor, Args),
	maplist(new_argument(Object), Args, Values),
	(   Values == []
	->  Term = new(Functor)
	;   Term =.. [Functor|Values]
	),
	(   length(Args, L),
	    length(ProtoArgs, L)
	->  true			% no more arguments
	;   new(Tmp, Term),
	    (	equal_on_remaining_proto_args(Values, ProtoArgs, Object, Tmp)
	    ->	!,
		free(Tmp)
	    ;	free(Tmp),
		fail
	    )
	).
new_term(Object, Term) :-
	portray_object(Object, Term).
	

new_argument(_, _Name := Value, Value) :- !.
new_argument(O, Name, Value) :-
	get_argument(O, Name, Value).

new_proto_term(Proto, Functor, Args) :-
	proto_term(Proto, Functor, ProtoArgs),
	delete_optional_new_arguments(ProtoArgs, Args).

delete_optional_new_arguments([], []).
delete_optional_new_arguments([[_]|_], []).
delete_optional_new_arguments([[X]|T], [X|R]) :- !,
	delete_optional_new_arguments(T, R).
delete_optional_new_arguments([H|T], [H|R]) :-
	delete_optional_new_arguments(T, R).

equal_on_remaining_proto_args([], Args, O1, O2) :-
	equal_on_remaining_proto_args(Args, O1, O2).
equal_on_remaining_proto_args([_|V], [_|T], O1, O2) :-
	equal_on_remaining_proto_args(V, T, O1, O2).

equal_on_remaining_proto_args([], _, _).
equal_on_remaining_proto_args([[Sel]|T], O1, O2) :- !,
	get_argument(O1, Sel, V1),
	get_argument(O2, Sel, V2),
	V1 = V2,
	equal_on_remaining_proto_args(T, O1, O2).
equal_on_remaining_proto_args([_|T], O1, O2) :-
	equal_on_remaining_proto_args(T, O1, O2).


		 /*******************************
		 *	 OBJECT ATTRIBUTES	*
		 *******************************/

object_attributes(Object, List) :-
	new_term(Object, NewTerm),
	new(Tmp, NewTerm),
	findall(Attr := Value,
		object_attribute(Object, Tmp, Attr, Value),
		List),
	free(Tmp).


object_attribute(Object, Tmp, Attr, Value) :-
	proto(Object, Proto),
	proto_source_attribute(_Mode, Proto, Attribute),
	(   atom(Attribute)
	->  Attr = Attribute
	;   functor(Attribute, Attr, _Arity)
	),
	get_argument(Object, Attr, Value),
	send(Tmp, compute),
	(   get_argument(Tmp, Attr, DefValue),
	    DefValue == Value
	->  fail			% donot generate argument
	;   send(Tmp, Attr, Value)
	).
object_attribute(Object, _Tmp, append, Value) :- % menu_items of a menu
	send(Object, instance_of, menu),
	get(Object, members, Chain),
	chain_list(Chain, Items),
	findall(MenuItemTerm,
		(member(Item, Items),
		 new_term(Item, NewTerm),
		 simplify_item(NewTerm, MenuItemTerm)),
		Value).


simplify_item(menu_item(Value), Value) :-
	atomic(Value), !.
simplify_item(Item, Item).


delete_attribute(fixed_reference).
delete_attribute(fixed_alignment).
delete_attribute(has_popup).
delete_attribute(popup_items(_)).
delete_attribute(members).

attribute_mapping(reference_x, reference).
attribute_mapping(reference_y, reference).

map_attribute(A, M) :-
	attribute_mapping(A, M), !.
map_attribute(A, A).

proto_source_attribute(Mode, Proto, Attr) :-
	findall(A, attribute(Mode, Proto, A), S0),
	maplist(map_attribute, S0, S1),
	list_to_set(S1, S2),
	member(Attr, S2),
	\+ delete_attribute(Attr).


		 /*******************************
		 *        BEHAVIOUR-MODEL	*
		 *******************************/

/*
source_attribute(Target, behaviour_model, ModelSource) :-
	get(Target, hypered, behaviour_model, Model).
*/




		 /*******************************
		 *	   GET-ARGUMENT		*
		 *******************************/

get_argument(Object, Name, PortMessage) :-
	get(Object, Name, Message),
	send(Message, instance_of, message),
	send(Object, has_get_method, proto), % dubious test
	get(Object, behaviour_model, Model),
	get(Message, receiver, Object), !,
	get(Model, member, Name, Port),
	(   port_message(Port, PortMessage)
	->  true
	;   PortMessage = @default
	).
get_argument(Object, Name, Value) :-
	get(Object, Name, PceValue),
	pce_to_prolog(PceValue, Value).

pce_to_prolog(Atomic, Atomic) :-
	atomic(Atomic), !.
pce_to_prolog(@Atom, @Atom) :-
	atom(Atom), !.
pce_to_prolog(Ref, Term) :-
	portray_object(Ref, Term).	% TBD


		 /*******************************
		 *    PROPOSE A VARIABLE-NAME	*
		 *******************************/

translate_char(Char, Char) :-
	between(0'a, 0'z, Char), !.
translate_char(Char, Char) :-
	between(0'A, 0'Z, Char), !.
translate_char(_, 0'_).

prolog_variable_name(GrName, VarName) :-
	new(Str, string(GrName)),
	send(Str, strip),
	get(Str, size, Size),
	End is Size - 1,
	forall(between(0, End, Index),
	       (get(Str, character, Index, C0),
		translate_char(C0, C1),
		send(Str, character, Index, C1))),
	get(Str, label_name, Str2),
	send(Str2, translate, ' ', '_'),
	get(Str2, character, 0, First),
	(   between(0'a, 0'z, First)
	->  Up is First + 0'A - 0'a,
	    send(Str2, character, 0, Up)
	;   true
	),
	get(Str2, value, VarName).
	
proto(Object, Proto) :-
	send(Object, has_get_method, proto), !,
	get(Object, proto, Proto).
proto(Object, Proto) :-
	get(Object, class_name, Proto).
