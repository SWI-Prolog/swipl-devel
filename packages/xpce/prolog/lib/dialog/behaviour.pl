/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/

:- module(msg_behaviour_model, []).
:- use_module(library(pce)).
:- use_module(library(pce_template)).
:- require([ between/3
	   , chain_list/2
	   , default/3
	   , forall/2
	   , ignore/1
	   , member/2
	   , memberchk/2
	   , portray_object/2
	   , random/3
	   , send_list/3
	   , term_to_atom/2
	   ]).


:- pce_autoload(editable_text, library(pce_editable_text)).
:- pce_autoload(tagged_connection, library(pce_tagged_connection)).
:- use_module(library(pce_prompter)).
:- use_module(proto).
:- use_module(generate, [new_term/2]).
:- consult(meta).

relation(get,		argument,	argument).
relation(get,		object,		expansion).
relation(event,		send,  		activate).
relation(event,		get, 		activate).
relation(init,		send, 		activate).
relation(init,		get, 		activate).
relation(activate,	send,		condition).

link_attributes(constraint, [arrows := both]).
link_attributes(activate,   [arrows := second, pen := 2]).
link_attributes(argument,   [arrows := second, texture := dotted]).
link_attributes(expansion,  [arrows := second, texture := dashed]).
link_attributes(condition,  [pen := 2]).

port_type(get,		'\C-v').
port_type(event,	'\C-e').
port_type(send,		'\C-c').
port_type(constant,	@nil).
port_type(init,		'\C-i').

port_class(get,		msg_get_port).
port_class(event,	msg_event_port).
port_class(send,	msg_send_port).
port_class(constant,	msg_constant_port).
port_class(init,	msg_init_port).

standard_object(@pce).
standard_object(@display).

standard_constant(@on).
standard_constant(@off).
standard_constant(@nil).
standard_constant(@default).
standard_constant('').

standard_code(@arg1 \== '').
standard_code(@arg1 == '').
standard_code(@arg1 \== @nil).
standard_code(@arg1 == @nil).
standard_code(@arg1 > 0).
standard_code(message(@arg1, instance_of, class)).

standard_function(when(@arg1 == @arg2, @on, @off)).

:- initialization
   (   get(@types, member, port_type, _)
   ->  true
   ;   new(PortTypes, chain),
       forall(port_type(Type, _), send(PortTypes, append, Type)),
       new(_, type(port_type, name_of, PortTypes))
   ).

:- pce_global(@msg_links, make_msg_links).

make_msg_links(Ch) :-
	new(Ch, chain),
	forall(relation(From, To, Name),
	       (link_attributes(Name, Attrs),
		send(Ch, append, new(L, link(From, To))),
		send(L, name, Name),
		forall(member(Attr := Value, Attrs),
		       send(L, Attr, Value)))).


:- pce_global(@msg_identify,
	      new(handler_group(handler(area_enter,
					message(@receiver, identify)),
				handler(area_exit,
					message(@receiver?device,
						identify))))).

:- pce_begin_class(connect_port_gesture, connect_gesture).

variable(links,		chain,	get,	"Chain with candidate links").

initialise(G, Button:[name], Modifier:[modifier], Links:chain) :->
	send(G, send_super, initialise, Button, Modifier),
	send(G, slot, links, Links).


verify(G, Ev:event) :->
	"Fix correct device"::
	send(G, send_super, verify, Ev),
	get(Ev, receiver, Port),
	get(G?links, find,
	    ?(Port, handles, @default, @arg1?from),
	    Link),
	send(G, link, Link),
	get(G, device, Device),
	(   send(Device, instance_of, msg_object)
	->  send(G, device, Device?device)
	;   true
	).


initiate(G, Ev:event) :->
	"Little sun-4/110 patch"::
	send(G, send_super, initiate, Ev),
	send(G?line, pen, 1),
	get(Ev, receiver, Port),
	send(Port, report, status, '%s -->', Port?identify).


pointed(G, Ev:event, Pointed:chain) :<-
	"Return all overlapping ports in the model"::
	get(G, device, Model),
	get(Model, pointed_objects, Ev, PointedObjects),
	new(Pointed, chain),
	send(PointedObjects, for_all,
	     if(message(@arg1, instance_of, msg_object),
		message(Pointed, merge, ?(@arg1, pointed_objects, Ev)),
		if(message(@arg1, instance_of, msg_port),
		   message(Pointed, append, @arg1)))),
	send(Pointed, for_all,
	     if(not(message(@arg1, instance_of, msg_port)),
		message(Pointed, delete, @arg1))),
	send(PointedObjects, for_all,
	     if(message(@arg1, instance_of, msg_connection),
		message(Pointed, append, @arg1))),
	chain_list(Pointed, L),
	get(Ev, receiver, Port),
	(   member(Target, L),
	    Target \== Port,
	    get(G, links, Links),
	    fitting_link(Port, Target, Links, Link),
	    (   get(G, link, Link)
	    ->  true
	    ;   send(G, link, Link)
	    )
	->  send(Port, report, status, '%s --> (%s) --> %s',
		 Port?identify, Link?name, Target?identify)
	;   (	get(Pointed, head, BadTarget), BadTarget \== Port
	    ->	send(Port, report, status,
		     'Cannot link %s --> %s',
		     Port?identify, BadTarget?identify)
	    ;   send(Port, report, status, '%s -->', Port?identify)
	    )
	).

fitting_link(From, To, Links, Link) :-
	get(Links, find,
	    and(?(To, handles, @default, @arg1?to),
		?(From, handles, @default, @arg1?from)),
	    Link).

terminate(G, Ev:event) :->
	send(G, send_super, terminate),
	send(Ev?receiver, report, status, '').


connect(_G, F:graphical, T:graphical, L:link, FH:[name], TH:[name]) :->
	"Create connection"::
	new(_, msg_connection(F, T, L, FH, TH)).

:- pce_end_class.



		 /*******************************
		 *	  MSG-CONNECTION	*
		 *******************************/

:- pce_begin_class(msg_connection, tagged_connection).

resource(tag_font,	font,	'@helvetica_bold_12', "Font for the tag").

:- pce_global(@argument_handle,
	      new(handle(w/2, h/2, argument, argument_center))).
:- pce_global(@condition_handle,
	      new(handle(w/2, h/2, activate, condition_center))).

initialise(C, F:graphical, T:graphical, L:[link], FH:[name], TH:[name]) :->
	(   L == @default
	->  fitting_link(F, T, @msg_links, Link)
	;   Link = L
	),
	send(C, send_super, initialise, F, T, Link, FH, TH),
	get(C, name, Type),
	(   Type == argument
	->  get(T, connections, Chain),
	    new(N, number(0)),
	    send(Chain, for_all,
		 if(@arg1?name == argument, message(N, plus, 1))),
	    get(C, resource_value, tag_font, Font),
	    new(Tag, editable_text(string('%d', N), center, Font)),
	    send(C, tag, Tag),
	    send(C, handle, @argument_handle)
	;   (Type == activate ; Type == condition ; Type == expansion )
	->  new(Tag, circle(10)),
	    send(Tag, fill_pattern, @black_image),
	    send(C, tag, Tag),
	    send(C, handle, @argument_handle),
	    send(C, handle, @condition_handle)
	;   true
	).


type(C, Type:name) :<-
	get(C, name, Type).


activate(C) :->
	"Run activation or condition link"::
	get(C, to, MethodPort),
	(   get(C, connections, Connections)
	->  get(Connections, find_all,
		@arg1?type == condition, Conditions),
	    send(Conditions, for_all, message(@arg1, simulate)),
	    get(C, arguments, ArgV)
	;   new(ArgV, vector)
	),
	send(MethodPort, send_vector, simulate, ArgV).


simulate(C) :->
	"Execute event --> something connection"::
	get(C, type, Type),
	(   memberchk(Type, [activate, condition])
	->  pen(C, send(C, activate), 4)
	;   get(C, tag, Tag),
	    invert(Tag, get(C, argument, Arg)),
	    portray_object(Arg, Term),
	    term_to_atom(Term, Atom),
	    send(@display, inform,
		 '%s relation returns %s', Type, Atom)
	).


arguments(C, ArgV:vector) :<-
	"Get arguments for operation"::
	new(ArgV, vector),
	(   get(C, connections, Connections)
	->  send(Connections, for_all,
		 if(@arg1?type == argument,
		    message(@arg1, fill_argument, ArgV)))
	;   true
	).


parameter(C, Parm:'int|name') :<-
	"Get index or name of argument"::
	get(C?tag, string, Str),
	(   get(@pce, convert, Str, int, Parm)
	->  true
	;   get(Str, value, Parm)
	).


argument(C, Arg:any) :<-
	"Argument of `argument' or `expansion' connection"::
	get(C, arguments, GetArgs),
	get(C, from, FromPort),
	user(get(FromPort, get_vector, simulate, GetArgs, RawArg)),
	get(@pce, convert, RawArg, any, Arg). % evaluate functions!


fill_argument(C, ArgV:vector) :->
	"Fill requested argument"::
	get(C, tag, Tag),
	invert(Tag, (	get(C, argument, Arg),
			get(C, parameter, Parm),
			(   integer(Parm)
			->  send(ArgV, element, Parm, Arg)
			;   send(ArgV, append, Parm := Arg)
			)
		    )).


count_arguments(C, Args:int) :<-
	"Count number of arguments"::
	(   get(C, connections, Cs)
	->  new(N, number(0)),
	    send(Cs, for_all,
		 if(@arg1?type == argument,
		    message(N, plus, 1))),
	    get(N, value, Args)
	;   Args = 0
	).


propose_formal_parameter_name(P, Name:name) :<-
	"Propose name for implementation template"::
	(   get(P, parameter, Name),
	    atom(Name)
	->  true
	;   get(P, from, FromPort),
	    (	get(FromPort, object, Object)
	    ->  get(Object, name, Name)
	    ;	send(FromPort, instance_of, msg_constant_port)
	    ->  get(FromPort?value, class_name, Name)
	    ;	Name = 'x'
	    )
	).


propose_formal_parameter_names(C, Names:vector) :<-
	"Propose argument-names for the activation"::
	new(Names, vector),
	(   get(C, connections, Cs)
	->  send(Cs, for_all,
		 if(and(@arg1?type == argument,
			message(type(int), validate, @arg1?parameter)),
		    message(Names, element,
			    @arg1?parameter,
			    @arg1?propose_formal_parameter_name))),
	    send(Cs, for_all,
		 if(and(@arg1?type == argument,
			message(type(name), validate, @arg1?parameter)),
		    message(Names, append,
			    @arg1?propose_formal_parameter_name)))
	;   true
	).
		 

:- pce_global(@msg_connection_gesture, make_msg_connection_gesture).
:- initialization free(@msg_connection_gesture).

make_msg_connection_gesture(G) :-
	new(P, popup_gesture(new(Popup, popup))),
	Link = @arg1,
	send_list(Popup, append,
		  [ menu_item(help,
			      message(@helper, give_help, dialog,
				      'link:menu'),
			      end_group := @on),
		    menu_item(simulate,
			      message(Link, simulate),
			      end_group := @on),
		    menu_item(cut,
			      message(Link, destroy))
		  ]),
	new(G, handler_group(@msg_identify,
			     P,
			     connect_port_gesture(@default, @default,
						  @msg_links))).


event(C, Ev:event) :->
	(   send(C, send_super, event, Ev)
	->  true
	;   send(@msg_connection_gesture, event, Ev)
	).


identify(C) :->
	"Describe the connection"::
	get(C, identify, Id),
	send(C, report, inform, '%s', Id).

identify(C, Id:string) :<-
	new(Id, string('%s relation', C?name?label_name)).

:- pce_end_class.


		 /*******************************
		 *	 MOVE-PORT-GESTURE	*
		 *******************************/

:- pce_begin_class(move_port_gesture, move_gesture).

drag(G, Ev:event) :->
	"Drag object along border of the object"::
	get(Ev, receiver, Port),
	get(Port, device,  Object),
	(   send(Object, instance_of, msg_object)
	->  get(Ev, position, Object, EvPos),
	    get(EvPos, difference, G?offset, point(EX, EY)),
	    send(Port, constrained_move, EX, EY)
	;   send(G, send_super, drag, Ev)
	).


terminate(G, Ev:event) :->
	"Calls ->drag"::
	send(G, drag, Ev).

:- pce_end_class.


		 /*******************************
		 *	        PORT		*
		 *******************************/

position(north, w/2, 0).
position(south, w/2, h).
position(east,  w, h/2).
position(west,  0, h/2).

make_handle(Type, Position, Handle) :-
	position(Position, X, Y),
	get(string('msg_%s_%s_handle', Position, Type), value, Ref),
	Handle = @Ref,
	(   object(@Ref)
	->  true
	;   new(Handle, handle(X, Y, Type, Ref))
	).

port_handle(Type, Handle) :-
	make_handle(Type, _, Handle).


:- pce_begin_class(msg_port, editable_text, "Graphical programming port").

resource(port_font,	font,	'@helvetica_roman_10',	"Default font").

initialise(P, Name:[name]) :->
	default(Name, '', Nm),
	get(P, resource_value, port_font, Font),
	send(P, send_super, initialise, Name, left, Font),
	send(P, name, Nm),
	send(P, transparent, @off),
	send(P, border, 2),
	send(P, pen, 1).

model(P, Model:msg_model) :<-
	model(P, Model).

model(P, P) :-
	send(P, instance_of, msg_model), !.
model(P, M) :-
	get(P, device, Dev),
	model(Dev, M).

object(P, Object:'msg_object|msg_model') :<-
	"Related msg_object"::
	get(P, device, Object),
	Object \== @nil.

:- pce_global(@msg_port_recogniser, make_msg_port_recogniser).
:- initialization free(@msg_port_recogniser).

make_msg_port_recogniser(R) :-
	new(R, handler_group(@msg_identify,
%			     new(move_port_gesture),
			     drag_and_drop_gesture(left, c),
			     connect_port_gesture(@default, @default,
						  @msg_links),
			     popup_gesture(new(P, popup)))),
	Port = @arg1,
	send_list(P, append,
		  [ menu_item(help,
			      message(@helper, give_help, dialog,
				      'port:menu'),
			      end_group := @on),
		    menu_item(expand,
			      message(Port, expand),
			      condition := (Port?type == get)),
		    menu_item(simulate,
			      message(Port, simulate),
			      condition := message(Port, has_send_method,
						   simulate),
			      end_group := @on),
		    menu_item(edit,
			      message(Port, edit),
			      condition := message(Port, has_get_method,
						   program_object)),
		    menu_item(documentation,
			      message(Port, documentation),
			      end_group := @on),
		    menu_item(cut,
			      message(Port, destroy))
		  ]).


identify(P, Id:string) :<-
	"Identify the port"::
	new(Id, string('%s port "%s" (not implemented)',
		       P?type?label_name, P?name)).

identify(P) :->
	"Identify the port"::
	send(P, report, inform, '%s', P?identify).


edit(P) :->
	"Edit implementation of port"::
	get(P, program_object, Method),
	auto_call(editpce(Method)).


documentation(P) :->
	"View documentation of port in manual"::
	get(P, program_object, Method),
	auto_call(manpce(Method)).


event(P, Ev:event) :->
	(   send(P, send_super, event, Ev)
	;   send(@msg_port_recogniser, event, Ev)
	).


loose_focus(P) :->
	"Keyboard is lost"::
	send(P, send_super, loose_focus),
	send(P, name, P?string).


enter(P) :->
	"Quit typing"::
	send(P, send_super, enter),
	(   get(P?string, size, 0)
	->  send(P, free)
	;   send(P, name, P?string)
	).


expand(Port) :->
	"Expand a (get) port"::
	get(Port, expand, _).

expand(Port, Expansion) :<-
	"Expand a (get) port"::
	(   get(Port, connections, Cs),
	    get(Cs, find, @arg1?type == expansion, C)
	->  get(C, to, Expansion)
	;   get(Port, type, get)
	->  get(Port, model, Model),
	    get(Port, absolute_position, Model, point(X, Y)),
	    send(Model, display, new(Expansion, msg_object(Port?name)),
		 point(X+Port?width+50, Y)),
	    send(Expansion, handle, handle(0, Port?height/2, object, object)),
	    get(@msg_links, find, @arg1?name == expansion, Link),
	    new(_, msg_connection(Port, Expansion, Link))
	;   send(Port, report, error, 'Only `get'' ports can be expanded')
	).


constrained_move(Port, EX:int, EY:int) :->
	"Move to closest position on the border"::
	get(Port, device,  Model),
	(   send(Model, instance_of, msg_object)
	->  get(Model, shape, Shape),
	    get(Port, width, PW),
	    get(Shape, left_side, L),
	    get(Shape, right_side, R),
	    (   EX - L < R - (EX + PW)
	    ->  PX = L
	    ;   PX is R - PW
	    ),
	    get(Port, height, PH),
	    get(Shape, top_side, T),
	    get(Shape, bottom_side, B),
	    (   EY - T < B - (EY + PH)
	    ->  PY = T
	    ;   PY is B - PH
	    ),
	    (   abs(PX - EX) < abs(PY - EY)
	    ->  PX2 = PX, PY2 = EY
	    ;   PX2 = EX, PY2 = PY
	    ),
	    (   PX2 < L
	    ->  PX3 = L
	    ;   PX2 + PW > R
	    ->  PX3 is R - PW
	    ;   PX3 = PX2
	    ),
	    (   PY2 < T
	    ->  PY3 = T
	    ;   PY2 + PH > B
	    ->  PY3 is B - PH
	    ;   PY3 = PY2
	    ),
	    send(Port, do_set, PX3, PY3)
	;   true
	).

:- pce_end_class.


		 /*******************************
		 *	SPECIALISED PORTS	*
		 *******************************/

attach_port_handles(Type) :-
	forall(port_handle(Type, Handle), send(@class, handle, Handle)).

		%%% VALUE PORT

:- pce_begin_class(msg_get_port, msg_port, "Port representing a get").
:- pce_class_directive(attach_port_handles(get)).
type(_P, T:port_type) :<- T = get.

value(P, Args:any ..., Value:any) :<-
	"Execute the port (compute the value)"::
	(   get(P, name, Selector),
	    get(P, object, Object),
	    (   get(Object, ui_object, Self)
	    ->  user(get(Self, get_vector, Selector, Args, Value))
	    ;   get(Object, connections, Cs),
		get(Cs, find, @arg1?name == expansion, C),
		get(C, from, Port),
		get(C, arguments, ExpArgs),
		get(Port, get_vector, simulate, ExpArgs, PortValue),
		user(get(PortValue, get_vector, Selector, Args, Value))
	    )
	).


simulate(P, Args:any ..., Value:any) :<-
	"Execute the port (compute the value)"::
	invert(P, user(get(P, get_vector, value, Args, Value))).


simulate(P, ArgV:any ...) :->
	"Execute the related method with arguments"::
	get(P, name, Selector),
	get(P, object, Object),
	get(Object, ui_object, Receiver),
	invert(P, user(send(Receiver, send_vector, Selector, ArgV))).


value_type(P, Type:type) :<-
	"Type returned by this port"::
	(   get(P, object, ModelObject),
	    get(ModelObject, ui_object, Object)
	->  get(P, name, Selector),
	    get(Object, dia_argument_type, Selector, Type)
	;   get(P, program_object, Method),
	    get(Method, return_type, Type)
	).


program_object(P, Method:object) :<-
	"Associated program-object"::
	get(P, name, Selector),
	get(P, object, ModelObject),
	(   get(ModelObject, ui_object, Object)
	->  (	send(Object, instance_of, class)
	    ->	get(Object?class, get_method, Selector, Method)
	    ;	get(Object, get_method, Selector, tuple(_, Method))
	    )
	;   get(ModelObject, connections, Cs),
	    get(Cs, find, @arg1?name == expansion, C),
	    get(C, from, Port),
	    get(Port, value_type, Type),
	    class_of_type(Type, Class),
	    get(Class, get_method, Selector, Method)
	).


identify(P, Id:string) :<-
	"Identification string"::
	(   get(P, program_object, Method),
	    get(Method, summary, Sum)
	->  get(P, name, Selector),
	    new(Id, string('Get port %s: "%s"', Selector, Sum))
	;   get(P, get_super, identify, Id)
	).

:- pce_end_class.

		%%% EVENT PORT

:- pce_begin_class(msg_event_port, msg_port, "Port for message").
:- pce_class_directive(attach_port_handles(event)).
type(_P, T:port_type) :<- T = event.

enter(P) :->
	"Relate to the object"::
	send(P, send_super, enter),
	send(P, attach).

attach(P) :->
	"Inform the modelled object"::
	get(P, name, Selector),
	get(P, object, Object),
	get(Object, ui_object, Self),
	get(Self, send_method, Selector, tuple(_, Method)),
	new(Message, message(Self, send_hyper, behaviour_model,
			     simulate, Selector)),
	get(Method, argument_type, 1, Type),
	get(Type, check, Message, Msg),
	send(Self, Selector, Msg).


detach(P) :->
	"Disconnect from the modelled object"::
	get(P, name, Selector),
	get(P, object, Object),
	get(Object, ui_object, Self),
	get(Self, send_method, Selector, tuple(_, Method)),
	get(Method, argument_type, 1, Type),
	send(Type, validate, @nil),
	send(Self, Selector, @nil).


device(P, Dev:'msg_object|msg_model*') :->
	"Update controller-relation"::
	(   get(P, device, Old), Old \== @nil
	->  ignore(send(P, detach))
	;   true
	),
	send(P, send_super, device, Dev),
	(   Dev \== @nil
	->  ignore(send(P, attach))
	;   true
	).


simulate(P) :->
	"Execute the event"::
	invert(P, (   get(P, connections, Connections)
		  ->  send(Connections, for_all, message(@arg1, simulate))
		  ;   true
		  )).


program_object(P, Method:object) :<-
	get(P, name, Selector),
	get(P?object, ui_object, Object),
	get(Object, get_method, Selector, tuple(_, Method)).


identify(P, Id:string) :<-
	"Identification string"::
	get(P, name, Selector),
	(   get(P?object, ui_object, Object),
	    get(Object, get_method, Selector, tuple(_, Method)),
	    get(Method, summary, Sum)
	->  new(Id, string('Event port %s: "%s"', Selector, Sum))
	;   get(P, get_super, identify, Id)
	).

:- pce_end_class.

		%%% INIT PORT

:- pce_begin_class(msg_init_port, msg_port, "Initialisation port").
:- pce_class_directive(attach_port_handles(init)).
type(_P, T:port_type) :<- T = init.

initialise(P, Name:[name]) :->
	default(Name, initialise, Nm),
	send(P, send_super, initialise, Nm).


simulate(P) :->
	"Execute the initialisation"::
	invert(P, (   get(P, connections, Connections)
		  ->  send(Connections, for_all, message(@arg1, simulate))
		  ;   true
		  )).

:- pce_end_class.

		%%% SEND PORT

:- pce_begin_class(msg_send_port, msg_port, "Callable port").
:- pce_class_directive(attach_port_handles(send)).
type(_P, T:port_type) :<- T = send.

identify(P, Id:string) :<-
	get(P, object, Object),
	get(P, name, Selector),
	(   get(Object, ui_object, @prolog)
	->  identify_predicate(Selector, Id)
	;   get(P, program_object, Method),
	    get(Method, summary, Sum)
	->  new(Id, string('Send port %s: "%s"', Selector, Sum))
	;   get(P, get_super, identify, Id)
	).


simulate(P, ArgV:any ...) :->
	"Execute the related method with arguments"::
	get(P, name, Selector),
	get(P, object, Object),
	get(Object, ui_object, Receiver),
	(   Receiver == @prolog
	->  get(ArgV, size, Arity),
	    functor(Head, Selector, Arity),
	    verify_predicate(Object, Head)
	;   true
	),
	invert(P, user(send(Receiver, send_vector, Selector, ArgV))).


verify_predicate(Host, Head) :-
	source_file(Head, Path),
	pce_host:modified_since_last_loaded(Path),
	send(@display, confirm, 'Reconsult modified file\n%s?', Path), !,
	user:consult(Path),
	verify_predicate(Host, Head).
verify_predicate(_, Head) :-
	pce_host:callable_predicate(user:Head), !.
verify_predicate(Host, Head) :-
	get(Host, file, File),
	get(File, absolute_path, Path),
	(   source_file(Path)
	->  (	pce_host:modified_since_last_loaded(Path)
	    ->	send(@display, confirm, 'Reconsult modified file\n%s?', Path),
		user:consult(Path),
		verify_predicate(Host, Head)
	    ;	report_undefined(Host, Head),
		fail
	    )
	;   (	send(File, exists)
	    ->	send(@display, confirm, 'Consult file %s?', Path),
		user:consult(Path),
		verify_predicate(Host, Head)
	    ;	report_undefined(Host, Head),
		fail
	    )
	).


report_undefined(Host, Module:Head) :- !,
	functor(Head, Name, Arity),
	send(Host, report, error,
	     'Undefined predicate %s:%s/%d', Module, Name, Arity).
report_undefined(Host, Head) :-
	functor(Head, Name, Arity),
	send(Host, report, error,
	     'Undefined predicate %s/%d', Name, Arity).



program_object(P, Method:object) :<-
	"Associated program-object"::
	get(P, name, Selector),
	get(P, object, ModelObject),
	(   get(ModelObject, connections, Cs),
	    get(Cs, find, @arg1?name == expansion, C)
	->  get(C, from, Port),
	    get(Port, value_type, Type),
	    class_of_type(Type, Class),
	    get(Class, send_method, Selector, Method)
	;   get(ModelObject, ui_object, Object)
	->  (	send(Object, instance_of, class)
	    ->	get(Object?class, get_method, Selector, Method)
	    ;	get(Object, get_method, Selector, tuple(_, Method))
	    )
	).


count_arguments(P, Set:chain) :<-
	"Count the number of incomming arguments"::
	get(P, connections, Cs),
	new(Set, chain),
	send(Cs, for_all,
	     if(@arg1?type == activate,
		message(Set, append, @arg1?count_arguments))).


propose_prolog_formal_parameter_list(P, Parms:vector) :<-
	"Propose parameters for Prolog"::
	get(P, connections, Cs),
	get(Cs, find, @arg1?type == activate, C),
	get(C, propose_formal_parameter_names, P0),
	new(Parms, vector),
	new(I, number(1)),
	send(P0, for_all,
	     and(message(Parms, element, I,
			 ?(@prolog, prolog_parameter_name, @arg1)),
		 message(I, plus, 1))).

prolog_parameter_name(N0, N) :-
	new(S, string(N0)),
	send(S, translate, ':', @nil),
	get(S, label_name, S2),
	send(S2, translate, ' ', @nil),
	get(S2, value, N).

append_parameter_list(_, Parms) :-
	get(Parms, size, 0), !.
append_parameter_list(Str, Parms) :-
	send(Str, append, string('(%s', ?(Parms, element, 1))),
	get(Parms, size, Size),
	forall(between(2, Size, I),
	       send(Str, append, string(', %s', ?(Parms, element, I)))),
	send(Str, append, ')').


prolog_defined(P) :->
	"Test if predicate is defined"::
	get(P, name, Selector),
	get(P, propose_prolog_formal_parameter_list, Vector),
	get(Vector, size, Arity),
	functor(Head, Selector, Arity),
	user:current_predicate(_, Head). % TBD: modules


prolog_source(P, Source:string) :<-
	"Prolog template for clause"::
	get(P, name, Selector),
	new(Source, string('%s', Selector)),
	get(P, propose_prolog_formal_parameter_list, Parms),
	append_parameter_list(Source, Parms),
	send(Source, append, string(' :-\n\t')).


edit(P) :->
	"Edit source of send-port"::
	get(P, object, Object),
	get(Object, ui_object, Self),
	get(P, name, Selector),
	(   Self == @prolog
	->  get(P, count_arguments, Set),
	    send(Set, unique),
	    get(Set, head, ArgC),
	    (	user:ed(Selector/ArgC)
	    ->	true
	    ;	get(P?frame, prolog_file, File),
		new(B, emacs_buffer(File)),
		send(B, open),
		get(B?editors, head, Editor),
		send(Editor, point_to_bottom_of_file),
		send(Editor, append, P?prolog_source)
	    )
	;   get(Self, send_method, Selector, tuple(_, Method)),
	    auto_call(editpce(Method))
	).

documentation(P) :->
	"Show documentation of send-port"::
	get(P, object, Object),
	get(Object, ui_object, Self),
	get(P, name, Selector),
	(   Self == @prolog
	->  user:ed(Selector)
	;   get(Self, send_method, Selector, tuple(_, Method)),
	    auto_call(manpce(Method))
	).

:- pce_end_class.

		%%% CONSTANT PORT

:- pce_begin_class(msg_constant_port, msg_port, "Port for constant").
:- pce_class_directive(attach_port_handles(get)). % constant?

:- pce_global(@msg_no_value,
	      new(constant(no_value,
			   string("No value (@nil is a value here)")))).

variable(value, 'any|function' := @msg_no_value, none,
	 "Represented constant values").

type(_P, T:port_type) :<- T = constant.

identify(P, Id:string) :<-
	"Identification string"::
	(   get(P, value, Value)
	->  object_to_name(Value, Atom),
	    new(Id, string('Constant %s', Atom))
	;   new(Id, string('Uninstantiated constant'))
	).

enter(P) :->
	"Associate new (typed) value"::
	send(P, send_super, enter),
	send(P, relink),
	send(P, identify).

relink(P) :->
	"Restore after reloading"::
	get(P?string, value, Name),
	name_to_object(Name, Value),
	send(P, value, Value).


value(P, Value:'any|function') :->
	"Associate value (using hyper)"::
	forall(get(P, find_hyper, msg_value, Hyper), send(Hyper, free)),
	(   (   atomic(Value)		% int, atom, float
	    ;   send(Value, '_instance_of', function)
	    )
	->  send(P, slot, value, Value)
	;   new(_, hyper(P, Value, msg_value, msg_constant))
	).
value(P, Value:'any|function') :<-
	"Associated object"::
	(   get(P, slot, value, V),
	    (   V == @msg_no_value
	    ->  get(P, hypered, msg_value, Value)
	    ;   Value = V
	    )
	).
	
simulate(P, Value:'any|function') :<-
	"Associated object (simulation)"::
	invert(P, get(P, value, Value)).


documentation(P) :->
	"Show documentation"::
	get(P, value, Value),
	(   Value = @Atom,
	    atom(Atom)
	->  auto_call(manpce(Value))
	;   get(Value, class, Class),
	    auto_call(manpce(Class))
	).


:- pce_end_class.


		 /*******************************
		 *	 OBJECT TEMPLATE	*
		 *******************************/

:- pce_begin_class(msg_object_template, template,
		   "Template for object-models").

simulate_message(Port, Msg) :-
	new(Msg, message(@receiver, send_hyper,
			 behaviour_model, simulate, Port)).


ui_object(O, Self:object) :->
	"Associate object"::
	forall(get(O, find_hyper, ui_object, Hyper), send(Hyper, free)),
	new(_, dia_transient_hyper(Self, O, behaviour_model, ui_object)),
	send(O?graphicals, for_all,
	     if(and(message(@arg1, instance_of, msg_port),
		    @arg1?type == event),
		message(Self, @arg1?name,
			create(message, Self, send_hyper, behaviour_model,
			       simulate, @arg1?name)))).

ui_object(O, Self:object) :<-
	"Associated object"::
	(   get(O, hypered, ui_object, Self)
	->  true
	;   get(O, connections, Cs),	% is this ok?
	    get(Cs, find, @arg1?name == expansion, C),
	    get(C, from, Port),
	    get(Port, value, Self)
	).


simulate(O, PortName:name) :->
	"Start simulation at named event-port"::
	get(O, member, PortName, Port),
	send(Port, simulate).


edit(O) :->
	"Start PceEmacs on source of class"::
	get(O, ui_object, Object),
	get(Object, class, Class),
	auto_call(editpce(Class)).


documentation(O) :->
	"Start PCE manual on class"::
	(   get(O, ui_object, Object)
	->  (   Object = @Atom,
	    atom(Atom)
	    ->  new(Global, man_global(Atom)),
		auto_call(manpce(Global))
	    ;   get(Object, class, Class),
		(   send(Object, has_get_method, proto)
		->  get(Class, super_class, DocClass)
		;   DocClass = Class
		),
		auto_call(manpce(DocClass))
	    )
	;   get(O, connections, Cs),
	    get(Cs, find, @arg1?type == expansion, C),
	    get(C, from, Port),
	    get(Port, value_type, Type),
	    class_of_type(Type, Class),
	    auto_call(manpce(Class))
	).


expansion_class_name(O, ClassName:name) :<-
	"Return class-name of expanded object"::
	get(O, connections, Cs),
	get(Cs, find, @arg1?type == expansion, C),
	get(C, from, Port),
	get(Port, value_type, Type),
	class_of_type(Type, Class),
	get(Class, name, ClassName).


identify(O) :->
	"Report identification"::
	send(O, report, status, O?identify).


identify(O, Id:string) :<-
	"New identification string"::
	(   get(O, connections, Cs),
	    get(Cs, find, @arg1?type == expansion, C),
	    get(C, from, Port)
	->  (	get(Port, value_type, Type)
	    ->	new(Id, string('Expansion of %s (%N)', Port?identify, Type))
	    ;	new(Id, string('Expansion of %s', Port?identify))
	    )
	;   get(O, ui_object, Self)
	->  (	send(Self, has_get_method, proto)
	    ->	new_term(Self, Term)
	    ;	portray_object(Self, T0),
		(   T0 = quote_function(T)
		->  Term = T
		;   Term = T0
		)
	    ),
	    term_to_atom(Term, Atom),
	    new(Id, string(Atom))
	;   new(Id, string('(Unlinked)'))
	).


add_port(O, Type:port_type, Name:[name], Where:[point]) :->
	"Add port of indicated type"::
	port_class(Type, Class),
	(   Name \== @default,
	    get(O?graphicals, find, 
		and(message(@arg1, instance_of, Class),
		    @arg1?name == Name),
		Port)
	->  send(Port, flash)
	;   (   Where == @default
	    ->  get(O?window?focus_event, position, O, Pos)
	    ;   Pos = Where
	    ),
	    NewTerm =.. [Class, Name],
	    send(O, display, new(P, NewTerm), Pos),
	    send(P, constrained_move, Pos?x, Pos?y),
	    (   Name == @default, get(P, name, '')
	    ->  send(O?window, keyboard_focus, P)
	    ;   true
	    )
	).


port_type_from_object(Obj, send) :-
	send(Obj, instance_of, send_method), !.
port_type_from_object(Obj, get) :-
	send(Obj, instance_of, get_method), !.
port_type_from_object(Obj, event) :-
	send(Obj, instance_of, variable),
	get(Obj, type, Type),
	send(Type, includes, code), !.
port_type_from_object(Obj, get) :-
	send(Obj, instance_of, variable),
	send(Obj, get_access).
port_type_from_object(Obj, get) :-
	send(Obj, instance_of, variable),
	send(Obj, send_access).


port(O, Port:msg_port) :->
	"Add new port at a sensible place"::
	get(O, shape, Shape),
	get(Shape, height, H),
	get(Shape, width, W),
	get(Port, height, PH),
	get(Port, width, PW),
	(   send(Port, x, 0),
	    place_y(O, 0, H, Port)
	->  send(O, display, Port)
	;   send(Port, x, W - PW),
	    place_y(O, 0, H, Port)
	->  send(O, display, Port)
	;   send(Port, y, 0),
	    place_x(O, 0, W, Port)
	->  send(O, display, Port)
	;   send(Port, y, H - PH),
	    place_x(O, 0, W, Port)
	->  send(O, display, Port)
	;   MX is W-PW,
	    MH is H-PH,
	    random(0, MX, X),
	    random(0, MH, Y),
	    send(O, display, Port, point(X, Y))
	).


place_y(O, MinY, MaxY, Gr) :-
	get(O, graphicals, Grs),
	send(Gr, y, MinY),
	(   get(Gr, bottom_side, BGr),
	    BGr > MaxY
	->  !, fail
	;   get(Grs, find,
		and(message(@arg1, instance_of, msg_port),
		    message(@arg1, overlap, Gr)),
		Offender)
	->  get(Offender, bottom_side, Bottom),
	    NewMinY is Bottom + 3,
	    place_y(O, NewMinY, MaxY, Gr)
	;   true
	).
place_x(O, MinX, MaxX, Gr) :-
	get(O, graphicals, Grs),
	send(Gr, x, MinX),
	(   get(Gr, right_side, RGr),
	    RGr > MaxX
	->  !, fail
	;   get(Grs, find,
		and(message(@arg1, instance_of, msg_port),
		    message(@arg1, overlap, Gr)),
		Offender)
	->  get(Offender, right_side, Right),
	    NewMinX is Right + 3,
	    place_x(O, NewMinX, MaxX, Gr)
	;   true
	).

:- pce_end_class.


		 /*******************************
		 *	      OBJECT		*
		 *******************************/

:- pce_begin_class(msg_object, device, "Graphical programming object").
:- use_class_template(msg_object_template).

resource(size,		size,	'size(100,50)',	     "Default size of object").
resource(label_font,	font,	'@helvetica_bold_10',"Default name-font").

initialise(O, Name:name) :->
	send(O, send_super, initialise),
	send(O, name, Name),
	get(O, resource_value, size, size(W, H)),
	get(O, resource_value, label_font, Font),
	send(O, display, new(B, box(W, H))),
	send(B, name, shape),
	send(B, fill_pattern, @grey12_image),
	send(O, display, new(T, editable_text(Name, center, Font))),
	send(T, name, text),
	send(T, message, value),
	send(T, center, B?center),
	send(T, transparent, @off).


shape(O, Shape:graphical) :<-
	"Primiary shape (outline)"::
	get(O, member, shape, Shape).


value(O, Value:name) :->
	"Name (value) has been typed"::
	(   get(O, ui_object, Self),
	    send(Self, instance_of, visual) % dubious
	->  true
	;   name_to_object(Value, Object),
	    (	send(Object, '_instance_of', function)
	    ->	send(O, ui_object, quote_function(Object))
	    ;	send(O, ui_object, Object)
	    ),
	    ignore(send(O, identify))
	).


relink(O) :->
	"Relink to self after reloading state"::
	(   get(O, ui_object, _)
	;   get(O, connections, Cs),
	    get(Cs, find, @arg1?type == expansion, _)
	;   get(O, member, text, Text),
	    send(O, value, Text?string)
	), !.


geometry(O, X:[int], Y:[int], W:[int], H:[int]) :->
	"Recenter text"::
	get(O?area, size, size(CW, CH)),
	get(O, shape, Shape),
	send(Shape, set, 0, 0, W, H),
	get(O, member, text, Text),
	send(Text, center, Shape?center),
	(W == @default -> Xfactor = 1 ; Xfactor is W / CW),
	(H == @default -> Yfactor = 1 ; Yfactor is H / CH),
	send(O?graphicals, for_all,
	     if(message(@arg1, instance_of, msg_port),
		and(message(@arg1, resize, Xfactor, Yfactor, point(0,0)),
		    message(@arg1, constrained_move, @arg1?x, @arg1?y)))),
	send(O, send_super, geometry, X, Y).

:- pce_global(@msg_object_recogniser, make_msg_object_recogniser).
:- initialization free(@msg_object_recogniser).

make_add_port_popup(Popup, Port) :-
	new(Popup, popup(string('Add %s Port', Port),
			 message(@arg2, add_port, Port, @arg1))),
	send(Popup, update_message,
	     message(@prolog, update_port_menu, Popup, Port, @arg1)).

update_port_menu(Popup, Port, Model) :-
	send(Popup, clear),
	(   get(Model, ui_object, Object)
	->  (   send(Object, has_get_method, dia_ports)
	    ->	get(Object, dia_ports, Port, Ports),
		send(Ports, for_all, message(Popup, append, @arg1))
	    ;	send(Object, has_get_method, proto)
	    ->  get(Object, proto, Proto),
		forall(port(Proto, _Kind, Name, Port),
		       send(Popup, append, Name))
	    ;   get(Object, class_name, Proto)
	    ->  forall(port(Proto, _Kind, Name, Port),
		       send(Popup, append, Name))
	    )
	;   get(Model, expansion_class_name, Proto)
	->  forall(port(Proto, _Kind, Name, Port),
		   send(Popup, append, Name))
	).


add_port_popup(@Ref, Port) :-
	get(string('msg_add_%s_port_popup', Port), value, Ref),
	(   object(@Ref)
	->  true
	;   make_add_port_popup(@Ref, Port)
	).


make_msg_object_recogniser(R) :-
	Obj = @arg1,
	new(P, popup_gesture(new(Pop, popup))),
	send_list(Pop, append,
		  [ menu_item(help,
			      message(@helper, give_help, dialog,
				      'behaviour_component:menu'),
			      end_group := @on),
		    new(C, menu_item(add_send_port,
				     message(Obj, add_port, send))),
		    new(V, menu_item(add_get_port,
				     message(Obj, add_port, get))),
		    new(E, menu_item(add_event_port,
				     message(Obj, add_port, event),
				     end_group := @on)),
		    menu_item(documentation,
			      message(Obj, documentation)),
		    menu_item(edit,
			      message(Obj, edit),
			      end_group := @on),
		    menu_item(cut,
			      message(Obj, destroy))
		  ]),
	new(R, handler_group(@msg_identify,
			     P,
			     resize_gesture(left),
			     new(drag_and_drop_gesture))),
	add_port_popup(CP, send),  send(C, popup, CP),
	add_port_popup(VP, get), send(V, popup, VP),
	add_port_popup(EP, event), send(E, popup, EP).


event(O, Ev:event) :->
	(   send(O, send_super, event, Ev)
	->  true
	;   send(@msg_object_recogniser, event, Ev)
	->  true
	;   send(O, typed, Ev)
	).


typed(O, Ev:event) :->
	get(Ev, key, Key),
	port_type(Type, Key), !,
	port_class(Type, Class),
	send(O, display, new(Port, Class)),
	get(Ev, position, O, point(X, Y)),
	get(Port, size, size(W, H)),
	send(Port, constrained_move, X-W/2, Y-H/2),
	send(O?window, keyboard_focus, Port).


preview_drop(O, Obj:any*, Pos:[point]) :->
	"Provide drop-feedback"::
	(   Obj == @nil
	->  send(O, report, status, ''),
	    (	get(O, attribute, preview_outline, OL)
	    ->	send(OL, device, @nil),
		send(O, delete_attribute, preview_outline)
	    ;	true
	    )
	;   (   get(O, attribute, preview_outline, OL)
	    ->  send(OL, position, Pos)
	    ;   send(Obj, instance_of, msg_port)
	    ->	(   get(Obj, device, O)
		->  send(O, report, status, 'Moving port'),
		    get(Obj, size, size(W, H)),
		    send(O, attribute, preview_outline, new(OL, box(W, H))),
		    send(OL, texture, dotted),
		    send(O, display, OL, Pos)
		;   \+ send(Obj, instance_of, msg_constant_port),
		    send(O, report, status,
			 'Drop moves port %s to %s', Obj?identify, O?identify),
		    send(O, attribute, preview_outline,
			 new(OL, msg_port(Obj?name))),
		    send(OL, colour, @grey50_image),
		    send(O, display, OL, Pos)
		)
	    ;   port_type_from_object(Obj, Type)
	    ->  send(O, attribute, preview_outline,
		     new(OL, msg_port(Obj?name))),
		send(OL, colour, @grey50_image),
		send(O, display, OL, Pos),
	        send(O, report, status,
		     'Drop adds %s port "%s"', Type, Obj?name)
	    )
	).
	

drop(O, Obj:any, Pos:point) :->
	"Create port from XPCE meta-object"::
	(   port_type_from_object(Obj, Type)
	->  send(O, add_port, Type, Obj?name, Pos)
	;   (   send(Obj, instance_of, msg_port),
	        \+ send(Obj, instance_of, msg_constant_port)
	    ->  (   get(Obj, device, O)
		->  send(Obj, constrained_move, Pos?x, Pos?y)
		;   send(O, display, Obj),
		    send(Obj, constrained_move, Pos?x, Pos?y)
		)
	    )
	;   fail
	).
	

:- pce_end_class.


		 /*******************************
		 *	     MSG_HOST		*
		 *******************************/

:- pce_begin_class(msg_host, msg_object, "Host representation").

variable(file,	file,	get,	"Associated source-file").

resource(file_font, font, '@times_bold_12', "Font for file id").

:- pce_global(@dia_center_below_spatial,
	      new(spatial(xref = x + w/2, yref = y+h,
			  xref = x + w/2, yref = y))).

initialise(Host, HostObject:host, File:[file]) :->
	term_to_atom(HostObject, Name),
	send(Host, send_super, initialise, Name),
	default(File, file('scratch.pl'), TheFile),
	send(Host, slot, file, TheFile),
	get(Host, resource_value, file_font, Font),
	send(Host, display,
	     new(T, editable_text(TheFile?name, center, Font))),
	send(T, name, file),
	send(T, transparent, @off),
	send(T, message, message(Host, file_name, @arg1)),
	get(Host, member, text, Label),
	send(@dia_center_below_spatial, forwards, Label, T).


geometry(Host, X:[int], Y:[int], W:[int], H:[int]) :->
	send(Host, send_super, geometry, X, Y, W, H),
	get(Host, member, text, Text),
	get(Host, member, file, File),
	get(Host, shape, Shape),
	get(Shape, center, point(CX, CY)),
	send(Text, center_x, CX),
	send(Text, y, CY-Text?height),
	send(File, center_x, CX),
	send(File, y, CY).


file_name(Host, FileName:name) :->
	"Append suffix if necessary"::
	send(Host, file, ?(FileName, ensure_suffix, '.pl')).


file(Host, File:file) :->
	"Associate a new file"::
	get(Host, member, file, Text),
	send(Text, string, File?name),
	send(Host, slot, file, File).


edit(Host) :->
	"Edit associated file"::
	get(Host, file, File),
	get(File, name, Name),
	auto_call(emacs(Name)).


prolog_source(Host, Source:string) :<-
	"Produce templates for undefined predicates"::
	new(Source, string),
	send(Host?graphicals, for_all,
	     if(and(message(@arg1, instance_of, msg_send_port),
		    not(message(@arg1, prolog_defined))),
		and(if(Source?size \== 0,
		       message(Source, newline, 2)),
		    message(Source, append, @arg1?prolog_source)))).

:- pce_end_class.


		 /*******************************
		 *	    MAIN (TEST)		*
		 *******************************/

:- pce_begin_class(msg_model, picture, "The model editor").
:- use_class_template(msg_object_template).

:- pce_global(@msg_editor_recogniser, make_msg_editor_recogniser).
:- free(@msg_editor_recogniser).

make_add_popup(P, Command, Var, Goal) :-
	Window = @arg2,
	Here = ?(Window?focus_event, position, Window),
	new(P, popup(Command,
		     message(Window, Command, Here, @arg1))),
	forall(Goal,
	       (   term_to_atom(Var, Atom),
		   send(P, append,
			menu_item(Atom, @default, Atom))
	       )).


add_object_popup(P) :-
	make_add_popup(P, add_object, O, standard_object(O)).
add_constant_popup(P) :-
	make_add_popup(P, add_constant, O, standard_constant(O)).
add_code_popup(P) :-
	make_add_popup(P, add_code, O, standard_code(O)).
add_function_popup(P) :-
	make_add_popup(P, add_function, O, standard_function(O)).


make_msg_editor_recogniser(R) :-
	new(Popup, popup_gesture(new(Pop, popup))),
	P = @arg1,
	new(Here, ?(@event, position, P)),
	send_list(Pop, append,
		  [ menu_item(help,
			      message(@helper, give_help, dialog,
				      'target:menu'),
			      end_group := @on),
		    menu_item(add_host,
			      message(P, add_host, @prolog, Here),
			      'Add @prolog'),
		    new(OO, menu_item(add_object,
				      message(P, add_object, Here))),
		    new(CO, menu_item(add_constant,
				      message(P, add_constant, Here))),
		    new(PO, menu_item(add_code,
				      message(P, add_code, Here))),
		    new(FO, menu_item(add_function,
				      message(P, add_function, Here),
				      end_group := @on)),
		    new(CP, menu_item(add_send_port,
				      message(P, add_port, send))),
		    new(VP, menu_item(add_get_port,
				      message(P, add_port, get))),
		    new(EP, menu_item(add_event_port,
				      message(P, add_port, event))),
		    new(_P, menu_item(add_init_port,
				      message(P, add_port, init),
				      end_group := @on)),
		    menu_item(documentation,
			      message(P, documentation))
		  ]),

	add_port_popup(CPP, send),    send(CP, popup, CPP),
	add_port_popup(VPP, get),     send(VP, popup, VPP),
	add_port_popup(EPP, event),   send(EP, popup, EPP),
	add_object_popup(OPP),        send(OO, popup, OPP),
	add_constant_popup(COP),      send(CO, popup, COP),
	add_code_popup(POP),	      send(PO, popup, POP),
	add_function_popup(FOP),      send(FO, popup, FOP),

	new(R, handler_group(Popup,
			     handler(area_enter,
				     message(@receiver, identify)),
			     handler(area_exit,
				     message(@receiver, report, status, '')))).


add_host(P, Host:host, Where:point) :->
	"Add host instance at location"::
	(   get(P, ui_object, Target),
	    (	get(Target, attribute, dia_save_file, File)
	    ->	get(File, name, Name),
		get(Name, delete_suffix, '.dia', N2),
		get(N2, ensure_suffix, '.pl', DefName)
	    ;	get(Target, name, Name),
		get(Name, ensure_suffix, '.pl', DefName)
	    )
	;   DefName = @default
	),
	send(P, display, new(Obj, msg_host(Host, DefName)), Where),
	send(Obj, relink).


add_object(P, Pos:point, Name:[name]) :->
	"Add new object here"::
	default(Name, '', Nm),
	send(P, display, new(Obj, msg_object(Nm)), Pos),
	(   Name == @default
	->  send(P?window, keyboard_focus, ?(Obj, member, text))
	;   send(Obj, relink)
	).


add_constant(P, Pos:point, Name:[name]) :->
	"Add constant entry"::
	default(Name, '', Nm),
	send(P, display, new(Port, msg_constant_port(Nm)), Pos),
	(   Name == @default
	->  send(P, keyboard_focus, Port)
	;   send(Port, relink)
	).


add_code(P, Pos:point, Name:[name]) :->
	"Add executable object"::
	default(Name, '', Nm),
	send(P, display, new(Obj, msg_object(Nm)), Pos),
	send(Obj, add_port, send, forward),
	(   Name == @default
	->  send(P?window, keyboard_focus, ?(Obj, member, text))
	;   send(Obj, relink)
	).


add_function(P, Pos:point, Name:[name]) :->
	"Add XPCE function object"::
	default(Name, '', Nm),
	send(P, display, new(Obj, msg_object(Nm)), Pos),
	send(Obj, add_port, get, '_forward'),
	(   Name == @default
	->  send(P?window, keyboard_focus, ?(Obj, member, text))
	;   send(Obj, relink)
	).


event(E, Ev:event) :->
	(   send(E, send_super, event, Ev)
	->  true
	;   send(@msg_editor_recogniser, event, Ev)
	).


drop(E, Obj:any, Pos:point) :->
	"Import from the model editor"::
	(   send(Obj, instance_of, msg_object) % move models.
	->  (   get(Obj, device, E)
	    ->	send(Obj, move, Pos)
	    ;	get(Obj, ui_object, UI),
		(   send(UI, instance_of, graphical)
		->  send(E, report, warning, 'Cannot drop behaviour items')
		;   get(Obj, member, text, Txt),
		    send(E, display, new(O, msg_object(Txt?string)), Pos),
		    send(O, ui_object, UI)
		)
	    )
	;   port_type_from_object(Obj, Type)
	->  send(E, add_port, Type, Obj?name, Pos)
	;   (   send(Obj, instance_of, msg_port)
	    ->  (   get(Obj, device, E)
		->  send(Obj, position, Pos)
		;   send(E, display, Obj, Pos)
		)
	    )
	;   send(Obj, has_get_method, proto), % heuristic for prototype
	    get(Obj, proto, Proto),
	    get(Obj, name, Name),
	    send(E, display, new(O, msg_object(Name)), Pos),
	    forall(port(Proto, obligatory, PortName, Type),
		   (port_class(Type, Class),
		    NewTerm =.. [Class, PortName],
		    send(O, port, NewTerm))),
	    send(O?shape, fill_pattern, @grey25_image),
	    send(O, ui_object, Obj)
	).


preview_drop(P, Obj:any, Pos:[point]) :->
	"Preview feedback for dropping"::
	(   Obj == @nil
	->  send(P, report, status, ''),
	    (	get(P, attribute, preview_outline, OL)
	    ->	send(OL, device, @nil),
		send(P, delete_attribute, preview_outline)
	    ;	true
	    )
	;   (   get(P, attribute, preview_outline, OL)
	    ->  send(OL, position, Pos)
	    ;	send(Obj, instance_of, graphical), % moving things around
		get(Obj, device, P)
	    ->	get(Obj?area, size, size(W, H)),
		send(P, attribute, preview_outline, new(OL, box(W, H))),
		send(OL, texture, dotted),
		send(P, display, OL, Pos)
	    ;	(   port_type_from_object(Obj, Type),
		    send(P, report, status, 'Drop to add "%s" port "%s"',
			 Type, Obj?name)
		;   send(Obj, instance_of, msg_port),
		    send(P, report, status, 'Drop to move port to dialog')
		)
	    ->	send(P, attribute, preview_outline,
		     new(OL, msg_port(Obj?name))),
		send(OL, colour, @grey50_image),
		send(P, display, OL, Pos)
	    ;	send(Obj, has_get_method, proto)
	    ->	send(P, attribute, preview_outline,
		     new(OL, msg_object(Obj?name))),
		send(OL, colour, @grey50_image),
		send(P, display, OL, Pos),
		send(P, report, status, 'Drop to add "%s" named "%s" to model',
		     Obj?proto, Obj?name)
	    )
	).
	

postscript_as(E) :->
	"Write PostScript to file"::
	PsFile = 'scratch.ps',
	get(@finder, file, @off, '.ps', @default, PsFile, ThePsFile),
	auto_call(postscript(E, ThePsFile)).


relink(M) :->
	"Relink all components"::
	send(M?graphicals, for_all,
	     if(message(@arg1, has_send_method, relink),
		if(not(message(@arg1, relink)),
		   message(M, report, warning,
			   'Failed to relink %s', @arg1)))).

:- pce_end_class.


		 /*******************************
		 *	   MODEL-EDITOR		*
		 *******************************/

:- pce_begin_class(msg_model_editor, frame, "Model-editor frame").

variable(prolog_file, name := 'scratch.pl', get, "File for new predicates").
variable(last_error_time, date, get, "Time of last error").

initialise(F, UI:visual) :->
	send(F, send_super, initialise, string('Behaviour of "%s"', UI?name)),
	send(F, confirm_done, @off),
	send(F, done_message, message(F, show, @off)),
	send(F, slot, last_error_time, new(date)),
	send(F, append, new(D, dialog)),
	send(new(M, msg_model), below, D),
	send(M, name, model),
	fill_editor_dialog(D),
	new(_, dia_transient_hyper(UI, M, behaviour_model, ui_object)).


initialise_new_slot(F, Var:variable) :-> % temporary
	(   get(Var, name, last_error_time)
	->  send(F, slot, last_error_time, new(date))
	;   true
	),
	send(F, send_super, initialise_new_slot, Var).


report(F, Kind:name, Format:char_array, Args:any ...) :->
	"Make sure errors are reported for a while"::
	(   Kind == error
	->  send(F, send_super_vector, report, Kind,
		 ?('ERROR: ', append, Format), Args),
	    send(F?last_error_time, current)
	;   get(new(date), difference, F?last_error_time, second, Diff),
	    Diff > 4,
	    send(F, send_super_vector, report, Kind, Format, Args)
	).


fill_editor_dialog(D) :-
	get(D, frame, Frame),
	send(D, append, new(MB, menu_bar)),
	send(MB, alignment, right),
	send(D, append, new(A, menu(animate, choice)), right),
	send(A, append, @off),
	send(A, append, @on),
	send(A, append, step),
	send(A, selection, @on),
	send(D, append, new(Speed, slider(speed, 1, 50, 25)), right),
	send(Speed, width, 75),
	send(D, append,
	     new(Step, button(step, message(Frame, return, step))),
	     right),
	send(A, message,
	     and(if(@arg1 == @off, and(message(Speed, show, @off),
				       message(Step, show, @off),
				       message(Frame, return, step)),
		    if(@arg1 == @on, and(message(Speed, show, @on),
					 message(Step, show, @off),
					 message(Frame, return, step)),
		       and(message(Speed, show, @off),
			   message(Step, show, @on)))),
		 message(D, layout))),
	send(Step, show, @off),
	send(D, append,
	     label(reporter, 'Please drag dialog-items from interface')),
	send(MB, append, new(File, popup(file))),
	send_list(File, append,
		  [ menu_item(help,
			      message(@helper, give_help, dialog,
				      'behaviour_model:menu'),
			      end_group := @on),
		    menu_item(postscript_as,
			      message(?(Frame, member, model), postscript_as),
			      end_group := @on),
		    menu_item(destroy,
			      and(message(D?display, confirm,
					  'Destroy behaviour model?'),
				  message(Frame, destroy))),
		    menu_item(quit,
			      message(Frame, show, @off))
		  ]).


animate(F, How:'bool|{step}') :<-
	get(F, member, dialog, Dialog),
	get(Dialog, member, animate, Menu),
	get(Menu, selection, How).


animation_sleep(F) :->
	"Sleep till continuation"::
	get(F, member, dialog, Dialog),
	send(F, busy_cursor, @nil),
	get(Dialog, member, animate, Menu),
	(   get(Menu, selection, @on)
	->  get(Dialog, member, speed, Slider),
	    get(Slider, selection, PerSecond),
	    Time is 1 / PerSecond,
	    send(timer(Time), delay)
	;   get(F, confirm, _Step)
	).

:- pce_end_class.


		 /*******************************
		 *	      UTILITY		*
		 *******************************/

name_to_object(Name, Object) :-
	term_to_atom(Term, Name),
	term_to_object(Term, Object).

term_to_object(@Ref, @Ref) :-
	get(@pce, object_from_reference, Ref, @Ref), !. % trap lazy creation
term_to_object(@Ref, _) :-
	send(@pce, report, error, 'No such object: @%s', Ref).
term_to_object(Atom, Atom) :-
	atomic(Atom).
term_to_object(Term, Object) :-
	functor(Term, Name, _Arity),
	get(@pce, convert, Name, class, _Class),
	new(Object, Term).

object_to_name(Object, Name) :-
	portray_object(Object, Term),
	term_to_atom(Term, Name).


identify_predicate(Name, Id) :-
	setof(Arity,
	      Module^Head^Name^(member(Module, [user,system]),
				Module:current_predicate(Selector, Head),
				functor(Head, Name, Arity)),
	      Arities), !,
	(   Arities = [A]
	->  true
	;   term_to_atom(Arities, A)
	),
	new(S, string('%s', A)),
	send(S, translate, ' ', @nil),
	new(Id, string('Predicate `%s/%s''', Selector, S)).
identify_predicate(Name, Id) :-
	new(Id, string('Undefined predicate `%s''', Name)).

		 /*******************************
		 *	ANIMATION FEEDBACK	*
		 *******************************/


pen(Gr, Goal, Pen) :-
	feedback(Gr, pen := Pen, Goal).


invert(Gr, Goal) :-
	feedback(Gr, inverted := @on, Goal).


yesno(Goal, Result) :-
	(   Goal
	->  Result = true
	;   Result = fail
	).


wait(Time) :-
	MSecs is Time * 1000,
	send(@display_manager, dispatch, @default, MSecs), !.
wait(_).


:- dynamic flashed/0.

feedback(Gr, _, Goal) :-
	get(Gr,	frame, Editor),
	get(Editor, animate, @off), !,
	Goal.
feedback(Gr, Attr := Value, Goal) :-
	retractall(flashed),
	get(Gr,	frame, Editor),
	get(Gr, Attr, OldValue),
	send(Gr, Attr, Value),
	send(Gr, flush),
	send(Editor, animation_sleep),
	yesno(Goal, RVal),
	send(Gr, Attr, OldValue),
	send(Gr, flush),
	(   RVal
	->  true
	;   (	flashed
	    ->  fail
	    ;   assert(flashed),
	        between(1, 5, _),
		send(Gr, flash),
		wait(0.1),
		fail
	    )
	).

		 /*******************************
		 *	     USER-CALL		*
		 *******************************/

user_error(no_behaviour).
user_error(argument_count).
user_error(argument_type).

user(Goal) :-
	forall(user_error(Error), send(error(Error), slot, feedback, report)),
	yesno(Goal, RVal),
	forall(user_error(Error), send(error(Error), slot, feedback, print)),
	RVal.
