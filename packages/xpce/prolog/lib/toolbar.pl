/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1998 University of Amsterdam. All rights reserved.
*/

:- module(pce_tool_button, []).
:- use_module(library(pce)).
:- use_module(library(help_message)).
:- require([ default/3
	   ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This library module defines  three   classes:  tool_bar, tool_button and
tool_status_button. It is intended for defining   rows  of buttons, also
called tool-bars. Each button represents an   action  on the `client' of
the tool_bar.

Below is a typical example:

resource(printer,	image,	image('printer.xpm')).
resource(floppy,	image,	image('floppy.xpm')).

:- pce_begin_class(myapp, frame).

initialise(MyApp) :->
	send_super(MyApp, initialise, 'My application'),
	send(F, append, new(D, dialog)),
	send(D, pen, 0),
	send(D, gap, size(0, 5)),
	send(D, append, new(TB, tool_bar(F))),
	send_list(TB, append,
		  [ tool_button(load,
				resource(floppy),
				load),
		    gap,		% skip a little
		    tool_button(print,
				resource(printer),
				print)
		  ]).
				
print(MyApp) :->
	<Print the current document>

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(tool_bar, dialog_group,
		   "Row of buttons").

variable(orientation, 	{horizontal,vertical},	get, "Stacking direction").
variable(client,	object*,		get, "Receiving object").

initialise(BG, Client:object*, Orientation:[{horizontal,vertical}]) :->
	default(Orientation, horizontal, O),
	send(BG, send_super, initialise, @default, group),
	send(BG, slot, client, Client),
	send(BG, slot, orientation, O),
	send(BG, gap, size(0,0)).
	
append(BG, B:'tool_button|{gap}') :->
	"Append button or gap"::
	(   get(BG, orientation, horizontal)
	->  Where = right
	;   Where = next_row
	),
	(   B == gap
	->  What = graphical(0, 0, 5, 5)
	;   What = B
	),
	send(BG, send_super, append, What, Where).

activate(BG) :->
	"Update activation of member buttons"::
	send(BG?graphicals, for_some, message(@arg1, activate)).

reference(BG, Ref:point) :<-
	"Reference is at the baseline"::
	get(BG, height, H),
	new(Ref, point(0, H)).

compute(BG) :->
	"Make all buttons the same size"::
	(   get(BG, request_compute, @nil)
	->  true
	;   new(S, size(0,0)),
	    send(BG?graphicals, for_all,
		 if(message(@arg1, instance_of, button),
		    message(S, union, @arg1?size))),
	    send(BG?graphicals, for_all,
		 if(message(@arg1, instance_of, button),
		    message(@arg1, size, S))),
	    send_super(BG, compute)
	).

:- pce_end_class.

:- pce_begin_class(tool_button, button,
		   "Button for a tool_bar").

variable(condition,	code*,	     get, "Condition for activation").

initialise(TB,
	   Action:action='name|code',
	   Label:label='name|image',
	   Balloon:balloon=[name|string],
	   Condition:condition=[code]*) :->
	default(Condition, @nil, Cond),
	make_label(Label, Lbl),
	make_name(Action, Name),
	make_message(Action, Msg),
	send(TB, send_super, initialise, Name, Msg),
	send(TB, label, Lbl),
	send(TB, slot, condition, Cond),
	(   Balloon == @default
	->  true
	;   atom(Balloon)
	->  get(Balloon, label_name, Text),
	    send(TB, help_message, tag, Text)
	;   send(TB, help_message, tag, Balloon)
	).
	     
client(TB, Client:object) :<-
	get(TB, device, Dev),
	get(Dev, client, Client).

make_message(Name, @default) :-
	atomic(Name), !.
make_message(Code, Code).


make_name(Name, Name) :-
	atomic(Name), !.
make_name(Code, Name) :-
	send(Code, instance_of, message),
	get(Code, selector, Name),
	atom(Name), !.
make_name(_, 'tool_button').


make_label(Image, Image) :-
	send(Image, instance_of, image), !.
make_label(Name, Image) :-
	pce_catch_error(cannot_find_file, new(Image, image(Name))), !.
make_label(Name, Image) :-
	new(T, text(Name, left, small)),
	get(T, size, size(W, H)),
	new(I0, image(@nil, W, H)),
	send(I0, draw_in, T),
	get(I0, scale, size(16,16), Image),
	free(T),
	free(I0).


forward(TB) :->
	"Send action to <-client"::
	get(TB, message, Msg),
	(   Msg == @default
	->  get(TB, name, Selector),
	    get(TB, client, Client),
	    send(Client, Selector)
	;   send(Msg, execute)
	).


activate(TB) :->
	"Update the activation using <-condition"::
	(   get(TB, condition, Cond),
	    Cond \== @nil
	->  (   get(TB, client, Client),
	        send(Cond, forward, Client)
	    ->	send(TB, active, @on)
	    ;	send(TB, active, @off)
	    )
	).

active(TB, Val:bool) :->
	"(de)activate the menu-item"::
	send(TB, send_super, active, Val),
	(   get(TB, label, Image),
	    send(Image, instance_of, image)
	->  get(Image, active, Val, Activated),
	    send(TB, label, Activated)
	;   true
	).

:- pce_end_class.

:- pce_begin_class(tool_status_button, tool_button,
		   "A tool button representing two states").

variable(value,		bool := @off,	get, "Current value").

execute(TB) :->
	"Switch status and send message"::
	get(TB, value, Value),
	get(Value, negate, NewValue),
	send(TB, value, NewValue),
	send(TB, flush),
	get(TB, message, Message),
	(   Message == @default
	->  get(TB, client, Client),
	    get(TB, name, Selector),
	    send(Client, Selector, NewValue)
	;   Message == @nil
	->  true
	;   send(Message, forward, NewValue)
	).
	

value(TB, Value:bool) :->
	(   get(TB, value, Value)
	->  true
	;   send(TB, slot, value, Value),
	    (	Value == @on
	    ->	send(TB, status, execute)
	    ;	send(TB, status, inactive)
	    )
	).


reset(TB) :->
	"Smooth handling of abort"::
	(   get(TB, value, @off)
	->  send(TB, status, inactive)
	;   send(TB, status, execute)
	).

:- pce_end_class.


		 /*******************************
		 *         GRAYING ICONS	*
		 *******************************/

:- pce_extend_class(image).

active(Img, Active:bool, Img2:image) :<-
	"Return image with proper activation"::
	(   Active == @off
	->  (   get(Img, hypered, inactive, Img2)
	    ->	true
	    ;	get(Img, hypered, active, _)
	    ->	Img2 = Img
	    ;	get(Img, greyed, Img2)
	    )
	;   (   get(Img, hypered, active, Img2)
	    ->	true
	    ;	Img2 = Img
	    )
	).
	    

greyed(Img, Grey:image) :<-
	"Created a greyed version of a colour image"::
	(   get(Img, hypered, inactive, Grey)
	->  true
	;   get(Img, size, size(W, H)),
	    new(Grey, image(@nil, W, H, pixmap)),
	    (   get(Img, mask, Mask),
		send(Mask, instance_of, image)
	    ->  send(Grey, mask, new(M, image(@nil, W, H, bitmap))),
		new(MB, bitmap(Mask)),
		send(MB, transparent, @on),
		send(M, draw_in, MB),
		send(M, draw_in, MB, point(1,1))
	    ;   true
	    ),
	    get(Img, monochrome, I2),
	    send(Grey, background, black),
	    new(B2, bitmap(I2)),
	    send(B2, transparent, on),
	    send(B2, colour, white),
	    send(Grey, draw_in, B2, point(1,1)),
	    get(class(menu), class_variable, inactive_colour, ClassVar),
	    get(ClassVar, value, GreyColour),
	    send(B2, colour, GreyColour),
	    send(Grey, draw_in, B2),
	    new(_, hyper(Img, Grey, inactive, active))
	).

:- pce_end_class.
