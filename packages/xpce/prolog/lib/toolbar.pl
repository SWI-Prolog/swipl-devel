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

:- module(pce_tool_button, []).
:- use_module(library(pce)).
:- use_module(library(help_message)).
:- use_module(library(imageops)).
:- require([ default/3
	   ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This library module defines  three   classes:  tool_bar, tool_button and
tool_status_button. It is intended for defining   rows  of buttons, also
called tool-bars. Each button represents an   action  on the `client' of
the tool_bar.

Below is a typical example:

resource(printer,	image,	image('16x16/print.xpm')).
resource(floppy,	image,	image('16x16/save.xpm')).

:- pce_begin_class(myapp, frame).

initialise(MyApp) :->
	send_super(MyApp, initialise, 'My application'),
	send(F, append, new(D, tool_dialog(MyApp))),
	send_list(TB, append,
		  [ tool_button(load,
				resource(floppy),
				load),
		    gap,		% skip a little
		    tool_button(print,
				resource(printer),
				print)
		  ]),
	...
				
print(MyApp) :->
	<Print the current document>

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(tool_bar, dialog_group,
		   "Row of buttons").

variable(orientation, 	{horizontal,vertical},	get,  "Stacking direction").
variable(client,	object*,		both, "Receiving object").

initialise(BG, Client:[object]*, Orientation:[{horizontal,vertical}]) :->
	default(Client, @nil, C),
	default(Orientation, horizontal, O),
	send(BG, send_super, initialise, @default, group),
	send(BG, slot, client, C),
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
	send(BG?graphicals, for_some,
	     if(message(@arg1, has_send_method, activate),
		message(@arg1, activate))).

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

:- pce_begin_class(tool_button(client, name), button,
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

:- pce_end_class(tool_status_button).


		 /*******************************
		 *	      DIALOG		*
		 *******************************/

:- pce_begin_class(tool_dialog, dialog,
		   "Dialog for menu-bar and toolbar").

variable(client,	[object],	get, "The client").

initialise(TD, Client:[object]) :->
	"Refine layout"::
	send_super(TD, initialise),
	send(TD, slot, client, Client),
	send(TD, pen, 0),
	send(TD, border, size(0, 2)).


menu_bar(TD, Create:[bool], MB:menu_bar) :<-
	"Get (or create) the menu_bar"::
	(   get(TD, member, menu_bar, MB)
	->  true
	;   Create == @on
	->  (   get(TD, tool_bar, TB)
	    ->	send(new(MB, menu_bar), above, TB)
	    ;	send(TD, append, new(MB, menu_bar))
	    )
	).

tool_bar(TD, Create:[bool], TB:tool_bar) :<-
	"Get (or create) the tool_bar"::
	(   get(TD, member, tool_bar, TB)
	->  true
	;   Create == @on
	->  (   get(TD, client, Client),
	        Client \== @default
	    ->	true
	    ;	get(TD, frame, Client)
	    ),
	    (   get(TD, menu_bar, MB)
	    ->	send(new(TB, tool_bar(Client)), below, MB)
	    ;	send(TD, append, new(TB, tool_bar(Client)))
	    )
	).


popup(TD, Name:name, Create:[bool], Popup:popup) :<-
	"Find named popup or create it"::
	get(TD, menu_bar, Create, MB),
	(   get(MB, member, Name, Popup)
	->  true
	;   Create == @on
	->  send(MB, append, new(Popup, popup(Name))),
	    send(Popup, message, message(TD, action, @arg1))
	).


append(TD,
       B:'popup|menu_item|tool_button|graphical|{gap}',
       Where:where=[name], Before:before=[any]) :->
	"Append buttons or popup for manu-bar"::
	(   send(B, instance_of, popup)
	->  get(TD, menu_bar, @on, MB),
	    send(MB, append, B),
	    (	get(B, message, @default)
	    ->	send(B, message, message(TD, action, @arg1))
	    ;	true
	    )
	;   send(B, instance_of, menu_item)
	->  get(TD, popup, Where, @on, Popup),
	    send(Popup, insert_before, B, Before)
	;   (   send(B, instance_of, tool_button)
	    ;	B == gap
	    )
	->  get(TD, tool_bar, @on, TB),
	    send(TB, append, B)
	;   send_super(TD, append, B, Where)
	).

action(TD, Action:name) :->
	"Forward popup-action to <-client"::
	(   get(TD, client, Client),
	    Client \== @default
	->  send(Client, Action)
	;   send(TD, report, error, 'No client for posting action')
	).

:- pce_end_class(tool_dialog).

