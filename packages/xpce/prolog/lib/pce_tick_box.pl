/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1995 University of Amsterdam. All rights reserved.
*/

:- module(pce_tick_box, []).
:- use_module(library(pce)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Class `tick_box' defines a label with a `tick-box' displayed left of the
label. The selection is expressed as a boolean.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(tick_box, menu, "Simple boolean tick-box").

resource(item_elevation, elevation*, @nil, "Elevation of the label").

:- pce_global(@tick_box_message,
	      new(message(@receiver, forward))).

initialise(TB, Name:name, Value:[bool], Message:[code]*) :->
	default(Value, @off, Def),
	send(TB, send_super, initialise, Name, marked, Message),
	send(TB, multiple_selection, @on),
	send(TB, show_label, @off),
	get(TB, resource_value, label_font, Font),
	send(TB, value_font, Font),
	send(TB, append, menu_item(Name, @tick_box_message)),
	send(TB, default, Def).

label_width(_TB, _LW:int) :->
	true.

selection(TB, Val:bool) :->
	"Set selection as boolean"::
	get(TB?members, head, Item),
	send(Item, selected, Val).
selection(TB, Val:bool) :<-
	"Get selection as boolean"::
	get(TB?members, head, Item),
	get(Item, selected, Val).

forward(TB) :->
	"Execute the message"::
	get(TB, message, Msg),
	(   Msg \== @nil
	->  get(TB, selection, Val),
	    send(Msg, forward, Val)
	;   true
	).
	
:- pce_end_class.
		   
