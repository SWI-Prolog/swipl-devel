/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1995 University of Amsterdam. All rights reserved.
*/

:- module(pce_tick_box, []).
:- use_module(library(pce)).

:- pce_begin_class(tick_box, menu, "Simple boolean tick-box").

resource(item_elevation, elevation*, @nil, "Elevation of the label").

:- pce_global(@tick_box_message,
	      new(message(@receiver?message, forward, @arg1?selected))).

initialise(TB, Name:name, Value:[bool], Message:[code]*) :->
	send(TB, send_super, initialise, Name, marked, Message),
	send(TB, multiple_selection, @on),
	send(TB, show_label, @off),
	get(TB, resource_value, label_font, Font),
	send(TB, value_font, Font),
	send(TB, append, new(MI, menu_item(Name, @tick_box_message))),
	(   Value == @default
	->  send(MI, selected, @off)
	;   send(MI, selected, Value)
	).

:- pce_end_class.
		   
