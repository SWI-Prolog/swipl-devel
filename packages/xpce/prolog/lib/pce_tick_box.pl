/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

:- module(pce_tick_box, []).
:- use_module(library(pce)).
:- require([ default/3
	   ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Class `tick_box' defines a label with a `tick-box' displayed left of the
label. The selection is expressed as a boolean.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(tick_box, menu, "Simple boolean tick-box").

class_variable(item_elevation, elevation*, @nil, "Elevation of the label").

initialise(TB, Name:name, Value:[bool], Message:[code]*) :->
	default(Value, @off, Def),
	send(TB, send_super, initialise, Name, marked, Message),
	send(TB, multiple_selection, @on),
	send(TB, send_super, show_label, @off),
	get(TB, label_font, Font),
	send(TB, value_font, Font),
	send(TB, append, menu_item(Name,
				   message(@receiver, forward))),
	send(TB, default, Def).

:- pce_group(appearance).

label_width(_TB, _LW:int) :->
	true.

label(TB, Label:'name|image') :->
	"Set the label"::
	(   get(TB, members, Members),
	    Members \== @nil
	->  get(Members, head, Item),
	    send(Item, label, Label)
	;   send(TB, send_super, label, Label) % during initialise
	).

show_label(TB, Show:bool) :->
	"Show the label"::
	get(TB?members, head, Item),
	(   Show == @on
	->  send(Item, label, Item?value?label_name)
	;   send(Item, label, '')
	).

:- pce_group(selection).

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
	get(TB, selection, Val),
	(   Msg == @default
	->  get(TB, name, Selector),
	    send(TB?device, Selector, Val)
	;   Msg == @nil
	->  true
	;   send(Msg, forward, Val)
	).
	
:- pce_end_class.
		   
