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

:- module(pce_tick_box, []).
:- use_module(library(pce)).
:- require([ default/3
	   ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Class `tick_box' defines a label with a `tick-box' displayed left of the
label. The selection is expressed as a boolean.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(tick_box, menu, "Simple boolean tick-box").

variable(align_with, {left,value} := left, both, "How to align").

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

label_width(TB, LW:int) :->
	"Honour label alignment if we align with value of <-above"::
	(   get(TB, align_with, value)
	->  send_super(TB, label_width, LW)
	;   true
	).

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
		   
