/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
NOTE: Should also include colour attribute
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- module(pce_style_item, []).
:- use_module(library(pce)).
:- require([ default/3
	   , forall/2
	   ]).

:- pce_autoload(font_item,  library(pce_font_item)).
:- pce_autoload(image_item, library('dialog/image_item')).
:- pce_autoload(tick_box,   library(pce_tick_box)).

style_attribute(highlight).
style_attribute(underline).
style_attribute(bold).
style_attribute(grey).

:- pce_begin_class(style_item, figure, "Item to define/edit a style object").

variable(message,	code*,	both, "Executed message").
variable(default,	style,	get,  "->restore value").
variable(selection,	style,  none, "Current style").

initialise(SI, Label:name, Default:[style], Message:[message]) :->
	"Create from label, default and message"::
	send(SI, send_super, initialise),
	send(SI, name, Label),
	default(Message, @nil, Msg),
	default(Default, new(style), Def),
	send(SI, message, Msg),
	send(SI, append_dialog_item,
	     tick_box(has_font, @off,
		      message(?(SI, member, font), active, @arg1))),
	send(SI, append_dialog_item, font_item(font), right),
	send(SI, append_dialog_item,
	     tick_box(has_icon, @off,
		      message(?(SI, member, icon), active, @arg1))),
	send(SI, append_dialog_item, image_item(icon), right),
	send(SI, append_dialog_item, new(AI, menu(attributes, toggle)), below),
	forall(style_attribute(Att),
	       send(AI, append, Att)),
	send(SI, layout_dialog, size(0,5)),
	send(SI, default, Def).

get_attributes(Style, Attrs) :-
	new(Attrs, chain),
	forall(style_attribute(Att),
	       (   get(Style, Att, @on)
	       ->  send(Attrs, append, Att)
	       ;   true
	       )).

set_attributes(Style, Attrs) :-
	forall(style_attribute(Att),
	       (   send(Attrs, member, Att)
	       ->  send(Style, Att, @on)
	       ;   send(Style, Att, @off)
	       )).


font(SI, Font:[font]) :->
	get(SI, member, has_font, Box),
	get(SI, member, font, FontItem),
	(   Font == @default
	->  send(Box, selection, @nil),
	    send(FontItem, active, @off)
	;   send(Box, selection, has_font),
	    send(FontItem, active, @on),
	    send(FontItem, selection, Font)
	).
font(SI, Font:[font]) :<-
	get(SI, member, has_font, Box),
	(   get(Box, selected, has_font, @off)
	->  Font = @default
	;   get(SI, member, font, FontItem),
	    get(FontItem, selection, Font)
	).
	
icon(SI, Icon:image*) :->
	get(SI, member, has_icon, Box),
	get(SI, member, icon, ImageItem),
	(   Icon == @nil
	->  send(Box, selection, @nil),
	    send(ImageItem, active, @off)
	;   send(Box, selection, has_icon),
	    send(ImageItem, active, @on),
	    send(ImageItem, selection, Icon)
	).
icon(SI, Icon:image*) :<-
	get(SI, member, has_icon, Box),
	(   get(Box, selected, has_icon, @off)
	->  Icon = @nil
	;   get(SI, member, icon, ImageItem),
	    get(ImageItem, selection, Icon)
	).
	

selection(SI, Style:style) :->
	"Set the currently edited style object"::
	send(SI, slot, selection, Style),
	send(SI, font, Style?font),
	send(SI, icon, Style?icon),
	get(SI, member, attributes, AI),
	get_attributes(Style, Attrs),
	send(AI, selection, Attrs).


selection(SI, Style:style) :<-
	"Update -selection and return it"::
	get(SI, slot, selection, Style),
	send(Style, font, SI?font),
	send(Style, icon, SI?icon),
	get(SI, member, attributes, AI),
	get(AI, selection, Attrs),
	set_attributes(Style, Attrs).


default(SI, Style:style) :->
	"Set the <-restore value"::
	send(SI, slot, default, Style),
	send(SI, restore).


restore(SI) :->
	"Restore the selection from <-default"::
	send(SI, selection, SI?default).


apply(SI, _Always:[bool]) :->
	"Execute <-message with <-selection"::
	get(SI, message, Message),
	(   Message \== @nil
	->  get(SI, selection, Style),
	    send(Message, forward, Style)
	;   true
	).


:- pce_end_class.
