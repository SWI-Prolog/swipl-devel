/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
NOTE: Should also include colour attribute
NOTE: image_item should be moved to library
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- module(pce_style_item, []).
:- use_module(library(pce)).
:- require([ default/3
	   , forall/2
	   ]).

:- pce_autoload(font_item, library(pce_font_item)).
:- pce_autoload(image_item, library('dialog/image_item')).

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
	make_tick_box(HF, has_font, font),
	make_tick_box(HI, has_icon, icon),
	send(SI, append_dialog_item, HF),
	send(SI, append_dialog_item, font_item(font), right),
	send(SI, append_dialog_item, HI),
	send(SI, append_dialog_item, image_item(icon), right),
	send(SI, append_dialog_item, new(AI, menu(attributes, toggle)), below),
	forall(style_attribute(Att),
	       send(AI, append, Att)),
	send(SI, layout_dialog, size(0,5)),
	send(SI, default, Def).

make_tick_box(B, Name, Controls) :-
	new(B, menu(Name, toggle)),
	send(B, append, Name),
	send(B, show_label, @off),
	send(B, feedback, image),
	send(B, pen, 0),
	get(B, resource_value, label_font, LF),
	send(B, value_font, LF),
	send(B, on_image, @mark_image),
	send(B, off_image, @nomark_image),
	send(B, message,
	     message(?(@receiver?device, member, Controls), active, @arg2)).
		     

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

		 /*******************************
		 *               TEST		*
		 *******************************/

test :-
	new(D, dialog),
	send(D, append,
	     new(@s, style_item(style, style(font := @helvetica_roman_12,
					     icon := 'pce.bm')))),
	send(D, open).
