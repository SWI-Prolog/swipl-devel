/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1997 University of Amsterdam. All rights reserved.
*/

:- module(pce_arrow_item, []).
:- use_module(library(pce)).
:- require([ forall/2
	   , send_list/3
	   ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Class arrow_item defines an item for editing   an arrow object. The item
does not edit the specified arrow itself,  but just uses its attributes.
Also, the <-selection returns a `fresh' arrow.

This module can be seen as an  advanced example of using class label_box
for creating compound dialog items.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_autoload(colour_item, library(pce_colour_item)).

:- pce_begin_class(arrow_item, label_box,
		   "Extry for viewing/editing an arrow-head").

initialise(AI, Name:[name], Default:[function|arrow], Message:[code]*) :->
	send(AI, send_super, initialise, Name, Message),
	send(AI, display, new(line)),
	send(AI, append, new(Arrow, arrow)),
	send(Arrow, points, 100, 0, 0, 0),
	send(AI, append, new(WI, int_item(wing,   0, @nil, 0, 50))),
	send(AI, append, new(LI, int_item(length, 0, @nil, 0, 50)), right),
	send(AI, append, new(PI, int_item(pen,    0, @nil, 0,  1)), right),
	send(AI, append, new(Style, menu(style, choice, @nil))),
	send(AI, append, new(Fill, menu(fill_pattern, choice, @nil)), right),
	styles(Style),
	fill_patterns(Fill),
	send(AI, append,
	     new(SelCol, button(select_colour,
				message(AI, select_colour))),
	     right),
	send_list([PI, SelCol], alignment, left),
	send_list([Style, Fill], show_label, @off),
	send_list([Style, Fill], auto_label_align, @off),
	send_list([WI, LI, PI, Style, Fill, SelCol], reference, point(0,0)),
	fix_labels(AI),
	(   Default \== @default
	->  send(AI, default, Default)
	;   new(Proto, arrow),
	    copy_attributes(AI, Proto)
	).

fill_pattern(@nil).
fill_pattern(@white_image).
fill_pattern(@black_image).

fill_patterns(Menu) :-
	fill_pattern(Pattern),
	new(I, image(@nil, 30, 16)),
	new(A, arrow(20, 10, closed, Pattern)),
	new(L, line(0, 7, 25, 7)),
	send(A, pen, 1),
	send(L, second_arrow, A),
	send(I, draw_in, L),
	send(Menu, append, menu_item(Pattern, @default, I)),
	fail.
fill_patterns(_).

style(open).
style(closed).

styles(Menu) :-
	style(Style),
	new(I, image(@nil, 30, 16)),
	new(A, arrow(20, 10, Style, @nil)),
	send(A, pen, 1),
	new(L, line(0, 7, 25, 7)),
	send(L, second_arrow, A),
	send(I, draw_in, L),
	send(Menu, append, menu_item(Style, @default, I)),
	fail.
styles(_).

%	->layout_dialog
%	
%	Does the normal layout, but afterwards adjusts the arrow and its
%	line, as this doesn't fit in the automatic layout specification.
%	Note that the arrow is not part of the line (see `joint->second_arrow')
%	because changes to the line will not be visualised then.

layout_dialog(AI) :->
	send(AI, send_super, layout_dialog),
	get(AI, member, arrow, Arrow),
	get(AI, member, line, Line),
	get(AI, reference, point(_, RY)),
	send(Arrow, points, 100, RY, 0, RY),
	send(Line, points, 0, RY, 100, RY).

		 /*******************************
		 *	       LABELS		*
		 *******************************/

%	The code below assigns the image labels.  This is easier to addapt
%	and more flexible then direct sending of messages.  Also, fix_label
%	takes care of the case where the image cannot be located.

label(wing,		'16x16/arrow_wing.xpm').
label(length,		'16x16/arrow_length.xpm').
label(select_colour,	'16x16/cpalette2.xpm').
label(pen,		'16x16/pen.xpm').

resource(Name, image, image(File)) :-
	label(Name, File).

fix_labels(AI) :-
	send(AI?graphicals, for_all, message(@prolog, fix_label, @arg1)).

fix_label(Gr) :-
	get(Gr, name, Name),
	label(Name, _),
	new(I, image(resource(Name))),
	send(Gr, label, I).
fix_label(_).

:- pce_group(selection).

selection(AI, Selection:arrow) :->
	"Set the selection"::
	copy_attributes(AI, Selection),
	send(AI, modified, @off).
selection(AI, Selection:arrow) :<-
	"Get the selection"::
	new(Selection, arrow),
	get_attributes(AI, Selection),
	send(AI, modified, @off).

clear(_AI) :-> true.

get_attributes(AI, Into) :-
	get(AI, member, arrow, Arrow),
	edited_attribute(Attribute),
	get(Arrow, Attribute, Value),
	send(Into, Attribute, Value),
	fail.
get_attributes(_, _).

copy_attributes(AI, Proto) :-
	edited_attribute(Attribute),
	get(Proto, Attribute, Value),
	set_value(AI, Attribute, Value),
	fail.
copy_attributes(_, _).

edited_attribute(wing).
edited_attribute(length).
edited_attribute(pen).
edited_attribute(style).
edited_attribute(fill_pattern).
edited_attribute(colour).

set_value(AI, Attribute, Value) :-
	get(AI, member, arrow, Arrow),
	send(Arrow, Attribute, Value),
	(   get(AI, member, Attribute, Item)
	->  send(Item, selection, Value)
	;   true
	).


:- pce_group(edit).

rule(fill_pattern = @black_image,	pen = 0).
rule(fill_pattern = @black_image,	style = closed).
rule(fill_pattern = @nil,		pen = 1).
rule(fill_pattern = @white_image,	pen = 1).
rule(style        = open,		fill_pattern = @nil).

apply_rules(AI, Sel, Val) :-
	set_value(AI, Sel, Val),
	forall(rule(Sel=Val, Sel2=Val2),
	       apply_rules(AI, Sel2, Val2)).


modified_item(AI, Item:graphical, _Modified:bool) :->
	get(Item, name, Sel),
	get(Item, selection, Val),
	apply_rules(AI, Sel, Val),
	send(AI, modified, @on),
	send(AI?device, modified_item, AI, @on).

select_colour(AI) :->
	"Query a colour"::
	get(AI, member, arrow, Arrow),
	get(Arrow, colour, Old),
	new(D, dialog('Select colour')),
	send(D, transient_for, AI?frame),
	send(D, append, new(CI,	colour_item(colour, Old))),
	send(D, append, button(ok, message(D, return, CI?selection))),
	send(D, append, button(cancel, message(D, return, @nil))),
	get(D, confirm_centered, AI?frame?area?center, Rval),
	(   Rval == @nil
	->  true
	;   send(Arrow, colour, Rval),
	    send(AI, modified, @on),
	    send(AI?device, modified_item, AI, @on)
	),
	send(D, destroy).

:- pce_end_class.

test :-
	new(D, dialog),
	send(D, append, new(_AI, arrow_item)),
	send(D, open).
