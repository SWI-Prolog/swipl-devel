/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 2001 University of Amsterdam. All rights reserved.
*/

:- module(pce_hsv_browser,
	  [ hsv_browser/0
	  ]).

hsv_browser :-
	send(new(hsv_browser), open).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module demonstrates the relation between the RBG colour-model used
internally and the HSV model.  The latter is often much more natural to
the user.  See colour->initialise for details.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(hsv_browser, dialog, "Browse colours in HSV model").

variable(current_colour, colour, get, "Current colour value").

item('H', hue,	       0-360).
item('S', saturnation, 0-100).
item('V', value,       0-100).
item('R', red,	       0-255).
item('G', green,       0-255).
item('B', blue,	       0-255).

initialise(D, Init:[colour]) :->
	send_super(D, initialise, 'Demonstrate HSV'),
	send(D, append, new(Box, box(100, 20))),
	send(Box, attribute, hor_stretch, 100),
	(   item(Label, Selector, Low-High),
	    send(D, append,
		 new(Slider, slider(Label, Low, High, Low,
				    message(D, Selector, @arg1)))),
	    send(Slider, drag, @on),
	    send(Slider, attribute, hor_stretch, 100),
	    fail
	;   true
	),
	send(D, append, new(HT, text_item(hex_triplet))),
	send(HT, length, 8),
	send(HT, alignment, right),
	send(HT, editable, @off),
	send(D, append, button(quit)),
	send(D, append, button(copy)),
	send(D, resize_message, message(D, layout, @arg2)),
	(   Init \== @default
	->  send(D, current_colour, Init)
	;   send(D, current_colour, colour(@default, 180, 50, 50, hsv))
	).

quit(D) :->
	"Quit the demo tool"::
	send(D, destroy).

copy(D) :->
	"Copy triplet to cpliboard"::
	get(D, member, hex_triplet, HT),
	get(HT, selection, Triplet),
	send(D?display, copy, Triplet).

:- pce_group(update).

current_colour(D, C:colour, From:[{rgb,hsv}]) :->
	"Set the current colour, updating all items"::
	send(D, slot, current_colour, C),
	(   From \== hsv
	->  update(D, 'H', C, hue),
	    update(D, 'S', C, saturnation),
	    update(D, 'V', C, value)
	;   true
	),
	(   From \== rgb
	->  update_rgb(D, 'R', C, red),
	    update_rgb(D, 'G', C, green),
	    update_rgb(D, 'B', C, blue)
	;   true
	),
	get(D, member, box, Box),
	send(Box, fill_pattern, C),
	get(D, member, hex_triplet, HT),
	send(HT, selection, C?name).

update(D, Name, Colour, Selector) :-
	get(Colour, Selector, Value),
	get(D, member, Name, Item),
	send(Item, selection, Value).

update_rgb(D, Name, Colour, Selector) :-
	get(Colour, Selector, Value),
	RGB is Value // 256,
	get(D, member, Name, Item),
	send(Item, selection, RGB).

value(D, Selector:name, Val) :<-
	"Get value from corresponding item"::
	item(ItemName, Selector, _),
	get(D, member, ItemName, Item),
	get(Item, selection, Val).

:- pce_group(hsv).

hue(D, H:'0..360') :->
	H2 is min(H, 359),
	get(D, value, saturnation, S),
	get(D, value, value, V),
	send(D, current_colour, colour(@default, H2, S, V, hsv), hsv).

saturnation(D, S:'0..100') :->
	get(D, value, hue, H),
	get(D, value, value, V),
	send(D, current_colour, colour(@default, H, S, V, hsv), hsv).

value(D, V:'0..360') :->
	get(D, value, hue, H),
	get(D, value, saturnation, S),
	send(D, current_colour, colour(@default, H, S, V, hsv), hsv).

:- pce_group(rgb).

red(D, R:'0..255') :->
	get(D, value, red, G),
	get(D, value, blue, B),
	rgb(R, G, B, Colour),
	send(D, current_colour, Colour, rgb).

green(D, G:'0..255') :->
	get(D, value, red, R),
	get(D, value, blue, B),
	rgb(R, G, B, Colour),
	send(D, current_colour, Colour, rgb).

blue(D, B:'0..255') :->
	get(D, value, red, R),
	get(D, value, green, G),
	rgb(R, G, B, Colour),
	send(D, current_colour, Colour, rgb).

rgb(R,G,B, colour(@default, Red, Green, Blue)) :-
	Red is R * 257,
	Green is G * 257,
	Blue is B * 257.

:- pce_end_class(hsv_browser).
