/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/


:- module(draw_attribute, []).
:- use_module(library(pce)).
:- require([ between/3
	   , chain_list/2
	   , default/3
	   , forall/2
	   , member/2
	   , send_list/3
	   ]).

:- pce_autoload(font_item, library(pce_font_item)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module defines a  separate frame that allows the  user to set the
values of attributes (pen, font, etc.) of  shapes in the drawing.  The
frame contains a single dialog window, which contains dialog_items for
each of the (graphical shape) attributes that can be edited.

Regardless of the shape(s) for  which we  are editing attributes,  all
dialog items are  always  displayed.  Items that represent  attributes
not present in the  shapes edited are  greyed out to indicate such  to
the user.  As the contents  of the window  changes  each time the user
changes the selection, non-used items are not removed from the dialog.
This would change too much  to the dialog,  transforming the interface
into a ``video clip''.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(draw_attribute_editor, frame).

variable(editor,	object,		get,
	 "Editor I'm attached too").
variable(client,	chain*,		get,
	 "Objects I'm editing the attributes for").
variable(blocked,	int := 0,	get,
	 "Blocked count to avoid quadratic behaviour").

%	attributes(?Label, ?Selector)
%
%	Label is the label of the menu is the dialog.  Selector is the
%	name of the method to be activated to change the value.   Used
%	both ways around and only local to  this file, Prolog is a far
%	easier way to store this  table.  The  alternative would be to
%	create  a  sheet and  attach  it to the   class.   This  needs
%	extensions to the preprocessor.

attribute(pen,		pen).
attribute(dash,		texture).
attribute(arrows,	arrows).
attribute(fill,		fill_pattern).
attribute(colour,	colour).
attribute(font,		font).
attribute(transparent,	transparent).
attribute(radius,	radius).
attribute(x,		x).
attribute(y,		y).
attribute(w,		width).
attribute(h,		height).
attribute(closed,	closed).
attribute(interpolation,interpolation).
attribute(shadow,	shadow).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Create the attribute  window.  Like  the drawing-tool as a whole,  the
window is a subclass of the PCE class `frame' for simple communication
with its various parts.  Note the use of default/3.

`Frame <->done_message' is activated  when the frame receives a DELETE
message from the window manager, normally from a `Delete Window' entry
of the window manager.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

initialise(A, Draw:object, Label:[name]) :->
	default(Label, 'Attributes', Lbl),
	send(A, send_super, initialise, Lbl),
	send(A, done_message, message(A, quit)),
	send(A, append, new(dialog)),
	send(A, slot, editor, Draw),
	send(A, fill_dialog).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Fill the dialog with the various menus.  We defined some generic Prolog
predicates to create the various menu's.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

fill_dialog(A) :->
	get(A, member, dialog, D),
	get(A, editor, Canvas),
	get(Canvas, frame, Draw),

	make_line_menu(Pen,	pen,	 [0,1,2,3,4,5]),
	make_line_menu(Texture, texture, [none, dotted, dashed, dashdot]),
	make_line_menu(Arrows,	arrows,  [none, second, first, both]),
	make_fill_pattern_menu(Draw, FillPattern),
	make_colour_menu(Draw, Colour),
	make_font_menu(Font),
	make_transparent_menu(Transparent),
	make_coordinate_menu(X, x),
	make_coordinate_menu(Y, y),
	make_coordinate_menu(W, width),
	make_coordinate_menu(H, height),
	make_radius_menu(Radius),
	make_closed_menu(Closed),
	make_shadow_menu(Shadow),
	make_interpolation_menu(Interpolation),

	send_list([Interpolation, Shadow], alignment, right),
        send_list([Y, W, H], alignment, left),

	send_list(D, append,
		  [Pen, Texture, Arrows, FillPattern, Colour, Radius]),
	send(D, append, Shadow, right),
	send(D, append, Closed),
	send(D, append, Interpolation, right),
	send(D, append, Font),
	send(Transparent, alignment, left),
	send(D, append, Transparent, right),
	send(D, append, X),
	send(D, append, Y, right),
	send(D, append, W, right),
	send(D, append, H, right),
	
	send(D, append, button(quit, message(A, quit, @on))).
	

		/********************************
		*             MENU'S		*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
To create the menu's, we defined a predicate  make_proto_menu/4.  Each
menu_item has as value the attribute value and as  label an image with
the prototype with the corresponding value  set.  Using this approach,
the user can easily  see what a specific   attribute means.  When  the
user selects a menu-item, the menu will send the value itself.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

make_line_menu(Menu, Attribute, Values) :-
	new(Proto, line(2, 8, 28, 8)),
	make_proto_menu(Menu, Proto, Attribute, Values),
	send(Proto, done).


make_fill_pattern_menu(Draw, Menu) :-
	get(Draw, resource_value, draw_fill_patterns, Patterns),
	chain_list(Patterns, List),
	new(Proto, box(30, 16)),
	make_proto_menu(Menu, Proto, fill_pattern, List),
	send(Proto, done).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The colour menu.  When the display  is not a colour  display, the only
possible colours of an object are @default (implying the colour of the
device),  `white' and `black'.   On colour displays  we will show some
more  possibilities.  For  a somewhat  larger  set of choices, a cycle
menu may be more appropriate.

\index{colour}
Currently  the only  way  to  find   out  whether you are   using    a
black-and-white or colour display is `@display  <-depth'.  This is the
number of bits the screen uses to represent a single pixel.

Note   that  the colour  palette   is  constructed  from   a  box with
@black_image    fill pattern.   The problem  here     is  the name  of
@black_image.  It does  not represent  the  colour black, but only  an
image with all pixels set to 1.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

colour_display :-
	\+ get(@display, depth, 1).

colour(_, white).
colour(Draw, Colour) :-
	colour_display,
	get(Draw, resource_value, draw_colours, Chain),
	chain_list(Chain, List),
	member(Colour, List).
colour(_, black).

colour_display_colour(red).
colour_display_colour(green).
colour_display_colour(blue).
colour_display_colour(yellow).


make_colour_menu(Draw, Menu) :-
	new(Proto, box(30, 16)),
	send(Proto, fill_pattern, @black_image),
	findall(colour(Colour), colour(Draw, Colour), Colours),
	make_proto_menu(Menu, Proto, colour, [@default|Colours]),
	send(Proto, done).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The menu below is for the `transparent'  attribute of text.   When @on
(default), only the pixels of  the font are affected.   Otherwise, the
bounding box of the text will be  cleared first.  Non-transparent text
is often used to mark lines or display on top of filled areas.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

make_transparent_menu(Menu) :-
	new(Proto, figure),
	send(Proto, display, new(B, box(30,16))),
	send(B, fill_pattern, @grey50_image),
	send(Proto, display, new(T, text('T', left,
					  font(screen, roman, 10)))),
	send(T, center, B?center),
	send(Proto, send_method, send_method(transparent, vector(bool),
					     message(T, transparent, @arg1))),
	make_proto_menu(Menu, Proto, transparent, [@on, @off]),
	get(Menu, reference, Ref),
	get(Ref, copy, CRef),
	send(Menu, show_label, @off),
	send(Menu, reference, CRef),
	send(Proto, done).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Create  a menu for  some prototype attribute.   Each menu_item   has a
`menu_item <->value'  equal   to  the corresponding  element    of the
`Values'  chain.  Each  label  is a image   with an  outline-box  and
`Proto' with the appropriate attribute setting drawn into it.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_global(@menu_proto_box, new(box(30,16))).

make_proto_menu(Menu, Proto, Attribute, Values) :-
	attribute(Label, Attribute),
	new(Menu, menu(Label, choice,
		       message(@receiver?frame, client_attribute,
			       Attribute, @arg1))),
	send(Menu, off_image, @nil),
	send(Menu, border, 2),
	send(Menu, layout, horizontal),
	(   Attribute == colour
	->  Kind = pixmap
	;   Kind = bitmap
	),
	(   member(Value, Values),
		send(Proto, Attribute, Value),
		new(I, image(@nil, 30, 16, Kind)),
		send(I, draw_in, @menu_proto_box),
		send(I, draw_in, Proto),
		send(Menu, append, menu_item(Value, @default, I)),
		fail
	;   true
	).
	

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The coordibate menu is a rather trivial  text_item.  Note the setting of
the field-width and `dialog_item ->alignment:  left'.  The latter places
the items just right to one  another   instead  of vertically aligned in
columns.

NOTE:	We should make a subclass to allow for entering integers only.
	To do this properly, we should know about each keystroke in
	the menu rather than only the return.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

make_coordinate_menu(Menu, Selector) :-
	attribute(Label, Selector),
	new(Menu, text_item(Label, 0,
			    message(@receiver?frame, client_attribute,
				    Selector, @arg1))),
	send(Menu, length, 5).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The radius of a box is  the radius of  the circle sections (arcs) used
for rounding  the corners.  As the  user propably   does  not want  to
specify an exact   number of pixels,   a  slider-menu is used.   As  a
disadvantage, the range has to be specified in advance, and 100 is not
the absolute limit.  Note that by setting both the range and the width
to 100, the slider operates 1:1.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

make_radius_menu(Menu) :-
	attribute(Label, radius),
	new(Menu, slider(Label, 0, 100, 0,
			 message(@receiver?frame, client_attribute,
				 radius, @arg1))),
	send(Menu, drag, @on),
	send(Menu, width, 100).


make_shadow_menu(Menu) :-
	attribute(Label, shadow),
	new(Menu, menu(Label, cycle,
		       message(@receiver?frame, client_attribute,
			       shadow, @arg1))),
	forall(between(0, 5, Shadow), send(Menu, append, Shadow)).


make_closed_menu(Menu) :-
	attribute(Label, closed),
	new(Menu, menu(Label, choice,
		       message(@receiver?frame, client_attribute,
			       closed, @arg1))),
	send_list(Menu, append, [@off, @on]).


make_interpolation_menu(Menu) :-
	attribute(Label, interpolation),
	new(Menu, slider(Label, 0, 10, 0,
			 message(@receiver?frame, client_attribute,
				 interpolation, @arg1))),
	send(Menu, width, 100).


		/********************************
		*             FONTS		*
		********************************/

make_font_menu(M) :-
	new(M, font_item(font, @default,
			 message(@receiver?frame, client_attribute,
				 font, @arg1))).


		/********************************
		*              QUIT		*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
For a secondary  window like  this  attribute  editor,  it might be  a
useful idea  not to destroy the window  if the user  hits  `quit', but
just to unmap it from the display using `Frame ->show: @off'.  In this
case, it can  be remapped on the  display very  quickly   and when the
window has certain status  information attached to  it,  this  will be
maintained.   For the   case of this editor,  this  only concernes the
coordinates of the window.

To control between  actual  destruction  and   just unmapping it,   an
optional   boolean   argument has been   attached.  This  approach has
several advantages.  If  the caller wants to  descriminate, it can  do
so.  For all cases where the caller does not want  to discriminate, we
have one central place to change the default behaviour.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

quit(A, ShowOff:[bool]) :->
	(   ShowOff == @on
	->  send(A, show, @off)
	;   send(A?editor, attribute_editor, @nil),
	    send(A, free)
	).


		/********************************
		*     CLIENT COMMUNICATION	*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
->fill_items fills and  (de)activates all  dialog items.  The argument
is a chain of shapes (normally the <-selection of the canvas).  If one
of the elements of the selection  has the specified attribute, it will
be activated and the ->selection of the menu will be set accordingly.

If more than one object   in the  selection  has some  attribute,  the
->selection  of the  item  will  be the attribute  value of  the first
object in the chain that is has the attibute.  This is a rather simple
way of handling this case, but what else can we do?
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

fill_items(A, Client:chain) :->
	"Fill the dialog items from chain of shapes"::
	get(A, member, dialog, Dialog),
	attribute(Label, Selector),
	get(Dialog, member, Label, Menu),
	(   get(Client, find,
		and(message(@arg1, has_send_method, has_attribute),
		    message(@arg1, has_attribute, Selector)),
		Proto),
	    get(Proto, attribute, Selector, Value)
	->  send(Menu, active, @on),
	    send(Menu, selection, Value)
	;   send(Menu, active, @off)
	),
	fail ; true.


block(A) :->
	"<-blocked++"::
	get(A, blocked, B0),
	B1 is B0 + 1,
	send(A, slot, blocked, B1).

unblock(A) :->
	"<-blocked--"::
	get(A, blocked, B0),
	B1 is B0 - 1,
	send(A, slot, blocked, B1).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Set the chain of shapes for which we are editing the attributes.  Note
that if the window is not shown, we won't update the contents.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

client(A, Client:chain*) :->
	"Set the graphical I'm editing"::
	get(A, member, dialog, Dialog),
	(   get(A, blocked, B), B == 0
	->  (    Client == @nil
	    ->   send(Dialog?graphicals, for_some,
		      message(@arg1, active, @off))
	    ;    send(A, fill_items, Client)
	    )
	;   true
	),
	send(A, slot, client, Client).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Set the value of an attribute for the clients.  The value is set for
each shape that accepts ->has_attribute.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

client_attribute(A, Selector:name, Val:any) :->
	"Set attribute of client object"::
	(   get(A, client, Chain), Chain \== @nil
	->  send(A, block),
	    send(A?client, for_some,
		 if(message(@arg1, has_attribute, Selector),
		    message(@arg1, attribute, Selector, Val))),
	    send(A, unblock)
	;   true
	).

:- pce_end_class.
