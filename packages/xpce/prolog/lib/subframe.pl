/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

:- module(pce_subframe, []).
:- use_module(library(pce)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This file defines the XPCE class subframe.  Class subframe is a subclass
of class window that  manages  a  set   of  sub-windows  and  allows for
`frames-on-windows'.  The behaviour of class subframe   is  based on the
TWM window manager.

The normal way to use this class is

	1) Relate a set of windows (using ->above, etc.)
	2) Create an instance of subframe.
	3) append one of the windows using ->append
	4) open the frame using ->open.  The argument is a window
	   on which the frame is to be opened.

See also the predicate test/0 at the end of this file.

Other important public methods are

	->button		define a title-bar button for this frame
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


:- pce_autoload(twm_resize_button,	library(twm_resize_button)).

:- pce_begin_class(subframe, window).

variable(left_buttons,	chain,	get, "Buttons to the left").
variable(right_buttons, chain,  get, "Buttons to the right").
variable(label_text,	text,	get, "Text for the label").
variable(members,	chain,  get, "Chain with member windows").
variable(title_line,	line,	get, "Line below titlebar").
variable(title_box,	box,	get, "Box in titlebar").

variable(closed,	bool,	get, "Iconic/open representation").
variable(saved_area,	area,	get, "Area of other representation").
variable(saved_pen,	int,	get, "Pen value of other representation").
variable(icon,		bitmap,	get, "Bitmap representing the icon").
variable(icon_label_text,text,  get, "Text for iconic representation").

resource(label_font,	font,	'@helvetica_bold_14').
resource(pen,		int,	'3').
resource(cursor,	cursor, 'hand2').
resource(icon,		bitmap, 'pce.bm').

initialise(F, Label:char_array) :->
	"Create from Label"::
	send(F, slot, icon, @default),

	send(F, send_super, initialise),

	get(F, resource_value, label_font, Font),
	default(Label, '', Lbl),
	send(F, slot, title_box, new(TB, box)),
	send(TB, pen, 0),
	send(TB, fill_pattern, @grey50_image),
	send(F, slot, title_line, new(TL, line)),
	send(TL, pen, F?pen),
	send(TL, points, 0, -F?pen/2, 0, -F?pen/2),
	send(F, slot, label_text, text(Lbl, left, Font)),
	send(F, slot, left_buttons, new(chain)),
	send(F, slot, right_buttons, new(chain)),
	send(F, slot, members, new(chain)),

	send(F, slot, saved_area, area(20,20,64,64)),
	send(F, slot, saved_pen, 0),
	send(F, slot, closed, @off),
	send(F, slot, icon_label_text, text(Lbl, left, Font)).


append(F, W:window) :->
	"Append a new window"::
	send(W?tile?root, for_all, message(F?members, add, @arg1)).


title_bar_height(F, Margin, Height) :-
	Margin = 2,
	
	new(H, number(0)),
	send(F?left_buttons, for_all, message(H, maximum, @arg1?height)),
	send(F?right_buttons, for_all, message(H, maximum, @arg1?height)),
	send(H, maximum, F?label_text?height),
	send(H, plus, 2*Margin + F?title_line?pen),

	get(H, value, Height).


geometry(F, FX:[int], FY:[int], FW:[int], FH:[int]) :->
	"Update position and layout of the title-bar"::
	send(F, send_super, geometry, FX, FY, FW, FH),
	
	(   get(F, closed, @off)
	->  title_bar_height(F, Margin, H),
	    send(F, scroll_to, point(0, -H)),
	    
	    new(X, number(Margin)),
	    get(H, value, PLH),
	    Y is -PLH + Margin,
	    send(F?left_buttons, for_all,
		 and(message(@arg1, set, X, Y),
		     message(F, display, @arg1),
		     message(X, plus, Margin + @arg1?width))),
	    send(F?label_text, set, X, Y),
	    send(F, display, F?label_text),
	    send(X, value, F?width - Margin - 2*F?pen),
	    send(F?right_buttons, for_all,
		 and(message(X, minus, @arg1?width),
		     message(@arg1, set, X, Y),
		     message(F, display, @arg1),
		     message(X, minus, Margin))),
	    
	    send(F, display, F?title_line),
	    send(F, display, F?title_box),
	    send(F?title_line, end_x, F?width),
	    send(F?title_box, set,
		 F?label_text?right_side + Margin,
		 -(H - Margin),
		 X - F?label_text?right_side - Margin,
		 H - 2*Margin - F?pen),
	    send(F?tile, set, 0, 0, F?width-2*F?pen, F?height - H - 2*F?pen),

	    send(F?members, for_all, message(F, display, @arg1))
	;   send(F, scroll_to, point(0, 0)),
	    send(F, display, F?icon),
	    send(F, display, F?icon_label_text),
	    send(F?icon_label_text, center_x, F?icon?center_x),
	    send(F?icon_label_text, y, F?icon?bottom_side)
	).


request_geometry(F, X:[int], Y:[int], W:[int], H:[int]) :->
	"Add height for the title-bar"::
	(   get(F, closed, @off)
	->  (	H \== @default
	    ->	title_bar_height(F, _Margin, TH),
	        FH is TH + H
	    ;	FH = H
	    ),
	    FW = W
	;   FH = F?icon?height + F?icon_label_text?height + 2 * F?pen,
	    new(FW, number(F?icon?width)),
	    send(FW, maximum, F?icon_label_text?width),
	    send(FW, plus, 2 * F?pen)
	),
	send(F, geometry, X, Y, FW, FH).


tile(F, Tile:tile) :<-
	"Get root-tile of the window tree"::
	get(F?members?head, tile, SubTile),
	get(SubTile, root, Tile).


fit(F) :->
	"Fix layout"::
	get(F, tile, RootTile),
	send(RootTile, enforce),
	send(F, set,
	     @default, @default, RootTile?ideal_width, RootTile?ideal_height).


open(F, W:device, Pos:[point]) :->
	"Open the sub-frame on indicated device"::
	send(F, fit),
	send(W, display, F, Pos).


		 /*******************************
		 *	     BUTTONS		*
		 *******************************/

button(F, Op:'name|code', Image:[image], Place:[{left,right}]) :->
	"Attach a button to the titlebar"::
	default(Image, ?(Op, append, '.bm'), Img),

	new(Image16, image(@nil, 18, 18)),
	new(Bm, bitmap(Img)),
	send(Bm, center, point(9,9)),
	send(Image16, draw_in, Bm),
	send(Bm, done),
	send(Image16, draw_in, box(18,18)),

	(   Op == resize
	->  new(Button, twm_resize_button(Image16))
	;   new(Button, bitmap(Image16)),
	    (	atom(Op)
	    ->	Action = message(F, Op)
	    ;	Action = Op
	    ),
	    send(Button, recogniser, click_gesture(left, '', single, Action))
	),
	(   Place == right
	->  send(F?right_buttons, append, Button)
	;   send(F?left_buttons, append, Button)
	),
	(   get(F, device, Dev),
	    Dev \== @nil
	->  send(F, geometry)		% update
	;   true
	).


		 /*******************************
		 *	     EVENTS		*
		 *******************************/

:- pce_global(@open_subframe_recogniser,
	      new(handler_group(click_gesture(left, '', single,
					      message(@receiver, expose)),
				new(move_outline_gesture(left))))).
:- pce_global(@closed_subframe_recogniser,
	      new(handler_group(click_gesture(left, '', double,
					      message(@receiver, closed,
						      @off)),
				new(move_outline_gesture(left))))).


event(F, Ev:event) :->
	(   send(F, send_super, event)
	->  true
	;   (	get(F, closed, @off)
	    ->	send(@open_subframe_recogniser, event, Ev)
	    ;	send(@closed_subframe_recogniser, event, Ev)
	    )
	).

		 /*******************************
		 *     ICONIC REPRESENTATION	*
		 *******************************/

closed(F, Val:bool) :->
	"Change closed status"::
	(   get(F, closed, Val)
	->  true
	;   new(OtherArea, area),
	    send(OtherArea, copy, F?saved_area),
	    send(F?saved_area, copy, F?area),
	    send(F?graphicals, for_all, message(@arg1, displayed, @off)),
	    send(F, slot, closed, Val),
	    get(F, saved_pen, OtherPen),
	    send(F, slot, saved_pen, F?pen),
	    send(F, pen, OtherPen),
	    send(F, area, OtherArea),
	    send(F, expose)
	).


iconify(F) :->
	"->closed: @on (button call-back)"::
	send(F, closed, @on).

:- pce_end_class.

		 /*******************************
		 *               TEST		*
		 *******************************/

test :-
	send(new(@p, picture('Desktop', size(600, 500))), open),

	new(@p2, picture),
	send(new(@b, browser), right, @p2),
	send_list(@b, append, [aap, noot, mies]),

	new(@f, subframe('Hello')),
	send(@f, button, iconify, 'iconify.bm', left),
	send(@f, button, destroy, 'delete.bm', left),
	send(@f, button, resize, 'resize.bm', right),

	send(@f, append, @p2),
	send(@f, open, @p, point(50,50)),
	
	send(@p2, display, new(B, bitmap('pce.bm'))),
	send(B, recogniser, new(move_gesture)),

	new(@f2, subframe('I am small')),
	send(@f2, append, new(@p3, picture)),

	send(@f2, button, iconify, 'iconify.bm', left),
	send(@f2, button, destroy, 'delete.bm', left),
	send(@f2, button, resize, 'resize.bm', right),

	send(@f2, open, @p, point(200, 200)).
