/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

:- module(drag_and_drop, []).
:- use_module(library(pce)).
:- require([ default/3
	   , ignore/1
	   ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Define a gesture that allows to  `drag-and-drop' objects.  The target on
which to drop should understand the method  ->drop, which will be called
with the dropped graphical  as  an   argument.   If  may  also implement
->preview_drop, which will be called to   provide visual feedback of the
drop that will take place when the button is released here.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(drag_and_drop_gesture, gesture,
		   "Drag and drop command-gesture").

variable(target,	graphical*,	get,  "Drop target").
variable(warp,		bool,		both, "Pointer in center?").
variable(offset,	point,		get,  "Offset X-y <->caret pointer").
variable(get_source,	function*,	both, "Function to map the source").
variable(source, 	any,	        get,  "Current source").

resource(warp,   bool,        '@on', "Pointer in center?").
resource(button, button_name, left,  "Button on which gesture operates").

initialise(G, But:[button_name], M:[modifier], W:[bool]) :->
	"Create from button, modifiers and warp"::
	send(G, send_super, initialise, But, M),
	default(W, resource(G, warp), Warp),
	send(G, warp, Warp),
	send(G, slot, offset, new(point)).


initiate(G, Ev:event) :->
	"Change the cursor"::
	get(Ev, receiver, Gr),
	send(G, set_source, Ev),
	get(G, cursor, Gr, Ev, Cursor),
	send(G, cursor, Cursor).


set_source(G, Ev:event) :->
	"Find source from event"::
	get(Ev, receiver, Gr),
	get(G, get_source, Function),
	(   Function == @nil
	->  send(G, slot, source, Gr)
	;   get(Function, '_forward', Gr, Source),
	    send(G, slot, source, Source)
	).


cursor(G, Gr, Ev, Cursor:cursor) :<-
	"Create cursor from the graphical"::
	get(Gr?area, size, size(W, H)),
	(   get(G, warp, @on)
	->  new(HotSpot, point(W/2, H/2)),
	    send(Gr, pointer, HotSpot)
	;   get(Ev, position, Gr, HotSpot)
	),  
	send(G?offset, copy, HotSpot),
	new(BM, image(@nil, W, H)),
	send(BM, draw_in, Gr, point(0,0)),
	send(BM, or, image('cross.bm'), point(HotSpot?x-8, HotSpot?y-8)),
	new(Cursor, cursor(@nil, BM, @default, HotSpot)).


drag(G, Ev:event) :->
	"Find possible ->drop target"::
	get(G, source, Source),
	(   get(Ev, inside_sub_window, Frame),
	    get(Ev, inside_sub_window, Frame, Window),
	    get(Window, find, Ev,
		or(G?target == @arg1,
		   message(G, target, Source, @arg1)),
		_Gr)
	->  true
	;   send(G, target, Source, @nil)
	).


target(G, Source:any, Gr:graphical*) :->
	"Make the given object the target"::
	(   Gr == @nil
	;   get(Gr, is_displayed, @on),
	    send(Gr, has_send_method, drop)
	),
	ignore((get(G, target, Old),
		send(Old, has_send_method, preview_drop),
		send(Old, preview_drop, @nil))),
	(   get(Gr, send_method, preview_drop, tuple(_, Method)),
	    get(Method, argument_type, 1, Type),
	    get(Type, check, Source, Src)
	->  send(Gr, preview_drop, Src)
	;   true
	),
	send(G, slot, target, Gr).
		

terminate(G, Ev:event) :->
	"->drop to <-target"::
	send(Ev?window, focus_cursor, @nil),
	send(G, cursor, @default),
	get(G, slot, target, Target),
	get(G, source, Source),
	send(G, slot, source, @nil),
	(   Target == @nil
	->  true
	;   send(G, target, Source, @nil),
	    get(Target, send_method, drop, tuple(_, Method)),
	    get(Method, argument_type, 1, T1),
	    get(T1, check, Source, Src),
	    get(Target, display, Display),
	    (	get(Method, argument_type, 2, Type),
		send(Type, validate, new(point))
	    ->	get(Ev, position, Target, Pos),
		get(Pos, copy, P2),
		send(P2, minus, G?offset),
		send(Display, busy_cursor),
		ignore(send(Target, drop, Src, P2)),
		send(Display, busy_cursor, @nil)
	    ;	send(Display, busy_cursor, @default),
		ignore(send(Target, drop, Src)),
		send(Display, busy_cursor, @nil)
	    )
	).

:- pce_end_class.
