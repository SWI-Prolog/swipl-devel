/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/


:- module(draw_gesture, []).

:- use_module(library(pce)).
:- require([ between/3
	   , concat/3
	   , send_list/3
	   ]).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module defines event handling for the shapes.  Event handling for
dialog_items  is  predefined   because  the  UI  of dialog_items    is
standardised.  Event handling for  general purpose  graphicals  can be
specified by defining the method `Graphical ->event'.

The default   behaviour  of ->event (defined  at  the  level  of class
graphical)  is to   look up   the `recognisers'  slot of  the attached
interceptor  (see `Object  ->recogniser')  and test  if   any  of  the
attached interceptor is prepared to accept the event.

This implies    there are  three ways to     define event parsing  for
graphical objects:

	1) Attach a recogniser the object.
	2) Write an ->event method that parses the events.
	3) Write an ->event method that forwards the event to
	   recognisers.

For PceDraw we chose the  latter  approach for shapes.   See  also the
file canvas.pl. Provided the recognisers do not directly refer  to the
object for which they handle events as in

	send(B, recogniser, click_gesture(left, '', single,
					  message(B, inverted, @on)))

but, refer indirectly as in

	send(B, recogniser, click_gesture(left, '', single,
					  message(@receiver, inverted,
						  @on)))

recognisers can be attached to any number of graphical  objects.  This
file defines generic recognisers that are used by `Shape ->event'.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

		/********************************
		*      RECOGNISER OBJECTS	*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Below are the declarations   of the  various recognisers.   Note  that
using  pce_global/2, the actual creation  of the recogniser is delayed
to the first time an event  occurs  on an  object that uses a specific
recogniser.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

			/* Create shapes */

:- pce_global(@draw_create_resize_gesture,
	      new(draw_create_resize_gesture)).
:- pce_global(@draw_create_line_gesture,
	      new(draw_create_line_gesture)).
:- pce_global(@draw_create_path_gesture,
	      new(draw_create_path_gesture)).
:- pce_global(@draw_connect_gesture,
	      new(handler_group(new(draw_connect_gesture),
				new(draw_connect_create_gesture)))).

			/* Select shapes */

:- pce_global(@draw_shape_select_recogniser,
	      make_draw_shape_select_recogniser).
:- pce_global(@draw_warp_select_gesture,
	      new(draw_warp_select_gesture)).

			/* Move/Resize shapes */

:- pce_global(@draw_move_outline_gesture,
	      new(handler_group(new(draw_move_selection_gesture),
				new(draw_move_gesture)))).
:- pce_global(@draw_resize_gesture,
	      new(handler_group(new(draw_resize_selection_gesture),
				new(draw_resize_gesture)))).

			/* Combined shape recognisers */

:- pce_global(@draw_resizable_shape_recogniser,
	      new(handler_group(@draw_shape_select_recogniser,
				@draw_resize_gesture,
				@draw_move_outline_gesture,
				@draw_connect_gesture,
				@draw_shape_popup_gesture))).
:- pce_global(@draw_text_recogniser,
	      new(handler_group(@draw_shape_select_recogniser,
				@draw_edit_text_recogniser,
				new(draw_resize_selection_gesture),
				@draw_move_outline_gesture,
				@draw_connect_gesture,
				@draw_shape_popup_gesture))).
:- pce_global(@draw_compound_recogniser,
	      new(handler_group(@draw_resizable_shape_recogniser,
				@draw_compound_draw_text_recogniser))).
:- pce_global(@draw_connection_recogniser,
	      new(handler_group(@draw_shape_select_recogniser,
				@draw_connect_gesture,
				@draw_shape_popup_gesture))).
:- pce_global(@draw_bitmap_recogniser,
	      new(handler_group(@draw_shape_select_recogniser,
				@draw_move_outline_gesture,
				@draw_connect_gesture,
				@draw_shape_popup_gesture))).
:- pce_global(@draw_line_recogniser,
	      new(handler_group(@draw_shape_select_recogniser,
				@draw_connect_gesture,
				@draw_shape_popup_gesture,
				new(draw_change_line_gesture),
				new(draw_move_selection_gesture),
				new(move_gesture)))).
:- pce_global(@draw_path_recogniser,
	      new(handler_group(@draw_shape_select_recogniser,
				@draw_shape_popup_gesture,
				new(draw_modify_path_gesture),
				@draw_edit_path_gesture,
				@draw_resize_gesture,
				@draw_move_outline_gesture,
				new(move_gesture)))).


		/********************************
		*            SELECT		*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
When in select mode, left-click on an object makes  it  the selection,
shift-left-click   adds  or  deletes it  to/from   the   selection and
left-dragging   indicates  an   area in which   all objects  should be
selected.

Clicking  on an object is  to be defined  at the level  of the  object
itself, where the drag  version is to be  defined at  the level of the
canvas.  This is not very elegant as it implies  we have to create two
recognisers;    one for the  shapes  and   one  for the canvas.    The
alternative  would be one  recogniser at  the level of the  canvas and
find the object below  the  mouse on a  click.  It is difficult to say
which of the two approaches is better.

The  recogniser  for  shapes is  defined   below.   It  consists  of a
handler_group with two  click_gestures.  This  implementation  is  far
simpler  than defining a  new  class.   Note  the definition  of   the
obtainers before     defining the gestures   themselves.   This method
employs reusability of object and is easier to read.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

make_draw_shape_select_recogniser(G) :-
	new(Shape, @event?receiver),
	new(Canvas, Shape?window),
	new(SelectMode, Canvas?(mode) == select),

	new(G, handler_group(click_gesture(left, '', single,
					   message(Canvas, selection,
						   Shape),
					   SelectMode),
			     click_gesture(left, s, single,
					   message(Canvas, toggle_select,
						   Shape),
					   SelectMode))).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The `warp_gesture' allows  the user to  indicate an area by dragging a
button and then selects all objects inside the indicated  area.  It is
a rather typical   example of a  gesture definition.   The  resource/3
declarations   define the  X-resources  that  apply:  the  button that
activates the gesture,  the modifiers required (shift, control,  meta)
and the  cursor that indicates the  gesture is active.  These resource
values are handled by the super-class gesture.

The variable `outline' keeps track of the box that is used to indicate
the area.  It can be stored here, as only one gesture can be active at
a time.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(draw_warp_select_gesture, gesture).

resource(button,	button_name,	left).
resource(modifier,	modifier,	'').
resource(cursor,	cursor,		hand2).

variable(outline,	box,		get,
	 "Outline to `warp' objects").

initialise(G, B:[button_name], M:[modifier]) :->
	send(G, send_super, initialise, B, M),
	send(G, slot, outline, new(Box, box(0,0))),
	send(Box, texture, dotted).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The verify method is called to validate it is ok to start the gesture.
In this context, this implies the canvas is  in select mode  and there
are actually objects displayed.  It is  called after a  button-down of
the appropriate button with the appropriate modifier is detected.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

verify(_G, Ev:event) :->
	get(Ev, receiver, Canvas),
	get(Canvas, mode, select),
	\+ send(Canvas?graphicals, empty).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
After `Gesture ->verify'  succeeds `Gesture ->initiate'  is  called to
start  the gesture.  It  resizes  the outline  to  size(0,0) using the
`Graphical ->set'  (which  avoids creating  a  size  object) and  than
displays it at the mouse-position.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

initiate(G, Ev:event) :->
	get(Ev, receiver, Canvas),
	send(G?outline, set, @default, @default, 0, 0),
	send(Canvas, display, G?outline, Ev?position).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
On each drag-event,   this  method is   called.  It  just resizes  the
outline.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

drag(G, Ev:event) :->
	send(G?outline, corner, Ev?position).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
On the corresponding up-event, this  method is called.  It removes the
outline from the   device and sends  `draw_canvas  ->selection' to the
canvas with a chain of all objects inside the area.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

terminate(G, Ev:event) :->
	send(G, drag, Ev),
	get(G, outline, Outline),
	get(Ev, receiver, Canvas),
	send(Outline, device, @nil),
	get(Canvas, inside, Outline?area, ToSelect),
	send(ToSelect, for_all,
	     if(not(message(@arg1?class, instance_of, draw_shape_class)),
		message(ToSelect, delete, @arg1))),
	send(Canvas, selection, ToSelect).

:- pce_end_class.



		/********************************
		*      CREATE FROM PROTOTYPE	*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Prototypes have their own size, which  implies creating a prototype is
done using a simple click.  It first displays  a clone of `draw_canvas
<-proto' at the position of the mouse.  Next it sends the ->start_text
message to  the created  prototype  to   allow the   user filling  the
text-fields of the proto instance.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_global(@draw_create_proto_recogniser,
	      make_create_proto_recogniser).

make_create_proto_recogniser(R) :-
	new(Canvas, @event?receiver),
	new(Proto, Canvas?proto),
	new(R, click_gesture(left, '', single,
			     and(assign(new(Clone, var), Proto?clone),
				 message(Clone, center, @event?position),
				 message(Canvas, display, Clone),
				 if(message(Clone, has_send_method,
					    start_text),
				    message(Clone, start_text)),
				 message(Canvas, auto_align, Clone, create)),
			     Canvas?(mode) == draw_proto)).


		/********************************
		*     CREATE RESIZABLE SHAPE	*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Create shapes that do not have a predefined size.  The top-left-corner
of   the  object   will     be at  the mouse-down      location,   the
bottom-right-corner at the mouse-up location.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(draw_create_resize_gesture,	gesture).

resource(button,	button_name,	left).
resource(modifier,	modifier,	'').
resource(cursor,	cursor,		bottom_right_corner).
resource(minimum_size,  int,		3,
	 "Mimimum width/height of the object").

variable(object,	graphical*,	both,
	 "Object created").

verify(_G, Ev:event) :->
	"Only active when in create_resize mode"::
	get(Ev?receiver, mode, draw_resize).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Display a clone of `draw_canvas <-proto' and attach it to the gesture.
The latter is necessary because @event?receiver refers to the canvas.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

initiate(G, Ev:event) :->
	"Paint the prototype"::
	get(Ev, receiver, Canvas),
	get(Canvas?proto, clone, Object),
	send(G, object, Object),
	send(Canvas, display, Object, Ev?position).
	

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Drag is easy.  The only non-standard thing it does is  to disallow the
width or height of the created object to become negative.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

drag(G, Ev:event) :->
	"Resize the object"::
	get(Ev, position, Pos),
	get(G, object, Obj),
	get(Pos, x, EX), get(Pos, y, EY),
	get(Obj, x, OX), get(Obj, y, OY),
	max(EX, OX, CX),
	max(EY, OY, CY),
	send(Obj, corner, point(CX, CY)).


max(A, B, M) :- A >= B, !, M = A.
max(_, B, B).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Terminate checks whether  the created object  is  too small  and  then
deletes it.  It  resets the <->object  variable  of the gesture.   The
latter is  necessary to  avoid  a dangling reference when  the created
object would be destroyed: this object does not know it  is referenced
by the gesture.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

terminate(G, Ev:event) :->
	"Delete the object if it is too small"::
	send(G, drag, Ev),
	get(G, object, Obj),
	send(G, object, @nil),
	get(Obj, width, W),
	get(Obj, height, H),
	abs(W, AbsW),
	abs(H, AbsH),
	get(G, resource_value, minimum_size, S),
	(   (AbsW < S ; AbsH < S)
	->  send(Obj, free)
	;   get(Ev, receiver, Canvas),
	    send(Canvas, auto_align, Obj, create),
	    send(Canvas, modified)
	).

abs(X, Y) :-
	(   X < 0
	->  Y is -X
	;   Y = X
	).

:- pce_end_class.

		/********************************
		*            LINE		*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Creating a line is very similar to creating a resizable shape.   Only,
->drag sets   the end-point rather  than   the corner  and ->terminate
should  validate the  length rather than  the  minimum  of width   and
height.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(draw_create_line_gesture, draw_create_resize_gesture).

resource(cursor,	 cursor,	plus).

verify(_G, Ev:event) :->
	"Only active when in create_line_mode"::
	get(Ev?receiver, mode, draw_line).

drag(G, Ev:event) :->
	send(G?object, end, Ev?position).

terminate(G, Ev:event) :->
	send(G, drag, Ev),
	get(G, object, Line),
	send(G, object, @nil),
	get(Line, length, L),
	get(G, resource_value, minimum_size, MS),
	(   L < MS
	->  send(Line, free)
	;   get(Ev, receiver, Canvas),
	    send(Canvas, auto_align, Line, create)
	).

:- pce_end_class.

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The  draw_change_line_gesture does to a  line what  the resize_gesture
does to an  object that has   a real area:  one can  drag one  of  the
end-points.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(draw_change_line_gesture, gesture).

resource(button,	button_name,	middle).
resource(cursor,	cursor,		plus).

variable(side,		name*,		both,
	 "Start or end").

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Verify tries to find   the end-point  and  records the result  in  the
variable <->side.  It fails  if the event is  too far away from either
end of the line.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

verify(G, Ev:event) :->
	get(Ev, receiver, Line),
	get(Ev, position, Line?device, Pos),
	(   get(Line?start, distance, Pos, D),
	    D < 5
	->  send(G, side, start)
	;   get(Line?end, distance, Pos, D),
	    D < 5
	->  send(G, side, end)
	;   fail
	).

initiate(G, Ev:event) :->
	get(Ev, receiver, Line),
	send(Line?device, pointer, Line?(G?side)).

drag(G, Ev:event) :->
	get(Ev, receiver, Line),
	get(G, side, Side),
	send(Line, Side, ?(Ev, position, Line?device)).

terminate(G, Ev:event) :->
	send(G, drag, Ev).

:- pce_end_class.


		/********************************
		*             PATH		*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Class `draw_create_path_gesture' is the most complicated  of PceDraw's
gestures because it does not yet fit in very well  with the concept of
`gesture' that describes event-handling  from  a button-down upto  the
corresponding  button-up.  A  path  is  created by  clicking  on  each
subsequent control-point.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(draw_create_path_gesture, gesture).

resource(cursor,	 cursor,	cross).
resource(button,	 button_name,	left).

variable(path, path*, both, "Currently painted path").
variable(line, line,  get,  "Line segment for last").

initialise(G, Button:[button_name]) :->
	send(G, send_super, initialise, Button),
	send(G, slot, line, new(Line, line)),
	send(Line, texture, dotted).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The ->event method is redefined for  two purposes:  1) when  a path is
beeing created a dotted line is displayed from the  last control-point
to the current mouse  location (achieved by trapping   the  `loc_move'
events) and 2) when the user presses ESC or another  mouse-button, the
path is terminated.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

event(G, Ev:event) :->
	"Process an event"::
	get(Ev?receiver, mode, draw_path),
	(   send(G, send_super, event, Ev)
	->  true
	;   get(G, path, Path), Path \== @nil,
	    (   send(Ev, is_a, loc_move)
	    ->  send(G, move, Ev)
	    ;   (send(Ev, is_a, 27) ; send(Ev, is_a, button))	% terminate
	    ->  send(Ev?window, focus, @nil),
	        send(G, terminate_path)
	    )
	).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
->Initiate is called on each button-down.  If there is no current path
it is a  `real' initiate.   If  there is already a  current  path this
method just succeeds.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

initiate(G, Ev:event) :->
	"Paint the prototype"::
	get(G, path, CurrentPath),
	(   CurrentPath == @nil
	->  get(Ev, receiver, Canvas),
	    get(Ev, position, Canvas, Pos),
	    get(Canvas?proto, clone, Path),
	    send(G, path, Path),
	    get(G, line, Line),
	    send(Line, start, Pos),
	    send(Line, end, Pos),
	    send(Canvas, display, Line),
	    send(Canvas, display, Path)
	;   true
	).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The method ->move is called from ->event when there  is a current path
and the  mouse  is moved.   It  replaces  the ->drag method  called in
normal gestures when the mouse is moved with a button pressed.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

move(G, Ev:event) :->
	get(G, line, Line),
	get(Ev, position, Pos),
	send(Line, end, Pos).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Terminate  implies   a button-up.   This method    appends the current
location to the path; moves the start of the feedback  line to the end
of the path and  invokes `window ->focus'.  The 3-th  argument of this
method is  the button that caused the  event-focus to  be  grabbed.  A
button-up event related to  this  button will release  the  focus.  By
setting this button to @nil, the focus will not be released.  See also
->event.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

terminate(G, Ev:event) :->
	send(G, move),
	send(G?path, append, G?line?end),
	send(G?line, start, G?line?end),
	send(Ev?window, focus, Ev?receiver, G, G?cursor, @nil).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Terminate the path.  Remove the feedback-line; set the current path to
@nil and finally  remove  the path  if  it consists   of  only 1 point
(similar removing text objects without characters; graphicals smallers
than a defined minimal size; etc.).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

terminate_path(G) :->
	get(G, path, Path),
	send(G?line, device, @nil),
	send(G, path, @nil),
	(   get(Path?points, size, Size),
	    Size =< 1
	->  send(Path, free)
	;   true
	).
	
:- pce_end_class.


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The `draw_modify_path_gesture' allows the user to  drag control-points
with the  middle-mouse  button.  The method  `path <-point' is used to
find the control-point.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(draw_modify_path_gesture, gesture).

resource(cursor,	cursor,		plus).
resource(button,	button_name,	middle).

variable(point,		point*,		both,	"Point to move").

verify(G, Ev:event) :->
	"Start if event is close to point"::
	get(Ev, receiver, Path),
	get(Path, point, Ev, Point),
	send(G, point, Point).

initiate(G, Ev:event) :->
	"Move pointer to point"::
	get(Ev, receiver, Path),
	get(G, point, Point),
	get(Path, offset, Offset),
	get(Point, copy, P2),
	send(P2, plus, Offset),
	send(Path?device, pointer, P2).

drag(G, Ev:event) :->
	"Move point to pointer"::
	get(Ev, receiver, Path),
	get(Path, device, Dev),
	get(Ev, position, Dev, Pos),
	get(Path, offset, Offset),
	send(Pos, minus, Offset),
	send(Path, set_point, G?point, Pos?x, Pos?y).

:- pce_end_class.


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The   two click-gestures   below  allow  the    user to  insert/delete
control-points  by left-clicking  on   them with     the   control-key
depressed.  If  the user clicks within 3  pixels from  a control-point
this point  is  deleted.  Otherwise,  if  the  user clicks close  to a
line-segment, a control-point is inserted between  the two points that
define the line-segment.

Note that the first click_gesture defines a condition.  Whether or not
an  event  is accepted  by a  click_gesture   does not  depend on  the
return-status of the called  message.  Without a condition, the  first
click_gesture will  accept all left-clicks  with the  control-key helt
down.  The second click_gesture would never be activated.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_global(@draw_edit_path_gesture, make_draw_edit_path_gesture).

make_draw_edit_path_gesture(G) :-
	new(G, handler_group),
	send(G, append,
	     new(C1, click_gesture(left, c, single,
				   message(@receiver, delete,
					   ?(@receiver, point, @event, 3))))),
	send(C1, condition, ?(@event?receiver, point, @event, 3)),
	send(G, append,
	     click_gesture(left, c, single,
			   message(@receiver, insert,
				   ?(@event, position, @receiver?device),
				   ?(@receiver, segment, @event)))).



		/********************************
		*             TEXT		*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The recognisers below  define the creation  of a text object and start
editing a text   object.  Note the use  of  keyboard_focus; if `Window
<->keyboard_focus'  is nonequal to  @nil, all typing  is tranferred to
the keyboard_focus.    Objects   receive  `obtain_keyboard_focus'  and
`release_keyboard_focus' events when they get   or loose the  keyboard
focus.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_global(@draw_create_text_recogniser,
	      make_draw_create_text_recogniser).
:- pce_global(@draw_edit_text_recogniser,
	      make_draw_edit_text_recogniser).
:- pce_global(@draw_compound_draw_text_recogniser,
	      make_draw_compound_draw_text_recogniser).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
After `Device  ->display'  the new  graphical is  at  the end  of  the
`Device <-graphicals' chain and thus can be found using:

	Canvas?graphicals?tail

Note that  the last  argument of  the  click_gesture  is   the preview
action, but may also be used as a condition.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

make_draw_create_text_recogniser(R) :-
	new(Canvas, @event?receiver),
	new(Pos, @event?position),
	new(Text, Canvas?graphicals?tail),
	new(R, click_gesture(left, '', single,
			     block(message(Canvas, display,
					   Canvas?proto?clone, Pos),
				   message(Canvas, keyboard_focus, Text),
				   message(Canvas, auto_align, Text, create)),
			     Canvas?(mode) == draw_text)).

make_draw_edit_text_recogniser(R) :-
	new(Text, @event?receiver),
	new(Canvas, Text?window),
	new(Pointed, ?(Text, pointed, @event?position)),
	new(R, click_gesture(left, '', single,
			     block(message(Text, caret, Pointed),
				   message(Canvas, keyboard_focus, Text)),
			     Canvas?(mode) == draw_edit)).

make_draw_compound_draw_text_recogniser(R) :-
	new(Compound, @event?receiver),
	new(Canvas, Compound?window),
	new(R, click_gesture(left, '', single,
			     message(Compound, start_text, @event),
			     Canvas?(mode) == draw_edit)).
	

		/********************************
		*             MOVE		*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The  move_selection gesture is active when  an object is moved that is
selected  and there  are  more objects  selected.   In this  case  all
selected objects are moved by the same  amount.   This is indicated by
showing  an outline that reflects  the  bounding  box of  all  objects
moved.

This gesture illustrates how another gesture can be  encapsulated.  It
is a  subclass  of `move_gesture' to inherit the  button  and modifier
resources.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(draw_move_selection_gesture, move_gesture).

variable(outline,	box,	get,
	 "Box used to indicate move").
variable(selection,	chain*, both,
	 "Stored value of device selection").
variable(origin,	point,  get,
	 "Start origin of selection").

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The  gesture maintains  an outline, the selection to  be moved and the
positon  where  the move orginiated.    The outline  itself is given a
normal  move_gesture to make  it move on  dragging.  This move_gesture
should operate on the same button and modifier.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

initialise(G, B:[button_name], M:[modifier]) :->
	send(G, send_super, initialise, B, M),
	send(G, slot, outline, new(Box, box(0,0))),
	send(G, slot, origin, point(0,0)),
	send(Box, texture, dotted),
	send(Box, recogniser, move_gesture(G?button, G?modifier)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Verify the object  is selected and there  is at least one  more object
selected.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

verify(_G, Ev:event) :->
	get(Ev, receiver, Receiver),
	get(Receiver, selected, @on),
	get(Receiver?device?graphicals, find,
	    and(@arg1?selected == @on,
		@arg1 \== Receiver), _).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Initiating implies finding the  device  and the bounding  box  of  all
selected objects (= the `union' of their areas).  Next, the outline is
displayed and all events are posted to  the outline.  The move_gesture
of the outline ensures the outline is moved by the dragging events.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

initiate(G, Ev:event) :->
	get(Ev?receiver, device, Dev),
	get(G, outline, Outline),
	send(G, selection, Dev?selection),
	get(G, selection, Selection),
	new(Union, area(0,0,0,0)),
	send(Selection, for_all, message(Union, union, @arg1?area)),
	send(G?origin, copy, Union?position),
	send(Outline, area, Union),
	send(Union, done),
	send(Dev, display, Outline),
	send(Ev, post, Outline).

drag(G, Ev) :->
	send(Ev, post, G?outline).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Terminate.   First undisplay the outline.  Next  calculate by how much
the outline has been dragged and move all objects  of the selection by
this amount.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

terminate(G, Ev:event) :->
	send(G, drag, Ev),
	get(G, outline, Outline),
	send(Outline, device, @nil),
	get(Outline?area?position, difference, G?origin, Offset),	
	send(G?selection, for_all, message(@arg1, relative_move, Offset)),
	send(G, selection, @nil),
	send(Ev?receiver?window, modified).

:- pce_end_class.


		/********************************
		*             RESIZE		*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Resizing the selection is very similar to moving it.  Resizing a group
of  object  implies finding  the   origin  of  the   resize  (e.i. the
coordinates of the corner of the resized area that  does not move) and
the resize  factor in both  X and  Y-direction.   Thus,  the following
steps are taken:

	1) On initiating, display a box indicating the bounding box
	   of the selection and start resizing this box.
	2) After resizing of the bounding box is completed, compute
	   the static origin and the resize factors.
	3) Send a ->resize message to all the individual graphicals.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(draw_resize_selection_gesture, resize_gesture).

variable(outline,	box,	get,
	 "Box used for feedback").
variable(selection,	chain*, both,
	 "Stored value of device selection").
variable(start,		area,   get,
	 "Area before resize started").

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The outline operates the same way as the outline of the selection_move
handler.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

initialise(G, B:[button_name], M:[modifier]) :->
	send(G, send_super, initialise, B, M),
	send(G, slot, outline, new(Box, box(0,0))),
	send(G, slot, start, area(0,0,0,0)),
	send(Box, texture, dotted),
	send(G, min_size, size(3, 3)),
	send(Box, recogniser, resize_gesture(G?button, G?modifier)).


verify(G, Ev:event) :->
	get(Ev, receiver, Receiver),
	get(Receiver, selected, @on),
	send(G, send_super, verify, Ev).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Compute the bounding box of the selection, display the outline and post
the event to the outline.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

initiate(G, Ev:event) :->
	get(Ev?receiver, device, Dev),
	get(G, outline, Outline),
	send(G, selection, Dev?selection),
	get(G, selection, Selection),
	get(G, start, Start),
	send(Start, clear),
	send(Selection, for_all, message(Start, union, @arg1?area)),
	send(Outline, area, Start),
	send(Dev, display, Outline),
	(   send(Ev, post, Outline)	% cancel!
	->  true
	;   send(Outline, device, @nil),
	    send(G, selection, @nil),
	    fail
	).


drag(G, Ev) :->
	send(Ev, post, G?outline).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Compute the resize factors and resize the contents of the selection.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

terminate(G, Ev:event) :->
	send(G, drag, Ev),
	get(G, outline, Outline),
	send(Outline, device, @nil),
	get(G, start, A0),
	get(Outline, area, A1),
	x_resize(A0, A1, X0, Xfactor),
	y_resize(A0, A1, Y0, Yfactor),
	send(G?selection, for_all,
	     message(@arg1, resize, Xfactor, Yfactor, point(X0, Y0))),
	send(G, selection, @nil),
	send(Ev?receiver?window, modified).

x_resize(A0, A1, X0, Xfactor) :-
	get(A0, left_side, Left),
	get(A1, left_side, Left), !,		  % left-side has not changed
	X0 = Left,
	get(A0, width, W0),
	get(A1, width, W1),
	Xfactor is W1 / W0.
x_resize(A0, A1, X0, Xfactor) :-
	get(A0, right_side, Right),
	X0 = Right,
	get(A0, width, W0),
	get(A1, width, W1),
	Xfactor is W1 / W0.

y_resize(A0, A1, Y0, Yfactor) :-
	get(A0, top_side, Top),
	get(A1, top_side, Top), !,		  % top has not changed
	Y0 = Top,
	get(A0, height, H0),
	get(A1, height, H1),
	Yfactor is H1 / H0.
y_resize(A0, A1, Y0, Yfactor) :-
	get(A0, bottom_side, Bottom),
	Y0 = Bottom,
	get(A0, height, H0),
	get(A1, height, H1),
	Yfactor is H1 / H0.

:- pce_end_class.


:- pce_begin_class(draw_resize_gesture, resize_outline_gesture).

terminate(G, Ev:event) :->
	"Invoke auto_align"::
	send(G, send_super, terminate, Ev),
	get(Ev, receiver, Shape),
	send(Shape?device, auto_align, Shape, resize).

:- pce_end_class.


:- pce_begin_class(draw_move_gesture, move_outline_gesture).

terminate(G, Ev:event) :->
	"Invoke auto_align"::
	send(G, send_super, terminate, Ev),
	get(Ev, receiver, Shape),
	send(Shape?device, auto_align, Shape, move).

:- pce_end_class.


		/********************************
		*           CONNECT		*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The code below is a refinement  of the connect_gesture  defined in PCE
itself.  It  verifies the canvas is  in  the right  mode  and sets the
<->link attribute of the gesture.  This attribute will  later  be used
to create the connection from.

The `connect_gesture ->connect' behaviour has been redefined  as well.
The standard one uses a  `connection', while this  one should create a
`draw_connection'.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(draw_connect_gesture, connect_gesture).

verify(G, Ev:event) :->
	"Verify canvas is in connect-mode"::
	get(Ev?receiver, device, Dev), Dev \== @nil,
	get(Dev, mode, draw_connect),
	send(G, link, Dev?proto),
	send(G, send_super, verify, Ev).

connect(_G, From:graphical, To:graphical, Link:link,
	    FH:[name], TH:[name]) :->
	"Connect the graphicals (using a draw_connection)"::
	(   get(Link, attribute, connection_class, ClassName)
	->  true
	;   ClassName = draw_connection
	),
	Term =.. [ClassName, From, To, Link, FH, TH],
	new(C, Term),
	send(C, start_text).

:- pce_end_class.

		/********************************
		*     CONNECT CREATE HANDLE	*
		********************************/

:- pce_begin_class(draw_connect_create_gesture, gesture).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The `draw_connect_create_gesture' is an example  of a complete gesture
class.   It connects two graphicals at  arbitrary  points by attaching
new handles to the graphicals and creating a connection between them.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

variable(line,			line,		get,
	 "Line indicating link").
variable(from_indicator,	bitmap,		get,
	 "Indicator at `from' side").
variable(to_indicator,		bitmap,		get,
	 "Indicator at `to' side").
variable(to,			graphical*,	get,
	 "Graphical to connect to").

resource(button,		button_name,	left,
	 "Button used to connect (left)").
resource(modifier,		modifier,	'',
	 "Modifier used to connect").

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Initialise the line and markers of the gesture.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

initialise(G, B:[button_name], M:[modifier]) :->
	send(G, send_super, initialise, B, M),
	send(G, slot, line, line(0,0,0,0)),
	send(G, slot, from_indicator, new(bitmap(@mark_handle_image))),
	send(G, slot, to_indicator, new(bitmap(@mark_handle_image))).


verify(_G, Ev:event) :->
	"Verify canvas is in connect_create-mode"::
	get(Ev?receiver?device, mode, draw_cconnect).	


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Indicate the  start-location  using  the <-from_indicator,   give  the
feedback-line the appropriate attributes and display it.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

initiate(G, Ev:event) :->
	"Start drawing line"::
	get(Ev?receiver, device, Dev),
	get(Dev, proto, Link),
	get(Ev, position, Dev, Pos),
	send(G?line, copy, Link?line),
	send(G?line, texture, dotted),
	send(G?line, start, Pos),
	send(G?line, end, Pos),
	send(Dev, display, G?line),
	send(G, indicate, Ev?receiver, Pos, G?from_indicator).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Update the line, check whether the mouse points to a  valid target and
display a marker on  it.  Note how  the target  is  located  using the
method  `Chain  <-find'.  This  keeps  everything inside PCE, avoiding
interface overhead  and producing far  less garbage.  `Gesture ->drag'
should be as fast as  possible and not  produce too much garbage as it
will be called about 40 times per second while the mouse is dragged.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

drag(G, Ev:event) :->
    get(Ev, receiver, Receiver),
    get(Receiver, device, Dev),
    get(Ev, position, Dev, Pos),
    send(G?line, end, Pos),
    (   get(?(Dev, pointed_objects, Pos), find,
	    and(Receiver \== @arg1,
		G?line \== @arg1,
		G?from_indicator \== @arg1,
		G?to_indicator \== @arg1), To)
    ->  send(G, indicate, To, Pos, G?to_indicator),
	send(G, slot, to, To)
    ;   send(G, slot, to, @nil),
	send(G?to_indicator, device, @nil)
    ).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
If there is a  target, create unique  handles  on both sides and  link
them together.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

terminate(G, Ev:event) :->
	send(G, drag, Ev),
	send(G?line, device, @nil),
	send(G?from_indicator, device, @nil),
	send(G?to_indicator, device, @nil),
	get(G, to, To),
	(   To \== @nil
	->  send(G, slot, to, @nil),
	    get(Ev, receiver, Receiver),
	    get(Receiver?device, proto, Link),
	    get(G, handle, Receiver, G?from_indicator?center, Link?from, FH),
	    get(G, handle, To, G?to_indicator?center, Link?to, TH),
	    new(_, draw_connection(Receiver, To, Link, FH, TH))
	;   true
	).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Create a unique handle on a graphical at the  indicated position.  The
position of the handle is taken relative to the size of the graphical.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

handle(_G, Gr:graphical, Pos:point, Kind:name, Name) :<-
	"Attach a handle at specified position and return it's name"::
	get(Gr, x, X), get(Gr, y, Y),
	get(Gr, width, W), get(Gr, height, H),
	get(Pos, x, PX), get(Pos, y, PY),
	RX is PX - X, RY is PY - Y,
	unique_handle_name(Gr, Name),
	send(Gr, handle, handle((RX/W) * w, (RY/H) * h, Kind, Name)).


unique_handle_name(Gr, Name) :-
	between(1, 10000, N),
	concat(c, N, Name),
	\+ get(Gr, handle, Name, _), !.


indicate(_G, Gr:graphical, Pos:point, Indicator:bitmap) :->
	"Display indication-marker for position"::
	send(Indicator, center, Pos),
	send(Gr?device, display, Indicator).

:- pce_end_class.


		/********************************
		*          SHAPE POPUP		*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The  code of this section  attaches a popup-menu  to the shapes.  On a
mouse-right-down  event,  the  shape on which   the  down  occurred is
selected  to indicate on which  object the operation  will take place.
Next, the menu is shown.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_global(@draw_shape_popup_gesture, make_draw_shape_popup_gesture).

make_draw_shape_popup_gesture(G) :-
	new(Gr, @event?receiver),
	new(Canvas, Gr?device),

	new(P, popup),
	send_list(P, append,
		  [ menu_item(align,
			      message(Canvas, align_with_selection, Gr),
			      @default, @on)
		  , menu_item(duplicate,
			      block(message(Canvas, selection, Gr),
				    message(Canvas, duplicate_selection)))
		  , menu_item(cut,
			      message(Canvas, cut_selection,
				      create(chain, @arg1)),
			      @default, @off)
		  , menu_item(copy,
			      message(Canvas, copy_selection,
				      create(chain, @arg1)),
			      @default, @on)
		  , menu_item(edit_attributes,
			      block(message(Canvas, selection, Gr),
				    message(Canvas, edit_selection)),
			      @default, @on)
		  , menu_item(hide,
			      message(Canvas, edit,
				      message(@arg1, hide), Gr))
		  , menu_item(expose,
			      message(Canvas, edit,
				      message(@arg1, expose), Gr),
			      @default, @on)
		  ]),

	new(G, draw_draw_shape_popup_gesture(P)).


:- pce_begin_class(draw_draw_shape_popup_gesture, popup_gesture).

variable(old_selected,	bool*,	both, "Was graphical selected").

verify(G, Ev:event) :->
	get(Ev?receiver, device, Dev),
	Dev \== @nil,
	send(Dev?class, is_a, draw_canvas),
	send(G, send_super, verify, Ev).


initiate(G, Ev:event) :->
	get(Ev, receiver, Receiver),
	send(G, old_selected, Receiver?selected),
	send(Receiver, selected, @on),
	send(G, send_super, initiate, Ev).


terminate(G, Ev:event) :->
	get(G, context, Gr),
	send(Gr, selected, G?old_selected),
	send(G, send_super, terminate, Ev).

:- pce_end_class.
