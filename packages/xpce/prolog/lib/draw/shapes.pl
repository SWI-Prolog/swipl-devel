/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

:- module(draw_shapes, []).

:- use_module(library(pce)).
:- require([ memberchk/2
	   ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module defines  the various shapes  that can be used to construct
the diagram.  Most  of the  shapes are  very close  the PCE's  drawing
primitives.  Two things have to be added for each of them: handles for
connecting lines (connections) and event-handling.

Both things can be added both at the  class and at the instance level.
I  decided to  add  them  at the class  level.   As there are normally
multiple instances of the classe, this  approach reduces  memory cost.
A  more important issue is kloning  and saving.  These operations work
recursively   and therefore would   clone  and  save  the object-level
extensions.  For saving, this has two  disadvantages.  The saved files
would get bigger and, more important, the gestures -defining the UI of
the tool- would be saved too.  This  leads to a  bad  separation of UI
and the actual data manipulated.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

		/********************************
		*            COMMON		*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The various   shapes are subclasses   of corresponding PCE graphicals.
Each of them has to  be expanded with ->geometry and  ->attribute.  We
define predicates to implement these  methods  and than just  refer to
these predicates.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

geometry(Gr, X, Y, W, H) :-
	send(Gr, send_super, geometry, X, Y, W, H),
	modified(Gr).

attribute(Gr, Att, Val) :-
	send(Gr, has_attribute, Att),
	send(Gr, Att, Val),
	modified(Gr).

modified(Gr) :-
	get(Gr, window, Window), Window \== @nil,
	send(Window, modified),
	get(Gr, selected, @on),
	send(Window, update_attribute_editor), !.
modified(_).
	

		/********************************
		*             BOX		*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Box is the most prototypical example of a graphical.  Boxes in PceDraw
have  handles  for  connections in the  middle  of  each  side.  Event
handling      is      realised       by     the   reusable      object
@draw_resizable_shape_recogniser.  Note that the  reference to the box
need not be provided.  ->event is invoked  from `Event ->post' and the
receiver slot of the event is a reference to the box.

Note that draw_box is  a subclass of box rather  than an extension  of
class box.  Extending class box might conflict with the consistency of
PCE itself or  other  applications running in   the same PCE   process
(never assume you are alone in the world).

The handle/4 construct  attaches a handle with  specified  <->kind and
<->name at the  specified position.  The   handle  is attached  to the
class  (see `Class ->handle')   rather  than  to  the  instances  (see
`Graphical ->handle').
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(draw_box, box).

handle(w/2, 0,   link, north).
handle(w/2, h,   link, south).
handle(0,   h/2, link, west).
handle(w,   h/2, link, east).

event(_Box, Ev:event) :->
	send(@draw_resizable_shape_recogniser, event, Ev).

geometry(B, X:[int], Y:[int], W:[int], H:[int]) :->
	"Forward change to device"::
	geometry(B, X, Y, W, H).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The ->has_attribute  method tests  whether the specified  attribute of
the shape can be set.  This  is a  bit of a  hack.  A  better solution
would have  been  to  test  whether the  graphical  has the  specified
method.  Unfortunately att graphicals have method <->pen, but for some
of them, changing the value has not effect.  The same applies for some
other attributes.  This should be changed in PCE.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

has_attribute(_B, Att:name) :->
	"Test if object has attribute"::
	memberchk(Att, [ pen, texture, colour, fill_pattern, radius
		       , shadow, x, y, width, height]).

attribute(B, Att:name, Val:any) :->
	attribute(B, Att, Val).

attribute(B, Att:name, Val) :<-
	get(B, Att, Val).

mode(B, Mode:name) :<-
	get(B?device, mode, Mode).

:- pce_end_class.

		/********************************
		*           ELLIPSE		*
		********************************/

:- pce_begin_class(draw_ellipse, ellipse).

handle(w/2, 0,   link, north).
handle(w/2, h,   link, south).
handle(0,   h/2, link, west).
handle(w,   h/2, link, east).

event(_Ellipse, Ev:event) :->
	send(@draw_resizable_shape_recogniser, event, Ev).

geometry(E, X:[int], Y:[int], W:[int], H:[int]) :->
	"Forward change to device"::
	geometry(E, X, Y, W, H).

has_attribute(_E, Att:name) :->
	"Test if object has attribute"::
	memberchk(Att, [ pen, texture, colour, fill_pattern
		       , shadow, x, y, width, height]).

attribute(E, Att:name, Val:any) :->
	attribute(E, Att, Val).

attribute(E, Att:name, Val) :<-
	get(E, Att, Val).

mode(B, Mode:name) :<-
	get(B?device, mode, Mode).

:- pce_end_class.


		/********************************
		*            TEXT		*
		********************************/

:- pce_begin_class(draw_text, text).

handle(w/2, 0,   link, north).
handle(w/2, h,   link, south).
handle(0,   h/2, link, west).
handle(w,   h/2, link, east).

initialise(T, String:string, Format:[name], Font:[font]) :->
	default(Format, center, 		     Fmt),
	default(Font,   font(helvetica, roman, 14),  Fnt),
	send(T, send_super, initialise, String, Fmt, Fnt).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This method illustrates  another way to  define  event-handling at the
class  level: just  analyse  the type  of  the  event and perform  the
necessary  action.  For complex   event-sequences  gestures are to  be
preferred as they  take  care of  many of   the difficulties such   as
managing the focus, cursor   and state-variables needed to parse   the
event sequence.   For simple events all  this is not necessary,  so we
might just as well parse them within the ->event method.

NOTE:	Events types will be changed shortly.  Having to refer to ESC
	as `27' is not the right way to program.  I'm not yet sure on
	the details.

NOTE:	PCE will probably provided higher-level primitives such as a
	special subclass of recogniser to deal with most of the details
	of this method.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

event(Text, Ev:event) :->
	(   get(Text, show_caret, @on),
	    get(Ev, id, Id),
	    event(Id, Text)
	->  true
	;   send(Ev, is_a, keyboard),
	    get(Text, show_caret, @on)
	->  send(Text, typed, Ev?id),
	    modified(Text)
	;   send(Ev, is_a, obtain_keyboard_focus)
	->  send(Text, show_caret, @on)
	;   send(Ev, is_a, release_keyboard_focus)
	->  (   get(Text?string, size, 0),
	        send(Text?device, instance_of, draw_canvas) % HACK
	    ->  send(Text, free)
	    ;   send(Text, show_caret, @off)
	    )
	;   send(@draw_text_recogniser, event, Ev)
	).

event(27, Text) :-				  % ESC
	send(Text?window, keyboard_focus, @nil).
event(9, Text) :-				  % TAB
	send(Text?device, advance, Text).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Indicate to  the device that this  graphical is willing to  accept the
keyboard focus.  It is interpreted by the `Device ->advance' method to
set the keyboard  focus  to  the  next object   that wants to   accept
keystrokes.

NOTE:	This mechanism needs some redesign and documentation.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

'_wants_keyboard_focus'(_T) :->
	"Indicate device I'm sensitive for typing"::
	true.

geometry(T, X:[int], Y:[int], W:[int], H:[int]) :->
	"Forward change to device"::
	geometry(T, X, Y, W, H).

has_attribute(_T, Att:name) :->
	"Test if object has attribute"::
	memberchk(Att, [pen, font, colour, transparent, x, y, width, height]).

attribute(T, Att:name, Val:any) :->
	attribute(T, Att, Val).

attribute(T, Att:name, Val) :<-
	get(T, Att, Val).

mode(B, Mode:name) :<-
	get(B?device, mode, Mode).

:- pce_end_class.

		/********************************
		*             LINE		*
		********************************/

:- pce_begin_class(draw_line, line).

handle(w/2, h/2, link, center).
handle(0,   0,   link, start).
handle(w,   h,   link, end).

event(_L, Ev:event) :->
	send(@draw_line_recogniser, event, Ev).

geometry(L, X:[int], Y:[int], W:[int], H:[int]) :->
	"Forward change to device"::
	geometry(L, X, Y, W, H).

has_attribute(_L, Att:name) :->
	"Test if object has attribute"::
	memberchk(Att, [ pen, texture, arrows, colour, x, y, width, height]).

attribute(L, Att:name, Val:any) :->
	attribute(L, Att, Val).

attribute(L, Att:name, Val) :<-
	get(L, Att, Val).

mode(B, Mode:name) :<-
	get(B?device, mode, Mode).

:- pce_end_class.


		/********************************
		*             PATH		*
		********************************/

:- pce_begin_class(draw_path, path).

event(_L, Ev:event) :->
	send(@draw_path_recogniser, event, Ev).

geometry(L, X:[int], Y:[int], W:[int], H:[int]) :->
	"Forward change to device"::
	geometry(L, X, Y, W, H).

has_attribute(_L, Att:name) :->
	"Test if object has attribute"::
	memberchk(Att,
		  [ pen, texture, colour, fill_pattern, arrows
		  , closed, interpolation
		  , x, y, width, height
		  ]).

attribute(L, Att:name, Val:any) :->
	attribute(L, Att, Val).

attribute(L, Att:name, Val) :<-
	get(L, Att, Val).

interpolation(L, N:int) :->
	(   N == 0
	->  send(L, kind, poly)
	;   send(L, intervals, N),
	    send(L, kind, smooth)
	).

interpolation(L, N:int) :<-
	(   get(L, kind, poly)
	->  N = 0
	;   get(L, intervals, N)
	).

mode(B, Mode:name) :<-
	get(B?device, mode, Mode).

:- pce_end_class.



		/********************************
		*           CONNECTIONS		*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A connection is a line between two handles on  two different graphical
objects.  See clas handle, graphical and connection for details.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(draw_connection, connection).

handle(w/2, h/2, link, center).

event(_C, Ev:event) :->
	send(@draw_connection_recogniser, event, Ev).

has_attribute(_C, Att:name) :->
	"Test if object has attribute"::
	memberchk(Att, [ pen, texture, arrows, colour, x, y, width, height]).

attribute(C, Att:name, Val:any) :->
	attribute(C, Att, Val).

attribute(C, Att:name, Val) :<-
	get(C, Att, Val).

mode(B, Mode:name) :<-
	get(B?device, mode, Mode).

:- pce_end_class.


		/********************************
		*             BITMAP		*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Bitmaps are used to import arbitrary images into a drawing.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(draw_bitmap, bitmap).

handle(w/2, 0,   link, north).
handle(w/2, h,   link, south).
handle(0,   h/2, link, west).
handle(w,   h/2, link, east).

event(_B, Ev:event) :->
	send(@draw_bitmap_recogniser, event, Ev).

has_attribute(_C, Att:name) :->
	"Test if object has attribute"::
	memberchk(Att, [colour, x, y]).

attribute(C, Att:name, Val:any) :->
	attribute(C, Att, Val).

attribute(C, Att:name, Val) :<-
	get(C, Att, Val).

mode(B, Mode:name) :<-
	get(B?device, mode, Mode).

:- pce_end_class.


		/********************************
		*           COMPOUNDS		*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Compounds are used  to realise (user-defined) prototypes that  consist
of  more than one shape.   Compound is a   subclass  of the  PCE class
`device',  that manages a  collection of graphicals.  In  addition  to
devices, compounds define distribution of  keyboard  events  to one of
the text objects inside it and resizing the device.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(draw_compound, device).

handle(w/2, 0,   link, north).
handle(w/2, h,   link, south).
handle(0,   h/2, link, west).
handle(w,   h/2, link, east).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Resizing  compounds.   PCE's   primitives  do  not  provide  for that.
However,  any attempt  to change  to  the area  of  the  graphical via
`Graphical  ->set', `Graphical  ->x',  `Graphical  ->area',  etc. will
invoke `Graphical ->geometry' to do the actual moving/resizing.

By   default, devices  will   move   themselve, but  not resize  their
contents.  In the method  below, we  first resize the contents  of the
device in a way very similar to resizing the selection as described in
the file  `gesture.pl' and  than invoke the super-behaviour to realise
the move.  Never try to do the move yourself: the  superclass might do
(and in the case  of  device does) additional  things to  changing the
coordinates.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

geometry(C, X:[int], Y:[int], W:[int], H:[int]) :->
	"Resize compound graphical"::
	resize_factor(W, C, width,  Xfactor),
	resize_factor(H, C, height, Yfactor),
	(   (Xfactor \== 1 ; Yfactor \== 1)
	->  get(C?area, position, Origin),
	    send(Origin, minus, C?position),
	    send(C?graphicals, for_all,
		 message(@arg1, resize, Xfactor, Yfactor, Origin)),
	    send(Origin, done)
	;   true
	),
	geometry(C, X, Y, W, H).

resize_factor(@default, _, _, 1) :- !.
resize_factor(W1, C, S, F) :-
	get(C, S, W0),
	F is W1 / W0.

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The method below  sets  the  string of  all text objects.  Used by the
icon manager (menu.pl) and the create  gesture (gesture.pl) to set the
strings to `T', resp '' (nothing).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

string(C, Str:string) :->
	"Set string of all texts"::
	send(C?graphicals, for_all,
	     if(message(@arg1, has_send_method, string),
		message(@arg1, string, Str))).


event(_C, Ev:event) :->
	send(@draw_compound_recogniser, event, Ev).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The  method   below is   called from  the    compound_recogniser  on a
ms_left_down if the  editor is in text_edit  mode.  If the  down is in
the area of  a text, the caret  is positioned as  close as possible to
the location  of the down.  Otherwise it  is placed on the  first text
object of the compound.

First all text  objects are found.  Next,  it tries to  find the first
text that   overlaps with  the position of   the  down-event.  If this
succeeds,  the  caret is   placed as  close as  possible   to the down
location.  Otherwise the caret is located at the end of the first text
object of the compound.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

start_text(C, Ev:[event]) :->
	"Enter typing mode"::
	get(C?graphicals, find_all,
	    message(@arg1, instance_of, text), Texts),
	(   Ev \== @default,
	    get(Texts, find, message(Ev, inside, @arg1), Pointed)
	->  send(Pointed, caret, ?(Pointed, pointed,
				   ?(Ev, position, Pointed))),
	    send(C?window, keyboard_focus, Pointed)
	;   get(Texts, head, First)
	->  send(First, caret, @default),
	    send(C?window, keyboard_focus, First)
	),
	send(Texts, done).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The  code below illustrates  another reason for  not communicating the
attribute setting using ->x,  ->pen, etc.  For a  compound, the x,  y,
width and height attributes should hold for  the  compound as a whole,
while the other attributes should only hold for the parts.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

geometry_selector(x).
geometry_selector(y).
geometry_selector(width).
geometry_selector(height).

has_attribute(C, Att:name) :->
	"Test if object has attribute"::
	(   geometry_selector(Att)
	->  true
	;   get(C?graphicals, find, message(@arg1, has_attribute, Att), _)
	).

attribute(C, Att:name, Val:any) :->
	(   geometry_selector(Att)
	->  send(C, Att, Val)
	;   send(C?graphicals, for_some,
		 message(@arg1, attribute, Att, Val))
	).

attribute(C, Att:name, Val) :<-
	(   geometry_selector(Att)
	->  get(C, Att, Val)
	;   get(C?graphicals, find, message(@arg1, has_attribute, Att), Shape),
	    get(Shape, Att, Val)
	).

mode(B, Mode:name) :<-
	get(B?device, mode, Mode).

:- pce_end_class.

