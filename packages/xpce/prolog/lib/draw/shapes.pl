/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

:- module(draw_shapes,
	  [ draw_begin_shape/4,
	    draw_end_shape/0
	  ]).

:- use_module(library(pce)).
:- use_module(library('dialog/lib/template')).
:- require([ forall/2
	   , ignore/1
	   , member/2
	   ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module defines the various shapes that can be used to construct the
diagram.   Most  of  the  shapes  are   very  close  the  PCE's  drawing
primitives.  Two things have to be added   for each of them: handles for
connecting lines (connections) and event-handling.

Programming can be done both at the class  and at the instance level.  I
decided to add them at the class  level.  As there are normally multiple
instances of the classe, this  approach   reduces  memory  cost.  A more
important  issue  is  kloning  and    saving.    These  operations  work
recursively  and  therefore  would  clone   and  save  the  object-level
extensions.  For saving, this has two   disadvantages.   The saved files
would get bigger and, more important, the   gestures -defining the UI of
the tool- would be saved too.  This leads  to a bad separation of UI and
the actual data manipulated.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

		/********************************
		*	  COMMON TEMPLATE	*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
To facilate users to refine PceDraw for   their own needs, we designed a
very simple schema for defining  new  shapes.    A  PceDraw  shape  is a
subclass of a PCE graphical or of   another PceDraw shape.  Such classes
are defined between the braces:

	:- draw_begin_shape(Name, Super, Summary, Recognisers).

	...

	:- draw_end_shape.

The public predicate draw_begin_shape/4 creates a  new XPCE class `Name'
below `Super'.  The  class  object  itself   is  an  instance  of  class
draw_shape_class, rather then of  the  normal   XPCE  class  class.  The
reason for this is to allow for certain  definitions to be raised to the
meta-class level.

We extend the  meta-knowledge  represented   in  classes  with  `hidden'
attributes (attributes that *can*, but *are not* edited by the attribute
editor (see `draw_shape ->has_attribute') and recognisers.

NOTE:	I consider allowing for class-level recognisers anyway, avoiding
	the need for explicit event-handling methods in many cases.

First the definition of the meta-class draw_shape_class:
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(draw_shape_class, class, "Handle class-level stuff").

variable(hidden_attributes, chain,  get, "Masked attributes").
variable(recognisers,	    chain,  get, "Event-handling recognisers").
variable(part_attributes,   sheet*, get, "Compound attribute dispatching").

initialise(Class, Name, Super) :->
	send(Class, send_super, initialise, Name, Super),
	(   get(Class, super_class, SuperClass),
	    send(SuperClass, instance_of, draw_shape_class)
	->  send(Class, slot, hidden_attributes,
		 SuperClass?hidden_attributes?copy),
	    send(Class, slot, recognisers,
		 SuperClass?recognisers?copy),
	    send(Class, slot, part_attributes,
		 SuperClass?part_attributes?clone)
	;   send(Class, slot, hidden_attributes, new(chain)),
	    send(Class, slot, recognisers, new(chain))
	).

:- pce_group(attribute).

hidden_attribute(Class, Attr:name) :->
	"Register a hidden attribute"::
	get(Class, hidden_attributes, Hidden),
	send(Hidden, add, Attr).

part_attribute(Class, Attribute:name, Part:name) :->
	"Attribute must be manipulated on part"::
	get(Class, part_attributes, A0),
	(   A0 == @nil
	->  send(Class, slot, part_attributes, new(Mapping, sheet))
	;   Mapping = A0
	),
	send(Mapping, value, Attribute, Part).

:- pce_group(event).

recogniser(Class, Recogniser:recogniser) :->
	"Register (prepend) a recogniser"::
	get(Class, recognisers, Recognisers),
	send(Recognisers, add, Recogniser).

:- pce_end_class.


draw_begin_shape(Name, Super, Summary, Recognisers) :-
	get(@pce, convert, Super, class, SuperClass),
	(   send(SuperClass, instance_of, draw_shape_class)
	->  pce_begin_class(Name, Super, Summary)
	;   new(_, draw_shape_class(Name, Super)),
	    pce_begin_class(Name, Super, Summary),
	    use_class_template(Name, draw_shape)
	),
	forall(member(R, Recognisers),
	       send(@class, recogniser, R)).

draw_end_shape :-
	pce_end_class.


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
All shape classes need to implement a  protocol to the draw_canvas class
(and  through  there  to   the   attribute    editor).    To   do  this,
draw_begin_shape/4 will include the `class  template' draw_shape in each
direct subclass of a non-PceDraw class.

Including a class template  implies  that   all  methods  defined on the
template class below class  `template'  (an   empty  class  below  class
object)  will  be  included  into  the    current  class.   Neither  the
implementation, nor the method/variable object   itself is copied: their
references are simply included  in   the  `class <-send_methods', `class
<-get_methods' or `class <-instance_variables', depending  on the object
included.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(draw_shape, template,
		   "Common methods for PceDraw objects").


geometry(Gr, X:[int], Y:[int], W:[int], H:[int]) :->
	"Like super-method, but activate ->modified"::
	send(Gr, send_super, geometry, X, Y, W, H),
	send(Gr, modified).

:- pce_group(attribute).

attribute(Gr, Att, Val) :->
	"Modify an attribute if ->has_attribute"::
	send(Gr, has_attribute, Att),
	send(Gr, Att, Val),
	send(Gr, modified).
attribute(Gr, Att, Val) :<-
	"Just completeness"::
	get(Gr, Att, Val).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
->has_attribute is used by the attribute   editor to find the attributes
that can be manipulated on the currently selected object.

A name is defined an attribute if it  can both be modified and requested
(i.e.  there is send- and get-behaviour for   the  name).  The class (an
instance of draw_shape_class), defines a chain   of  attributes that are
explicitely masked.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

has_attribute(Gr, Att:name) :->
	"Test if object defines attribute"::
	send(Gr, has_send_method, Att),
	send(Gr, has_get_method, Att),
	\+ send(Gr, hidden_attribute, Att).


hidden_attribute(Gr, Att:name) :->
	"True if attibute is not editable"::
	get(Gr, class, Class),
	get(Class, hidden_attributes, Hidden),
	send(Hidden, member, Att).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
If an object is ->modified, the modified   flag of the drawing should be
updated and the attribute editor should be notified.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_group(modified).

modified(Gr) :->
	"Inform <-window and update attribute editor"::
	(   get(Gr, window, Window), Window \== @nil,
	    send(Window, modified),
	    get(Gr, selected, @on),
	    send(Window, update_attribute_editor)
	->  true
	;   true
	).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
->event just walks through the recognisers defined on the class.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_group(event).

event(Gr, Ev:event) :->
	"Handle <-class recognisers"::
	(   send(Gr, send_super, event, Ev)
	;   get(Gr?class, recognisers, Chain),
	    get(Chain, find,
		message(@arg1, event, Ev),
		_)
	).

:- pce_group(mode).

mode(Gr, Mode:name) :<-
	"Request <-window's <-mode"::
	get(Gr, window, Window),
	get(Window, mode, Mode).

:- pce_end_class.


		/********************************
		*             BOX		*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Box is the most prototypical example of a graphical.  Boxes in PceDraw
have  handles  for  connections in the  middle  of  each  side.  Event
handling      is      realised       by     the   reusable      object
@draw_resizable_shape_recogniser.

The handle/4 construct  attaches a handle with  specified  <->kind and
<->name at the  specified position.  The   handle  is attached  to the
class  (see `class ->handle')   rather  than  to  the  instances  (see
`graphical ->handle').
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- draw_begin_shape(draw_box, box, "PceDraw editable box",
		    [@draw_resizable_shape_recogniser]).

handle(w/2, 0,   link, north).
handle(w/2, h,   link, south).
handle(0,   h/2, link, west).
handle(w,   h/2, link, east).

:- draw_end_shape.

		/********************************
		*           ELLIPSE		*
		********************************/

:- draw_begin_shape(draw_ellipse, ellipse, "PceDraw editable ellipse",
		    [@draw_resizable_shape_recogniser]).

handle(w/2, 0,   link, north).
handle(w/2, h,   link, south).
handle(0,   h/2, link, west).
handle(w,   h/2, link, east).

:- draw_end_shape.


		/********************************
		*            TEXT		*
		********************************/

:- draw_begin_shape(draw_text, text, "PceDraw editable text", []).

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
	(   send(Ev, is_a, focus)
	->  ignore(send(Text, send_super, event, Ev)),
	    (	send(Ev, is_a, release_keyboard_focus)
	    ->	(   get(Text?string, size, 0),
		    send(Text?device, instance_of, draw_canvas) % HACK
		->  send(Text, free)
		;   send(Text, show_caret, @off)
		)
	    ;	true
	    )
	;   get(Text, show_caret, @on),
	    (   get(Ev, id, Id),
		event(Id, Text)
	    ->  true
	    ;   send(Ev, is_a, keyboard),
		send(Text, typed, Ev),
		send(Text, modified)
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

:- pce_group(menu).

menu_text(T) :->
	"Prepare text for menu ('T')"::
	send(T, string, 'T'),
	get(T, font, Font),
	get(Font, family, Family),
	get(Font, style, Style),
	new(S, var(value := Font)),
	send(@fonts, for_all,
	     if(and(@arg2?family == Family,
		    @arg2?style == Style,
		    @arg2?points < S?points,
		    @arg2?points > 5),
		assign(S, @arg2, global))),
	send(T, font, S).

:- draw_end_shape.

		/********************************
		*             LINE		*
		********************************/

:- draw_begin_shape(draw_line, line, "PceDraw editable line",
		    [@draw_line_recogniser]).

handle(w/2, h/2, link, center).
handle(0,   0,   link, start).
handle(w,   h,   link, end).

:- draw_end_shape.


		/********************************
		*             PATH		*
		********************************/

:- draw_begin_shape(draw_path, path, "PceDraw editable path",
		    [@draw_path_recogniser]).
:- send(@class, hidden_attribute, radius).

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

:- draw_end_shape.



		/********************************
		*           CONNECTIONS		*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A connection is a line between two handles on  two different graphical
objects.  See clas handle, graphical and connection for details.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- draw_begin_shape(draw_connection, connection, "PceDraw editable connection",
		    [@draw_connection_recogniser]).

handle(w/2, h/2, link, center).

start_text(_C, _Ev:[event]) :->
	"Dummy method"::
	true.

:- draw_end_shape.


		/********************************
		*             BITMAP		*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Bitmaps are used to import arbitrary images into a drawing.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- draw_begin_shape(draw_bitmap, bitmap, "PceDraw editable bitmap",
		    [@draw_bitmap_recogniser]).

handle(w/2, 0,   link, north).
handle(w/2, h,   link, south).
handle(0,   h/2, link, west).
handle(w,   h/2, link, east).

:- draw_end_shape.


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

:- draw_begin_shape(draw_compound, device, "PceDraw editable device",
		    [@draw_compound_recogniser]).

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
	send(C, send_super, geometry, X, Y, W, H),
	send(C, modified).


resize_factor(@default, _, _, 1) :- !.
resize_factor(W1, C, S, F) :-
	get(C, S, W0),
	F is W1 / W0.


event(C, Ev:event) :->
	"Handle <-class recognisers"::
	(   get(C, all_recognisers, InstanceRecognisers),
	    get(InstanceRecognisers, find, message(@arg1, event, Ev), _)
	;   get(C?class, recognisers, Chain),
	    get(Chain, find, message(@arg1, event, Ev), _)
	).


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

menu_text(C) :->
	"Set all <-graphicals to `T'"::
	send(C?graphicals, for_all,
	     if(message(@arg1, has_send_method, menu_text),
		message(@arg1, menu_text))).


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

:- pce_group(attribute).

geometry_selector(x).
geometry_selector(y).
geometry_selector(width).
geometry_selector(height).

:- pce_global(@is_draw_shape,
	      new(message(@arg1?class, instance_of, draw_shape_class))).

has_attribute(C, Att:name) :->
	"Test if object has attribute"::
	\+ send(C, hidden_attribute, Att),
	(   geometry_selector(Att)
	->  true
	;   get(C?graphicals, find,
		if(@is_draw_shape,
		   message(@arg1, has_attribute, Att),
		   and(message(@arg1, has_send_method, Att),
		       message(@arg1, has_get_method, Att))),
		_)
	).

attribute(C, Att:name, Val:any) :->
	(   geometry_selector(Att)
	->  send(C, Att, Val)
	;   get(C?class, part_attributes, Sheet),  Sheet \== @nil,
	    get(Sheet, value, Att, PartName)
	->  get(C, member, PartName, Part),
	    send(Part, attribute, Att, Val)
	;   send(C?graphicals, for_some,
		 if(@is_draw_shape,
		    message(@arg1, attribute, Att, Val),
		    and(message(@arg1, has_send_method, Att),
			message(@arg1, Att, Val),
			message(C, modified))))
	).

attribute(C, Att:name, Val) :<-
	(   geometry_selector(Att)
	->  get(C, Att, Val)
	;   get(C?class, part_attributes, Sheet),  Sheet \== @nil,
	    get(Sheet, value, Att, PartName)
	->  get(C, member, PartName, Part),
	    get(Part, attribute, Att, Val)
	;   get(C?graphicals, find,
		if(@is_draw_shape,
		   message(@arg1, has_attribute, Att),
		   and(message(@arg1, has_send_method, Att),
		       message(@arg1, has_get_method, Att))),
		Shape),
	    get(Shape, Att, Val)
	).

:- draw_end_shape.

