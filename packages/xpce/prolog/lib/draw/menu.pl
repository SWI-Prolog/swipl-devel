/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module defines the  mode-selection  menu at  the left-side of the
canvas.  It consists of two classes: draw_menu, which is a subclass of
picture  and which  is responsible for  communication, load/save, etc.
and draw_icon, which defines the combination of a mode, a cursor and a
prototype.

There are two reasonable primitives  for implementing this menu.   The
first   is to use a  dialog  window and  a choice  menu, of which  the
menu_items have image labels.  The  second  is  the approach taken  in
this file, to use a picture with a 1-column  format attached to it and
images for the options.  Which of them is to be preferred is difficult
to   tell.    Both  approaches   require   about  the  same  amount of
programming.   I've  chosen  for  the  latter   approach,  partly  for
`historical'  reasons and partly to  illustrate how non-standard menus
can be created using ordinary graphicals.

As the  user can modify  the   menu by adding/deleting prototypes  and
changing prototype attributes, the contents of  this menu can be saved
to file.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- module(draw_menu, []).

:- use_module(library(pce)).
:- require([ concat/3
	   , ignore/1
	   , memberchk/2
	   , send_list/3
	   ]).


		/********************************
		*          ICON MENU		*
		********************************/

:- pce_begin_class(draw_menu, window).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Variables to keep track of load/save.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

variable(file,		file*,	both,
	 "File for storing prototypes").
variable(modified,	bool,	get,
	 "Menu has been modified").

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Create  the picture.   The width  of the  picture   is fixed using the
->hor_stretch and  ->hor_shrink methods.   Next, a  `format' object is
attached to the picture.  When a  format is attached  to a device, the
graphicals are  located   according  to  the   format   specification.
Attaching a  format object to a  device is a   simple way to represent
tabular information in PCE.

NOTE:	Formats are a rather hacky solution.  There are plans to
	extend them with a more powerful table mechanisms.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

initialise(M) :->
	send(M, send_super, initialise, 'Icons', size(48, 200)),
	send_list(M, [hor_stretch, hor_shrink], 0),
	send(M, format, new(Fmt, format(horizontal, 1, @on))),
	send(Fmt, row_sep, 0),
	send(M, modified, @off).


modified(M, Value:[bool]) :->
	default(Value, @on, Val),
	send(M, slot, modified, Val).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Attach   a new prototype.   Note  that we  do  not have  to  specify a
position as  the attached format object  will  ensure  the new icon is
displayed at the bottom.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

proto(M, Proto:'graphical|link*', Mode:name, Cursor:cursor) :->
	"Attach a new prototype"::
	send(M, display, draw_icon(Proto, Mode, Cursor)),
	send(M, modified, @on).


current(M, Icon) :<-
	"Find current icon"::
	get(M?graphicals, find, @arg1?inverted == @on, Icon).


activate_select(M) :->
	"Activate icon that does select"::
	get(M?graphicals, find, @arg1?(mode) == select, Icon),
	send(Icon, activate).


		/********************************
		*            CREATE		*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Create a prototype from a  chain of graphicals (usually the selection;
in the future this might  also come from  a prototype editor).  If the
chain has one element, no compound is needed.

NOTE:	Due to the improper functioning of <-clone with regards to
	connections to the outside world, all connections should be
	internal to the chain of graphicals.  We won't try to program
	around this problem here, but improve PCE's kloning schema
	later.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

create_proto(M, Graphicals:chain) :->
	"Create a prototype from a chain of graphicals"::
	get(Graphicals, size, Size),
	(   Size == 0
	->  send(@display, inform, 'No selection')
	;   Size == 1
	->  get(Graphicals?head, clone, Proto),
	    send(Proto, selected, @off)
	;   new(Proto, draw_compound),
	    get(Graphicals, clone, Members),
	    send(Members, for_all,
		 and(message(Proto, display, @arg1),
		     message(@arg1, selected, @off))),
	    send(Proto, reference, @default),
	    send(Proto, string, '')
	),
	send(M, proto, Proto, draw_proto, dotbox).
	



		/********************************
		*            DELETE		*
		********************************/

can_delete(M) :->
	"Test if current prototype may be deleted"::
	get(M, current, Icon),
	send(Icon, can_delete).


delete(M) :->
	"Delete current prototype"::
	get(M, current, Icon),
	(   send(Icon, can_delete)
	->  send(M, activate_select),
	    send(Icon, free),
	    send(M, modified, @on)
	;   send(@display, inform, 'Can''t delete this prototype'),
	    fail
	).


		/********************************
		*           SAVE/LOAD		*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Saving/loading is very similar to the corresponding code in canvas.pl.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

save_as(M) :->
	"Save in user-requested file"::
	get(@finder, file, @off, '.proto', File),
	send(M, save, File).


save(M, File:[file]) :->
	"Save prototypes to named file"::
	(   File == @default
	->  get(M, file, SaveFile),
	    SaveFile \== @nil
	;   send(M, file, File),
	    SaveFile = File
	),
	send(M?graphicals, save_in_file, SaveFile),
	send(M, modified, @off).


load_from(M) :->
	"Load from user-requested file"::
	get(@finder, file, @on, '.proto', File),
	send(M, load, File).


load(M, File:[file]) :->
	"Load prototypes from named file"::
	(   File == @default
	->  get(M, file, LoadFile),
	    LoadFile \== @nil
	;   send(M, file, File),
	    LoadFile = File
	),
	send(M, clear),
	get(LoadFile, object, Chain),
	send(Chain, for_all, message(M, display, @arg1)),
	send(M?graphicals?head, activate),
	send(M, modified, @off).
	
:- pce_end_class.


		/********************************
		*             ICONS		*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
We have chosen  to specialise class   `bitmap' to represent the  icon.
Each icon represents a prototype, a mode and a cursor that is  used by
the canvas to indicate the mode.  The visual representation of an icon
is  an outline that indicates  the mode  and  a  small  version of the
prototype to indicate what is drawn.

There are two   reasonable choices  for this job.   One is   to use  a
subclass of device and display the outline  and a resized clone of the
prototype.  The other is to use class bitmap and  draw  a clone of the
prototype in it.  It is difficult to say which of the two is better. I
finally decided that just a bitmap is cheaper to save (considering the
fact  that  the device  case holds a  bitmap  of the  same  size too).
Another criterium is how difficult it is to  change an argument of the
prototype.  For a device this is slightly simpler  as we just pass the
message to change the argument to the prototype  and the clone  of the
prototype displayed in the icon.  Using a bitmap, we have to recompute
the contents of the bitmap.  This however is not very hard.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(draw_icon, bitmap).

variable(proto,		'graphical|link*',	get,
	 "Prototype represented").
variable(mode,		name,		both,
	 "Mode initiated by the icon").
variable(mode_cursor,	name,		both,
	 "Associated cursor-name").


initialise(I, Proto:'graphical|link*', Mode:name, Cursor:cursor) :->
	"Create an icon for a specific mode"::
	send(I, send_super, initialise, image(@nil, 48, 32)),
	send(I, mode, Mode),
	send(I, proto, Proto),
	send(I, slot, mode_cursor, Cursor?name).


can_delete(I) :->
	"Can I delete this icon?"::
	get(I, mode, create_proto).


		/********************************
		*           PROTOTYPES		*
		********************************/

proto(I, Proto:'graphical|link*') :->
	"Set the prototype"::
	send(I, slot, proto, Proto),
	send(I, paint_proto, Proto).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Create the image of  the  icon.   First,  we  will paint the  outline,
indicating the mode.  Next, we make a copy of  the  prototype (because
we have to modify it and we should not change the original prototype),
modify the text to `T' and the size to  fit  in the icon.  Finally, we
draw the prototype  in the icon and  send `Object ->done' to the clone
to inform PCE we have done with it.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

paint_proto(I, Proto:'link|graphical*') :->
	"Paint a small version of the prototype"::
	send(I, paint_outline),
	(   Proto == @nil
	->  true
	;   send(Proto, instance_of, link)
	->  get(Proto?line, clone, Clone),
	    send(Clone, points, 11, 10, 27, 20),
	    send(I, draw_in, Clone)
	;   send(Proto, instance_of, path),
	    send(Proto?points, empty)
	->  get(Proto, clone, Clone),
	    send(Clone, clear),
	    send(Clone, append, point(10,10)),
	    send(Clone, append, point(20,7)),
	    send(Clone, append, point(30,15)),
	    send(Clone, append, point(15,21)),
	    send(I, draw_in, Clone)
	;   get(Proto, clone, Clone),
	    (   send(Clone, has_send_method, string)
	    ->  send(Clone, string, 'T')
	    ;   true
	    ),
	    send(Clone, size, size(30, 14)),
	    send(Clone, center, point(22, 14)),
	    send(I, draw_in, Clone)
	).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Paint the outline in the bitmap.  For each of the outlines, there is a
bitmap file named `Mode.bm' in PCE's bitmap search-path.  We copy this
image in the bitmap.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

paint_outline(I) :->
	"Paint the mode indicating bitmap"::
	get(I, mode, Mode),
	concat(Mode, '.bm', Outline),
	send(I, copy, image(Outline)).


		/********************************
		*           ATTRIBUTES		*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
These two methods from the  interface to  the attribute editor.    See
also the files `attribute.pl' and `shape.pl'.  Note that prototypes do
not have a position and  therefore  the `x'  and  `y'  should  not  be
regarded arguments.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

has_attribute(I, Att:name) :->
	"Test if prototype has named attribute"::
	\+ memberchk(Att, [x, y]),
	send(I?proto, has_attribute, Att).


attribute(I, Att:name, Val:any) :->
	"Set attribute of prototype"::
	send(I?proto, Att, Val),
	send(I, repaint_proto),
	send(I?window, modified, @on).


		/********************************
		*          ACTIVATION		*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The event  parsing.  Currently we  only define left-click  to activate
the icon.  Activating the  gesture is done via  the ->event method, so
the gestures won't be saved to file.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_global(@icon_recogniser,
	      new(handler_group(click_gesture(left, '', single,
					      message(@event?receiver,
						      activate))))).


event(_I, Ev:event) :->
	send(@icon_recogniser, event, Ev).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Activate an icon.   First it sets `Graphical   ->inverted' to @on  for
only this icon in  the menu.  Note  the  use of `Device ->for_all' and
`if'.   This is the  most efficient way to  reach our  goals,  both in
terms  of the  amount of   code we  have  to  write   as in  terms  of
performance.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

activate(I) :->
	"Select the icon; set mode and proto"::
	send(I?device, for_all, @default,
	     if(@arg1 == I,
		message(@arg1, inverted, @on),
		message(@arg1, inverted, @off))),
	send(I?frame, mode, I?mode, I?mode_cursor),
	send(I?frame, proto, I?proto).

:- pce_end_class.
