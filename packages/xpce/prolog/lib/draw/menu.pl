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
:- require([ default/3
	   , memberchk/2
	   ]).


		/********************************
		*          ICON MENU		*
		********************************/

:- pce_begin_class(draw_menu, dialog).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Variables to keep track of load/save.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

variable(file,		file*,	both,
	 "File for storing prototypes").
variable(modified,	bool,	get,
	 "Menu has been modified").

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The menu of with the modes and  prototypes   is  a  dialog window with a
displayed menu. This is one  of   the  many possibilities. When updating
this documentation, it would have been a   more  natural choice to use a
dialog window with an attached format and buttons using an `image' label
for the modes.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

initialise(M) :->
	send(M, send_super, initialise, 'Icons'),
	send(M, gap, size(0,0)),
	send(M, ver_shrink, 100),
	send(M, ver_stretch, 100),
	send(M, display,
	     new(P, menu(proto, choice, message(@arg1, activate)))),
	send(P, layout, vertical),
	send(P, show_label, @off),
	send(M, resize_message, message(M, new_size, @arg2)),
	send(M, modified, @off).


new_size(M, Size:size) :->
	get(M, member, proto, Menu),
	get(Size, width, W),
	Cols is max(1, W // 48),
	send(Menu, columns, Cols).


modified(M, Value:[bool]) :->
	default(Value, @on, Val),
	send(M, slot, modified, Val).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Attach   a new prototype. 
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

proto(M, Proto:proto='graphical|link*', Mode:'mode=name',
      Cursor:cursor=cursor, Icon:icon=[image], Tag:tag=[name],
      UserProto:user_proto=[bool]) :->
	"Attach a new prototype"::
	default(UserProto, @off, UProto),
	get(M, member, proto, Menu),
	send(Menu, append, new(I, draw_icon(Proto, Mode, Cursor, Icon))),
	(   Tag \== @default,
	    send(I, has_send_method, help_message) % help message package
	->  send(I, help_message, tag, Tag) 	   % loaded
	;   true
	),
	send(I, user_proto, UProto),
	send(M, adjust),
	send(M, modified, @on).


adjust(M) :->
	"Make it wide enough to display all items"::
	(   get(M, displayed, @on)
	->  get(M?visible, height, H),
	    get(M, member, proto, Menu),
	    get(Menu?members, size, S),
	    get(Menu, border, B),
	    Rows is max(1, H//(32+2*B)),
	    Cols is (S+Rows-1)//Rows,
	    (	get(Menu, columns, Cols)
	    ->	true
	    ;	send(Menu, columns, Cols),
		send(M, ideal_width, Menu?width),
		send(M?frame, resize)
	    )
	;   true
	).


current(M, Icon:draw_icon) :<-
	"Find current icon"::
	get(M, member, proto, Menu),
	get(Menu, selection, Icon).


find_icon(M, Cond:code, Icon:draw_icon) :<-
	"Find icon from condition"::
	get(M, member, proto, Menu),
	get(Menu?members, map, @arg1?value, Icons),
	get(Icons, find, Cond, Icon).


activate_select(M) :->
	"Activate icon that does select"::
	get(M, member, proto, Menu),
	get(Menu?members, find, @arg1?(mode) == select, Item),
	send(Item, activate).

		 /*******************************
		 *	     STYLE  		*
		 *******************************/

columns(M, Cols:'0..') :->
	"Set # columns in menu"::
	get(M, member, proto, TheMenu),
	send(TheMenu, columns, Cols).


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

create_proto(M, Graphicals:chain, How:{as_is,virgin}) :->
	"Create a prototype from a chain of graphicals"::
	get(Graphicals, size, Size),
	(   Size == 0
	->  send(M, report, error, 'No selection')
	;   Size == 1
	->  get(Graphicals, head, Sel1),
	    clone_proto(How, Sel1, Proto),
	    send(Proto, selected, @off)
	;   new(Proto, draw_compound),
	    get(Graphicals, clone, Members),
	    send(Members, for_all,
		 and(message(Proto, display, @arg1),
		     message(@arg1, selected, @off))),
	    send(Proto, reference, @default),
	    send(Proto, string, '')
	),
	mode_and_cursor_from_proto(How, Proto, Mode, Cursor),
	send(M, proto, Proto, Mode, Cursor, user_proto := @on).
	
clone_proto(_, Connection, Clone) :-
	send(Connection, instance_of, connection), !,
	get(Connection, link, Link),
	get(Link, clone, Clone),
	send(Clone, copy, Connection).
clone_proto(virgin, Path, Clone) :-
	send(Path, instance_of, path),
	get(Path, clone, Clone),
	send(Clone, clear).
clone_proto(_, Graphical, Clone) :-
	get(Graphical, clone, Clone).


mode_and_cursor(text,  		draw_text,    xterm).
mode_and_cursor(box,   		draw_resize,  crosshair).
mode_and_cursor(ellipse,	draw_resize,  crosshair).
mode_and_cursor(line,  		draw_line,    crosshair).
mode_and_cursor(path,  		draw_path,    cross).
mode_and_cursor(link,  		draw_connect, plus).

mode_and_cursor_from_proto(as_is, _Proto, draw_proto, dotbox) :- !.
mode_and_cursor_from_proto(virgin, Proto, Mode, Cursor) :-
	mode_and_cursor(Class, Mode, Cursor),
	send(Proto, instance_of, Class).

		/********************************
		*            DELETE		*
		********************************/

can_delete(M) :->
	"Test if current prototype may be deleted"::
	get(M, current, Icon),
	send(Icon, can_delete).


delete(M, Icon0:[draw_icon]) :->
	"Delete current prototype"::
	(   Icon0 == @default
	->  get(M, current, Icon),
	    (	send(Icon, can_delete)
	    ->	send(M, activate_select)
	    ;	send(@display, inform, 'Can''t delete this prototype'),
		fail
	    )
	;   Icon = Icon0
	),
	send(Icon, free),
	send(M, modified, @on).

clear(M) :->
	"Remove all prototypes"::
	get(M, member, proto, TheMenu),
	send(TheMenu, clear).


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
	get(M, proto_sheet, Sheet),
	send(Sheet, save_in_file, SaveFile),
	send(M, report, status,
	     'Prototypes saved in %s', SaveFile?absolute_path),
	send(M, modified, @off).

proto_sheet(M, Sheet:sheet) :<-
	"Fetch description of user prototypes in a sheet"::
	new(Sheet, sheet(attribute(pcedraw_prototype_version, 1),
			 attribute(protos, new(Protos, chain)))),
	get(M, member, proto, Menu),
	send(Menu?members, for_all,
	     message(M, append_proto_sheet, Protos, @arg1)).


append_proto_sheet(_M, Protos:chain, Icon:draw_icon) :->
	"Append a single proto to Protos"::
	(   get(Icon, user_proto, @on)
	->  get(Icon, mode_cursor, CursorName),
	    get(Icon, mode, Mode),
	    get(Icon, slot, proto, Proto), % also @nil !
	    send(Protos, append,
		 sheet(attribute(cursor, CursorName),
		       attribute(mode, Mode),
		       attribute(proto, Proto)))
	;   true
	).


proto_sheet(M, Sheet:sheet, Clear:[bool]) :->
	"Load prototype definitions from sheet"::
	get(M, member, proto, Menu),
	(   Clear == @on
	->  send(Menu, clear),
	    send(M?frame, fill_menu)
	;   true
	),
	get(Sheet, protos, Chain),
	send(Chain, for_all,
	     message(M, proto,
		     @arg1?proto, @arg1?mode, @arg1?cursor,
		     user_proto := @on)).


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
	get(LoadFile, object, ProtoSheet),
	(   send(ProtoSheet, instance_of, sheet),
	    get(ProtoSheet, pcedraw_prototype_version, 1)
	->  send(M, proto_sheet, ProtoSheet, @on),
	    send(M, activate_select),
	    send(M, adjust),
	    send(M, modified, @off)
	;   send(M, report, error,
		 'File contains old or no PceDraw prototypes')
	).
	
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

:- pce_begin_class(draw_icon, menu_item).

variable(proto,		'graphical|link*', none,
	 "Prototype represented").
variable(mode,		name,		both,
	 "Mode initiated by the icon").
variable(mode_cursor,	name,		both,
	 "Associated cursor-name").
variable(user_proto,	bool := @off,	both,
	 "Prototype was created by the user").

item_size(48,32).

initialise(I, Proto:'graphical|link*', Mode:name,
	   Cursor:cursor, Image:[image]) :->
	"Create an icon for a specific mode"::
	send(I, send_super, initialise, @nil),
	send(I, slot, value, I),	% hack, needs to be fixed!
	send(I, mode, Mode),
	send(I, proto, Proto, Image),
	send(I, slot, mode_cursor, Cursor?name).

proto(I, Proto:'graphical|link') :<-
	get(I, slot, proto, Proto),
	Proto \== @nil.


can_delete(I) :->
	"Can I delete this icon?"::
	get(I, user_proto, @on).


		/********************************
		*           PROTOTYPES		*
		********************************/

proto(I, Proto:'graphical|link*', Image:[image]) :->
	"Set the prototype"::
	send(I, slot, proto, Proto),
	(   Image == @default
	->  send(I, paint_proto)
	;   send(I, label, Image)
	).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Create the image of  the  icon.   First,  we  will paint the  outline,
indicating the mode.  Next, we make a copy of  the  prototype (because
we have to modify it and we should not change the original prototype),
modify the text to `T' and the size to  fit  in the icon.  Finally, we
draw the prototype  in the icon and  send `Object ->done' to the clone
to inform PCE we have done with it.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

paint_proto(MI) :->
	"Paint a small version of the prototype"::
	get(MI, slot, proto, Proto),
	item_size(IW, IH),
	send(MI, label, new(I, image(@nil, IW, IH))),
	send(MI, paint_outline),
	(   Proto == @nil
	->  true
	;   send(Proto, instance_of, link)
	->  new(Dev, device),			% links (connection)
	    send(Dev, display, new(B1, box(0,0)), point(11, 10)),
	    send(Dev, display, new(B2, box(0,0)), point(27, 20)),
	    send(B1, handle, handle(0, 0, Proto?from)),
	    send(B2, handle, handle(0, 0, Proto?to)),
	    (	get(Proto, attribute, draw_connection_class, Class)
	    ->	true
	    ;	Class = draw_connection
	    ),
	    Term =.. [Class, B1, B2, Proto],
	    new(Connection, Term),
	    (	send(Connection, has_send_method, menu_text)
	    ->  send(Connection, menu_text)
	    ;   true
	    ),
	    send(I, draw_in, Dev),
	    send(Dev, destroy)
	;   send(Proto, instance_of, path), 	% Path case
	    send(Proto?points, empty)
	->  get(Proto, clone, Clone),
	    send(Clone, clear),
	    send(Clone, append, point(10,10)),
	    send(Clone, append, point(20,7)),
	    send(Clone, append, point(30,15)),
	    send(Clone, append, point(15,21)),
	    send(I, draw_in, Clone)
	;   send(Proto, instance_of, bezier_curve)
	->  get(Proto, clone, Clone),
	    send(Clone, start, point(5,23)),
	    send(Clone, end, point(35,23)),
	    send(Clone, control1, point(15,0)),
	    send(Clone, control2, point(48,0)),
	    send(I, draw_in, Clone)
	;   get(Proto, clone, Clone),		% general case
	    (   send(Clone, has_send_method, menu_text)
	    ->  send(Clone, menu_text)
	    ;   true
	    ),
	    (	get(MI, mode, draw_proto)
	    ->	get(Clone, size, size(PW, PH)),
		(   (PW/30) > (PH/20)
		->  DW = 30, DH is integer(30 * PH/PW)
		;   DH = 20, DW is integer(20 * PW/PH)
		)
	    ;	DW = 30,
		DH = 14
	    ),
	    send(Clone, size, size(DW, DH)),
	    send(Clone, center, point(22, 14)),
	    send(I, draw_in, Clone)
	),
	send(MI, label, I).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Paint the outline in the bitmap.  For each of the outlines, there is a
bitmap file named `Mode.bm' in PCE's bitmap search-path.  We copy this
image in the bitmap.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

paint_outline(MI) :->
	"Paint the mode indicating bitmap"::
	get(MI, label, I),
	get(MI, mode, Mode),
	outline_image(Mode, ImageFile),
	send(I, copy, image(resource(ImageFile))).

outline_image(select,	     'select.bm').
outline_image(draw_text,     'draw_text.bm').
outline_image(draw_resize,   'draw_resize.bm').
outline_image(draw_line,     'draw_line.bm').
outline_image(draw_bezier,   'draw_line.bm').
outline_image(draw_path,     'draw_path.bm').
outline_image(draw_connect,  'draw_connect.bm').
outline_image(draw_cconnect, 'draw_cconnect.bm').
outline_image(draw_proto,    'draw_proto.bm').

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Hook to find the resource.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

resource(Name, image, image(Name)) :-
	outline_image(_, Name).


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


draw_attribute(I, Att:name, Val:any) :->
	"Set attribute of prototype"::
	send(I?proto, draw_attribute, Att, Val),
	send(I, repaint_proto),
	send(I?window, modified, @on).


		/********************************
		*          ACTIVATION		*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Activate an icon.   First it sets `Graphical   ->inverted' to @on  for
only this icon in  the menu.  Note  the  use of `Device ->for_all' and
`if'.   This is the  most efficient way to  reach our  goals,  both in
terms  of the  amount of   code we  have  to  write   as in  terms  of
performance.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

activate(I) :->
	"Select the icon; set mode and proto"::
	get(I, menu, Menu),
	send(Menu, selection, I),
	send(Menu?frame, mode, I?mode, I?mode_cursor),
	(   get(I, proto, Proto)
	->  send(Menu?frame, proto, Proto)
	;   send(Menu?frame, proto, @nil)
	).

:- pce_end_class.
