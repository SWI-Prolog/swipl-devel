/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

:- module(pce_image_item, []).
:- use_module(library(pce)).
:- require([ ignore/1
	   ]).

:- pce_autoload(image_browser, library(pce_image_browser)).

:- pce_begin_class(image_item, label_box,
		   "Display and browse for an image").


:- pce_global(@image_item_nil_image, new(image(@nil, 32, 32))).

variable(center_bitmap,	point*, 	none, "Center of the bitmap").
variable(directory,	[directory],	both, "Default search directory").
variable(extensions,	[chain],	both, "Default search extensions").

initialise(II, Name:[name], Default:[function|image], Msg:[code]*) :->
	send(II, directory, @default),
	send(II, extensions, @default),
	send(II, send_super, initialise, Name, Msg),
	send(II, append, new(BM, bitmap(@image_item_nil_image))),
	send(II, append, new(Br, button(browse)), right),
	send(BM, pen, 2),
	send(BM, reference, point(0, 0)),	% BM?height/2)),
	send(Br, reference, point(0, 0)),	% Br?height/2)),
	send(Br, label, 'Browse ...'),
	(   Default \== @default
	->  send(II, default, Default)
	;   true
	).


layout_dialog(II) :->
	send(II, send_super, layout_dialog),
	get(II, member, bitmap, BM),
	send(II, slot, center_bitmap, BM?center).


update_layout(_II) :->
/*	(   get(II, slot, center_bitmap, Center),
	    Center \== @nil
	->  get(II, member, bitmap, BM),
	    send(BM, center, Center)
	;   true
	),
*/
	true.

:- pce_group(selection).

selection(II, Selection:image) :->
	get(II, member, bitmap, BM),
	send(BM, image, Selection),
	send(II, update_layout),
	send(II, modified, @off).
selection(II, Selection:image) :<-
	get(II, member, bitmap, BM),
	get(BM, image, Selection),
	Selection \== @image_item_nil_image,
	send(II, modified, @off).

clear(II) :->
	"Clear the selection"::
	get(II, member, bitmap, BM),
	send(BM, image, @image_item_nil_image),
	send(II, modified, @off).

:- pce_group(browse).

browse(II) :->
	"Browse for a new image"::
	get(II, display, Display),
	send(Display, busy_cursor),
	get(II, directory, Dir),
	get(II, extensions, Exts),
	new(IB, image_browser(Dir, Exts)),
	send(Display, busy_cursor, @nil),
	send(IB, open_message, message(IB, return, @arg1)),
	send(new(D, dialog), below, IB),
	send(D, append, new(OK, button(ok, message(D, return, IB?selection)))),
	send(D, append, button(cancel, message(D, return, @nil))),
	send(IB, select_message, message(OK, active, @on)),
	send(OK, active, @off),
	send(IB, transient_for, II?frame),
	send(IB, modal, transient),
	get(IB, confirm_centered, II?frame?area?center, Image),
	(   Image \== @nil
	->  get(II, member, bitmap, BM),
	    send(BM, image, Image),
	    send(II, update_layout),
	    send(II, modified, @on),
	    ignore(send(II?device, modified_item, II, @on))
	;   true
	),
	send(IB, free).

:- pce_end_class.

/*
test :-
	new(D, dialog),
	send(D, append, new(II, image_item(image, 'pce.bm'))),
	send(II, directory, '$PCEHOME/bitmaps/16x16'),
	send(D, open).
*/
