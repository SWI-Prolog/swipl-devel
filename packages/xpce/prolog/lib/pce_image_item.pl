/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (C): 1985-2002, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
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
