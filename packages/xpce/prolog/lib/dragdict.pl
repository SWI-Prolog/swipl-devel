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

:- module(pce_drag_and_drop_browser, []).
:- use_module(library(pce)).
:- use_module(library(dragdrop)).	% no autoload, we need it now

:- pce_begin_class(drag_and_drop_dict_item_gesture,
		   drag_and_drop_gesture,
		   "Drag and drop items from a browser").

class_variable(button, button_name, left,
	       "By default drag-and-drop from left").

set_source(G, Ev:event) :->
	"Set <-source to dict_item or <-get_source(dict_item)"::
	get(Ev, receiver, LB),
	get(LB, dict_item, Ev, DI),
	get(G, get_source, Function),
	(   Function == @nil
	->  send(G, slot, source, DI)
	;   get(Function, '_forward', DI, Source),
	    send(G, slot, source, Source)
	).


cursor(G, LB:list_browser, Ev:event, Cursor:cursor) :<-
	"Make cursor for the dict_item"::
	(   get(G, class_variable_value, cursor, Cursor), Cursor \== @nil
	->  send(G?offset, set, 0, 0)
	;   get(LB, dict_item, Ev, DI),
	    get(DI, label, Label),
	    font(DI, Font),
	    new(T, text(Label, left, Font)),
	    get(T, size, size(W, H)),
	    new(BM, image(@nil, W, H)),
	    send(BM, draw_in, T),
	    get(DI, image, LB),
	    get(DI, position, DiPos),
	    (   get(G, warp, @on)
	    ->  new(HotSpot, point(W/2, H/2)),
		send(DiPos, plus, HotSpot),
		send(LB, pointer, DiPos)
	    ;   get(Ev, position, LB, EvPos),
		get(EvPos, difference, DiPos, HotSpot),
		get(HotSpot, x, HX),
		get(HotSpot, y, HY),
		( HX > W-8 -> send(HotSpot, x, W-8) ; true ),
		( HY > H -> send(HotSpot, y, H) ; true )
	    ),
	    send(BM, or, image('cross.bm'), point(HotSpot?x-8, HotSpot?y-8)),
	    new(Cursor, cursor(@nil, BM, @default, HotSpot))
	).

font(DI, Font) :-
	get(DI, style, StyleName),
	atom(StyleName),
	get(DI, image, Browser),
	get(Browser?styles, StyleName, Style),
	get(Style, font, Font), !.
font(DI, Font) :-
	get(DI, style, @default),
	get(DI, image, Browser),
	get(Browser, font, Font).
	

:- pce_end_class.

/*
test :-
	send(new(B, browser), open),
	send(@classes, for_all, message(B, append, @arg1)),
	send(B, sort),
	send(B?list_browser, recogniser,
	     new(G, drag_and_drop_dict_item_gesture)),
	send(G, warp, @off).
*/
