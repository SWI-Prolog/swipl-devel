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

:- module(pce_tile_hierarchy, []).
:- use_module(library(pce)).

:- pce_begin_class(tile_hierarchy, frame).

initialise(F, Tile:tile) :->
	"Display hierarchy of tile"::
	send(F, send_super, initialise, 'Tile Hierarchy'),
	send(F, append, new(P, picture)),
	send(new(D, dialog), below, P),
	fill_dialog(D),
	send(F, display_tile, Tile),
	send(F, open).


fill_dialog(D) :-
	get(D, frame, Frame),
	send(D, append, button(quit, message(Frame, destroy))).


display_tile(F, Tile:tile) :->
	"Display hierarchy below tile"::
	get(F, member, picture, P),
	send(P, clear),
	send(P, display, tree(new(R, tile_node(Tile)))),
	send(R, expand).

:- pce_end_class.

:- pce_begin_class(tile_node, node).

initialise(N, Tile:tile) :->
	"Create node for tile"::
	(   get(Tile, object, Obj),
	    Obj \== @nil
	->  get(Obj, name, Label)
	;   get(Tile, orientation, Label)
	),

	new(S, string('%s %dx%d\n-%dx+%d -%dy+%d',
		      Label,
		      Tile?ideal_width, Tile?ideal_height,
		      Tile?hor_shrink, Tile?hor_stretch,
		      Tile?ver_shrink, Tile?ver_stretch)),

	new(_, hyper(N, Tile, tile)),

	send(N, send_super, initialise, text(S, center)).

expand(N) :->
	"Expand tile sub-tree"::
	get(N, hypered, tile, Tile),
	(   get(Tile, members, SubTiles),
	    SubTiles \== @nil
	->  send(Tile?members, for_all,
		 and(assign(new(SubNode, var), ?(N?class, instance, @arg1)),
		     message(N, son, SubNode),
		     message(SubNode, expand)))
	;   true
	).

:- pce_end_class.
