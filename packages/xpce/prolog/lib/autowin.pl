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

:- module(pce_auto_window, []).
:- use_module(library(pce)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This   library   defines    the     classes    auto_sized_picture    and
auto_sized_dialog, subclasses of picture and dialog that decide on their
size and scrollbars depending on the size of the contents.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


:- pce_begin_class(auto_sized_picture, picture,
		   "Window that automatically fits the contents").

variable(border,   int := 10,	both, "Border around contents").
variable(max_size, size,	both, "Maximum size").

class_variable(max_size, size,	size(700,500), "Maximum size").

initialise(W, L:label=[name], D:display=[display]) :->
	send(W, send_super, initialise, L, @default, D).

'_compute_desired_size'(W) :->
	get(W, bounding_box, BB),
	get(W, border, B),
	send(W, scroll_to, point(BB?x-B, BB?y-B)),
	get(BB, width, BW),
	get(BB, height, BH),
	get(W, max_size, size(MW, MH)),
	WW is min(BW + 2*B, MW),
	WH is min(BH + 2*B, MH),
	(   WH < BH + 2*B		% force SB to compute!?
	->  get(W, vertical_scrollbar, _)
	;   true
	),
	(   WW < BW + 2*B		% force SB to compute!?
	->  get(W, horizontal_scrollbar, _)
	;   true
	),
	send(W, size, size(WW, WH)).

:- pce_end_class.


:- pce_begin_class(auto_sized_dialog, dialog,
		   "Dialog with scroll-bars if the contents are too big").

variable(max_size, size,	both, "Maximum size").
class_variable(max_size, size,	size(700,500), "Maximum size").

initialise(W, L:label=[name], D:display=[display]) :->
	send(W, send_super, initialise, L, @default, D).

'_compute_desired_size'(D) :->
	send(D, send_super, '_compute_desired_size'),
	get(D, ideal_width, BW),
	get(D, ideal_height, BH),
	get(D, max_size, size(MW, MH)),
	WW is min(BW, MW),
	WH is min(BH, MH),
	(   WH < BH
	->  send(D, vertical_scrollbar, @on)
	;   true
	),
	(   WW < BW
	->  send(D, horizontal_scrollbar, @on)
	;   true
	),
	send(D, set, @default, @default, WW, WH).

:- pce_end_class.
