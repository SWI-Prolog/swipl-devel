/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
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
