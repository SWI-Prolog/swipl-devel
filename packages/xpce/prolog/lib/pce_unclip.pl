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

:- module(pce_unclip, []).
:- use_module(library(pce)).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This library deals with showing graphicals   that  are partly clipped by
the window on which  they  are  displayed.   It  is  used  by  the class
toc_image from library(pce_toc) to  show   nodes  that  (typically) have
their right-side clipped and provides a  convient mechanism to deal with
a few long labels in a relatively small window.

It is upto the clipped graphical to  detect the mouse is positioned over
it   and   part   of   the   graphical     is    clipped.   The   method
`graphical->clipped_by_window' can be used to   detect  the graphical is
(partly) obscured.

For an example, please start  the   SWI-Prolog  manual  browser using ?-
help.   The   source-code   that   attaches     this   library   is   in
`toc_image->entered'.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_extend_class(graphical).

clipped_by_window(Gr) :->
	"Test if graphical is clipped by window border"::
	get(Gr, window, Window),
	get(Window, visible, Visible),
	get(Gr, absolute_position, Window, point(X,Y)),
	get(Gr, area, area(_,_,W,H)),
	\+ send(Visible, inside, area(X,Y,W,H)).

:- pce_end_class(graphical).


		 /*******************************
		 *	     INVISIBLE		*
		 *******************************/

:- pce_global(@unclip_window, new(pce_unclip_window)).

:- pce_begin_class(pce_unclip_window, window).

variable(handler, handler, get, "Handler used to fetch all events").

class_variable(background, colour, azure).

initialise(W) :->
	send_super(W, initialise),
	get(W, frame, Fr),
	send(Fr, kind, popup),
	send(W, pen, 0),
	send(Fr, border, 1),
	send(Fr?tile, border, 0),
	send(W, slot, handler,
	     handler(any, message(W, unclipped_event, @event))).

attach(W, To:graphical) :->
	"Attach to graphical"::
	get(To, window, ToWindow),
	(   get(W, hypered, mirroring, Old)
	->  send(W, delete_hypers, mirroring),
	    (	get(Old, window, ToWindow)
	    ->	true
	    ;	send(Old, grab_pointer, @off),
		send(ToWindow, grab_pointer, @on)
	    )
	;   get(W, handler, H),
	    send(ToWindow, grab_pointer, @on),
	    send(@display?inspect_handlers, prepend, H)
	),
	new(_, hyper(To, W, mirror, mirroring)),
	send(W, update),
	get(To, display_position, point(X,Y)),
	(   get(@pce, window_system, windows)
	->  Border = 0			% TBD: Fix inside kernel
	;   get(W, border, Border)
	),
	send(W, open, point(X-Border,Y-Border)),
	send(W, expose).

update(W) :->
	"Update for changed receiver"::
	send(W, clear),
	(   get(W, hypered, mirroring, Gr)
	->  get(Gr, clone, Clone),
	    get(Clone, size, Size),
	    send(W, size, Size),
	    send(Clone, set, 0, 0),
	    send(W, display, Clone)
	;   true
	).


detach(W) :->
	"Detach and hide"::
	(   get(W, hypered, mirroring, Gr)
	->  send(W, delete_hypers, mirroring),
	    send(W, clear),
	    send(W, show, @off),
	    get(W, handler, H),
	    send(Gr?window, grab_pointer, @off),
	    send(@display?inspect_handlers, delete, H)
	;   true
	).


unclipped_event(W, Ev:event) :->
	(   send(Ev, is_a, loc_move),
	    \+ send(Ev, inside, W)
	->  send(W, detach)
	;   (   send(Ev, is_a, button)
	    ;	send(Ev, is_a, keyboard)
	    )
	->  send(W, detach),
	    fail			% normal event-processing
	).

:- pce_end_class(pce_unclip_window).
