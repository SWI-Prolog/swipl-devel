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

:- module(constraint_demo,
	  [ constraint_demo/0
	  ]).

:- use_module(library(pce)).
:- require([ send_list/3
	   ]).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Create a relation expressing ``The center of both constrained objects is
equal''.  The same relation object may   be  used by multiple constraint
objects (e.i.  is *reusable*).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_global(@center, new(identity(center))).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Create a graphical window with two boxes   that may be moved and resized
using the middle button.  Two sliders are   defined to modify the center
of the two boxes as too.  All relations are expressed using constraints.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

constraint_demo :-
	new(P, picture('Constraint Demo')),
	send(new(D, dialog), below, P),

	send(P, display, new(B1, box(100,100))),
	send(P, display, new(B2, box(50,50))),
	send_list([B1, B2], recogniser,
		  handler_group(resize_gesture(left),
				move_gesture(left))),
	new(_, constraint(B1, B2, @center)),

	send(D, append, new(S1, slider(center_x, 0, 500, 200))),
	send(D, append, new(S2, slider(center_y, 0, 500, 100))),
	send_list([S1, S2], drag, @on),
	send_list([S1, S2], message, message(@receiver, update_constraints)),
	send(D, append, button(quit, message(D, destroy))),
	
	new(_, constraint(S1, B1, identity(selection, center_x))),
	new(_, constraint(S2, B2, identity(selection, center_y))),

	send(D, open).
	
