/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
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
	
