/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

:- module(pce_objects, []).
:- use_module(library(pce)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module defines a number of commonly used global objects.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

		 /*******************************
		 *	   EVENT CONTEXT	*
		 *******************************/

:- pce_global(@event_receiver,	new(@event?receiver)).
:- pce_global(@event_char,	new(@event?id)).
:- pce_global(@node,		new(@event_receiver?node)).
:- pce_global(@tree,		new(@event_receiver?device)).


		 /*******************************
		 *	   CODE OBJECTS		*
		 *******************************/

:- pce_global(@true,		new(and)).
:- pce_global(@false,		new(or)).


		 /*******************************
		 *	     SPATIALS		*
		 *******************************/

:- pce_global(@center,		new(spatial(xref = x + w/2, yref = y + h/2,
					    xref = x + w/2, yref = y + h/2))).
:- pce_global(@center_x,	new(spatial(xref = x + w/2, @default,
					    xref = x + w/2, @default))).
:- pce_global(@center_y,	new(spatial(@default, yref = y + h/2,
					    @default, yref = y + h/2))).

		 /*******************************
		 *    HANDLES AND CONNECTIONS	*
		 *******************************/
					    
:- pce_global(@north, handle(x + w/2, y,       link, north)).
:- pce_global(@south, handle(x + w/2, y + h,   link, south)).
:- pce_global(@east,  handle(x + w,   y + h/2, link, east)).
:- pce_global(@west,  handle(x,       y + h/2, link, west)).
