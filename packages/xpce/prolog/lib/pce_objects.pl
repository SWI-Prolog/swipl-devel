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
