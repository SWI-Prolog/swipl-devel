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


:- module(pce_splash_screen, []).
:- use_module(library(pce)).
:- use_module(library(autowin)).
:- use_module(library(hyper)).
:- use_module(library(help_message)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Provide a splash-screen for an application.  The simplest operation is
to open it with a timer that destroys it:

	send(new(splash_screen(Image)), open, Time)
	<continue initialisation>

Note that the window will only disappear after the elapsed time if
events are dispatched (see @display->synchronise).

You can also open it permanently and   associate active regions to parts
of the image:

	new(S, splash_screen(Image)),
	send(S, map, graphical(25,50,100,20),
	     message(@prolog, start_my_app),
	     'Start application'),
	send(S, map, graphical(25,70,100,20),
	     message(@prolog, browse_manual),
	     'Browse manual'),
	send(S, open).
	
The graphical is used to define the sensitive area as well as the cursor
that is visible in the area. It is not displayed.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(splash_screen, auto_sized_picture,
		   "Show splash window").

variable(maps,	chain*,	 get, "Event-handling map").

initialise(S, Img:image, Label:[char_array]) :->
	send_super(S, initialise, Label),
	get(@display, size, MaxSize),
	send(S, max_size, MaxSize),
	send(S, border, 0),
	(   Label == @default
	->  send(S, kind, popup)	% do not show border
	;   true
	),
	send(S, display, bitmap(Img)).

open(S, Time:[real]*) :->
	send(S, open_centered),
	send(S, wait),
	(   number(Time)
	->  new(T, timer(Time, message(S, destroy))),
	    new(_, partof_hyper(S, T, timer)),
	    send(T, start)
	;   true
	).

map(S, Gr:[graphical]*, Msg:code, Alt:[char_array]) :->
	"Extend the event-handling map"::
	default(Gr, @nil, TheGr),
	(   get(S, maps, Maps),
	    Maps \== @nil
	->  true
	;   send(S, slot, maps, new(Maps, chain))
	),
	send(Maps, append, splash_map(TheGr, Msg, Alt)).
	    
map(S, At:point, Map:splash_map) :<-
	"Find map from position"::
	get(S, maps, Maps), Maps \== @nil,
	object(At, point(X, Y)),
	get(Maps, find, 
	    or(@arg1?graphical == @nil,
	       message(@arg1?graphical, in_event_area, X, Y)),
	    Map).

:- pce_group(event).

:- pce_global(@spash_screen_recogniser,
	      new(click_gesture(left, '', single,
				message(@receiver, clicked,
					@event?position)))).

event(S, Ev:event) :->
	(   send_super(S, event, Ev)
	->  true
	;   send(@spash_screen_recogniser, event, Ev)
	;   send(Ev, is_a, loc_move),
	    (	get(S, map, ?(Ev, position, S), Map)
	    ->  get(Map, graphical, Gr),
		(   Gr \== @nil,
		    get(Gr, cursor, Cursor),
		    Cursor \== @nil
		->  send(S, cursor, Cursor)
		;   send(S, cursor, hand2)
		)
	    ;	get(S, class_variable_value, cursor, DefCursor),
		send(S, cursor, DefCursor)
	    )
	).

help_message(S, What:{tag,summary}, Ev:event, Alt:string) :<-
	"Show balloon"::
	What == tag,
	get(S, map, ?(Ev, position, S), Map),
	get(Map, alt, Alt),
	Alt \== @nil.

clicked(S, At:point) :->
	"Handle clicked"::
	send(S, expose),
	get(S, map, At, Map),
	get(Map, message, Code),
	get(S, display, Display),
	send(Display, busy_cursor),
	call_cleanup(send(Code, forward, S),
		     send(Display, busy_cursor, @nil)).

:- pce_end_class(splash_screen).

:- pce_begin_class(splash_map, object, "Area in splash screen").

variable(graphical,	graphical*,	get, "Graphical defining area").
variable(message,	code,		get, "Associated code").
variable(alt,		char_array*,	get, "Balloon text").

initialise(SM, Gr:graphical*, Msg:code, Alt:[char_array]*) :->
	send_super(SM, initialise),
	send(SM, slot, graphical, Gr),
	send(SM, slot, message, Msg),
	default(Alt, @nil, TheAlt),
	send(SM, slot, alt, TheAlt).

:- pce_end_class(splash_map).
