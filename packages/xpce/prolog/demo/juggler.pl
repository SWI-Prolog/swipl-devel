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


:- module(juggler,
	  [ juggle_demo/0
	  ]).

:- use_module(library(pce)).
:- require([ concat_atom/2
	   , forall/2
	   , member/2
	   , send_list/3
	   ]).

juggle_demo :-
	new(_, juggler).

:- pce_begin_class(juggler, frame).

variable(timer, timer, get, "Timer for animation").
variable(speed, int,   get, "Animations/second").

class_variable(geometry, geometry,	'72x72+0+0',	"Default geometry").
class_variable(speed,    int,		10,		"Animations/second").

:- pce_global(@juggler_popup, make_juggler_popup).

make_juggler_popup(P) :-
	new(P, popup),
	new(J, @arg1?frame),
	send_list(P, append,
		  [ menu_item(stop,  message(J, stop))
		  , menu_item(start, message(J, start))
		  , menu_item(speed, message(J, set_speed), @default, @on)
		  , menu_item(quit,  message(J, free))
		  ]).
	       

initialise(F) :->
	"Create a juggler-window"::
	send(F, send_super, initialise, 'Juggler', popup),
	send(F, append, new(P, picture)),
	send(P, scrollbars, none),
	send(P, popup, @juggler_popup),

	send(P, icon, 'juggler1.bm', 'Juggler'),

	send(P, display, new(Fig, figure)),
	send(Fig, status, 1),
	forall(member(N, [1,2,3,4,5]),
	       (concat_atom([juggler, N, '.bm'], IconName),
		new(I, bitmap(IconName)),
	        send(I, name, N),
	        send(Fig, display, I))),

	send(F, slot, timer, new(T, timer(0.1, message(Fig, next_status)))),
	send(T, start),
  	get(F, class_variable_value, speed, Speed),
	send(F, speed, Speed),
	
	send(F, open).
	
unlink(F) :->
	send(F?timer, stop),
	send(F, send_super, unlink).

speed(F, Speed:int) :->
	"Set animations/second"::
	Interval is 1 / Speed,
	send(F?timer, interval, Interval),
	send(F, slot, speed, Speed).

stop(F) :->
	"Stop the juggler"::
	send(F?timer, stop).

start(F) :->
	"(Re)Start the juggler"::
	send(F?timer, start).

set_speed(F) :->
	"Popup a dialog to set the speed"::
	new(D, dialog('Speed of juggler')),
	send(D, append, new(S, slider(speed, 1, 50, F?speed,
				      message(F, speed, @arg1)))),
	send(S, drag, @on),
	send(D, append, button(quit, message(D, free))),
	send(D, open).

:- pce_end_class.
