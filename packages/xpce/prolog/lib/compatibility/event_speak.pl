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
        format('Not yet implemented~n').


%   +stretch_region(Corner, Region)
%
%   Corner is a symbolic name of Region.  Regions are defined as being 
%   quarter width and height.

stretch_region(north_east, region(w-w/4, 0    , w/4, h/4)).
stretch_region(north_west, region(0    , 0    , w/4, h/4)).
stretch_region(south_west, region(0    , h-h/4, w/4, h/4)).
stretch_region(south_east, region(w-w/4, h-h/4, w/4, h/4)).


%   +stretch_opposite(Corner, OppositeCorner)
%
%   OppositeCorner is the corner opposite of Corner.

stretch_opposite(north_west, south_east).
stretch_opposite(north_east, south_west).
stretch_opposite(south_west, north_east).
stretch_opposite(south_east, north_west).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			     MOVING GRAPHICALS

move-Button
    Drags a graphical by moving the entire graphical each drag-event.  This
    is pleasant in simple drawings as the display always shows the correct
    contents, but annoyingly slow if complex graphicals are moved this way
    or the graphical is moved over complex graphicals.  Use with care!    
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

do_make_handlers(move-Button, Group) :- !, 
	mouse_event_type(Button-down, Down), 
	mouse_event_type(Button-drag, Drag), 
	mouse_event_type(Button-up, Up), !, 

	new_handler(DragHandler, Drag, 
		    block(message(@event_receiver, position, 
				  ?(@event_position,
				    difference,
				    ?(@event_window, saved_cursor))))),
        send(DragHandler, active, @off),
	new_handler(DownHandler, Down,
		    block(message(DragHandler, active, @on),
			  message(@event_window,
				  saved_cursor, @event_relative))),
	new_handler(UpHandler, Up,
		    message(DragHandler, active, @off)),

	new(Group, handler_group(DragHandler, UpHandler, DownHandler)).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			ATTACH/DETACH HANDLERS

attach_handlers(+Object, +HandlerTypes)
detach_handlers(+Object, +HandlerTypes)
    Attaches (detaches) event handlers defined by HandlerTypes to Object.  
    The elements in HandlerTypes are defined by the argument to 
    make_handlers/1.  For example:

	attach_handlers(@b, [stretch-left, move-middle]).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

attach_handlers(_, []) :- !.
attach_handlers(Object, [H|T]) :- !,
       attach_handlers(Object, H),
       attach_handlers(Object, T).
attach_handlers(Object, Type) :-
       find_handler(Type, Handler),
       send(Object, recogniser, Handler).
       
detach_handlers(_, []) :- !.
detach_handlers(Object, [H|T]) :- !,
       detach_handlers(Object, H),
       detach_handlers(Object, T).
detach_handlers(Object, Type) :-
       find_handler(Type, Handler),
       send(Object, delete_recogniser, Handler).
       
find_handler(Type, Handler) :-
       es_handlers(Type, Handler), !.
find_handler(Type, Handler) :-
       make_handlers(Type),
       es_handlers(Type, Handler).


		/********************************
		*            UTILITIES		*
		********************************/

assert_handlers(Type, Handlers) :-
	asserta(es_handlers(Type, Handlers)).

%   new_handler(?@Handler, +EventType, +Message)
%   new_handler(?@Handler, +EventType, +Message, +Region)
%
%   Creates a PCE Handler object which handles events of type EventType 
%   and sends out Message.  The optional Region determines where the event 
%   is valid (default is the entire area of the graphical).

new_handler(Handler, EventType, Message) :-
	new_handler(Handler, EventType, Message, @default).

new_handler(Handler, EventType, Message, Region) :-
	new(Handler, handler(EventType, Message, Region)).
