% File:		event_speak.pl
% Module:	event_speak
% Where:	PCE-4 Library
% Purpose:	Define standard handlers for graphical objects
% Author:	Anjo Anjewierden, Jan Wielemaker
% Notice:	Copyright (c) 1988, University of Amsterdam
% Works with:	SWI-Prolog 1.5, X-PCE-4.0
% History:	29/03/88 (AA; Created)
%		08/07/88 (JW; Adapted for PCE 3.1)
%		11/08/88 (AA; Moduled)
%		28/09/88 (AA; Adapted for PCE-3.3)
%		04/01/91 (JW; Adapted for X-PCE-4.0)


:- module(event_speak, 
	[ attach_handlers/2	% +@Object, +EventCodes
	, detach_handlers/2	% +@Object, +EventCodes
	, make_handlers/1	% +Type
	, make_event_obtainers/0% Create event obtainers
	]).


:- format('>>>> NOTE: the library module event_speak is not to be used
>>>> with new code.  It will be removed from the distribution shortly!!~n').

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
BACKWARD COMPATIBILITY ONLY.  DO NOT RELY ON THIS MODULE, IT IS BOUND
TO BE REPLACED.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			      INTRODUCTION

This  file deals  with  generic event  handlers defined on   graphical
objects.  Currently these event handlers support:

- Stretching a graphical at any of the corners (Code=stretch-Button)
- Moving a graphical with a mouse-button drag (Code=move-Button)

In addition  the programmer can  define an association  between a Code
and a list of events.

make_handlers/[1,2] should be called once for all  event handlers used
in the   application.    attach_handlers/2  is  used to   attach event
handlers to a particular graphical object.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- dynamic
	es_handlers/2.

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			    STANDARD OBTAINERS

make_event_obtainers/0

Defines the following obtainers for standard Handlers:

	@event_receiver		receiver of the event
	@event_id		id of event (= ASCII value of ascii event)
	@event_position		position of the event relative to window
	@event_position_device	position of the event relative to device
	@event_shift_down	boolean: shift button was down at event
	@event_shift_up		boolean: shift button was up at event
	@event_device		device of receiver
	@event_window		window of the event
	@event_focus		focus of the event's receiver's device
	@event_relative		relative position of the event to the area
				of the receiver
	@event_saved_cursor	saved_cursor attribute of associated window
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

make_event_obtainers :-
	object(@event_receiver), !.
make_event_obtainers :-
	new(@event_id, @arg1?id), 
	new(@event_receiver, @arg1?receiver), 
	new(@event_device, @event_receiver?device),
	new(@event_window, @arg1?window),
	new(@event_position, ?(@arg1, position, @event_window)), 
	new(@event_position_device, ?(@arg1, position, @event_device)), 
	new(@event_focus, @event_window?focus), 
	new(@event_relative, ?(@arg1, area_position, @event_receiver)),
	new(@event_saved_cursor, @event_window?saved_cursor),
        new(@event_difference_device,
	    ?(@event_position_device, difference, @event_saved_cursor)),
        new(@event_difference,
	    ?(@event_position, difference, @event_saved_cursor)),
        new(@event_window_focus, @event_window?focus),
        new(@event_position_receiver, @arg1?position).


%   +mouse_event_type(Button-Action, EventType)
%
%   EventType is the PCE name for the event generated when Action
%   (drag, up, down) appears on Button (left, middle, right).

mouse_event_type(left-drag,   ms_left_drag).
mouse_event_type(middle-drag, ms_middle_drag).
mouse_event_type(right-drag,  ms_right_drag).
mouse_event_type(left-up,     ms_left_up).
mouse_event_type(middle-up,   ms_middle_up).
mouse_event_type(right-up,    ms_right_up).
mouse_event_type(left-down,   ms_left_down).
mouse_event_type(middle-down, ms_middle_down).
mouse_event_type(right-down,  ms_right_down).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			     CREATING HANDLERS
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

make_handlers(Type) :-
	make_event_obtainers,
	handler_name(Type, Name),
	do_make_handlers(Type, Handlers),
	send(Handlers, name_reference, Name),
	assert_handlers(Type, Handlers).

handler_name(A-B, Name) :- !,
	concat_atom([A, '_', B], Name).
handler_name(Atom, Atom) :-
	atomic(Atom).

:- discontiguous
	do_make_handlers/2.

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			    STRETCHING GRAPHICALS

stretch-Button
    Allows the user to stretch (resize) a graphical by dragging a corner.
    The display is updated on each stretch event.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

do_make_handlers(stretch-_Button, _Group) :- !, 
/*	mouse_event_type(Button-down, Down), 
	mouse_event_type(Button-drag, Drag), 
	mouse_event_type(Button-up, Up), !, 
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
