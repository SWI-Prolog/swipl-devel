/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

:- module(pce_help_messages, []).
:- use_module(library(pce)).

:- pce_global(@help_message_window, new(help_message_window)).

:- pce_begin_class(help_message_window, dialog,
		   "Window to display <-help_message").

variable(handler,	handler,	get, "Handler for intercept").
variable(message,	string*,	get, "Currently displayed message").

initialise(W) :->
	send(W, slot, handler,
	     handler(mouse, message(W, hide, @receiver, @event))),
	send(W, send_super, initialise),
	send(W, kind, popup),
	send(W, background, burlywood1),
	send(W?frame, border, 0),
	send(W?frame?tile, border, 0),
	send(W, gap, size(5, 2)),
	send(W, pen, 0),
	send(W, append, new(L, label(feedback, '', normal))),
	send(L, length, 0),
	send(W?frame, create).


hide(W, Gr:any, Ev:event) :->
	(   (   send(Ev, is_a, loc_move)
	    ;	send(Ev, is_a, loc_still)
	    )
	->  get(W, message, OldMsg),
	    (   get(Gr, help_message, tag, Ev, Msg)
	    ->	(   OldMsg \== @nil,
		    send(Msg, equal, OldMsg)
		->  true
		;   send(W, feedback, Msg, Ev)
		)
	    ;	(   get(W, message, @nil)
		->  true
		;   send(W, feedback, @nil, Ev)
		)
	    )
	;   send(W, show, @off),
	    get(W, handler, H),
	    send(W?display?inspect_handlers, delete, H),
	    fail			% normal event-processing
	).


feedback(W, S:string*, Ev:event) :->
	"Display window holding string and grab pointer"::
	send(W, slot, message, S),
	(   S == @nil
	->  send(W, show, @off)
	;   get(W, member, feedback, L),
	    send(L, selection, S),
	    send(W, layout),
	    send(W?frame, fit),
	    get(Ev, position, W?display, P),
	    get(P, plus, point(5,5), point(FX, FY)),
	    send(W?frame, set, FX, FY),
	    send(W?frame, open),
	    send(W?frame, expose),
	    send(W?display, inspect_handler, W?handler)
	).

:- pce_end_class.


attribute_name(tag,	help_tag).
attribute_name(summary,	help_summary).

:- pce_extend_class(visual).

help_message(Gr, What:{tag,summary}, Msg:string*) :->
	"Associate a help message"::
	attribute_name(What, AttName),
	(   Msg == @nil
	->  send(Gr, delete_attribute, AttName)
	;   send(Gr, attribute, AttName, Msg)
	).
help_message(V, What:{tag,summary}, _Ev:[event], Msg:string) :<-
	attribute_name(What, AttName),
	get(V, attribute, AttName, Msg).

:- pce_end_class.


:- pce_extend_class(graphical).

help(Gr, What:name, Ev:event) :->
	find_help_message(Gr, What, Ev, Msg),
	send(@help_message_window, feedback, Msg, Ev).


find_help_message(Gr, What, Ev, Msg) :-
	get(Gr, help_message, What, Ev, Msg), !.
find_help_message(Gr, What, Ev, Msg) :-
	get(Gr, contained_in, Container),
	find_help_message(Container, What, Ev, Msg).

:- pce_end_class.


:- pce_extend_class(menu).

help_message(Gr, What:{tag,summary}, Ev:[event], Msg:string) :<-
	"Fetch associated help message"::
	(   get(Gr, item_from_event, Ev, Item),
	    get(Item, help_message, What, Msg)
	->  true
	;   get(Gr, get_super, help_message, What, Ev, Msg)
	).

:- pce_end_class.


		 /*******************************
		 *	     REGISTER		*
		 *******************************/

register_help_message_window :-
	send(@display, inspect_handler,
	     handler(loc_still, message(@receiver, help, tag, @event))),
	send(@display, inspect_handler,
	     handler(help, message(@receiver, help, summary, @event))).

:- initialization
   register_help_message_window.
