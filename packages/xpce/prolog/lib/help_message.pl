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

:- module(pce_help_messages, []).
:- use_module(library(pce)).

:- pce_global(@help_message_window, new(help_message_window)).

:- pce_begin_class(help_message_window, dialog,
		   "Window to display <-help_message").

class_variable(background, colour, burlywood1, "Ballon background").

variable(handler,	handler,	get, "Handler for intercept").
variable(message,	string*,	get, "Currently displayed message").

initialise(W) :->
	send(W, slot, handler,
	     handler(mouse, message(W, try_hide, @event))),
	send_super(W, initialise),
	send(W, kind, popup),
	send(W?frame, border, 0),
	send(W?frame?tile, border, 0),
	send(W, gap, size(5, 2)),
	send(W, pen, 0),
	send(W, append, new(L, label(feedback, '', normal))),
	send(L, length, 0),
	send(W?frame, create).

owner(W, Owner:[any]*) :->
	"Maintain hyperlink to the owner"::
	(   Owner == @nil
	->  send(W, delete_hypers, help_balloon)
	;   Owner == @default
	->  true			% no change
	;   new(_, help_hyper(Owner, W, help_baloon, owner))
	).
owner(W, Owner:any) :<-
	get(W, hypered, owner, Owner).
		

try_hide(W, Ev:event) :->
	get(W, owner, Owner),
	(   (   send(Ev, is_a, loc_move)
	    ;	send(Ev, is_a, loc_still)
	    )
	->  %send(@pce, format, '%O: Move/still event\n', Owner),
	    get(W, message, OldMsg),
	    (   get(Owner, help_message, tag, Ev, Msg)
	    ->	%send(@pce, format, '%O: yielding %s\n', Owner, Msg),
	        (   OldMsg \== @nil,
		    send(Msg, equal, OldMsg)
		->  (   send(Ev, is_a, loc_still)
		    ->	send(W, adjust_position, Ev)
		    ;	true
		    )
		;   send(W, feedback, Msg, Ev)
		)
	    ;	(   get(W, message, @nil)
		->  true
		;   send(W, feedback, @nil, Ev)
		)
	    )
	;   send(W, owner, @nil),
	    send(W, hide),
	    fail			% normal event-processing
	).


hide(W) :->
	"Remove from the display"::
	send(W, show, @off),
	get(W, handler, H),
	send(W?display?inspect_handlers, delete, H).


feedback(W, S:string*, Ev:event, For:[any]*) :->
	"Display window holding string and grab pointer"::
	send(W, owner, For),
	send(W, slot, message, S),
	(   S == @nil
	->  send(W, show, @off)
	;   get(W, member, feedback, L),
	    send(L, selection, S),
	    send(W, layout),
	    send(W?frame, fit),
	    send(W, adjust_position, Ev),
	    send(W?display, inspect_handler, W?handler)
	).


adjust_position(W, Ev:event) :->
	"Fix the position of the feedback window"::
	get(Ev, position, W?display, P),
	get(P, plus, point(5,5), point(FX, FY)),
	send(W?frame, set, FX, FY),
	send(W?frame, expose).

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
	find_help_message(Gr, What, Ev, Owner, Msg),
	send(@help_message_window, feedback, Msg, Ev, Owner).


find_help_message(Gr, What, Ev, Gr, Msg) :-
	get(Gr, help_message, What, Ev, Msg), !.
find_help_message(Gr, What, Ev, Owner, Msg) :-
	get(Gr, contained_in, Container),
	find_help_message(Container, What, Ev, Owner, Msg).

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


:- pce_begin_class(help_hyper, hyper,
		   "Hyper between help-balloon and ownder").

unlink_from(H) :->
	"->hide the <-to part"::
	get(H, to, Part),
	(   object(Part)
	->  send(Part, hide)
	;   free(Part)
	),
	free(H).

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
