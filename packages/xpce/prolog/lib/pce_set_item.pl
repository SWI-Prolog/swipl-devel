/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1997 University of Amsterdam. All rights reserved.
*/

:- module(pce_seteditor, []).
:- use_module(library(pce)).
:- require([ send_list/3
	   ]).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Class  set_item  defines a  compound  dialog   object,  consisting  of a
browser for a list of object with  a user-supplied item below it.  Right
of the browser is a `remove' button, and right of the user-supplied item
is the `Add' button.

All normal dialog-item behaviour is implemented:

	<->selection:	set the selection from a chain of values
	->clear:	remove all items
	<->modified:	set/query modified status
	->apply:	activate message
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


:- pce_begin_class(set_item, dialog_group,
		   "Edit a set of values").


variable(modified,	bool := @off,	both, "Modified status").
variable(message,	code*,		both, "Associated message").

initialise(SE, One:graphical, Label:[name], Msg:[code]*) :->
	"Create from an editor for a single value"::
	send(SE, send_super, initialise, Label, box),
	default(Msg, @nil, TheMsg),
	send(SE, slot, message, TheMsg),
	send(SE, append, new(LB, list_browser)),
	send(SE, append, new(RM, button(remove)), right),
	send(LB, multiple_selection, @on),
	send(LB, attribute, hor_stretch, 100),
	send(LB, select_message,
	     if(message(LB?selection, empty),
		message(RM, active, @off),
		message(RM, active, @on))),
	send(LB, open_message,
	     if(LB?selection?size == 1,
		message(One, selection, LB?selection?head?key))),
	send(SE, append, One, next_row),
	(   send(One, has_send_method, show_label)
	->  send(One, show_label, @off)
	;   true
	),
	send(One, name, one),
	send(SE, append, new(Add, button(add)), right),
	send(RM, reference, point(0,0)),
	send_list([RM, Add], active, @off),
	send_list([LB, One], alignment, column),
	send_list([RM, Add], alignment, right),
	(   get(One, hor_stretch, HS)
	->  send(SE, attribute, hor_stretch, HS)
	;   true
	).

size(SE, Size:size) :->
	send(SE, send_super, size, Size),
	send(SE, layout_dialog).


modified_item(SE, Item:graphical, Modified:bool) :->
	get(SE, member, one, Item),
	Modified == @on,
	get(SE, member, add, Add),
	send(Add, active, @on).

:- pce_group(apply).

apply(SE, Always:[bool]) :->
	"Forward <-selection over <-message"::
	(   (   Always == @on
	    ;	get(SE, modified, @on)
	    ),
	    get(SE, message, Msg),
	    Msg \== @nil
	->  get(SE, selection, Value),
	    ignore(send(Msg, forward, Value))
	;   true
	).

	
:- pce_group(edit).

remove(SE) :->
	"Remove selected item(s)"::
	get(SE, member, list_browser, LB),
	get(LB, selection, Chain),
	(   send(Chain, empty)
	->  true
	;   send(Chain, for_all, message(LB, delete, @arg1)),
	    get(SE, member, remove, Remove),
	    send(Remove, active, off),
	    send(SE, modified, @on)
	).

add(SE) :->
	"Add selection of single item to browser"::
	get(SE, member, one, One),
	get(One, selection, Selection),
	get(SE, member, list_browser, LB),
	send(LB, append, Selection),
	send(One, clear),
	send(One, modified, @off),
	get(SE, member, add, Add),
	send(Add, active, @off),
	send(SE, modified, @on).

:- pce_group(selection).

selection(SE, Values:chain) :->
	"Set the selection"::
	get(SE, member, list_browser, LB),
	send(LB, members, Values),
	send(SE, slot, modified, @off).
selection(SE, Values:chain) :<-
	"Fetch the selection"::
	get(SE, member, list_browser, LB),
	get(LB, members, DictItems),
	get(DictItems, map, @arg1?key, Values),
	send(SE, slot, modified, @off).

clear(SE) :->
	"Remove all members from the set"::
	send(SE, selection, new(chain)).

:- pce_end_class.

/*
test :-
	new(D, dialog('Set Editor test')),
	new(SE, set_item(new(text_item), names)),
	send(D, append, SE),
	send(D, open).
*/
