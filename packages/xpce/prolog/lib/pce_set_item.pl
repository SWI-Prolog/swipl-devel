/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1997 University of Amsterdam. All rights reserved.
*/

:- module(pce_set_item, []).
:- use_module(library(pce)).
:- require([ default/3
	   , ignore/1
	   , send_list/3
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

Set rendering
-------------

By default, the set  it  represented   by  a  list_browser object, which
implies <-print_name is  used  to  render   the  member  to  its textual
representation.

It is possible to provide an alternative   browser for the set using the
browser argument.  This browser should understand the methods

	->members	Define current set of members
	<-members	Get current set of members
	->append	Add a member
	->delete	Delete a member
	
See class list_browser for intended  behaviour   of  these methods.  The
library(pce_graphical_browser) provides a graphical browser  that can be
used with this class.  See the  test_arrows/0   demo/test  at the end of
this file.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


:- pce_begin_class(set_item, dialog_group,
		   "Edit a set of values").


variable(modified,	bool := @off,	both, "Modified status").
variable(message,	code*,		both, "Associated message").
variable(browser,	graphical,	get,  "Browser for the set").

initialise(SE, One:item=graphical, Label:name=[name], Msg:message=[code]*,
	   LB0:browser=[graphical]) :->
	"Create from an editor for a single value"::
	send(SE, send_super, initialise, Label, box),
	default(Msg, @nil, TheMsg),
	(   LB0 == @default
	->  new(LB, list_browser)
	;   LB = LB0
	),
	send(SE, slot, message, TheMsg),
	send(SE, append, LB),
	send(SE, slot, browser, LB),	% hack, may be window (decorator)
	send(SE, append, new(RM, button(remove)), right),
	send(LB, multiple_selection, @on),
	make_browser_stretchable(LB, 100),
	send(LB, select_message,
	     if(message(LB?selection, empty),
		message(RM, active, @off),
		message(RM, active, @on))),
	send(LB, open_message,
	     if(LB?selection?size == 1,
		message(SE, edit_selection, LB?selection?head))),
	send(SE, append, One, next_row),
	(   send(One, has_send_method, show_label)
	->  send(One, show_label, @off)
	;   true
	),
	send(One, name, one),
	send(SE, append, new(Add, button(add)), right),
	send(RM, reference, point(0,0)),
	send(RM, active, @off),
	send_list([LB, One], alignment, column),
	send_list([RM, Add], alignment, right),
	(   get(One, hor_stretch, HS)
	->  send(SE, attribute, hor_stretch, HS)
	;   true
	).

make_browser_stretchable(LB, Stretch) :-
	send(LB, instance_of, window),
	get(LB, decoration, Decoration),
	Decoration \== @nil, !,
	send(Decoration, attribute, hor_stretch, Stretch).
make_browser_stretchable(LB, Stretch) :-
	send(LB, attribute, hor_stretch, Stretch).

size(SE, Size:size) :->
	send(SE, send_super, size, Size),
	send(SE, layout_dialog).


modified_item(SE, Item:graphical, Modified:bool) :->
	Modified == @on,
	get(SE, member, one, Item),
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
	get(SE, browser, LB),
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
	get(SE, browser, LB),
	send(LB, append, Selection),
	send(One, clear),
	send(One, modified, @off),
	get(SE, member, add, Add),
	send(Add, active, @off),
	send(SE, modified, @on).

edit_selection(SE, Sel:any) :->
	"Edit object opened in browser"::
	(   get(SE, browser, Browser),
	    send(Browser, instance_of, list_browser),
	    get(Sel, key, Value)
	;   Value = Sel
	),
	get(SE, member, one, One),
	send(One, selection, Value).
	

:- pce_group(selection).

selection(SE, Values:chain) :->
	"Set the selection"::
	get(SE, browser, LB),
	send(LB, members, Values),
	send(SE, slot, modified, @off).
selection(SE, Values:chain) :<-
	"Fetch the selection"::
	get(SE, browser, LB),
	(   send(LB, instance_of, list_browser)
	->  get(LB, members, DictItems),
	    get(DictItems, map, @arg1?key, Values)
	;   get(LB, members, Values)
	),
	send(SE, slot, modified, @off).

clear(SE) :->
	"Remove all members from the set"::
	send(SE, selection, new(chain)).

:- pce_end_class.

test_arrows :-
	pce_autoload(arrow_item, library(pce_arrow_item)),
	ensure_loaded(library(pce_graphical_browser)),
	new(LB, graphical_browser(@default, @default,
				  ?(@prolog, make_arrow_line, @arg1),
				  @arg1?second_arrow)),
	send(LB, single_column, @on),
	new(D, dialog('Set Editor test')),
	new(SE, set_item(new(arrow_item), arrows, browser:=LB)),
	send(D, append, SE),
	send(D, open).

make_arrow_line(Arrow, Line) :-
	new(Line, line(0, 0, 50, 0)),
	send(Line, second_arrow, Arrow).
				       

