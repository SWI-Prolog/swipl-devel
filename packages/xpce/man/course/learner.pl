/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/

:- discontiguous
	send_event_text_box/2.

:- pce_begin_class(text_box, device, "Box with centered text").

resource(size,		size,	'100x50',		"Default size").
resource(label_font,	font,	'@helvetica_bold_12',	"Font for text").

initialise(TB, Label:[name]) :->
	"Create a box with centered editable text"::
	send(TB, send_super, initialise),
	get(TB, resource_value, size, size(W, H)),
	get(TB, resource_value, label_font, Font),
	send(TB, display, box(W, H)),
	send(TB, display, text(Label, center, Font)),
	send(TB, recenter).


recenter(TB) :->
	"Center text in box"::
	get(TB, member, box, Box),
	get(TB, member, text, Text),
	send(Text, center, Box?center).


geometry(TB, X:[int], Y:[int], W:[int], H:[int]) :->
	"Handle geometry-changes"::
	get(TB, member, box, Box),
	send(Box, set, 0, 0, W, H),
	send(TB, recenter),
	send(TB, send_super, geometry, X, Y).


fill_pattern(TB, Pattern:image*) :->
	"Specify fill-pattern of the box"::
	get(TB, member, box, Box),
	send(Box, fill_pattern, Pattern).
fill_pattern(TB, Pattern:image*) :<-
	"Read fill-pattern of the box"::
	get(TB, member, box, Box),
	get(Box, fill_pattern, Pattern).


string(TB, String:char_array) :->
	"Specify string of the text"::
	get(TB, member, text, Text),
	send(Text, string, String).
string(TB, String:char_array) :<-
	"Read string of the text"::
	get(TB, member, text, Text),
	get(Text, string, String).

:- pce_end_class.


		 /*******************************
		 *	   GRAPH EDITOR		*
		 *******************************/

:- use_module(library(prompt)).

make_graph_editor(E) :-
	new(E, picture('Graph Editor')),
	send(E, popup, new(P, popup(options))),
	send_list(P, append, 
		  [ menu_item(add_new_box,
			      message(@prolog, add_new_box,
				      E, E?focus_event?position)),
		    menu_item(clear,
			      and(message(@display, confirm,
					  'Clear entire drawing?'),
				  message(E, clear)))
		  ]),
	send(E, open).


add_new_box(E, Pos) :-
	prompt('Name of box',
	       [ name:name = Name
	       ]),
	send(E, display, text_box(Name), Pos).


		 /*******************************
		 *	      EDITING		*
		 *******************************/

:- pce_extend_class(text_box).

:- pce_global(@move_resize_recogniser,
	      make_move_resize_recogniser).

make_move_resize_recogniser(R) :-
	new(R, handler_group(new(resize_gesture),
			     new(move_gesture),
			     popup_gesture(new(P, popup(options))))),
	TB = @arg1,
	send_list(P, append,
		  [ menu_item(rename,
			      message(TB, rename),
			      end_group := @on),
		    menu_item(delete,
			      message(TB, free))
		  ]).
	    

event(TB, Ev:event) :->
	(   send(TB, send_super, event, Ev)
	->  true
	;   send(@move_resize_recogniser, event, Ev)
	).


rename(TB) :->
	"Prompt for new name"::
	prompt('Rename text box',
	       [ name:name = Name/TB?string
	       ]),
	send(TB, string, Name).

:- pce_end_class.


		 /*******************************
		 *	     LINKING		*
		 *******************************/


:- pce_extend_class(text_box).

handle(0, h/2,	link,	west).
handle(w, h/2,	link,	east).
handle(w/2, 0,	link,	north).
handle(w/2, h,	link,	sourth).

:- pce_global(@link_recogniser, new(connect_gesture)).

event(TB, Ev:event) :->
	(   send(TB, send_super, event, Ev)
	;   send(@move_resize_recogniser, event, Ev)
	;   send(@link_recogniser, event, Ev)
	).

:- pce_end_class.


		 /*******************************
		 *	  DUMP TO PROLOG	*
		 *******************************/

assert_graph(E, Predicate) :-
	functor(Term, Predicate, 2),
	retractall(Term),
	send(E?graphicals, for_all,
	     if(message(@arg1, instance_of, connection),
		send(@prolog, assert_link,
		     Predicate,
		     @arg1?from?string?value,
		     @arg2?from?string?value))).

assert_link(Predicate, From, To) :-
	Term =.. [Predicate, From, To],
	assert(Term).
	     
