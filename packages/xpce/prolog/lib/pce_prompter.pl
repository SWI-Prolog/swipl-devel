/*  prompt.pl,v 1.0.0.1 1992/07/27 09:17:31 toussain Exp

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

:- module(pce_prompter,
	  [ prompter/2
	  ]).
:- use_module(library(pce)).
:- require([ checklist/2
	   ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module defines a standard prompter-box for PCE applications.  It is
invoked with:

    prompter(+Tile, +ListOfAttributes)

where each attribute is a term of the form

   +Label:+Type = -Value[/+Default]

Examples:

?- prompter('Create class',
	    [ name:name = Name
	    , super:name = Super
	    ]).

Last updated:	Wed Sep 13 1995 by Jan Wielemaker
		- Added menu/browser for multiple values.
		- Added automatic stretching of dialog items on resize
		- Improved type and error handling

NOTE:	Package is under development.  Needs support for more types;
	optional/obligatory fields and better error-messages.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_global(@prompter, make_promper).

make_promper(P) :-
	new(P, dialog),
	send(P, resize_message, message(@prolog, stretch_items, P)).


prompter(Title, Attributes) :-
	maplist(canonise_attribute, Attributes, CAtts),
	send(@prompter, clear),
	send(@prompter, append, label(reporter)),
	checklist(append_prompter(@prompter), CAtts),
	send(@prompter, append,
	     new(Ok, button(ok, message(@prompter, return, ok))), next_row),
	send(Ok, default_button, @on),
	send(@prompter, append,
	     button(cancel, message(@prompter, return, cancel))),
	send(@prompter?frame, label, Title),
	send(@prompter, fit),
	stretch_items(@prompter),
	(   send(@event, instance_of, event)
	->  get(@event, position, @display, Pos)
	;   Pos = @default
	),
	repeat,
	    (	get(@prompter, confirm_centered, Pos, OK)
	    ;	get(@prompter, confirm, OK) % second time
	    ),
	    (   OK == ok
	    ->  checklist(read_prompter(@prompter), CAtts), !,
	        send(@prompter, show, @off)
	    ;   !,
		send(@prompter, show, @off),
		fail
	    ).

canonise_attribute(Label:Type = Value, Label:PceType = Value) :-
	pce_type(Type, PceType).

pce_type(Term, Type) :-
	term_to_atom(Term, A0),
	atom_chars(A0, S0),
	delete(S0, 0' , S1),
	atom_chars(Type, S1).


		 /*******************************
		 *   STRETCH ITEMS TO THE RIGHT *
		 *******************************/

stretch_items(Dialog) :-
	send(Dialog?graphicals, for_all,
	     message(@prolog, stretch_item, @arg1)).

stretchable(text_item).
stretchable(list_browser).

stretch_item(Item) :-
	get(Item, class_name, ClassName),
	stretchable(ClassName),
	\+ (get(Item, right, RI), RI \== @nil), !,
	get(Item?device?visible, right_side, Right),
	get(Item?device?gap, width, W),
	R is Right - W,
	get(Item, left_side, L),
	Width is R - L,
	send(Item, do_set, width := Width).
stretch_item(_).


		/********************************
		*      CREATE DIALOG ITEMS	*
		********************************/

append_prompter(P, Label:Type = Value) :-
	make_dialog_item(Type, Label, DI),
	set_default(Value, DI),
	send(P, append, DI).

						  % TBD: specialised types
make_dialog_item(Type, Label, DI) :-
	get(@pce, convert, Type, type, PceType),
	get(PceType, value_set, Set), !,
	get(Set, size, Size),
	(   Size < 6
	->  new(DI, menu(Label, choice))
	;   new(DI, list_browser(@default, 10, 6)),
	    send(DI, label, Label),
	    send(DI, name, Label)
	),
	send(Set, for_all, message(DI, append, @arg1)).
make_dialog_item(Type, Label, DI) :-
	new(DI, text_item(Label, '')),
	send(DI, type, Type).


		/********************************
		*          SET DEFAULTS		*
		********************************/

set_default(Value, DI) :-
	nonvar(Value),
	Value = _RVal/Default, !,
	send(DI, selection, Default).
set_default(_, _).

		/********************************
		*           READ VALUES		*
		********************************/

read_prompter(P, Label:Type = Value) :-
	get(P, member, Label, DI),
	(   get(DI, selection, V0)
	->  canonise(DI, V0, V1),
	    (   get(@pce, convert, V1, Type, Val)
	    ->  (   nonvar(Value),
		    Value = RVal/_
		->  RVal = Val
		;   Value = Val
		)
	    ;   send(DI, report, warning, 'Invalid value for %s', Label),
		fail
	    )
	;   send(DI, report, warning, 'No selection for %s', Label),
	    fail
	).
	

canonise(DI, A, B) :-
	send(DI, instance_of, text_item), !,
	new(S, string(A)),
	send(S, strip),
	get(S, value, B),
	send(S, done).
canonise(DI, A, B) :-
	send(DI, instance_of, list_browser), !,
	get(A, key, B).
canonise(_, Val, Val).				  % TBD
