/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

:- module(man_object_browser, []).

:- use_module(library(pce)).
:- use_module(util).
:- require([ concat/3
	   , default/3
	   , send_list/3
	   ]).

:- pce_begin_class(man_object_browser, man_frame,
		   "Gobal object browser").

variable(objects,	man_module,		get).

initialise(OB, Manual:man_manual) :->
	"Create from Manual"::
	send(OB, send_super, initialise, Manual, 'Object Browser'),

	get(Manual, module, objects, @on, Module),
	send(OB, slot, objects, Module),
	
	new(B, man_summary_browser(man_summary, size(70, 15))),
	send(B?image, tab_stops, vector(20, 200)),
	send(B, name, browser),
	dialog(Dialog),

	send(OB, append, B),
	send(Dialog, below, B),
	send(OB, fill, ''),
	
	send(OB, open).


		/********************************
		*            DIALOG		*
		********************************/

dialog(D) :-
	new(D, dialog),
	new(OB, D?frame),
	send(D, append, new(A, menu(show, marked, @nil))),
	send(A, layout, horizontal),
	send_list(A, append, [documented, all]),
	send(D, append, new(SS, text_item(search, regex(''),
					  message(D?apply_member, execute))),
	     right),
	send(SS, length, 15),
	send(D, append, button(apply, message(OB, fill,
					      SS?selection, A?selection))),

	send(D, append, button(help,  message(OB, help))),
	send(D, append, button(quit,  message(OB, quit))).


		/********************************
		*             FILL		*
		********************************/

fill(OB, Pattern:regex, What:[name]) :->
	"Fill with all global objects matching pattern"::
	default(What, documented, Show),
	get(OB, member, browser, B),
	send(B, clear),
	new(Chain, chain),
	(   Show == documented
	->  get(@manual, module, objects, @on, ObjModule),
	    send(ObjModule?id_table, for_some,
		 message(@prolog, append_card, Chain, Pattern, @arg2))
	;   send(@pce, for_name_reference,
		 message(@prolog, append_object, Chain, Pattern, @arg1))
	),
	get(Chain, size, S),
	send(OB, report, progress, 'Sorting %d objects ...', S),
	send(Chain, sort, ?(@arg1?reference, compare, @arg2?reference)),
	send(OB, report, done),
	send(B, members, Chain).


append_object(Chain, Pattern, Ref) :-
	new(G, man_global(Ref)),
	(   get(G, man_summary, Summary),
	    send(Pattern, search, Summary)
	->  send(Chain, append, G)
	;   true
	).


append_card(Chain, Pattern, Card) :-
	get(Card, identifier, Id),
	concat('O.', Name, Id),
	get(Card, summary, S0),
	(   S0 == @nil
	->  S1 = @default
	;   S1 = S0
	),
	new(G, man_global(Name, S1)),
	(   get(G, man_summary, Summary),
	    send(Pattern, search, Summary)
	->  send(Chain, append, G)
	;   true
	).

		/********************************
		*          COMMUNICATION	*
		********************************/

selected(OB, Obj:object*) :->
	"Set the selection"::
	get(OB, member, browser, B),
	send(B, selected, Obj).


release_selection(OB) :->
	"Clear the selection"::
	get(OB, member, browser, B),
	send(B, release_selection).

:- pce_end_class.

