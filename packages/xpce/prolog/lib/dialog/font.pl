/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

:- module(pce_font_item, []).
:- use_module(library(pce)).
:- require([ send_list/2
	   ]).

:- pce_begin_class(font_item, device).

variable(message,	code*,		both,	"Message executed on change").
variable(default,	'font|function',get,	"Default value").
variable(value_set,	chain,		get,	"List of fonts").

initialise(FI, Name:[name],
	   Default:'[font|function]', Message:[code]*,
	   ValueSet:[chain]) :->
	"Create font-selector"::
	default(Name, font, Nm),
	default(Message, @nil, Msg),
	default(Default, font(screen, roman, 13), Def),
	send(FI, slot, message, Msg),
	send(FI, slot, default, Def),
	send(FI, send_super, initialise),
	send(FI, name, Nm),
	send(FI, alignment, column),
	send(FI, append_dialog_item, new(Fam, menu(family, cycle))),
	send(FI, append_dialog_item, new(Wgt, menu(weight, cycle)), right),
	send(FI, append_dialog_item, new(Pts, menu(points, cycle)), right),
	send(Fam, label, ?(Fam, label_name, Nm)),
	send(Wgt, label, ''),
	send(Pts, label, ''),
	send(FI, value_set, ValueSet),
	send(FI, default, Def),
	send(FI, layout_dialog, size(0, 0)).


label(FI, Label:name) :->
	"Set the label"::
	get(FI, member, family, Fam),
	send(Fam, label, Label),
	send(FI, layout_dialog, size(0, 0)).

label_width(FI, W:int) :<-
	get(FI, member, family, Fam),
	get(Fam, label_width, W).
label_width(FI, W:int) :->
	get(FI, member, family, Fam),
	get(Fam, label_width, O),
	send(Fam, label_width, W),
	get(FI, member, weight, Wgt),
	get(FI, member, points, Pts),
	send(Wgt, relative_move, point(W-O, 0)),
	send(Pts, relative_move, point(W-O, 0)).


value_set(FI, ValueSet:[chain]) :->
	"Define set of available fonts"::
	(   ValueSet == @default
	->  new(Set, chain),
	    send(@fonts, for_all, message(Set, append, @arg2))
	;   Set = ValueSet
	),
	send(FI, slot, value_set, Set),
	get(FI, member, family, Fam),
	get(FI, member, weight, Wgt),
	get(FI, member, points, Pts),
	send_list([Fam, Wgt, Pts], clear),
	send(Set, for_all,
	     and(if(not(?(Fam, member, @arg1?family)),
		    message(Fam, append, @arg1?family)),
		 if(not(?(Wgt, member, @arg1?style)),
		    message(Wgt, append, @arg1?style)),
		 if(not(?(Pts, member, @arg1?points)),
		    message(Pts, append, @arg1?points)))),
	send(Fam, sort),
	send(Wgt, sort),
	send(Pts, sort, ?(@arg1?value, compare, @arg2?value)).


		 /*******************************
		 *	GENERIC DIALOG-ITEM	*
		 *******************************/

selection(FI, Font:font) :->
	"Set the selection"::
	get(FI, member, family, Fam),
	get(FI, member, weight, Wgt),
	get(FI, member, points, Pts),
	send(Fam, selection, Font?family),
	send(Wgt, selection, Font?style),
	send(Pts, selection, Font?points).

selection(FI, Font:font) :<-
	"Get the current font"::
	get(FI, member, family, Fam),
	get(FI, member, weight, Wgt),
	get(FI, member, points, Pts),
	get(Fam, selection, Family),
	get(Wgt, selection, Style),
	get(Pts, selection, Points),
	new(Font, font(Family, Style, Points)).


default(FI, Def:'font|function') :->
	send(FI, slot, default, Def),
	send(FI, restore).


restore(FI) :->
	get(FI, default, Def),
	send(FI, selection, Def).


apply(FI, _Always:[bool]) :->
	get(FI, message, Msg),
	(   Msg \== @nil
	->  send(Msg, forward, FI?selection)
	;   true
	).

:- pce_end_class.
