/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

:- module(man_error_browser, []).

:- use_module(library(pce)).
:- use_module(util).
:- require([ ignore/1
	   ]).

:- pce_begin_class(man_error_browser, man_frame,
		   "Error browser").

variable(errors,	man_module,		get).
variable(current,	error*,			get).

initialise(EB, Manual:man_manual) :->
	"Create from Manual"::
	send(EB, send_super, initialise, Manual, 'Error Browser'),

	get(Manual, module, errors, @on, Module),
	send(EB, slot, errors, Module),
	
	new(B, man_summary_browser(man_summary, size(100, 15))),
	send(B?image, tab_stops, vector(15, 220, 280)),
	send(B, name, browser),
	dialog(Dialog),

	send(EB, append, B),
	send(Dialog, below, B),
	send(EB, fill, ''),
	
	send(EB, open).


		/********************************
		*            DIALOG		*
		********************************/

dialog(D) :-
	new(D, dialog),
	new(EB, D?frame),
	new(B, ?(EB, member, browser)),
	send(D, append,
	     new(T, menu(kind, cycle,
			 if(EB?current \== @nil,
			    and(message(EB?current, kind, @arg1),
				message(B, update, EB?current)))))),
	get(class(error), instance_variable, kind, Var),
	get(Var, type, Type),
	get(Type, context, TypeNames),
	send(TypeNames, for_all, message(T, append, @arg1)),
	send(T, active, @off),
	new(SS, text_item(search, regex(''))),
	send(D, append, button(apply, message(EB, fill, SS?selection))),
	send(D, append, button(help,  message(EB, help))),
	send(D, append, button(quit,  message(EB, quit))),
	send(D, append, SS, right),

	send(D, default_button, apply).
	


		/********************************
		*             FILL		*
		********************************/

fill(EB, Pattern:regex) :->
	"Fill with all errors matching pattern"::
	get(EB, member, browser, B),
	send(B, clear),
	new(Chain, chain),
	send(@errors, for_all,
	     if(message(Pattern, search, @arg2?man_summary),
		message(Chain, append, @arg2))),
	get(Chain, size, S),
	send(EB, report, progress, 'Sorting %d objects ...', S),
	send(Chain, sort, ?(@arg1?id, compare, @arg2?id)),
	send(EB, report, done),
	send(Chain, for_all, message(B, append_card, @arg1)),
	ignore(send(EB, selected, EB?current)).


		/********************************
		*          COMMUNICATION	*
		********************************/

selected(EB, Obj:object*) :->
	"Set the selection"::
	send(Obj, instance_of, error),
	send(EB, slot, current, Obj),
	get(?(EB, member, dialog), member, kind, KindMenu),
	send(KindMenu, selection, Obj?kind),
	send(KindMenu, active, @on),
	get(EB, member, browser, B),
	send(B, selected, Obj).


release_selection(EB) :->
	"Clear the selection"::
	get(EB, member, browser, B),
	send(B, release_selection),
	send(?(?(EB, member, dialog), member, kind), active, @off).

:- pce_end_class.

