/*  $Id$

    Designed and implemented by Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1996 University of Amsterdam. All rights reserved.
*/

:- module(prolog_locator,
	  [
	  ]).
:- use_module(preditem).
:- use_module(util).
:- use_module(clause).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module is designed  to  help   the  user  finding predicates and/or
clauses.  There are several useful options for finding predicates:

	* Hierarchical by module or sourcefile.  There should be an
	  easy way of defining `projects'; collections of file and or
	  modules.  One of the possibilities for defining a project
	  is by giving the `loadfile'.  All files that can be reached
	  from there constitute the `project'.  The structure of relations
	  between the sourcefiles may be used to organis the visualisation.

	* Straight by name: using prefix completion and/or substring
	  search over the predicates of the project.

	* Using cross-reference material: what predicates refer to me?

Required functions:

	* Cross-referencer, which will collect the project database:
	  files, predicates by file, import/export relations of the
	  files/modules.

	* Visualisation of the above.

	* Completer over all modules/predicates or a subset.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(prolog_locator, frame,
		   "Locator in a Prolog project").

:- pce_group(init).

initialise(PL, Tool:[frame]) :->
	send(PL, send_super, initialise,
	     'Prolog Locator',
	     application := @prolog_gui),
	(   Tool \== @nil
	->  new(_, hyper(Tool, PL, locator, tool))
	;   true
	),
	send(PL, append, new(MBD, dialog)),
	send(MBD, append, new(MB, menu_bar)),
	send(MBD, gap, size(0, 2)),
	send(MBD, pen, 0),
	send(PL, fill_menu_bar, MB),
	send(new(D, dialog), below, MBD),
	send(new(FBD, dialog), below, D),
	send(FBD, gap, size(5, 0)),
	send(FBD, pen, 0),
	send(FBD, append, label(reporter)),
	send(D, append, new(PI, prolog_predicate_item(predicate, ''))),
	send(D, append, button(quit, message(PL, destroy))),
	send(D, append, button(spy,  message(PL, spy, PI?selection))),
	send(D, append, button(view, message(PL, view, PI?selection))),
	send(D, append, button(edit, message(PL, edit, PI?selection))).
	

fill_menu_bar(PL, MB:menu_bar) :->
	send(MB, append, new(S, popup(settings))),
	send(MB, append, new(H, popup(help))),
	send_list(S, append,
		  [ menu_item(project,
			      message(PL?tool, settings)),
		    menu_item(locator,
			      message(PL, settings))
		  ]),
	send_list(H, append,
		  [ menu_item(help,
			      message(PL, help))
		  ]).

:- pce_group(tool).

tool(PL, Tool:any) :<-
	"Find the tool that is running me"::
	get(PL, hypered, tool, Tool).


:- pce_group(action).

spy(PL, What:sheet) :->
	prolog_predicates_from_selection(What, Preds),
	(   Preds == []
	->  send(PL, report, error, 'No matching predicates'),
	    fail
	;   Preds = [Pred],
	    predicate_name(Pred, Name),
	    gui_spy(Pred),
	    send(PL, report, status, 'Spy point on %s', Name)
	;   new(S, string('Spy points on\n')),
	    forall(member(Pred, Preds),
		   (   gui_spy(Pred),
		       predicate_name(Pred, Name),
		       send(S, append, string('\t%s', Name)))),
	    send(@display, inform, S)
	).
	    

view(PL, What:sheet) :->
	prolog_predicates_from_selection(What, Preds),
	(   Preds == []
	->  send(PL, report, error, 'No matching predicates'),
	    fail
	;   Preds = [Pred],
	    view(PL, Pred)
	;   select_predicate(PL, view, Preds, Pred),
	    view(PL, Pred)
	).


edit(PL, What:sheet) :->
	prolog_predicates_from_selection(What, Preds),
	(   Preds == []
	->  send(PL, report, error, 'No matching predicates'),
	    fail
	;   Preds = [Pred],
	    ed(Pred)
	;   select_predicate(PL, view, Preds, Pred),
	    ed(Pred)
	).


view(PL, Pred) :-
	find_source(Pred, File, Line),
	send(PL?tool, show_line, File, Line, listing).


select_predicate(PL, Op, Preds, Pred) :-
	new(B, browser),
	send(B, open_message, message(B, return, @arg1?key)),
	forall(nth1(I, Preds, Head),
	       (   predicate_name(Head, Name),
		   send(B, append, dict_item(I, Name))
	       )),
	send(new(D, dialog), below, B),
	send(D, append,
	     button(Op, message(B, return, B?selection?key))),
	send(D, append,
	     button(cancel, message(B, return, @nil))),
	send(B?frame, label, 'Select predicate'),
	get(B, confirm_centered, PL?area?center, Rval),
	send(B, destroy),
	(   integer(Rval)
	->  nth1(Rval, Preds, Pred)
	).


%	gui_spy/1 is spy/1, but doesn accept lists and doesn't report
%	or ask anything.

gui_spy(Head) :-
	'$define_predicate'(Head),
	'$spy'(Head).

:- pce_end_class.
