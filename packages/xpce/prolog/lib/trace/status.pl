/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 2001 University of Amsterdam. All rights reserved.
*/

:- module(prolog_debug_status, []).
:- use_module(library(pce)).
:- use_module(library(pce_report)).
:- use_module(library('trace/clause')).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This  module  defines  the  class   prolog_debug_status,  a  status  dialog
representing the current debugger-status (cf.  debugging/0) with entries
to alter the state of the  debugger   by  changing  the mode and editing
trace, spy and break-points.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

resource(delete, image,	image('16x16/cut.xpm')).
resource(stop,	 image,	image('16x16/stop.xpm')).
resource(trace,	 image,	image('16x16/eye.xpm')).
resource(edit,	 image,	image('16x16/edit.xpm')).
resource(spy,	 image,	library('trace/icons/spy.xpm')).

:- dynamic
	debug_status_window/1.

:- pce_begin_class(prolog_debug_status, dialog,
		   "View/change debug_status information").

initialise(D) :->
	send_super(D, initialise('Prolog debugging')),
	send(D, append, new(TB, tool_bar(D))),
	send_list(TB, append,
		  [ tool_button(cut,
				resource(delete),
				delete)
		  ]),
	send(D, append,
	     new(Ch, menu(mode, choice, message(D, mode, @arg1))),
	     right),
	send(Ch, layout, horizontal),
	send(Ch, alignment, right),
	send_list(Ch, append, [ normal, debug, trace ]),
	send(D, append, new(LB, list_browser)),
	send(LB, select_message, message(D, identify, @arg1)),
	send(LB, open_message, message(D, edit, @arg1)),
	send(LB, style, spy, style(icon := resource(spy))),
	send(LB, style, break, style(icon := resource(stop))),
	send(LB, style, trace, style(icon := resource(trace))),
	send(LB, attribute, hor_stretch, 100),
	send(D, append, prolog_predicate_item(predicate)),
	send(D, append, new(TB2, tool_bar(D)), right),
	send(TB2, name, tb2),
	send(TB2, alignment, right),
	send_list(TB2, append,
		  [ tool_button(spy,
				resource(spy),
				'Set spy point'),
		    tool_button(trace,
				resource(trace),
				'Set trace point'),
		    tool_button(edit,
				resource(edit),
				'Edit predicate')
		  ]),
	send(D, append, new(reporter)),
	send(D, resize_message, message(D, layout, @arg2)),
	send(D, update),
	assert(debug_status_window(D)).

unlink(D) :->
	retractall(debug_status_window(D)),
	send_super(D, unlink).
	
layout(D, Size:[size]) :->
	"Fix layout"::
	send_super(D, layout, Size),
	get(D, member, tb2, TB2),
	get(D, member, predicate, PI),
	send(PI, right_side, TB2?left_side - D?gap?width).

:- pce_group(update).

clear(D) :->
	"Clear the browser"::
	get(D, member, list_browser, LB),
	send(LB, clear).

update(D) :->
	"Update for current settings"::
	send(D, clear),
	(   debugging(How, Where),
	    send(D, append_debug, How, Where),
	    fail
	;   true
	).
	
append_debug(D, What:{spy,trace,break}, Where:prolog) :->
	get(D, member, list_browser, LB),
	name_of(Where, Label),
	send(LB, append, dict_item(Label,
				   @default,
				   prolog(Where),
				   What)).


debugging(How, Where) :-
	Where = _:_,
	current_predicate(_, Where),
	\+ predicate_property(Where, imported_from(_)),
	debugging_(Where, How).
debugging(break, clause(ClauseRef,PC)) :-
	'$current_break'(ClauseRef, PC).

debugging_(Where, spy) :-
	'$get_predicate_attribute'(Where, spy, 1).
debugging_(Where, trace) :-
	'$get_predicate_attribute'(Where, trace_any, 1).

name_of(clause(Ref, _PC), Label) :- !,
	clause_name(Ref, Label).
name_of(Where, Label) :-
	predicate_name(Where, Label).

item(D, What:{spy,trace,break}, Where:prolog, Item:dict_item) :<-
	"Find item representing this debug"::
	get(D, member, list_browser, LB),
	get(LB?members, find_all, @arg1?style == What, Candidates),
	chain_list(Candidates, List),
	member(Item, List),
	get(Item, object, Where).

:- pce_group(action).

selection(D, DI:dict_item) :<-
	"Get selected item"::
	get(D, member, list_browser, LB),
	get(LB, selection, DI).

spy(D) :->
	"Set spy point on predicate"::
	get(D, member, predicate, PI),
	(   get(PI, selection, What)
	->  user:spy(What)
	;   send(D, report, warning, 'No predicate')
	).

trace(D) :->
	"Set spy point on predicate"::
	get(D, member, predicate, PI),
	(   get(PI, selection, What)
	->  user:trace(What, +all)
	;   send(D, report, warning, 'No predicate')
	).

identify(D, DI:dict_item) :->
	"Report verbosely the meaning of current item"::
	get(DI, label, Label),
	get(DI, style, Style),
	style_name(Style, Name),
	send(D, report, status, '%s %s', Name, Label).

style_name(spy,   'Spy point on').
style_name(break, 'Break-point in').
style_name(trace, 'Trace-point on').

edit(D, DI:[dict_item]) :->
	"Edit source"::
	(   DI \== @default
	->  get(DI, object, What)
	;   get(D, member, predicate, PI),
	    get(PI, selection, What)
	->  true
	;   get(D, selection, DI)
	->  get(DI, object, What)
	),
	edit(What).

cut(D) :->
	"Delete associated object"::
	(   get(D, selection, DI)
	->  get(DI, object, Where),
	    get(DI, style, Style),
	    delete(Style, Where)
	;   send(D, report, warning, 'No selection')
	).

delete(spy, Head) :- !,
	'$nospy'(Head).
delete(trace, Head) :- !,
	trace(Head, -all).
delete(break, clause(Ref, PC)) :-
	'$break_at'(Ref, PC, false).

mode(_D, Mode:{normal,debug,trace}) :->
	(   Mode == normal
	->  set_prolog_flag(debug, false)
	;   Mode == debug
	->  set_prolog_flag(debug, true)
	;   Mode == trace
	->  guitracer
	).

:- pce_end_class.


		 /*******************************
		 *	   MESSAGE HOOKS	*
		 *******************************/

:- multifile
	user:message_hook/3.

					% ADDING ITEMS
%user:message_hook(Term, Level, _Lines) :-
%	format('~p ~p~n', [Level, Term]),
%	fail.
user:message_hook(spy(Head), _Level, _Lines) :-
	debug_status_window(D),
	send(D, append_debug, spy, Head),
	fail.
user:message_hook(break(true, ClauseRef, PC), _Level, _Lines) :-
	debug_status_window(D),
	send(D, append_debug, break, clause(ClauseRef, PC)),
	fail.
user:message_hook(trace(Head, Ports), _Level, _Lines) :-
	Ports \== [],
	debug_status_window(D),
	\+ get(D, item, trace, Head, _),
	send(D, append_debug, trace, Head),
	fail.
					% DELETING ITEMS
user:message_hook(nospy(Head), _Level, _Lines) :-
	debug_status_window(D),
	get(D, item, spy, Head, DI),
	free(DI),
	fail.
user:message_hook(break(false, ClauseRef, PC), _Level, _Lines) :-
	debug_status_window(D),
	get(D, item, break, clause(ClauseRef, PC), DI),
	free(DI),
	fail.
user:message_hook(trace(Head, []), _Level, _Lines) :-
	debug_status_window(D),
	get(D, item, trace, Head, DI),
	free(DI),
	fail.
user:message_hook(debug_mode(OnOff), _Level, _Lines) :-
	debug_status_window(D),
	get(D, member, mode, Mode),
	(   OnOff == off
	->  send(Mode, selection, normal)
	;   tracing
	->  send(Mode, selection, trace)
	;   send(Mode, selection, debug)
	).
user:message_hook(trace_mode(OnOff), _Level, _Lines) :-
	debug_status_window(D),
	get(D, member, mode, Mode),
	(   OnOff == on
	->  send(Mode, selection, trace)
	;   current_prolog_flag(debug, true)
	->  send(Mode, selection, debug)
	;   send(Mode, selection, normal)
	).
