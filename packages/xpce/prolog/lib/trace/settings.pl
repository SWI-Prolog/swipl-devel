/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

:- module(trace_settings,
	  [ trace_settings/0
	  ]).
:- use_module(util).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setting(?Name, ?ValueSet, ?Comment)
	Defines the settable attributes for the GUI based tracer.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

setting(verbose,
	[true, false],
	'Print GUI debugging info (maintenance)').
setting(show_unbound,
	[true, false],
	'`Bindings'' window shows unbound variables').
setting(cluster_variables,
	[true, false],
	'`Bindings'' window clusters variables with the same value').
setting(stack_depth,
	int(2, infinite),
	'Number of stack-frames displayed').
setting(choice_depth,
	int(0, infinite),
	'Number of choice-points displayed').
%setting(term_depth,
%	int(2, infinite),
%	'Nesting depth of terms printed').
setting(list_max_clauses,
	int(2, infinite),
	'Maximum number of clauses decompiled when listing dynamic code').
setting(auto_raise,
	[true, false],
	'Automatically raise the tracer window').
setting(use_pce_emacs,
	[true, false],
	'Use Built-in PceEmacs editor').

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
trace_setting/0
	Show the current settings, and allows for editing them.  There
	isn't a help yet.  Modal.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

trace_settings :-
	new(D, dialog('Trace Settings')),
	send(D, application, @prolog_gui),
	forall(setting(Name, _, _), make_item(D, Name)),
	send(D, append, new(A, button(apply, and(message(D, apply),
						 message(D, destroy))))),
	send(D, append, button(reset, message(D, restore))),
	send(D, append, button(cancel, message(D, destroy))),
	send(D, default_button, A),
	send(A, active, @off),
	send(D, modal, application),
	(   send(@event, instance_of, event),
	    get(@event?receiver, frame, Frame)
	->  send(D, transient_for, Frame),
	    get(Frame?area, center, Pos)
	;   Pos = @default
	),
	send(D, open_centered, Pos).


make_item(D, Name) :-
	setting(Name, ValueSet, Comment),
	is_list(ValueSet), !,
	send(D, append, new(M, menu(Name, marked,
				    message(@prolog, set_trace_setting,
					    Name, @arg1)))),
	send(M, layout, horizontal),
	send_list(M, append, ValueSet),
	send(M, default, ?(@prolog, trace_setting, Name)),
	send(M, help_message, tag, Comment).
make_item(D, Name) :-
	setting(Name, int(Low, infinite), Comment),
	send(D, append, new(TI, text_item(Name, Low,
					  message(@prolog, set_trace_setting,
						  Name, @arg1)))),
	send(TI, length, 5),
	send(TI, default, ?(@prolog, trace_setting, Name)),
	send(TI, help_message, tag, Comment).

set_trace_setting(Name, Value) :-
	trace_setting(Name, _, Value).

