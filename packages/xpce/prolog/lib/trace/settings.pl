/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (C): 1985-2002, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
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

