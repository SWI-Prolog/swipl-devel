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

:- module(prolog_query,
	  [ 
	  ]).
:- use_module(library(pce)).
:- use_module(library(pce_report)).

:- dynamic
	query_window/1.

:- pce_begin_class(prolog_query_frame, persistent_frame,
		   "Enter query in a frame").

initialise(QF) :->
	send_super(QF, initialise, 'Prolog Query'),
	send(QF, append, new(PQ, prolog_query)),
	send(new(report_dialog), below, PQ).

:- pce_end_class(prolog_query_frame).


		 /*******************************
		 *	   QUERY-DIALOG		*
		 *******************************/

:- pce_begin_class(prolog_query, dialog,
		   "Ask a Prolog query").

variable(qstatus, {fill,ready,run,alt}, get, "Current status").	

initialise(PQ) :->
	send_super(PQ, initialise, 'Prolog Query'),
	send(PQ, append, prolog_query_item(query)),
	send(PQ, append, new(ST, menu(options, toggle))),
	send_list(ST, append, [debug, trace]),
	send(ST, layout, horizontal),
	send(ST, show_label, @off),
	send(PQ, append, button(run), right),
	send(PQ, append, button(next, message(PQ, return, next)), right),
	send(PQ, append, button(done, message(PQ, return, done)), right),
	send(PQ, qstatus, fill),
	assert(query_window(PQ)).

unlink(PQ) :->
	retractall(query_window(PQ)),
	send_super(PQ, unlink).

:- pce_group(active).

qstatus(PQ, Status:{fill,ready,run,alt}) :->
	"Set button activation status"::
	send(PQ, slot, qstatus, Status),
	active(Status, Buttons, Default),
	set_active(Buttons, PQ),
	send(PQ, default_button, Default),
	send(PQ, synchronise).

active(fill,  [run(@off), next(@off), done(@off)], @nil).
active(ready, [run(@on),  next(@off), done(@off)], run).
active(run,   [run(@off), next(@off), done(@off)], @nil).
active(alt,   [run(@off), next(@on),  done(@on)],  done).

set_active([], _).
set_active([H|T], PQ) :-
	H =.. [Name, OnOff],
	get(PQ, member, Name, Button),
	send(Button, active, OnOff),
	set_active(T, PQ).

exception(PQ, E:prolog) :->
	"Print a Prolog exception"::
	message_to_string(E, String),
	send(PQ, report, error, String).

aborted(PQ) :->
	"An abort occurred"::
	(   get(PQ, qstatus, run)
	->  send(PQ, qstatus, ready),
	    send(PQ, report, status, 'Aborted')
	;   true
	).

:- pce_group(action).

run(PQ) :->
	"Run a query"::
	send(PQ, focus, @nil),		% otherwise the run-button remains
	get(PQ, member, query, QI),	% active
	get(PQ, member, options, Menu),
	get_chain(Menu, selection, Options),
	get(QI, selection, Query-Bindings),
	send(PQ, qstatus, run),
	(   catch(prolog_user_run(Options, user:Query), E, true),
	    (   var(E)
	    ->  (   Bindings == []
		->  send(PQ, report, status, 'Yes'),
		    send(PQ, qstatus, ready)
		;   send(PQ, report_bindings, Bindings),
		    get(PQ, ask_next, Next),
		    (   Next == next
		    ->  send(PQ, qstatus, run),
			fail
		    ;   !,
			nodebug,
			send(PQ, qstatus, ready)
		    )
		)
	    ;	send(PQ, exception, E),
		send(PQ, qstatus, ready)
	    )
	;   send(PQ, report, warning, 'No'),
	    send(PQ, qstatus, ready)
	).
	
prolog_user_run([], Query) :- !,
	Query.
prolog_user_run([debug], Query) :- !,
	(   debug,
	    Query
	*-> (   nodebug
	    ;	debug, fail
	    )
	;   nodebug,
	    fail
	).
prolog_user_run(_, Query) :-
	(   guitracer,			% make sure
	    trace,
	    Query
	*-> (   notrace
	    ;	trace, fail
	    )
	;   notrace,
	    nodebug,
	    fail
	).

ask_next(PQ, Reply:{next,done}) :<-
	"Ask for continuations"::
	send(PQ, qstatus, alt),
	get(PQ, confirm, Reply).

report_bindings(PQ, Bindings:prolog) :->
	"Report the bindings in a short form"::
	merge_bindings(Bindings, Merged),
	new(TB, text_buffer),
	pce_open(TB, write, Stream),
	write_bindings(Merged, Stream),
	close(Stream),
	send(PQ, report, status, TB?contents),
	free(TB).

%	merge_bindings(In, Out)
%
%	Translates the binding list into a list of VarList=Value, where
%	all variables having the same value are merged.

merge_bindings([], []).
merge_bindings([Var=Value|T0], [[Var|Vars]=Value|T]) :-
	same_binding(T0, Value, Vars, T1),
	merge_bindings(T1, T).

same_binding([], _, [], []).
same_binding([Var=Value|T0], V, [Var|Vars], T) :-
	Value =@= V, !,
	same_binding(T0, V, Vars, T).
same_binding([B|T0], V, Vars, [B|T]) :-
	same_binding(T0, V, Vars, T).

write_bindings([], _).
write_bindings([Vars=Value|T], Fd) :-
	write_vars(Vars, Fd),
	write_term(Fd, Value,
		   [ quoted(true),
		     portray(true),
		     max_depth(2)
		   ]),
	write_bindings(T, Fd).

write_vars([], _).
write_vars([Name|T], Fd) :-
	format(Fd, '~w = ', [Name]),
	write_vars(T, Fd).

:- pce_end_class(prolog_query).

:- pce_begin_class(prolog_query_item, text_item,
		   "Enter a Prolog query").

variable(qstatus,  {empty,syntax_error,undefined,ready}, get,
	 "Current evaluation qstatus").

initialise(QI, Name:[name], Def:[any|function], Msg:[code]*) :->
	send_super(QI, initialise, Name, Def, Msg),
	send(QI, style, combo_box).

event(QI, Ev:event) :->
	"Process event and update qstatus"::
	(   get(Ev, id, 32)
	->  send(QI, append, ' '),
	    send(QI, caret, @default)
	;   send_super(QI, event, Ev)
	),
	send(QI, update_qstatus).

completions(_, From:char_array, Matches:chain) :<-
	"Complete from Prolog history"::
	get(From, value, Prefix),
	findall(Event, matching_event(Prefix, Event), Events0),
	list_to_set(Events0, Events),
%	reverse(Events1, Events),
	chain_list(Matches, Events).

matching_event(Prefix, Event) :-
	recorded('$history_list', _/Event), % Should be more abstract
	sub_atom(Event, 0, _, _, Prefix).

selected_completion(QI, Text:char_array, Apply:[bool]) :->
	send_super(QI, selected_completion, Text, Apply),
	send(QI, update_qstatus).

qstatus(QI, Status:{empty,syntax_error,undefined,ready}) :->
	(   get(QI, slot, qstatus, Status)
	->  true
	;   send(QI, slot, qstatus, Status),
	    qstatus_style(Status, Font, Colour),
	    send(QI, value_font, Font),
	    send(QI?value_text, colour, Colour),
	    (	send(QI?device, has_send_method, qstatus)
	    ->	(   Status == ready
		->  send(QI?device, qstatus, ready)
		;   send(QI?device, qstatus, fill)
		)
	    ;	true
	    )
	).
	
qstatus_style(empty,	    normal, @default).
qstatus_style(syntax_error, normal, red).
qstatus_style(undefined,    normal, @default).
qstatus_style(ready,	    bold,   @default).

update_qstatus(QI) :->
	get(QI?value_text?string, value, Text),
	(   get(Text, size, 0)
	->  Status = empty
	;   catch(atom_to_term(Text, Goal, _), _, fail)
	->  (   current_predicate(_, user:Goal)
	    ->	Status = ready
	    ;	Status = undefined
	    )
	;   Status = syntax_error
	),
	send(QI, qstatus, Status).

selection(QI, Selection:prolog) :<-
	"Get selection as Query-Bindings"::
	get_super(QI, selection, Text),
	catch(atom_to_term(Text, Goal, Bindings), _, fail),
	send(QI, add_history, Text),
	Selection = Goal-Bindings.

add_history(_, Text:name) :->
	"Add to the history-list"::
	'$save_history'(Text).

:- pce_end_class(prolog_query_item).


		 /*******************************
		 *	       HOOKS		*
		 *******************************/

:- multifile
	user:message_hook/3.

user:message_hook('$aborted', _, _Lines) :-
	aborted,
	fail.

aborted :-
	query_window(W),
	send(W, aborted),
	fail.
