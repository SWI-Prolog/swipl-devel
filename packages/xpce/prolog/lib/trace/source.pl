/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

:- module(prolog_source_view,
	  [ mark_stop_point/2,		% +ClauseRef, +PC
	    unmark_stop_point/2,	% +ClauseRef, +PC
	    current_source_buffer/2	% +File, -Buffer
	  ]).
:- use_module(library(pce)).
:- use_module(library(pce_emacs)).
:- use_module(break).
:- use_module(util).

		 /*******************************
		 *	       STYLES		*
		 *******************************/

style(call,  		style(background := green,
			      icon := 'call.xpm')).
style(break, 		style(background := cyan)).
style(exit,  		style(background := green,
			      icon := 'exit.xpm')).
style(redo,  		style(background := yellow,
			      icon := 'redo.xpm')).
style(fail,  		style(background := '#ff8080',
			      icon := 'fail.xpm')).
style(exception,  	style(background := magenta,
			      icon := 'except.xpm')).
style(unify, 		style(background := sky_blue)).

style(breakpoint, 	style(icon := 'stop.xpm')).


		 /*******************************
		 *	    SOURCE VIEW		*
		 *******************************/

:- pce_begin_class(prolog_source_view, emacs_view,
		   "Prolog GUI source viewer").

class_variable(size,	size,	size(80,20), "Default size in characters").

variable(source,	'name|emacs_buffer*', get, "Currently shown source").

initialise(V) :->
	send(V, send_super, initialise),
	send(V, reuse, @off),		% donot share in the pool
	send(V, mode, prolog),
	send(V, margin_width, 22),
	forall(style(Name, Style), send(V, style, Name, Style)),
	send(V, editable, @off),
	send(V, update_label).

lost_text_buffer(V) :->
	"The textbuffer has been destroyed, replace by a new one"::
	send(V, file, @nil).

update_label(V) :->
	"Create label from <-editable and <-source"::
	get(V, source, Source),
	(   atom(Source)
	->  Label0 = Source
	;   Source == @nil
	->  Label0 = '<no source>'
	;   get(Source, attribute, comment, Label0)
	->  true
	;   get(Source, name, Label0)
	),
	(   get(V, editable, @on)
	->  send(V, label, string('[edit] %s', Label0))
	;   send(V, label, Label0)
	).

:- pce_group(event).

event(V, Ev:event) :->
	(   send(Ev, is_a, keyboard),
	    get(V, editable, @off),
	    get(V, frame, Tracer),
	    send(Tracer, has_send_method, source_typed),
	    send(Tracer, source_typed, Ev)
	->  true
	;   send(V, send_super, event, Ev)
	).
	
:- pce_group(edit).

edit(V, Val:[bool]) :->
	"Toggle read-only mode"::
	(   Val == @default
	->  get(V?editable, negate, NewVal)
	;   NewVal = Val
	),
	send(V, editable, NewVal),
	send(V, update_label).

:- pce_group(stop).

stop_at(V) :->
	"Set stop-point at location"::
	(   get(V, source_file, File)
	->  get(V, caret, Caret),
	    get(V, line_number, Line),
	    break_at(File, Line, Caret)
	;   send(V, report, error, 'No source'),
	    fail
	).
	
delete_selected_stop(V) :->
	"Deleted selected stop"::
	get(V, selected_fragment, F),
	F \== @nil,
	get(F, attribute, clause, ClauseRef),
	get(F, attribute, pc, PC), !,
	'$break_at'(ClauseRef, PC, false).


:- pce_group(source).

:- pce_global(@gui_last_change_check, new(date)).

not_recently_checked :-
	new(D, date),
	get(D, difference, @gui_last_change_check, Secs),
	(   Secs < 5
	->  free(D),
	    fail
	;   send(@gui_last_change_check, copy, D)
	).

source(V, Source:'name|emacs_buffer*') :->
	"Attach to indicated file"::
	(   get(V, source, Source)
	->  true
	;   (   Source == @nil
	    ->  send(V, text_buffer, emacs_buffer(@nil, '<no source>'))
	    ;   send(Source, instance_of, emacs_buffer)
	    ->  send(V, text_buffer, Source)
	    ;   absolute_file_name(Source, Canonical),
		buffer(Canonical, B),
		send(V, text_buffer, B),
		send(V, check_modified)
	    ),
	    send(V, slot, source, Source),
	    send(V, update_label),
	    send(V?editor, auto_colourise_buffer)
	).

source_file(V, File:name) :<-
	"Currently shown sourcefile"::
	get(V, source, Source),
	atom(Source),
	canonical_source_file(Source, File).

check_modified(V) :->
	"Check for possibly modified file"::
	(   not_recently_checked
	->  get(V, text_buffer, TB),
	    send(TB, check_modified_file, @off)
	;   true
	).

:- pce_group(show).

show_range(V, File:'name|emacs_buffer', From:int, To:int, Style:name) :->
	"Show indicated region using Style"::
	send(V, source, File),
	send(V, caret, To),
	new(F, fragment(V, From, To-From, Style)),
	ignore(send(V?frame, send_hyper, fragment, free)),
	new(_, trace_hyper(V?frame, F, fragment, tracer)),
	send(V, normalise, From, To).

show_line(V, File:'name|emacs_buffer', Line:int, Style:name) :->
	"Show numbered line"::
	debug('Show ~w:~w, style = ~w~n', [File, Line, Style]),
	send(V, source, File),
	get(V, text_buffer, TB),
	get(TB, scan, 0, line, Line - 1, start, SOL),
	get(TB, scan, SOL, line, 0, end, EOL),
	debug('Char range ~w ... ~w~n', [SOL, EOL]),
	send(V, show_range, File, SOL, EOL, Style).

listing(V, Module:name, Predicate:name, Arity:int) :->
	"List the specified predicate"::
	functor(Head, Predicate, Arity),
	send(V, source, @nil),
	get(V, text_buffer, TB),
	open(TB, write, Fd),
	telling(Old), set_output(Fd),
	ignore(listing(Module:Head)),
	tell(Old),
	close(Fd).

:- pce_end_class.

		 /*******************************
		 *      BUFFER MANAGEMENT	*
		 *******************************/

current_source_buffer(File, Buffer) :-
	get(@emacs, file_buffer, File, Buffer).

buffer(File, Buffer) :-
	new(Buffer, emacs_buffer(File)),
	send(Buffer, mode, prolog),
	mark_special(File, Buffer).

mark_special(_, Buffer) :-
	get(Buffer, attribute, debugger_marks_done, @on), !.
mark_special(File, Buffer) :-
	canonical_source_file(File, Source),
	send(Buffer, attribute, debugger_marks_done, @on),
	send(Buffer, margin_width, 22),
	mark_stop_points(Buffer, Source).

mark_stop_points(_, Source) :-
	'$current_break'(ClauseRef, PC),
	clause_property(ClauseRef, file(Source)),
	mark_stop_point(ClauseRef, PC),
	fail.
mark_stop_points(_, _).

:- pce_global(@prolog_debugger, new(object)).

%	mark_stop_point(+ClauseRef, +PC)
%
%	Mark stop-points using a breakpoint fragment.

mark_stop_point(ClauseRef, PC) :-
	stop_fragment(ClauseRef, PC, _), !. 		% already got this one
mark_stop_point(ClauseRef, PC) :-
	break_location(ClauseRef, PC, File, A-Z),
	current_source_buffer(File, Buffer),
	new(F, fragment(Buffer, A, Z-A, breakpoint)),
	send(F, attribute, clause, ClauseRef),
	send(F, attribute, pc, PC),
	new(_, hyper(@prolog_debugger, F, break, debugger)).

unmark_stop_point(ClauseRef, PC) :-
	(   stop_fragment(ClauseRef, PC, Fragment)
	->  free(Fragment)
	;   true
	).

stop_fragment(ClauseRef, PC, Fragment) :-
	get(@prolog_debugger, find_hyper, break,
	    and(@arg3?clause == ClauseRef,
		@arg3?pc == PC),
	    Hyper),
	get(Hyper, to, Fragment).


		 /*******************************
		 *	      TRACE HYPER	*
		 *******************************/

:- pce_begin_class(trace_hyper, hyper).

unlink_from(H) :->
	get(H, to, Fragment),
	free(Fragment),
	free(H).

:- pce_end_class.

