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
:- use_module(break).
:- use_module(util).

		 /*******************************
		 *	       STYLES		*
		 *******************************/

style(call,  		style(background := green,
			      icon := 'call.xpm')).
style(break, 		style(background := aquamarine)).
style(exit,  		style(background := green,
			      icon := 'exit.xpm')).
style(redo,  		style(background := yellow,
			      icon := 'redo.xpm')).
style(fail,  		style(background := red,
			      icon := 'fail.xpm')).
style(exception,  	style(background := purple,
			      icon := 'except.xpm')).
style(unify, 		style(background := sky_blue)).
style(listing,		style(background := bisque)).

style(breakpoint, 	style(icon := 'stop.xpm')).


		 /*******************************
		 *	    SOURCE VIEW		*
		 *******************************/

:- pce_begin_class(prolog_source_view, view,
		   "Prolog GUI source viewer").

variable(source,	'name|text_buffer*', both, "Currently shown source").

initialise(V, Size:size) :->
	send(V, send_super, initialise, size := Size),
	send(V, label, 'No source'),
	send(V, margin_width, 22),
	send(V?margin, recogniser,
	     click_gesture(left, '', single,
			   if(?(@event?receiver, fragment, @event),
			      message(V, selected_fragment,
				      ?(@event?receiver, fragment, @event)),
			      message(V, selected_fragment, @nil)))),
	forall(style(Name, Style),
	       send(V, style, Name, Style)),
	send(V, editable, @off).

lost_text_buffer(V) :->
	"The textbuffer has been destroyed, replace by a new one"::
	send(V, file, @nil).

:- pce_group(event).

event(V, Ev:event) :->
	(   send(Ev, is_a, keyboard),
	    get(V, frame, Tracer),
	    send(Tracer, typed, Ev)
	->  true
	;   send(V, send_super, event, Ev)
	).
	
:- pce_group(edit).

edit(V) :->
	"Prepare for editing the source"::
	get(V, text_buffer, TB),
	get(V, start, StartOfWindow),
	get(V, caret, Caret),
	debug('Edit, caret at ~w~n', [Caret]),
	(   (   get(@pce, convert, emacs_buffer, class, _)
	    ;	setting(use_pce_emacs, true),
		start_emacs
	    )
	->  (	send(TB, instance_of, emacs_buffer)
	    ->  get(TB, open, Frame),
		get(Frame, editor, Editor),
		send(Editor, scroll_to, StartOfWindow),
		send(Editor, caret, Caret)
	    ;   get(TB, attribute, file, FileObj),
		new(B, emacs_buffer(FileObj)),
		get(B, open, Frame),
		get(Frame, editor, Editor),
		send(Editor, scroll_to, StartOfWindow),
		send(Editor, caret, Caret),
		send(V, text_buffer, B),
		send(V, scroll_to, StartOfWindow),
		send(V, caret, Caret)
	    )
	;   get(TB, attribute, file, FileObj),
	    get(FileObj, name, File),
	    edit(File)
	).

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
	
nostop(V) :->
	"Deleted selected stop"::
	(   get(V, selected_fragment, F),
	    F \== @nil,
	    get(F, attribute, clause, ClauseRef),
	    get(F, attribute, pc, PC)
	->  '$break_at'(ClauseRef, PC, false)
	;   send(V, report, error, 'No selected stop-point'),
	    fail
	).


:- pce_group(file).

:- pce_global(@gui_last_change_check, new(date)).

not_recently_checked :-
	new(D, date),
	get(D, difference, @gui_last_change_check, Secs),
	(   Secs < 5
	->  free(D),
	    fail
	;   send(@gui_last_change_check, copy, D)
	).


file(V, File:'name|text_buffer*') :->
	"Attach to indicated file"::
	(   File == @nil
	->  send(V, text_buffer, new(text_buffer)),
	    send(V, label, '<No source>')
	;   send(File, instance_of, text_buffer)
	->  send(V, text_buffer, File),
	    (	get(File, attribute, comment, Label)
	    ->	send(V, label, Label)
	    ;	send(V, label, '<Decompiled code>')
	    )
	;   absolute_file_name(File, Canonical),
	    send(V, label, Canonical),
	    buffer(Canonical, B),
	    send(V, text_buffer, B),
	    send(V, check_modified)
	),
	send(V, source, File).

source_file(V, File:name) :<-
	"Currently shown sourcefile"::
	get(V, source, Source),
	atom(Source),
	canonical_source_file(Source, File).

check_modified(V) :->
	"Check for possibly modified file"::
	(   not_recently_checked
	->  get(V, text_buffer, TB),
	    (   get(@classes, member, emacs_buffer, _),
		send(TB, instance_of, emacs_buffer)
	    ->  send(TB, check_modified_file, @off)
	    ;   (   get(TB, attribute, time_stamp, Stamp),
		    get(TB, attribute, file, File),
		    get(File, time, FileStamp),
		    \+ send(Stamp, equal, FileStamp)
		->  reload_buffer(TB)
		;   true
		)
	    )
	;   true
	).

:- pce_group(show).

show_range(V, File:'name|text_buffer', From:int, To:int, Style:name) :->
	"Show indicated region using Style"::
	send(V, file, File),
	send(V, caret, To),
	new(F, fragment(V, From, To-From, Style)),
	ignore(send(V?frame, send_hyper, fragment, free)),
	new(_, trace_hyper(V?frame, F, fragment, tracer)),
	send(V, normalise, From, To).

show_line(V, File:'name|text_buffer', Line:int, Style:name) :->
	"Show numbered line"::
	debug('Show ~w:~w, style = ~w~n', [File, Line, Style]),
	send(V, file, File),
	get(V, text_buffer, TB),
	get(TB, scan, 0, line, Line - 1, start, SOL),
	get(TB, scan, SOL, line, 0, end, EOL),
	debug('Char range ~w ... ~w~n', [SOL, EOL]),
	send(V, show_range, File, SOL, EOL, Style).

listing(V, Module:name, Predicate:name, Arity:int) :->
	"List the specified predicate"::
	functor(Head, Predicate, Arity),
	send(V, file, @nil),
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

:- dynamic
	user:message_hook/3,
	current_source_buffer/2.			% +File, -Buffer
:- multifile
	user:message_hook/3.

user:message_hook(load_file(start(_Level, file(_Spec, Path))), _, _Lines) :-
	current_source_buffer(Path, Buffer),
	reload_buffer(Buffer),
	fail.

buffer(File, Buffer) :-
	object(@emacs), !,
	destroy_buffers,
	new(Buffer, emacs_buffer(File)),
	mark_special(File, Buffer).
buffer(File, Buffer) :-
	current_source_buffer(File, Buffer), !.
buffer(File, Buffer) :-
	new(FileObj, file(File)),
	new(Buffer, text_buffer),
	send(Buffer, attribute, file, FileObj),
	send(Buffer, attribute, time_stamp, FileObj?time),
	send(Buffer, insert_file, 0, FileObj, 1),
	send(Buffer, lock_object, @on),
	asserta(current_source_buffer(File, Buffer)),
	mark_special(File, Buffer).

reload_buffer(Buffer) :-
	get(@pce, convert, emacs_buffer, class, _),
	send(Buffer, instance_of, emacs_buffer), !,
	send(Buffer, revert),
	current_source_buffer(File, Buffer),
	send(Buffer, delete_attribute, debugger_marks_done),
	mark_special(File, Buffer),
	send(Buffer, report, status, 'Reloaded %s', File).
reload_buffer(Buffer) :-
	send(Buffer, clear),
	get(Buffer, attribute, file, FileObj),
	send(Buffer, insert_file, 0, FileObj, 1),
	get(FileObj, name, File),
	send(Buffer, delete_attribute, debugger_marks_done),
	mark_special(File, Buffer),
	send(Buffer, report, status, 'Reloaded %s', File).

destroy_buffers :-
	forall(retract(current_source_buffer(_, Buffer)),
	       free(Buffer)).

mark_special(_, Buffer) :-
	get(Buffer, attribute, debugger_marks_done, @on), !.
mark_special(File, Buffer) :-
	canonical_source_file(File, Source),
	send(Buffer, attribute, debugger_marks_done, @on),
	mark_stop_points(Buffer, Source).

mark_stop_points(_, Source) :-
	'$current_break'(ClauseRef, PC),
	clause_property(ClauseRef, file(Source)),
	mark_stop_point(ClauseRef, PC),
	fail.
mark_stop_points(_, _).

%	mark_stop_point(+ClauseRef, +PC)
%
%	Mark stop-points using a breakpoint fragment.

mark_stop_point(ClauseRef, PC) :-
	break_location(ClauseRef, PC, File, A-Z),
	buffer(File, Buffer),
	new(F, fragment(Buffer, A, Z-A, breakpoint)),
	send(F, attribute, clause, ClauseRef),
	send(F, attribute, pc, PC),
	new(_, hyper(@prolog_debugger, F, break, debugger)).

unmark_stop_point(ClauseRef, PC) :-
	(   get(@prolog_debugger, find_hyper, break,
		and(@arg3?clause == ClauseRef,
		    @arg3?pc == PC),
		Hyper)
	->  get(Hyper, to, Fragment),
	    free(Fragment)
	;   true
	).


		 /*******************************
		 *	      TRACE HYPER	*
		 *******************************/

:- pce_begin_class(trace_hyper, hyper).

unlink_from(H) :->
	get(H, to, Fragment),
	free(Fragment),
	free(H).

:- pce_end_class.

