/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

:- module(prolog_gui,
	  [ prolog_tracer/1,
	    send_tracer/1,
	    send_if_tracer/1,
	    get_tracer/2,
	    display_stack/3
	  ]).
:- use_module(library(pce)).
:- use_module(settings).
:- use_module(trace).
:- use_module(source).
:- use_module(library(toolbar)).
:- use_module(util).
:- use_module(clause).
:- use_module(viewterm).
:- use_module(stack).

:- pce_autoload(prolog_source_browser, library('trace/browse')).

:- multifile
	user:prolog_event_hook/1,
	user:message_hook/3.

register_directories :-
	(   member(SpyBase, ['icons/nospy', library('trace/icons/nospy')]),
	    absolute_file_name(SpyBase,
			       [ extensions([xpm]), access(read)],
			       SpyIcon)
	->  file_directory_name(SpyIcon, Icons),
	    pce_image_directory(Icons)
	),
	(   member(HlpBase, ['pltracer', library('trace/pltracer')]),
	    absolute_file_name(HlpBase,
			       [ extensions([hlp]), access(read)],
			       HlpFile)
	->  pce_help_file(pltracer, HlpFile)
	).
	    
:- initialization register_directories.

version('1.1.0').

		 /*******************************
		 *	      RESOURCES		*
		 *******************************/

resource(debug,	image,	image('debug.xpm')).

		 /*******************************
		 *	     TOPLEVEL		*
		 *******************************/

:- dynamic
	gui/2.				% +Level, -Gui

prolog_tracer(Ref) :-
	flag('$break_level', Level, Level),
	(   gui(Level, Ref)
	->  true
	;   send(new(Ref, prolog_debugger), open),
	    asserta(gui(Level, Ref))
	).

send_tracer(Term) :-
	prolog_tracer(Ref),
	send(Ref, Term).

send_if_tracer(Term) :-
	flag('$break_level', Level, Level),
	(   gui(Level, Ref)
	->  send(Ref, Term)
	;   true
	).

get_tracer(Term, Result) :-
	prolog_tracer(Ref),
	get(Ref, Term, Result).

user:prolog_event_hook(Term) :-
	debug('prolog_event_hook(~w).~n', [Term]),
	fail.
user:prolog_event_hook(frame_finished(Frame)) :-
	gui(_, Gui),
	send(Gui, frame_finished, Frame),
	fail.
user:prolog_event_hook(exit_break(Level)) :-
	gui(Level, Gui),
	send(Gui, destroy),
	fail.
user:prolog_event_hook(finished_query(_Qid, YesNo)) :-
	flag('$break_level', Level, Level),
	gui(Level, Ref),
	send(Ref, clear),		% active, @off?
	(   YesNo == true
	->  send(Ref, report, status, 'Query succeeded')
	;   send(Ref, report, status, 'Query failed')
	),
	fail.

user:message_hook('$aborted', _, _Lines) :-
	aborted,
	fail.

aborted :-
	gui(Level, Gui),
	(   Level \== 0
	->  send(Gui, destroy)
	;   send(Gui, clear),
	    send(Gui, report, status, 'Execution aborted')
	).
aborted :-
	free(@confirm_prolog_gui_quitted).


		 /*******************************
		 *     DEBUGGER APPLICATION	*
		 *******************************/

:- pce_global(@prolog_gui, new(prolog_gui)).

:- pce_begin_class(prolog_gui, application,
		   "Toplevel driver for the Prolog GUI").

initialise(App) :->
	send(App, send_super, initialise, 'Prolog GUI'),
	send(App, kind, service).

:- pce_end_class.


		 /*******************************
		 *	   DEBUGGER FRAME	*
		 *******************************/

:- pce_begin_class(prolog_debugger, frame,
		   "Toplevel driver for the debugger").

variable(source,	any,	both, "Source view").

initialise(F) :->
	version(Version),
	send(F, send_super, initialise,
	     string('SWI-Prolog tracer version %s', Version),
	     application := @prolog_gui),
	send(F, icon, resource(debug)),
	send(F, append, new(MBD, dialog)),
	send(MBD, gap, size(0, 2)),
	send(MBD, pen, 0),
	send(MBD, append, new(menu_bar)),
	send(MBD, name, menu_bar_dialog),
	send(F, fill_menu_bar),
	send(new(D, prolog_button_dialog), below, MBD),
	send(D, name, buttons),

	new(V, prolog_bindings_view),
	send(V, label, 'Bindings'),
	send(V, name, bindings),
	send(new(S, prolog_stack_view), right, V),
	send(V, below, D),
	send(new(Src, prolog_source_view(size(80, 20))), below, V),
	send(F, source, Src),
	send(new(D2, dialog), below, Src),
	send(D2, name, report_dialog),
	send(D2, gap, size(5, 0)),
	send(D2, append, label(reporter)),
	send(D2, pen, 0),
	send(S, label, 'Call Stack'),
	send(S, name, stack).
 

unlink(F) :->
	get(F, object_reference, R),
	retractall(gui(_, @(R))),
	clear_clause_info_cache,	% safety first
	send(F, send_super, unlink).


clear(F) :->
	"Deactivate all views"::
	ignore(send(F, send_hyper, fragment, free)),
	get(F, member, stack, StackView),
	send(StackView, clear),
	get(F, member, bindings, BindingView),
	send(BindingView, clear).


fill_menu_bar(F) :->
	get(F, member, menu_bar_dialog, MBD),
	get(MBD, member, menu_bar, MB),
	send(MB, append, new(Tool, popup(tool))),
	send(MB, append, new(Help, popup(help))),
	send_list(Tool, append,
		  [ menu_item(settings,
			      message(F, settings),
			      end_group := @on),
		    menu_item(make,
			      message(@prolog, make),
			      end_group := @on),
		    menu_item(clear_source_cache,
			      message(@prolog, clear_clause_info_cache),
			      end_group := @on),
		    menu_item(quit,
			      message(F, destroy))
		  ]),
	send_list(Help, append,
		  [ menu_item(about, message(F, about)),
		    menu_item(help, message(F, help)),
		    menu_item(licence, message(F, licence))
		  ]).

settings(_F) :->
	"Edit the preferences"::
	trace_settings.


about(_) :->
	"Display aout message"::
	version(Version),
	send(@display, inform,
	     'SWI-Prolog debugger version %s\n\
	      By Jan Wielemaker',
	     Version).

help(_) :->
        "Show window with help-text"::
        send(@helper, give_help, pltracer, main).

licence(_) :->
        "Show help at distribution conditions"::
        send(@helper, give_help, pltracer, licence).

show_frame(_Tool, Frame:int, PC:'int|name') :->
	"Show the variables of this frame"::
	prolog_show_frame(Frame, [pc(PC), source, bindings]).


		 /*******************************
		 *	     ACTIONS		*
		 *******************************/

action(Frame, Action:name) :<-
	"Wait for the user to return an action"::
	action(Frame, Action).

action(Frame, Action) :-
	send(Frame, open),		% make sure
	get(Frame, display, Display),
	send(Display, busy_cursor, @nil),
	send(Display, synchronise),
	(   get(Frame, confirm, Action)
	->  true
	;   tracer_quitted(Action)
	).


tracer_quitted(Action) :-
	new(D, dialog('Tracer quitted')),
	send(D, application, @prolog_gui),
	send(D, append,
	     button(continue_without_debugging,
		    message(D, return, nodebug))),
	send(D, append,
	     button(abort,
		    message(D, return, abort))),
	send(D, append,
	     button(exit_prolog,
		    message(D, return, halt))),
	get(D, confirm_centered, Action),
	(   Action == abort
	->  send(D, hide),
	    send(D, name_reference, confirm_prolog_gui_quitted)
	;   send(D, destroy)
	).


selected_frame(F, Frame:int) :<-
	get(F, member, stack, Browser),
	get(Browser, selection, Frame).


		 /*******************************
		 *  ACTIONS OF THE SOURCE VIEW	*
		 *******************************/

:- pce_group(actions).

edit(F) :->
	"Edit current source_location"::
	send(F?source, edit).

spy(F) :->
	"Set a spy-point"::
	new(D, dialog('Set spy point')),
	send(D, append, new(TI, text_item(predicate, ''))),
	send(D, append, button(ok, message(D, return, TI?selection))),
	send(D, append, button(cancel, message(D, return, @nil))),
	send(D, default_button, ok),
	get(D, confirm_centered, F?area?center, Answer),
	send(D, destroy),
	Answer \== @nil,
	term_to_atom(Term, Answer),
	user:spy(Term).

goal(F, Goal:prolog) :<-
	"Return qualitied term for selected frame"::
	get(F, selected_frame, Frame),
	prolog_frame_attribute(Frame, goal, Goal0),
	(   Goal0 = _:_
	->  Goal = Goal0
	;   Goal = user:Goal0
	).

nospy(F) :->
	"Clear spy-point"::
	get(F, goal, Goal),
	nospy(Goal).

browse(F) :->
	"Provides overview for edit/spy/break"::
	get(F, application, App),
	(   get(App, member, prolog_source_browser, Browser)
	->  send(Browser, expose)
	;   send(new(PB, prolog_source_browser('.')), open),
	    send(PB, application, F?application)
	).

stop_at(F) :->
	"Set stop at caret"::
	get(F, source, SourceWindow),
	send(SourceWindow, stop_at).

nostop(F) :->
	"Delete selected stop"::
	(   get(F, goal, Goal),
	    '$get_predicate_attribute'(Goal, spy, 1)
	->  nospy(Goal)
	;   get(F, source, SourceWindow),
	    send(SourceWindow, nostop)
	).

up(F) :->
	"Select child frame"::
	get(F, member, stack, Stack),
	send(Stack, up).

down(F) :->
	"Select parent frame"::
	get(F, member, stack, Stack),
	send(Stack, down).

details(F) :->
	"Show (variable) details"::
	get(F, member, bindings, Bindings),
	send(Bindings, details).

abort(_) :->
	"Abort to the Prolog toplevel"::
	abort.

break(_) :->
	"Run a break"::
	break.

:- pce_group(delegate).

file(F, File:'name|text_buffer*') :->
	"Attach to indicated file"::
	send(F?source, file, File).

show_range(F, File:'name|text_buffer', From:int, To:int, Style:name) :->
	"Show indicated region using Style"::
	send(F?source, show_range, File, From, To, Style).

show_line(F, File:'name|text_buffer', Line:int, Style:name) :->
	"Show numbered line"::
	send(F?source, show_line, File, Line, Style).

listing(F, Module:name, Predicate:name, Arity:int) :->
	"List the specified predicate"::
	send(F?source, listing, Module, Predicate, Arity).

frame_finished(F, Frame:int) :->
	"This frame was terminated; remove it"::
	get(F, member, stack, StackView),
	send(StackView, frame_finished, Frame),
	(   get(F, member, bindings, Bindings),
	    get(Bindings, prolog_frame, Frame)
	->  send(Bindings, clear),
	    send(Bindings, slot, prolog_frame, @nil),
	    ignore(send(F, send_hyper, fragment, free))
	;   true
	).

:- pce_end_class.

		 /*******************************
		 *	      BUTTONS		*
		 *******************************/

:- pce_begin_class(prolog_button_dialog, dialog,
		   "Dialog holding the function buttons").

%	button(Action, Keys, Image, Balloon)
%
%	If action is +Action, send message Action to the frame.  Otherwise
%	return Action to the caller.

button(into,	 "i",	'into.xpm',	'Show unification').
button(creep,	 "\n ",	'creep.xpm',	'Step').
button(skip,	 "s",	'skip.xpm',	'Skip over this goal').
button(finish,	 "f",	'finish.xpm',	'Finish selected goal').
button(gap,	 -,	-,		-).
button(retry,	 "r",	'retry.xpm',	'Retry selected goal').
button(gap,	 -,	-,		-).
button(nodebug,  "n",	'nodebug.xpm',	'Continue without debugging').
button(+break,	 "b",	'break.xpm',	'Enter a recursive toplevel').
button(+abort,	 "a",	'abort.xpm',	'Abort to the Prolog toplevel').
button(gap,	 -,	-,		-).
button(+up,	 "u",	'up.xpm',	'Select child frame').
button(+down,	 "d",	'down.xpm',	'Select parent frame').
button(gap,	 -,	-,		-).
button(+browse,  "",	'16x16/butterfly.xpm',	'Browse program structure').
button(gap,	 -,	-,		-).
button(leap,	 "l",	'leap.xpm',	'Continue to spy- or breakpoint').
button(+spy,	 "+",	'spy.xpm',	'Set spy point').
button(+nospy,	 "-",	'nospy.xpm',	'Remove spy point').
button(+stop_at, "!",	'stop.xpm',	'Set Stop at caret').
button(+nostop,	 "-",	'nostop.xpm',	'Delete selected stop').
button(gap,	 -,	-,		-).
button(+details, "v",	'details.xpm',	'Show (variable) details').
button(+edit,	 "e",	'edit.xpm',	'Edit').


tag_balloon(Balloon0, Keys, Balloon) :-
	maplist(key_name, Keys, Names),
	concat_atom(Names, ', ', Tag),
	concat_atom([Balloon0, ' (', Tag, ')'], Balloon).

key_name(10, return) :- !.
key_name(32,  space) :- !.
key_name(C, A) :-
	char_code(A, C).

initialise(D) :->
	send(D, send_super, initialise),	
	send(D, pen, 0),
	send(D, gap, size(0,0)),
	get(D, frame, Frame),
	send(D, append, new(TB, tool_bar(Frame))),
	(   button(Action, Keys, Image, Balloon0),
	    (	Action == gap
	    ->	send(TB, append, gap)
	    ;   tag_balloon(Balloon0, Keys, Balloon),
		make_message(Action, D, Message),
	        send(TB, append,
		     new(B, tool_button(Message,
					image(Image),
					Balloon))),
	        chain_list(KL, Keys),
	        send(B, attribute, keys, KL)
	    ),
	    fail
	;   true
	).

make_message(+Action, D, message(D?frame, Action)) :- !.
make_message(Action,  D, message(D, return, Action)).

typed(D, Id:event_id) :->
	"Handle typing"::
	get(D, find, @default, 
	    and(message(@arg1, has_get_method, keys),
		message(@arg1?keys, member, Id)),
	    Button),
	send(Button, execute).

event(D, Ev:event) :->
	(   send(Ev, is_a, keyboard)
	->  send(D, typed, Ev)
	;   send(D, send_super, event, Ev)
	).

:- pce_end_class.

		 /*******************************
		 *	     VARIABLES		*
		 *******************************/

:- pce_begin_class(prolog_bindings_view, browser,
		   "Overview of bindings of the current frame").

variable(prolog_frame, int*, both, "Frame who's variables we are showing").

initialise(B) :->
	send(B, send_super, initialise, size := size(40, 11)),
	get(B, font, Font),
	get(Font, ex, Ex),
	Tab is 15 * Ex,
	send(B?image, tab_stops, vector(Tab)),
	send(B, open_message, message(B, details, @arg1)),
	send(B, ver_stretch, 0).

clear(B) :->
	send(B, send_super, clear),
	send(B, prolog_frame, @nil).

details(B, Item:[dict_item]) :->	% also handle a variable name!
	"View details of the binding"::
	get(B, prolog_frame, Frame),
	(   Frame \== @nil
	->  true
	;   send(B, report, warning, 'No current frame'),
	    fail
	),
	(   Item == @default
	->  (   get(B, selection, DI)
	    ->	true
	    ;	send(B, report, warning, 'No selected variable'),
		fail
	    )
	;   DI = Item
	),
	get(DI, key, VarName),
	get(DI, object, ArgN),
	prolog_frame_attribute(Frame, argument(ArgN), Value),
	prolog_frame_attribute(Frame, level, Level),
	prolog_frame_attribute(Frame, goal, Goal),
	predicate_name(Goal, PredName),
	(   integer(VarName)
	->  VarType = 'Argument'
	;   VarType = 'Variable'
	),
	sformat(Label, '~w ~w of frame at level ~d running ~w',
		[ VarType, VarName, Level, PredName ]),
	view_term(Value, [comment(Label)]).

:- pce_end_class.

