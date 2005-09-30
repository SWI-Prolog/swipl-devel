/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2005, University of Amsterdam

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

:- module(pce_debug_monitor,
	  [ prolog_debug_monitor/0
	  ]).
:- use_module(library(lists)).
:- use_module(library(debug)).
:- use_module(library(pce)).
:- use_module(library(pce_util)).
:- use_module(library(persistent_frame)).
:- use_module(library(pce_report)).
:- use_module(library(toolbar)).

:- pce_autoload(finder, library(find_file)).
:- pce_global(@finder, new(finder)).

prolog_debug_monitor :-
	send(new(prolog_debug_monitor), open).

:- pce_begin_class(prolog_debug_monitor, persistent_frame,
		   "Manage debug topics").

initialise(M) :->
	send_super(M, initialise, 'Prolog debug monitor'),
	new(B, prolog_debug_browser),
	send(new(V, prolog_debug_view), right, B),
	send(new(report_dialog), below, B),
	send(new(D, tool_dialog(M)), above, B),
	send(V, name, view),
	send(B, name, browser),
	send(M, append, B),
	send(M, fill_tool_dialog, D),
	send(M, refresh).
	     
fill_tool_dialog(_M, D:tool_dialog) :->
	send(D, append, new(File, popup(file))),
	send_list(File, append,
		  [ menu_item(clear),
		    gap,
		    menu_item(save_as),
		    gap,
		    menu_item(quit)
		  ]),
	send(D, append, new(View, popup(view))),
	send_list(View, append,
		  [ menu_item(refresh)
		  ]),
	send(D, append, new(Settings, popup(settings))),
	send_list(Settings, append,
		  [ menu_item(disable_all),
		    menu_item(enable_all)
		  ]).

:- pce_group(actions).

clear(M) :->
	get(M, member, view, View),
	send(View, clear).

refresh(M) :->
	get(M, member, browser, Browser),
	send(Browser, update).

save_as(M) :->
	"Save log messages to file"::
	get(@finder, file, save, FileName),
	get(M, member, view, View),
	send(View, save, FileName).

quit(M) :->
	send(M, destroy).

disable_all(B) :->
	"Disable all debug topics"::
	forall(debugging(Topic, true),
	       nodebug(Topic)),
	send(B, refresh).

enable_all(B) :->
	"Enable all debug topics"::
	forall(debugging(Topic, false),
	       debug(Topic)),
	send(B, refresh).

:- pce_end_class(prolog_debug_monitor).


		 /*******************************
		 *	     BROWSER		*
		 *******************************/

:- pce_global(@prolog_debug_monitor_browsers, new(chain)).

:- multifile
	user:message_hook/3.

user:message_hook(load_file(Done), _, _) :-
	functor(Done, done, _),
	arg(1, Done, 0),		% level
	in_pce_thread(update_monitor_browsers),
	fail.

update_monitor_browsers :-
	send(@prolog_debug_monitor_browsers, for_all,
	     message(@arg1, update)).

:- pce_begin_class(prolog_debug_browser, browser,
		   "Show current debug topics").

initialise(B) :->
	send_super(B, initialise),
	send(B, select_message, message(B, selected, @arg1)),
	send(B, selection_style, style(font := bold)),
	send(B, popup, new(P, popup)),
	send(P, update_message, message(B, update_popup, P)),
	send_list(P, append,
		  [ menu_item(enable,  message(B, enable, @arg1)),
		    menu_item(disable, message(B, disable, @arg1))
		  ]),
	send_list(B,
		  [ style(true, style(background := green))
		  ]),
	send(@prolog_debug_monitor_browsers, append, B).

unlink(B) :->
	send(@prolog_debug_monitor_browsers, delete_all, B),
	send_super(B, unlink).


update(B) :->
	send(B, clear),
	findall(Topic, debugging(Topic, _), Topics0),
	sort(Topics0, Topics),
	forall(member(Topic, Topics),
	       send(B, append_topic(Topic))).

append_topic(B, Topic:prolog) :->
	debugging(Topic, State),
	topic_to_atom(Topic, Atom),
	send(B, append,
	     dict_item(Atom,
		       string('%s\t%s', Atom, State),
		       prolog(Topic),
		       State)).
	
update_item(_B, DI:dict_item) :->
	"Update state of item"::
	get(DI, object, Topic),
	(   debugging(Topic, State)
	->  topic_to_atom(Topic, Atom),
	    send(DI, label, string('%s\t%s', Atom, State)),
	    send(DI, style, State)
	;   true
	).

resize(B) :->
	"Update tab-stops"::
	get(B?visible, width, W),
	send(B, tab_stops, vector(W-50)),
	send_super(B, resize).

selected(B, DI:dict_item) :->
	get(DI, object, Topic),
	get(B?frame, member, view, View),
	send(View, hightlight_messages, Topic).

:- pce_group(popup).

update_popup(B, P:popup) :->
	(   get(B?list_browser, dict_item, @event, DI)
	->  send(B, selection, DI),
	    get(DI, object, Topic),
	    (	debugging(Topic, State)
	    ->  (   State == true
		->  send(P, active_item, disable, @on),
		    send(P, active_item, enable, @off)
		;   send(P, active_item, disable, @off),
		    send(P, active_item, enable, @on)
		)
	    ;   true
	    )
	;   true
	).

disable(B, DI:dict_item) :->
	get(DI, object, Topic),
	nodebug(Topic),
	send(B, update_item, DI).
	
enable(B, DI:dict_item) :->
	get(DI, object, Topic),
	debug(Topic),
	send(B, update_item, DI).

:- pce_end_class(prolog_debug_browser).


		 /*******************************
		 *	       VIEW		*
		 *******************************/

:- volatile
	view_window/1.
:- dynamic
	view_window/1.

:- multifile
	prolog:debug_print_hook/3.

prolog:debug_print_hook(Topic, Format, Arguments) :-
	(   view_window(_)
	->  (   thread_self(main)
	    ->	debug_message(Topic, Format, Arguments)
	    ;   in_pce_thread(debug_message(Topic, Format, Arguments))
	    )
	).

debug_message(Topic, Format, Arguments) :-
	forall(view_window(V),
	       send(V, debug_message,
		    prolog(Topic),
		    prolog(Format),
		    prolog(Arguments))).
	

:- pce_begin_class(prolog_debug_view, view,
		   "SHow debug messages").

initialise(V) :->
	send_super(V, initialise),
	assert(view_window(V)).

unlink(V) :->
	retractall(view_window(V)),
	send_super(V, unlink).

hightlight_messages(V, Topic:prolog) :->
	get(V, styles, Sheet),
	send(Sheet, for_all, message(V, style, @arg1?name, @nil)),
	topic_to_atom(Topic, Style),
	send(V, style, Style, style(bold := @on)).

debug_message(V, Topic:prolog, Format:prolog, Arguments:prolog) :->
	get(V, text_buffer, TB),
	get(TB, size, S0),
	pce_open(TB, append, Out),
	format(Out, Format, Arguments),
	nl(Out),
	close(Out),
	get(TB, size, S1),
	Len is S1 - S0 - 1,
	topic_to_atom(Topic, Style),
	new(_, fragment(TB, S0, Len, Style)).

:- pce_end_class(prolog_debug_view).


		 /*******************************
		 *	       UTIL		*
		 *******************************/

topic_to_atom(Topic, Topic) :-
	atom(Topic), !.
topic_to_atom(Topic, Atom) :-
	term_to_atom(Topic, Atom).
