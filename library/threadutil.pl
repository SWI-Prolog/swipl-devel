/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1999-2013, University of Amsterdam
			      VU University Amsterdam

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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(thread_util,
	  [ thread_run_interactor/0,	% interactor main loop
	    threads/0,			% List available threads
	    join_threads/0,		% Join all terminated threads
	    interactor/0,		% Create a new interactor
	    thread_has_console/0,	% Test whether calling thread has a console
	    attach_console/0,		% Create an xterm-console for thread.

	    tspy/1,			% :Spec
	    tspy/2,			% :Spec, +ThreadId
	    tdebug/0,
	    tdebug/1,			% +ThreadId
	    tnodebug/0,
	    tnodebug/1,			% +ThreadId
	    tprofile/1			% +ThreadId
	  ]).
:- set_prolog_flag(generate_debug_info, false).

:- module_transparent
	tspy/1,
	tspy/2.

%%	threads
%
%	List currently active threads.  If a thread has exited, get
%	rid of them.

threads :-
	format('~`-t~60|~n', []),
	format('~t~w~20|  ~w~32|~n', ['Thread', 'Status']),
	format('~`-t~60|~n', []),
	thread_property(Id, status(Status)),
	format('~t~w~20|  ~p~32|~n', [Id, Status]),
	fail.
threads :-
	format('~`-t~60|~n', []).

%%	join_threads
%
%	Join all terminated threads.

join_threads :-
	(   thread_property(Id, status(Status)),
	    rip_thread(Status, Id),
	    fail ; true
	).

rip_thread(running, _) :- !.
rip_thread(_Status, Id) :-
	thread_self(Id), !.
rip_thread(Status, Id) :-
	print_message(informational, join_thread(Id, Status)),
	thread_join(Id, _).

%%	interactor
%
%	Run a Prolog toplevel in another thread with a new console window.

interactor :-
	thread_create(thread_run_interactor, _Id,
		      [ detached(true)
		      ]).

thread_run_interactor :-
	notrace,
	set_prolog_flag(debug, false),
	set_prolog_flag(query_debug_settings, debug(false, false)),
	attach_console,
	print_message(banner, thread_welcome),
	prolog.

%%	thread_has_console is semidet.
%
%	True when the calling thread has an attached console.
%
%	@see attach_console/0

:- dynamic
	has_console/4.			% Id, In, Out, Err

thread_has_console(main) :- !.			% we assume main has one.
thread_has_console(Id) :-
	has_console(Id, _, _, _).

thread_has_console :-
	current_prolog_flag(break_level, _), !.
thread_has_console :-
	thread_self(Id),
	thread_has_console(Id), !.

%%	attach_console is det.
%
%	Create an xterm-console and make the standard Prolog streams point to
%	it.

attach_console :-
	thread_has_console, !.
attach_console :-
	thread_self(Id),
	console_title(Id, Title),
	open_console(Title, In, Out, Err),
	assert(has_console(Id, In, Out, Err)),
	set_stream(In,  alias(user_input)),
	set_stream(Out, alias(user_output)),
	set_stream(Err, alias(user_error)),
	set_stream(In,  alias(current_input)),
	set_stream(Out, alias(current_output)),
	thread_at_exit(detach_console(Id)).

console_title(Thread, Title) :-		% uses tabbed consoles
	current_prolog_flag(console_menu_version, qt), !,
	format(atom(Title), 'Thread ~w', [Thread]).
console_title(Thread, Title) :-
	current_prolog_flag(system_thread_id, SysId),
	format(atom(Title),
	       'SWI-Prolog Thread ~w (~d) Interactor',
	       [Thread, SysId]).

:- if(current_predicate(win_open_console/5)).

open_console(Title, In, Out, Err) :-
	thread_self(Id),
	regkey(Id, Key),
	win_open_console(Title, In, Out, Err,
			 [ registry_key(Key)
			 ]).

regkey(Key, Key) :-
	atom(Key).
regkey(_, 'Anonymous').

:- else.

open_console(Title, In, Out, Err) :-
	open_xterm(Title, In, Out, Err).

:- endif.

detach_console(Id) :-
	(   retract(has_console(Id, In, Out, Err))
	->  close(In, [force(true)]),
	    close(Out, [force(true)]),
	    close(Err, [force(true)])
	;   true
	).


		 /*******************************
		 *	    DEBUGGING		*
		 *******************************/

%%	tspy(:Spec) is det.
%%	tspy(:Spec, +ThreadId) is det.
%
%	Trap the graphical debugger on reaching Spec in the specified or
%	any thread.

tspy(Spec) :-
	spy(Spec),
	tdebug.

tspy(Spec, ThreadID) :-
	spy(Spec),
	tdebug(ThreadID).


%%	tdebug is det.
%%	tdebug(+Thread) is det.
%
%	Enable debug-mode, trapping the graphical debugger on reaching
%	spy-points or errors.

tdebug :-
	forall(thread_property(Id, status(running)),
	       thread_signal(Id, gdebug)).

tdebug(ThreadID) :-
	thread_signal(ThreadID, gdebug).

%%	tnodebug is det.
%%	tnodebug(+Thread) is det.
%
%	Disable debug-mode in all threads or the specified Thread.

tnodebug :-
	forall(thread_property(Id, status(running)),
	       thread_signal(Id, nodebug)).

tnodebug(ThreadID) :-
	thread_signal(ThreadID, nodebug).


		 /*******************************
		 *	 REMOTE PROFILING	*
		 *******************************/

%%	tprofile(+Thread) is det.
%
%	Profile the operation of Thread until the user hits a key.

tprofile(Thread) :-
	init_pce,
	thread_signal(Thread,
		      (	  reset_profiler,
			  profiler(_, true)
		      )),
	format('Running profiler in thread ~w (press RET to show results) ...',
	       [Thread]),
	flush_output,
	get0(_),
	thread_signal(Thread,
		      (	  profiler(_, false),
			  show_profile(plain, 25)
		      )).


%%	init_pce
%
%	Make sure XPCE is running if it is   attached, so we can use the
%	graphical display using in_pce_thread/1.

init_pce :-
	current_prolog_flag(gui, true), !,
	call(send(@(display), open)).	% avoid autoloading
init_pce.


		 /*******************************
		 *	       HOOKS		*
		 *******************************/

:- multifile
	user:message_hook/3.

user:message_hook(trace_mode(on), _, Lines) :-
	\+ thread_has_console,
	\+ current_prolog_flag(gui_tracer, true),
	catch(attach_console, _, fail),
	print_message_lines(user_error, '% ', Lines).

:- multifile
	prolog:message/3.

prolog:message(thread_welcome) -->
	{ thread_self(Self)
	},
	[ 'SWI-Prolog console for thread ~w'-[Self],
	  nl, nl
	].
prolog:message(join_thread(Id, Status)) -->
	[ 'Joining thread ~w, ended with status ~p'-[Id, Status]
	].

