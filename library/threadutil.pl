/*  $Id$

    Part of SWI-Prolog
    Designed and implemented by Jan Wielemaker

    Copyright (C) 1999 SWI, University of Amsterdam. All rights reserved.
*/

:- module(thread_util,
	  [ threads/0,			% List available threads
	    interactor/0,		% Create a new interactor
	    attach_console/0		% Create an xterm-console for thread.
	  ]).

%	threads
%
%	List currently active threads.  If a thread has exited, get
%	rid of them.

threads :-
	format('~*t~40|~n', "-"),
	format('~t~w~8|  ~w~32|~n', ['Thread', 'Status']),
	format('~*t~40|~n', "-"),
	current_thread(Id, Status),
	format('~t~w~8|  ~p~32|~n', [Id, Status]),
	rip_thread(Status, Id),
	fail.
threads :-
	format('~*t~40|~n', "-").

rip_thread(running, _) :- !.
rip_thread(_Status, Id) :-
	thread_join(Id, _).
	
%	interactor
%
%	Run a Prolog toplevel in another thread with a new console window.

interactor :-
	thread_create(run_interactor, _Id, []).

run_interactor :-
	attach_console,
	'$welcome',
	run_prolog.

run_prolog :-
	catch(prolog, E,
	      ( print_message(error, E),
%		E = error(_, _),
		run_prolog)).

%	attach_console
%
%	Create an xterm-console and make the standard Prolog streams point to
%	it.

:- dynamic
	has_console/3.			% which thread has a console?

has_console(main).			% we assume main has one.
has_console(Id) :-
	has_console(Id, _, _).

attach_console :-
	thread_self(Id),
	has_console(Id, _, _), !.
attach_console :-
	thread_self(Id),
	'$get_pid'(Pid),
	sformat(Title, 'SWI-Prolog Thread ~w (pid ~d) interactor', [Id, Pid]),
	open_xterm(Title, In, Out),
	assert(has_console(Id, In, Out)),
	set_stream(In,  alias(user_input)),
	set_stream(Out, alias(user_output)),
	set_stream(Out, alias(user_error)),
	set_stream(In,  alias(current_input)),
	set_stream(Out, alias(current_output)),
	thread_at_exit(detach_console(Id)).

detach_console(Id) :-
	retract(has_console(Id, In, Out)),
	catch(close(In), _, true),
	catch(close(Out), _, true).

		
		 /*******************************
		 *	       HOOKS		*
		 *******************************/

:- multifile
	user:message_hook/3.

user:message_hook(trace_mode(on), _, Lines) :-
	attach_console,
	print_message_lines(user_error, '% ', Lines).
	
