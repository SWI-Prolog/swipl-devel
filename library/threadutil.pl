/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
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
	thread_self(Id), !.
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
	has_console(Id), !.
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
	
