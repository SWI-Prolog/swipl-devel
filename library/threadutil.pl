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
	  [ thread_run_interactor/0,	% interactor main loop
	    threads/0,			% List available threads
	    interactor/0,		% Create a new interactor
	    attach_console/0		% Create an xterm-console for thread.
	  ]).

%	threads
%
%	List currently active threads.  If a thread has exited, get
%	rid of them.

threads :-
	format('~*t~60|~n', "-"),
	format('~t~w~20|  ~w~32|~n', ['Thread', 'Status']),
	format('~*t~60|~n', "-"),
	current_thread(Id, Status),
	format('~t~w~20|  ~p~32|~n', [Id, Status]),
	rip_thread(Status, Id),
	fail.
threads :-
	format('~*t~60|~n', "-").

rip_thread(running, _) :- !.
rip_thread(_Status, Id) :-
	thread_self(Id), !.
rip_thread(_Status, Id) :-
	thread_join(Id, _).
	
%	interactor
%
%	Run a Prolog toplevel in another thread with a new console window.

interactor :-
	thread_create(thread_run_interactor, _Id,
		      [ detached(true)
		      ]).

thread_run_interactor :-
	attach_console,
	print_message(banner, thread_welcome),
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
	has_console/4.			% Id, In, Out, Err

has_console(main).			% we assume main has one.
has_console(Id) :-
	has_console(Id, _, _, _).

has_console :-
	thread_self(Id),
	has_console(Id), !.

attach_console :-
	has_console, !.
attach_console :-
	thread_self(Id),
	current_prolog_flag(system_thread_id, SysId),
	sformat(Title,
		'SWI-Prolog Thread ~w (~d) Interactor',
		[Id, SysId]),
	(   current_prolog_flag(windows, true)
	->  regkey(Id, Key),
	    win_open_console(Title, In, Out, Err,
			     [ registry_key(Key)
			     ])
	;   open_xterm(Title, In, Out, Err)
	),
	assert(has_console(Id, In, Out, Err)),
	set_stream(In,  alias(user_input)),
	set_stream(Out, alias(user_output)),
	set_stream(Err, alias(user_error)),
	set_stream(In,  alias(current_input)),
	set_stream(Out, alias(current_output)),
	thread_at_exit(detach_console(Id)).

regkey(Key, Key) :-
	atom(Key).
regkey(_, 'Anonymous').

detach_console(Id) :-
	retract(has_console(Id, In, Out, Err)),
	catch(close(In), _, true),
	catch(close(Out), _, true),
	catch(close(Err), _, true).

		
		 /*******************************
		 *	       HOOKS		*
		 *******************************/

:- multifile
	user:message_hook/3.

user:message_hook(trace_mode(on), _, Lines) :-
	\+ has_console,
	attach_console,
	print_message_lines(user_error, '% ', Lines).
	
:- multifile
	prolog:message/3.

prolog:message(thread_welcome) -->
	{ thread_self(Self)
	},
	[ 'SWI-Prolog console for thread ~w'-[Self],
	  nl, nl
	].
