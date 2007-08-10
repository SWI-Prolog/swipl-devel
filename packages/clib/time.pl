/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2007, University of Amsterdam

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

:- module(time,
	  [ alarm/3,			% +Time, :Callable, -Id
	    alarm/4,			% +Time, :Callable, -Id, +Options
	    remove_alarm/1,		% +Id
	    install_alarm/1,		% +Id
	    current_alarm/4,		% ?At, ?:Goal, ?Id, ?Status
	    call_with_time_limit/2	% +Time, :Callable
	  ]).
:- use_module(library(lists)).
:- set_prolog_flag(generate_debug_info, false).

%%	alarm(+Time, :Callable, -Id) is det.
%%	alarm(+Time, :Callable, -Id, +Options) is det.
%	
%	Set up an alarm to be signaled Time seconds from now. If the
%	alarm expires, Callable is called asynchronously. Callable can
%	be used to raise an exception using throw/1 to abort some
%	execution.
%	
%	The alarm system manages a sorted list of scheduled alarms. Each
%	time an alarm is added, removed or expires, this list is
%	re-examined and a new signal is scheduled using setitimer()
%	
%	Options is a list of Name(Value) options.  Currently defined
%	options are:
%	
%		* remove(Bool)
%		If =true= (default =false=), remove the alarm-event (as
%		remove_alarm/1) after it has been fired.
%		* install(Bool)
%		If =false= (default =true=) do not install the alarm.
%		It must be installed seperately using install_alarm/1.

%%	install_alarm(+Id) is det.
%
%	Install an alarm allocated using alarm(Time, Goal, Id,
%	[install(false)]).  Typically used with call_cleanup/2:
%	
%	==
%		alarm(Time, Goal1, Id, [install(false)]),
%		call_cleanup((install_alarm(Id), Goal2),
%			     remove_alarm(Id)),
%	==

%%	remove_alarm(+Id) is det.
%	
%	Remove an alarm.  If it has not yet been fired, it never will.

%%	current_alarm(?Time, :Goal, ?Id, ?Status) is nondet.
%	
%	Enumerate the alarms in the schedule.  Time is the absolute time
%	the event is scheduled for (see also get_time/1). Goal is the
%	goal to execute, Id is the identifier and Status is the
%	scheduling status. It takes the value =done= if the alarm has
%	been fired, =next= if the event is the next to be executed and
%	=scheduled= otherwise.

:- initialization
   load_foreign_library(foreign(time)).

:- module_transparent
	call_with_time_limit/2.

%%	call_with_time_limit(+Time, :Goal) is det.
%	
%	Call Goal, while watching out for   a (wall-time) limit. If this
%	limit  is  exceeded,  the   exception  =time_limit_exceeded=  is
%	raised. Goal is called as in once/1.
%
%	@throws =time_limit_exceeded=

call_with_time_limit(Time, Goal) :-
	Time > 0, !,
	strip_module(Goal, M, G),
	call_with_time_limit2(Time, M:G).
call_with_time_limit(_Time, _Goal) :-
	throw(time_limit_exceeded).
		     
call_with_time_limit2(Time, Goal) :-
	setup_and_call_cleanup(alarm(Time, throw(time_limit_exceeded), Id),
			       Goal,
			       remove_alarm_notrace(Id)).

current_alarm(Time, Goal, Id, Status) :-
	current_alarms(Time, Goal, Id, Status, List),
	member(alarm(Time, Goal, Id, Status), List).

		 /*******************************
		 *	  HANDLE MESSAGES	*
		 *******************************/

:- multifile
	prolog:message/3.

prolog:message(time_limit_exceeded) -->
	[ 'Time limit exceeded' ].

