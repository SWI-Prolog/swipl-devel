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

:- module(time,
	  [ alarm/3,			% +Time, :Callable, -Id
	    alarm/4,			% +Time, :Callable, -Id, +Options
	    remove_alarm/1,		% +Id
	    current_alarm/4,		% ?At, ?:Goal, ?Id, ?Status
	    call_with_time_limit/2	% +Time, :Callable
	  ]).

%	alarm(+Time, :Callable, -Id)
%	alarm(+Time, :Callable, -Id, +Options)
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
%		If Bool is true, remove the alarm-event (as
%		remove_alarm/1) after it has been fired.

%	remove_alarm(+Id)
%	
%	Remove an alarm.  If it has not yet been fired, it never will.

%	current_alarm(?Time, ?:Goal, ?Id, ?Status)
%	
%	Enumerate the alarms in the schedule.  Time is the absolute time
%	the event is scheduled for (see also get_time/1). Goal is the
%	goal to execute, Id is the identifier and Status is the
%	scheduling status. It takes the value =done= if the alarm has
%	been fired, =next= if the event is the next to be executed and
%	=scheduled= otherwise.

:- initialization
   load_foreign_library(foreign(time)).

:- meta_predicate(call_with_time_limit(+, :)).

%	call_with_time_limit(+Time, :Goal)
%	
%	Call :Goal, while watching out for a (real-time) limit.  If this
%	limit is exceeded, the exception time_limit_exceeded is raised.

call_with_time_limit(Time, Goal) :-
	alarm(Time, throw(time_limit_exceeded), Id),
	call_cleanup(once(Goal), remove_alarm(Id)).
		     
