/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2010, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(timeout,
	  [ time_out/3			% :Goal, +Time, -Result
	  ]).
:- use_module(library(time)).

/** <module> SICStus compatible timeout handling

@author Ulrich Neumerkel
@author Jan Wielemaker
*/

:- meta_predicate
	time_out(0, +, -).

%%	time_out(:Goal, +Time_ms, -Result) is nondet.
%
%	This library provides a  SICStus   compatible  implementation of
%	time-outs. Unfortunately, it is not fully compatible:
%
%	    * Time is measured in wall-time instead of virtual CPU.
%
%	Virtual CPU time  is  hard   in  threaded-environments.  On most
%	systems, you probably need a thread  that measures the CPU usage
%	of the monitored thread.

time_out(Goal, Time_ms, Result) :-
	Time_s is (Time_ms//1)/1000,
        catch( ( Result0 = success,
                 setup_call_cleanup(
                        alarm(Time_s, throw(time_out), Id),
                        Goal,
                        ( Removed = true, remove_alarm(Id) )),
                 (   var(Removed)
                 ->  uninstall_alarm(Id),
                     ( true ; install_alarm(Id,Time_s), fail )
                 ;   true
                 )
	       ),
	       time_out,
	       Result0 = time_out ),
        Result = Result0.
