/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2010-2011, University of Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
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
