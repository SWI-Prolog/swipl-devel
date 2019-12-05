/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2010-2019, University of Amsterdam
			      CWI, Amsterdam
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

/** <module> SICStus compatible time out handling

@author Ulrich Neumerkel
@author Jan Wielemaker
*/

:- meta_predicate
    time_out(0, +, -).

%!   time_out(:Goal, +Time_ms, -Result) is nondet.
%
%    This  library  provides  a  SICStus  compatible  implementation  of
%    time-outs. This predicate runs Goal as   call/1 and binds Result to
%    either `success` (the  answer  was   produced  within  Time_ms)  or
%    `time_out`  (Goal  did  not  terminate  within  Time_ms).  If  Goal
%    succeeds with a choice point, backtracking   into it re-applies the
%    time limit, i.e., each solution gets a Time_ms time limit.
%
%    Calls to time_out/3 can  be  nested.  If   an  outer  time  out  is
%    triggered  first,  the  inner  time  out    is  cancelled  using  a
%    time_out(Id)  exception  and  the  outer    one   binds  Result  to
%    `time_out`.
%
%    @bug Unfortunately, our emulation is not  fully compatible with the
%    SICStus original. Notably, Time is  measured in __wall-time instead
%    of  virtual  CPU   time__.   Virtual   CPU    time   is   hard   in
%    threaded-environments. On most systems, you  probably need a thread
%    that measures the CPU usage of the monitored thread.
%
%    @see alarm/3, call_with_time_limit/2,   call_with_inference_limit/3
%    and   thread-based   primitives   such   as   thread_signal/2   and
%    first_solution/3.

time_out(Goal, Time_ms, Result) :-
    Time_s is (Time_ms//1)/1000,
    prolog_current_frame(Fid),                  % Unique id for this call.
    catch( ( Result0 = success,
             setup_call_cleanup(
                 alarm(Time_s, throw(time_out(Fid)), Id),
                 Goal,
                 ( Removed = true, remove_alarm(Id) )),
             (   var(Removed)
             ->  uninstall_alarm(Id),
                 ( true ; install_alarm(Id,Time_s), fail )
             ;   true
             )
           ),
           time_out(Fid),
           Result0 = time_out ),
    Result = Result0.
