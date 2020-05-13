/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2019, VU University Amsterdam
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

:- module(timed_call,
          [ timed_call/2
          ]).
:- use_module(library(time)).

/** <module> XSB timed call

This module emulates  the  XSB  built-in   timed_call/2.  As  this  is a
built-in, this module is re-exported   from library(dialect/xsb). It has
been placed in a separete module to   facilitate  reuse and allow to not
load this if the prerequisites   (library(time) and multi-threading) are
not satisfied.
*/

%!  timed_call(:Goal, :Options)
%
%   Emulation for XSB timed_call/2.  Runs  Goal   as  once/1  with timed
%   interrupts. The interrupt goals are called   as interrupts using the
%   semantics of ignore/1: possible choice points are cut and failure is
%   ignored.  If the interrupt throws an exception this is propagated.
%
%   Options is a list of the terms  below.   At  least  one of the terms
%   max/2 or repeating/2 must be present.
%
%      - max(+MaxInterval, :MaxHandler)
%        Schedule a single interrupt calling MaxHandler at MaxInterval
%        __milliseconds__ from now.
%      - repeating(+RepInterval, :RepHandler)
%        Schedule a repeating interrupt calling RepHandler each
%        RepInterval __milliseconds__.
%      - nesting
%        Nested calls to timed_call/2 are transformed into calls to
%        once/1.  Without `nesting`, a nested call raises a
%        `permission_error` exception.
%
%   @see call_with_time_limit/2, alarm/4, thread_signal/2.
%
%   @compat  This  predicate  is  a  generalization  of  the  SWI-Prolog
%   library(time) interface. It is left in  the XSB emulation because it
%   uses non-standard option syntax and the  time is in __milliseconds__
%   where all SWI-Prolog time handling uses seconds.

:- meta_predicate
    timed_call(0, :).

timed_call(Goal, _Options) :-
    ok_nested(Goal),
    !,
    debug(timed_call, 'Calling nested ~p', [Goal]),
    once(Goal).
timed_call(Goal, Options) :-
    '$timed_call_nested'(Goal, Options),
    no_lco.

no_lco.

ok_nested(Goal) :-
    prolog_current_frame(Fr),
    prolog_frame_attribute(Fr, parent, Fr2),
    prolog_frame_attribute(Fr2, parent, Parent),
    prolog_frame_attribute(Parent, parent_goal,
                           timed_call:timed_call(_,_:Options)),
    !,
    debug(timed_call, 'Nested ~p: found parent timed call with options ~p',
          [Goal, Options]),
    (   memberchk(nesting, Options)
    ->  true
    ;   permission_error(nest, timed_goal, Goal)
    ).


'$timed_call_nested'(Goal, M:Options) :-
    memberchk(max(MaxInterval, MaxHandler), Options),
    !,
    run_max_goal(Goal, MaxInterval, MaxHandler, M:Options).
'$timed_call_nested'(Goal, M:Options) :-
    memberchk(repeating(RepInterval, RepHandler), Options),
    !,
    run_repeating_goal(Goal, RepInterval, RepHandler, M:Options).
'$timed_call_nested'(_Goal, _M:Options) :-
    domain_error(timed_call_options, Options).

run_max_goal(Goal, MaxInterval, MaxHandler, M:Options) :-
    (   MaxInterval > 0
    ->  Time is MaxInterval/1000,
        setup_call_cleanup(alarm(Time, ignore(M:MaxHandler),
                                 Id, [install(false)]),
                           run_opt_releating(Id, Goal, M:Options),
                           time:remove_alarm_notrace(Id))
    ;   call(M:MaxHandler)
    ).

run_opt_releating(AlarmID, Goal, M:Options) :-
    memberchk(repeating(RepInterval, RepHandler), Options),
    !,
    install_alarm(AlarmID),
    run_repeating_goal(Goal, RepInterval, RepHandler, M:Options).
run_opt_releating(AlarmID, Goal, _Options) :-
    install_alarm(AlarmID),
    call(Goal),
    !.

run_repeating_goal(Goal, RepInterval, RepHandler, M:_Options) :-
    (   RepInterval > 0
    ->  Time is RepInterval/1000,
        setup_call_cleanup(alarm(Time, repeat_handler(Id, Time, M:RepHandler),
                                 Id, [install(false)]),
                           run_alarm_goal(Id, Goal),
                           time:remove_alarm_notrace(Id))
    ;   domain_error(repeating_interval, RepInterval)
    ).


repeat_handler(Id, Time, M:RepHandler) :-
    ignore(M:RepHandler),
    uninstall_alarm(Id),
    install_alarm(Id, Time).

run_alarm_goal(AlarmID, Goal) :-
    install_alarm(AlarmID),
    call(Goal),
    !.
