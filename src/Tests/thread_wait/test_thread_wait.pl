/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2020, University of Amsterdam
                         VU University Amsterdam
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

:- module(test_thread_wait,
          [ test_thread_wait/0
          ]).
:- use_module(library(plunit)).
:- use_module(library(time)).

test_thread_wait :-
    run_tests([ thread_wait
              ]).

:- begin_tests(thread_wait, [sto(rational_trees)]).

:- meta_predicate
    do_later(+, 0),
    later(+, 0),
    +(0),
    +(+,0).

do_later(Time, Goal) :-
    thread_create(later(Time, Goal), _, [detached(true)]).

later(Time, Goal) :-
    sleep(Time),
    call(Goal).

+(Goal) :-
    call_with_time_limit(5, Goal).
Max+(Goal) :-
    call_with_time_limit(Max, Goal).

:- dynamic p/0.

p(_) :-
    p.

cleanup :-
    retractall(p).

test(signal, exception(time_limit_exceeded)) :-
    call_with_time_limit(
        0.05,
        thread_wait_on_goal(fail,
                            [ retry_every(0.01),
                              db(false)
                            ])).
test(wakeall, cleanup(cleanup)) :-
    do_later(0.05, assert(p)),
    +thread_wait_on_goal(p, []).
test(wakep, cleanup(cleanup)) :-
    do_later(0.05, assert(p)),
    +thread_wait_on_goal(p, [wait_preds([p/0])]).
test(nowakep, [exception(time_limit_exceeded),cleanup(cleanup)]) :-
    do_later(0.05, assert(p)),
    0.2+thread_wait_on_goal(p, [retry_every(0.3),wait_preds([-(p/0)])]).
test(nowakep, [exception(time_limit_exceeded),cleanup(cleanup)]) :-
    assert(p),
    do_later(0.05, retract(p)),
    0.2+thread_wait_on_goal(p, [retry_every(0.3),wait_preds([+(p/0)])]).
test(modified, [cleanup(cleanup)]) :-
    do_later(0.05, assert(p)),
    +thread_wait_on_goal(p(M), [modified(M), wait_preds([p/0])]).
test(module, cleanup(cleanup)) :-
    do_later(0.05, assert(p)),
    context_module(M),
    +thread_wait_on_goal(p, [module(M)]).

:- end_tests(thread_wait).
