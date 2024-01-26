/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2020-2023, University of Amsterdam
                              VU University Amsterdam
                              CWI, Amsterdam
                              SWI-Prolog Solutions b.v.
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

% only run these these if `SWIPL_TEST_FAIL_ON_UNLIKELY` is `y` as
% these tests may fail under extreme load.
if_safe :-
   getenv('SWIPL_TEST_FAIL_ON_UNLIKELY', y).

test(signal,
     [ timeout(0),
       exception(time_limit_exceeded)
     ]) :-
    call_with_time_limit(
        0.05,
        thread_wait(fail,
                    [ retry_every(0.01),
                      db(false)
                    ])).
test(wakeall, cleanup(cleanup)) :-
    do_later(0.05, assert(p)),
    +thread_wait(p, []).
test(wakeup, cleanup(cleanup)) :-
    do_later(0.05, assert(p)),
    +thread_wait(p, [wait_preds([p/0])]).
test(wakeup, cleanup(cleanup)) :-
    assert(p),
    +thread_wait(p, [wait_preds([p/0])]).
test(nowakeup,
     [ timeout(0),
       exception(time_limit_exceeded),
       cleanup(cleanup),
       condition(if_safe)
     ]) :-
    do_later(0.05, assert(p)),
    0.2+thread_wait(p, [retry_every(0.3),wait_preds([-(p/0)])]).
test(nowakep,
     [ timeout(0),
       exception(time_limit_exceeded),
       cleanup(cleanup),
       condition(if_safe)
     ]) :-
    assert(p),
    do_later(0.05, retract(p)),
    0.2+thread_wait(\+ p, [retry_every(0.3),wait_preds([+(p/0)])]).
test(modified,
     [ cleanup(cleanup),
       condition(if_safe)
     ]) :-
    do_later(0.05, assert(p)),
    +thread_wait(p(M), [modified(M), wait_preds([p/0])]).
test(module,
     [ cleanup(cleanup),
       condition(if_safe)
     ]) :-
    do_later(0.05, assert(p)),
    context_module(M),
    +thread_wait(p, [module(M)]).
test(update,
     [ Ready == true,
       condition(if_safe)
     ]) :-
    do_later(0.05, thread_update(true, [])),
    +thread_wait(Ready = true, []).
test(update,
     [ Ready == true,
       condition(if_safe)
     ]) :-
    M = tw_module,
    set_flag(tw_flag, false),
    do_later(0.05,
             ( set_flag(tw_flag, true),
               thread_update(true, [notify(broadcast),module(M)])
             )),
    0.2+thread_wait(( get_flag(tw_flag, true),
                      Ready = true
                    ),
                    [db(false),module(M)]).

:- end_tests(thread_wait).
