/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2026, SWI-Prolog Solutions b.v.
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


:- module(test_signal_wait,
          [ test_signal_wait/0
          ]).
:- use_module(library(plunit)).

/** <module> Test waiting with pending signals that cannot be handled

Inside sig_atomic/1 or a cleanup handler, signals are not processed.
Waiting for a message or condition must then time out normally rather
than treating the pending signal as an interrupt and retrying forever.
*/

test_signal_wait :-
    run_tests([ signal_wait
              ]).

:- begin_tests(signal_wait).

test(cleanup_zero_timeout, Result-Signal == timeout-done) :-
    nb_setval(test_signal, none),
    setup_call_cleanup(true, true,
                       ( poll([timeout(0)], R),
                         nb_setval(test_result, R)
                       )),
    nb_getval(test_result, Result),
    nb_getval(test_signal, Signal).
test(atomic_timeout, Result-Signal == timeout-done) :-
    nb_setval(test_signal, none),
    get_time(T0),
    sig_atomic(poll([timeout(0.1)], Result)),
    get_time(T1),
    assertion(T1-T0 >= 0.09),
    nb_getval(test_signal, Signal).
test(atomic_wait, Msg-Signal == hello-done) :-
    nb_setval(test_signal, none),
    thread_self(Me),
    thread_create(( sleep(0.1),
                    thread_send_message(Me, hello)
                  ), Id),
    sig_atomic(( signal_self,
                 thread_get_message(Me, Msg)
               )),
    thread_join(Id),
    nb_getval(test_signal, Signal).
test(atomic_thread_wait, Signal == done) :-
    nb_setval(test_signal, none),
    sig_atomic(( signal_self,
                 \+ thread_wait(fail, [timeout(0)])
               )),
    nb_getval(test_signal, Signal).

:- end_tests(signal_wait).

poll(Options, Result) :-
    signal_self,
    thread_self(Me),
    (   thread_get_message(Me, absent, Options)
    ->  Result = absent
    ;   Result = timeout
    ).

signal_self :-
    thread_self(Me),
    thread_signal(Me, nb_setval(test_signal, done)).
