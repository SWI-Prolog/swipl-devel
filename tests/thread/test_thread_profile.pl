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

:- module(test_thread_profile,
          [ test_thread_profile/0
          ]).
:- use_module(library(plunit)).

/** <module> Test profiling a thread using profiler/2

The thread monitor of the IDE profiles a thread by signalling it to
call profiler/2.  This must sample the thread, also if the thread never
used profile/1,2, which sets the sample rate.
*/

test_thread_profile :-
    run_tests([thread_profile]).

:- begin_tests(thread_profile).

test(profiler_samples, true(Samples > 0)) :-
    thread_self(Me),
    thread_create(send_samples(Me), Id),
    thread_get_message(samples(Samples)),
    thread_join(Id).

:- end_tests(thread_profile).

send_samples(To) :-
    profiled_samples(Samples),
    thread_send_message(To, samples(Samples)).

%   profiled_samples(-Samples)
%
%   Samples is the number of samples taken while profiling 0.2 sec of
%   CPU time.

profiled_samples(Samples) :-
    reset_profiler,
    profiler(_, cputime),
    statistics(cputime, T0),
    spin(T0),
    profiler(_, false),
    '$prof_statistics'(Samples, _, _, _, _, _, _).

spin(T0) :-
    statistics(cputime, T),
    (   T - T0 > 0.2
    ->  true
    ;   atom_length(abc, _),
        spin(T0)
    ).
