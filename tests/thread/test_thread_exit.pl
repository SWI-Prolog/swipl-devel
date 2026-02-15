/*  Part of SWI-Prolog

    Author:        Eric Taucher
    E-mail:        eric.taucher@gmail.com
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

:- module(test_thread_exit,
	  [ test_thread_exit/0
	  ]).
:- if((exists_source(library(process)),
       exists_source(library(http/thread_httpd)))).

:- use_module(library(plunit)).
:- use_module(library(readutil)).
:- use_module(library(process)).

/** <module> Test clean process exit after thread usage

Verify that a process using threads   exits cleanly without crashing. On
Windows with PThreads4W, the  library's DllMain(DLL_PROCESS_DETACH) runs
after PL_cleanup() has freed thread data.   Without explicit cleanup via
pthread_win32_process_detach_np(), this causes ACCESS_VIOLATION in Debug
builds where the CRT fills freed heap with 0xDD.

@see pthread_win32_process_detach_np() in src/pl-init.c
*/

test_thread_exit :-
    run_tests([thread_exit]).

:- begin_tests(thread_exit).

%!  can_alert_thread is semidet.
%
%   True if we thread_signal/2 can immediately   alert  a thread that is
%   blocked in a system call. In this   case  the HTTP server is stopped
%   while (normally) being blocked in tcp_accept/3.

can_alert_thread :-
    current_prolog_flag(windows, true),
    !.
:- if(current_predicate(prolog_alert_signal/2)).
can_alert_thread :-
    prolog_alert_signal(Sig, Sig),
    Sig \== 0.
:- endif.

test(clean_exit,
     [ condition(can_alert_thread),
       Status == exit(0)
     ]) :-
    Goal = 'use_module(library(http/thread_httpd)),\c
            use_module(library(http/http_dispatch)),\c
            http_server(http_dispatch,[port(Port)]),\c
            http_stop_server(Port,[]),\c
            halt(0)',
    process_create(
        prolog(swipl),
        [ '-f', 'none', '--no-packs',
          '-g', Goal,
          '-t', 'halt(1)'
        ],
        [ process(Pid),
          stderr(pipe(Err))
        ]),
    read_stream_to_codes(Err, _),
    close(Err),
    process_wait(Pid, Status).

:- end_tests(thread_exit).

:- else.

test_thread_exit.

:- endif.
