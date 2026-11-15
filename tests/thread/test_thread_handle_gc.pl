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


:- module(test_thread_handle_gc,
          [ test_thread_handle_gc/0
          ]).
:- use_module(library(plunit)).
:- use_module(library(apply)).

/** <module> Atom-GC of thread handles

Threads that complete without being joined lose their handle to atom-GC,
which queues them for the thread that joins them.  Concurrently, other
threads enumerate all threads, which creates new handles for these
threads, and run atom-GC.  This crashed on handles that atom-GC was
destroying, on handles created for threads that were already freed and
on the thread-local data of completed threads.
*/

test_thread_handle_gc :-
    run_tests([ thread_handle_gc ]).

:- begin_tests(thread_handle_gc,
               [ condition(current_prolog_flag(threads, true))
               ]).

test(concurrent, Status == [true,true,true,true,true]) :-
    N = 1000,
    thread_create(spawn(N), W1, []),
    thread_create(spawn(N), W2, []),
    thread_create(enumerate(N), E1, []),
    thread_create(enumerate(N), E2, []),
    thread_create(agc(N), A, []),
    maplist(thread_join, [W1,W2,E1,E2,A], Status),
    garbage_collect_atoms.

:- end_tests(thread_handle_gc).

spawn(N) :-
    forall(between(1, N, _),
           thread_create(true, _, [])).

enumerate(N) :-
    forall(between(1, N, _),
           forall(catch(thread_property(_, status(_)), _, true), true)).

agc(N) :-
    forall(between(1, N, _), garbage_collect_atoms).
