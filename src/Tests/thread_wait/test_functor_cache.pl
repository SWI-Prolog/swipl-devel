/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2022, SWI-Prolog Solutions b.v.
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

:- module(test_functor_cache,
	  [ test_functor_cache/0
	  ]).
:- autoload(library(apply), [maplist/2]).
:- use_module(library(plunit)).

/** <module> Test functor caching and thread_wait

This  module verifies  that  PL_functor() cannot  return  a not  fully
established functor and tests  thread_wait/2 race conditions.  It uses
thread_wait/2 as that  is the best way to fire  many threads at nearly
the same time.
*/

test_functor_cache :-
    run_tests([ functor_cache
	      ]).


:- begin_tests(functor_cache).

test(race) :-
    current_prolog_flag(cpu_count, Cores),
    Threads is Cores*2,
    g(Threads).

:- table
       p/3.
:- dynamic
       go/0.

p(1,2,3).

g(N) :-
    length(Threads, N),
    maplist(thread_create(start), Threads),
    assert(go),
    maplist(thread_join, Threads).

start :-
    thread_wait(go, [wait_preds([go/0])]),
    p(_A,_B,_C).

:- end_tests(functor_cache).
