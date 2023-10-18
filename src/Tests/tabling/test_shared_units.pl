/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2019-2023, University of Amsterdam
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

:- module(test_shared_units,
          [ test_shared_units/0
          ]).

:- if((exists_source(library(time)),
       current_prolog_flag(threads,true))).

:- use_module(library(plunit)).
:- use_module(library(debug)).
:- use_module(library(time)).

test_shared_units :-
    run_tests([ shared_reeval
              ]).

:- begin_tests(shared_reeval,
               [ sto(rational_trees),
                 condition(current_prolog_flag(threads, true))
               ]).

:- table (p/1, q/1) as (incremental,shared).
:- dynamic d/1 as incremental.

p(X) :- q(X).
q(X) :- d(X), X < 10.

ps(Ok) :-
    findall(X, p(X), Xs0),
    sort(Xs0, Xs),
    assertion(Ok =@= Xs).

test(propagate_falsecount) :-
    retractall(d(_)),
    assert(d(1)),
    ps([1]),
    assert(d(11)),
    ps([1]),
    thread_create(call_with_time_limit(1, ps([1])), Id),
    thread_join(Id).

:- end_tests(shared_reeval).

:- else.                                % no library(time) or no threads.

test_shared_units.

:- endif.
