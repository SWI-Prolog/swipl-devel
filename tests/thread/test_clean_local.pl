/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2020, SWI-Prolog Solutions b.v.
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

:- module(test_clean_local,
          [ test_clean_local/0
          ]).
:- use_module(library(debug)).
:- use_module(library(thread)).

% :- debug(tcmalloc).

%!  test_clean_local
%
%   This test was created after a race  condition between a dying thread
%   reclaiming its clauses and the  clause   garbage  collector. It both
%   checks handling the clauses and the indexes.

test_clean_local :-
    test_clean_local(100).

:- thread_local
    p/1.

test_clean_local(N) :-
    mem(initial),
    thread_create(collector, Collector, []),
    length(L, N),
    concurrent_maplist(clause_maker, L),
    thread_signal(Collector, abort),
    thread_join(Collector, _),
    mem(join),
    garbage_collect_clauses,
    garbage_collect_clauses,
    mem(final).

collector:-
    repeat,
    garbage_collect_clauses,
    fail.

clause_maker(_) :-
    forall(between(1, 100, N), assert(p(N))),
    p(24),
    forall(between(1, 90, I), retract(p(I))).

mem(Msg) :-
    (   debugging(tcmalloc)
    ->  malloc_property('generic.current_allocated_bytes'(I)),
        format('~w: ~D bytes~n', [Msg, I])
    ;   true
    ).

