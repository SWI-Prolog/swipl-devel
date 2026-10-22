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

:- module(test_thread_property,
          [ test_thread_property/0
          ]).
:- use_module(library(aggregate)).
:- use_module(library(debug)).

:- thread_local p/1, q/1.

%!  test_thread_property
%
%   Tests asynchronous polling for  thread   properties  while (notably)
%   threads  terminate.  Checks the size(Bytes) property and the size of
%   the copies of a thread-local predicate in other threads.

test_thread_property :-
    test_local_size,
    test_thread_property(100).

%   The copy of p/1 in another thread has 100 clauses.  The main thread
%   has no copy unless it used p/1.

test_local_size :-
    thread_create(( forall(between(1, 100, I), assertz(p(I))),
                    thread_get_message(done)
                  ), Id, []),
    wait_clauses(Id, 100),
    '$thread_local_predicate_size'(test_thread_property:p(_), Id, Size),
    assertion(Size > 100*24),
    thread_send_message(Id, done),
    thread_join(Id),
    assertion(\+ '$thread_local_predicate_size'(test_thread_property:p(_),
                                                Id, _)).

wait_clauses(Id, N) :-
    (   '$thread_local_clause_count'(test_thread_property:p(_), Id, N)
    ->  true
    ;   sleep(0.01),
        wait_clauses(Id, N)
    ).

test_thread_property(N) :-
    thread_create(create_threads(N), Id, []),
    thread_create(poll_threads, Id2, []),
    thread_join(Id),
    thread_send_message(Id2, done),
    thread_join(Id2).

create_threads(N) :-
    forall(between(1, N, _),
           ( thread_create(tmain, Id, [detached(true)]),
             catch(( thread_send_message(Id, hello(world)),
                     thread_send_message(Id, done)),
                   E,
                   print_message(warning, E)),
             wait_drain
           )).

wait_drain :-
    (   statistics(threads, N),
        N < 10
    ->  true
    ;   sleep(0.01),
        wait_drain
    ).

tmain :-
    forall(between(1, 100, I),
           ( assert(p(I)),
             assert(q(I)))).

poll_threads :-
    thread_self(Me),
    thread_get_message(Me, done, [timeout(0)]),
    !.
poll_threads :-
    aggregate_all(sum(S),
                  thread_property(_, size(S)),
                  Sum),
    aggregate_all(sum(S),
                  ( thread_property(Id, id(_)),
                    '$thread_local_predicate_size'(test_thread_property:p(_),
                                                   Id, S)
                  ),
                  LocalSum),
    debug(thread(size), 'Sum = ~D, p/1: ~D', [Sum, LocalSum]),
    poll_threads.

