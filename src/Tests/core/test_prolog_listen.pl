/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2019, VU University Amsterdam
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

:- module(test_prolog_listen,
          [ test_prolog_listen/0
          ]).
:- use_module(library(plunit)).
:- use_module(library(debug)).

%:- debug(event).

test_prolog_listen :-
    run_tests(prolog_listen).

:- begin_tests(prolog_listen,
               [ condition(current_prolog_flag(threads, true))
               ]).

:- dynamic p/1.

test(at_exit_option) :-
    queue(Me),
    thread_create(true, Id, [at_exit(thread_send_message(Me, done))]),
    thread_join(Id),
    thread_get_message(Me, done, [timeout(1)]).
test(local_at_exit_event) :-
    queue(Me),
    thread_create(prolog_listen(this_thread_exit,
                                thread_send_message(Me, done)),
                  Id),
    thread_join(Id),
    thread_get_message(Me, done, [timeout(1)]).
test(global_at_exit_event) :-
    queue(Me),
    prolog_listen(thread_exit, send_event(Me)),
    thread_create(true, Id),
    thread_join(Id),
    assertion(thread_get_message(Me, done(Id), [timeout(1)])),
    prolog_unlisten(thread_exit, send_event(Me)),
    thread_create(true, Id2),
    thread_join(Id2),
    assertion(\+ thread_get_message(Me, done(Id2), [timeout(0.01)])).
test(erase_clause, [cleanup(prolog_unlisten(erase, send_event(Me)))]) :-
    queue(Me),
    prolog_listen(erase, send_event(Me)),
    (   asserta(p(a), Ref),
        erase(Ref),
        fail
    ;   true
    ),
    garbage_collect_clauses,
    (   thread_get_message(Me, done(Ref), [timeout(0)])
    ->  true
    ;   format('Clause not reclaimed (but this can happen)~n')
    ).
test(erase_record, [cleanup(prolog_unlisten(erase, send_event(Me)))]) :-
    queue(Me),
    prolog_listen(erase, send_event(Me)),
    (   recorda(p, a, Ref),
        erase(Ref),
        fail
    ;   true
    ),
    thread_get_message(Me, done(Ref), [timeout(0)]).
test(update, [ cleanup(prolog_unlisten(p/1, send_event(Me))),
               Events =@=
               [ done(assertz,PA),
                 done(assertz,PB),
                 done(retract,PB),
                 done(retractall,start(M:p(_))),
                 done(retract,PA),
                 done(retractall,end(M:p(_)))
               ]
             ]) :-
    context_module(M),
    queue(Me),
    prolog_listen(p/1, send_event(Me)),
    assert(p(a), PA),
    assert(p(b), PB),
    retract(p(b)),
    retractall(p(_)),
    get_events(Me, Events).

queue(Me) :-
    thread_self(Me),
    repeat,
      \+ thread_get_message(Me, _, [timeout(0)]),
    !.

get_events(Queue, [H|T]) :-
    thread_get_message(Queue, H, [timeout(0)]),
    !,
    get_events(Queue, T).
get_events(_, []).

send_event(Queue, Arg) :-
    debug(event, 'Received ~p', [Arg]),
    thread_send_message(Queue, done(Arg)).
send_event(Queue, Arg1, Arg2) :-
    debug(event, 'Received ~p, ~p', [Arg1, Arg2]),
    thread_send_message(Queue, done(Arg1, Arg2)).
send_event(Queue, Arg1, Arg2, Arg3) :-
    debug(event, 'Received ~p ~p ~p', [Arg1, Arg2, Arg3]),
    thread_send_message(Queue, done(Arg1, Arg2, Arg3)).

:- end_tests(prolog_listen).
