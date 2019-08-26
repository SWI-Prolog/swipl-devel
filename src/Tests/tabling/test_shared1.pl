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

:- module(test_shared1,
          [ test_shared1/0
          ]).
:- if(current_prolog_flag(threads, true)).

:- use_module(library(statistics)).
:- use_module(library(debug)).
:- use_module(library(random)).
:- use_module(library(lists)).
:- use_module(library(random)).

:- meta_predicate
    with(0,0,+).

% setup debug messages
%:- set_prolog_flag(message_context, [thread, time]).
%:- debug(sh).
%:- debug(bg).
:- debug(wrong).
%:- debug(tabling(deadlock)).
user:message_property(debug(wrong), color([hfg(red)])).
user:message_property(debug(bg),    color([bg(red),fg(white)])).

:- dynamic failed/0.

test_shared1 :-
    retractall(failed),
    test_deadlock,
    \+ failed,
    test_abolish,
    \+ failed.

%!  p(-X) is nondet.
%!  q(-X) is nondet.
%
%   The core program: two mutual dependent   shared  tables. Both have a
%   short sleep at the start such  that   if  two  threads start on both
%   predicate it is unlikely that one   thread  gets both predicates and
%   thus we are likely to create a deadlock.

:- table (p/1, q/1) as shared.

max(10).

p(Y) :- sleep(0.01), q(X), max(Max), X < Max, Y is X+1.
p(0).
q(Y) :- sleep(0.01), p(X), max(Max), X < Max, Y is X+1.
q(0).

%!  ok(+Xs)
%
%   Validate the answer.  If wrong, abort the process.

ok(Xs) :-
    sort(Xs, Sorted),
    max(Max),
    numlist(0, Max, Sorted),
    !.
ok(Xs) :-
    debug(wrong, 'WRONG: ~q', [Xs]),
    assert(failed).
%   halt(abort).

%!  bp is det.
%!  bq is det.
%
%   Run p/1 or q/1 and validate the result.

bp :-
    debug(sh, 'Using p/1', []),
    findall(X, p(X), Xs),
    ok(Xs).
bq :-
    debug(sh, 'Using q/1', []),
    findall(X, q(X), Xs),
    ok(Xs).

%!  b
%
%   Run either bp/0 or bq/0 randomly.

b :- maybe, !, bp.
b :- bq.

%!  test_deadlock
%
%   Run into a sure deadlock between p/1 and q/1 to test the recovery.

test_deadlock :-
%   prolog_debug(msg_tabling_shared),
%   prolog_debug(msg_tabling_vtrie_dependencies),
    thread_create(bp, T1),
    thread_create(bq, T2),
    thread_join(T1),
    thread_join(T2).

%!  t(+Count)
%
%   Run Count threads, randomly starting at p/1 or q/1.

t(N) :-
    length(Tries, N),
    maplist(thread_create(b), Tries),
    maplist(thread_join, Tries).

%!  l(+Iterations, +Threads)
%
%   Run t(Threads) Iterations times.

l(N, M) :-
    N > 0,
    !,
    abolish_all_tables,
    t(M),
    N2 is N - 1,
    l(N2, M).
l(_,_).

test_abolish :-
    with(l(50, 4), abolish_all_tables, random(0.01, 0.1)).

%!  with(:Goal, :Concurrent, +TimeSpec)
%
%   Run Goal and call Concurrent   repeatedly, sleeping TimeSpec seconds
%   between the calls.
%
%   @arg TimeSpec is either a number or a callable term for which
%   call(TimeSpec,Time) produces a number.

with(Goal, Concurrent, Sleep) :-
    setup_call_cleanup(
        thread_create(concur(Concurrent, Sleep), Id),
        Goal,
        ( thread_signal(Id, throw(stop)),
          thread_join(Id, _)
        )).

concur(Concurrent, TimeSpec) :-
    repeat,
    (   number(TimeSpec)
    ->  sleep(TimeSpec)
    ;   call(TimeSpec, Time),
        sleep(Time)
    ),
    debug(bg, 'Running ~p', [Concurrent]),
    call(Concurrent),
    debug(bg, 'Done ~p', [Concurrent]),
    fail.

:- else.

test_shared1.

:- endif.
