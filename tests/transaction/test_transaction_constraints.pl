/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2022, VU University Amsterdam
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

:- module(test_transaction_constraints,
          [ test_transaction_constraints/0,
            test_transaction_constraints/4
          ]).

/** <module> Test transactions with constraints.

This module implements a rather  silly   constraint  to avoid concurrent
transactions from creating multiple values for   what should be a simple
fact with exactly one clause. Silly in the  sense that a lock around the
entire transaction is for this example a   much  simpler way to maintain
consistency.
*/

:- if(current_prolog_flag(threads, true)).

:- use_module(library(random)).
:- use_module(library(thread)).
:- use_module(library(debug)).

%:- debug(veto).
%:- debug(veto(constraint)).
:- debug(veto(verify)).

:- meta_predicate
    concurrent_with(0,0).

test_transaction_constraints :-
    test_transaction_constraints(1000, 2, 0, call),
    test_transaction_constraints(1000, 2, 0, snapshot).

%!  test_transaction_constraints(+N, +M, +Delay, +Snapshot)
%
%   Update a single fact (temperature/1) N   times, concurrently using M
%   threads.  An  additional  thread  checks  that  there  is  only  one
%   temperature/1 fact visible at any point in  time. It can do this two
%   ways: simple check (`call`) or doing   the check inside a _snapshot_
%   (`snapshot`).
%
%   @arg Delay sets a random delay 0..Delay between each update

test_transaction_constraints(N, M, Delay, Snapshot) :-
    retractall(temperature(_)),
    asserta(temperature(0)),
    flag(illegal_state, _, 0),
    concurrent_with(no_duplicate_temp_loop(Snapshot),
                    test(N, M, Delay)),
    get_flag(illegal_state, Count),
    (   Count == 0
    ->  true
    ;   format(user_error, 'Got ~D illegal states~n', [Count]),
        fail
    ).

:- dynamic
    temperature/1.

test(N, M, Delay) :-
    set_flag(conflict, 0),
    concurrent_forall(
        between(1, N, I),
        set_temp(I, Delay),
        [ threads(M)
        ]),
    get_flag(conflict, C),
    debug(veto, 'Resolved ~D conflicts', [C]).

%!  set_temp(+Degrees)
%
%   Update the single clause for temperature/1   using a transaction. As
%   two transactions be started that both want   to assert a new clause,
%   we must guard against this using   a  constraint. If the transaction
%   fails because the constraint it violated we just try again (we could
%   also generate an exception from  the   constraint  and not retry the
%   transaction).

set_temp(Degrees) :-
    repeat,
    transaction(update_temp(Degrees),
                no_duplicate_temp(constraint),
                temp),
    !.

%!  update_temp(+Term)
%
%   The classical Prolog way to update a fact.

update_temp(Temp) :-
    temperature(Temp),
    !.
update_temp(Temp) :-
    retractall(temperature(_)),
    asserta(temperature(Temp)).

%!  no_duplicate_temp(+Why)
%
%   Ensure there is only one clause.  This is our constraint.

no_duplicate_temp(Why) :-
    findall(Temp-CRef, clause(temperature(Temp),_,CRef), List),
    (   List = [_]
    ->  true
    ;   debug(veto(Why), '~w: ~p', [Why, List]),
        (   Why == verify
        ->  mutex_statistics
        ;   true
        ),
        flag(conflict, N, N+1),
        fail
    ).

set_temp(random, Delay) :-              % original test; not used now.
    !,
    A is random_float*Delay,
    sleep(A),
    random_between(-40, 40, Temp),
    set_temp(Temp).
set_temp(Temp, Delay) :-                % setting to concrete values make
    A is random_float*Delay,            % failures easier to interpret.
    sleep(A),
    tid(Id),
    set_temp(Id-Temp).

tid(Id) :-
    thread_self(Me),
    thread_property(Me, id(Id)).

concurrent_with(G1, G2) :-
    setup_call_cleanup(
        thread_create(G1, Id, []),
        G2,
        (   catch(thread_signal(Id, abort),
                  error(existence_error(thread, Id), _),
                  true),
            thread_join(Id, _)
        )).

no_duplicate_temp_loop(snapshot) :-
    (   snapshot(no_duplicate_temp(verify))
    ->  true
    ;   flag(illegal_state, C, C+1)
    ),
    no_duplicate_temp_loop(snapshot).
no_duplicate_temp_loop(call) :-
    (   no_duplicate_temp(verify)
    ->  true
    ;   flag(illegal_state, C, C+1)
    ),
    no_duplicate_temp_loop(call).

:- else.

test_transaction_constraints.
test_transaction_constraints(_,_,_,_).

:- endif.
