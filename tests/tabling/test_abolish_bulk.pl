/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
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

:- module(test_abolish_bulk,
          [ test_abolish_bulk/0
          ]).
:- use_module(library(plunit)).
:- use_module(library(lists)).

/** <module> Test abolishing all private tables in bulk

Abolishing all tables of a thread or terminating a thread releases its
private tables in bulk: the IDG edges between private nodes are not
removed one by one and the registry of IDG nodes is released at once
(see clear_variant_table() in pl-tabling.c).  These tests verify that
the shared tables and the remaining state are consistent afterwards.
They are most useful in an AddressSanitizer build.
*/

test_abolish_bulk :-
    run_tests([ abolish_bulk
              ]).

threads :-
    current_prolog_flag(threads, true).

%   A private incremental table over a shared incremental table.  The
%   IDG node of ps/1 has a shared child.

:- dynamic sd/1 as incremental.
:- table ss/1 as (shared, incremental).
:- table ps/1 as incremental.

sd(1).

ss(X) :- sd(X).
ps(X) :- ss(X).

%   The updates are in this module: assert/1 in a test adds to the
%   module of the unit.

set_sd(L) :-
    retractall(sd(_)),
    forall(member(X, L), assertz(sd(X))).
add_sd(X) :-
    assertz(sd(X)).

%   Conditional answers that delay on other tables

:- table cu/1, cw/0, cv/1.

cu(X) :- member(X, [1,2]), tnot(cw).
cw :- tnot(cw).
cv(X) :- cu(X).

%   Monotonic tables

:- dynamic md/1 as monotonic.
:- table (mp/1, mq/1) as monotonic.

mp(X) :- md(X).
mq(X) :- mp(X).

set_md(L) :-
    retractall(md(_)),
    forall(member(X, L), assertz(md(X))).
add_md(X) :-
    assertz(md(X)).

%   Many incremental tables over dynamic calls

:- dynamic fd/1 as incremental.
:- table (fp/1, fq/1) as incremental.

fd(1).

fp(I) :- fd(X), X >= 0, I >= 0.
fq(N) :- forall(between(1, N, I), fp(I)).

fill(N) :-
    abolish_all_tables,
    fq(N).

add_fd(X) :-
    assertz(fd(X)).
del_fd(X) :-
    retract(fd(X)).

%   abolish_and_query(-Shared, -Private)
%
%   Abolish the private tables and update the shared table that a
%   private table depended on.

abolish_and_query(Shared, Private) :-
    set_sd([1]),
    findall(X, ps(X), _),
    abolish_private_tables,
    add_sd(2),
    findall(X, ss(X), Shared0), msort(Shared0, Shared),
    findall(X, ps(X), Private0), msort(Private0, Private).

shared_affected(Count) :-
    current_table(test_abolish_bulk:V, T),
    V = ss(_),
    !,
    aggregate_all(count, '$idg_edge'(T, affected, _), Count).

:- begin_tests(abolish_bulk, [cleanup(abolish_all_tables)]).

test(private_over_shared, [ condition(threads),
                            [S1,P1,S2,P2] == [[1,2],[1,2],[1,2],[1,2]]
                          ]) :-
    abolish_and_query(S1, P1),
    abolish_and_query(S2, P2).
test(shared_edges_removed, [condition(threads), Count == 0]) :-
    set_sd([1]),
    findall(X, ps(X), _),
    abolish_private_tables,
    shared_affected(Count).
test(thread_exit, [ condition(threads),
                    [Status,L] == [true, [1,2,3]]
                  ]) :-
    set_sd([1,2]),
    thread_create(findall(X, ps(X), _), Id),
    thread_join(Id, Status),
    add_sd(3),
    findall(X, ss(X), L0), msort(L0, L).
test(conditional, [L1-L2 == [cv(1),cv(2)]-[cv(1),cv(2)]]) :-
    abolish_all_tables,
    findall(cv(X), cv(X), L10), msort(L10, L1),
    abolish_all_tables,
    findall(cv(X), cv(X), L20), msort(L20, L2),
    abolish_all_tables.
test(monotonic, [L1-L2 == [1,2]-[1,2,3]]) :-
    set_md([1]),
    abolish_all_tables,
    findall(X, mq(X), _),
    add_md(2),
    findall(X, mq(X), L10), msort(L10, L1),
    abolish_all_tables,
    findall(X, mq(X), _),
    add_md(3),
    findall(X, mq(X), L20), msort(L20, L2),
    abolish_all_tables,
    set_md([]).
test(registry, [ true(N1 > 1000),
                 N2-E2 == 0-0,
                 true(N3 > 1000)
               ]) :-
    thread_self(Me),
    fill(1000),
    '$idg_thread_summary'(Me, N1, _),
    abolish_all_tables,
    '$idg_thread_summary'(Me, N2, E2),
    fq(1000),
    '$idg_thread_summary'(Me, N3, _),
    abolish_all_tables.
test(update_after_abolish, [L == [fp(1),fp(2)]]) :-
    fill(10),
    abolish_all_tables,
    findall(fp(I), (between(1,2,I), fp(I)), _),
    add_fd(2),
    findall(fp(I), (between(1,2,I), fp(I)), L),
    del_fd(2).
test(space, [condition(threads), true(After*10 < Before)]) :-
    thread_create(space, Id),
    thread_join(Id, exited(Before-After)).

space :-
    fill(1000),
    statistics(table_space_used, Before),
    abolish_all_tables,
    agc_settled(Before, 100, After),
    thread_exit(Before-After).

%   agc_settled(+Before, +N, -After)
%
%   Atom-GC frees the answer tries.  Wait at most N tenths of a second
%   until it did.

agc_settled(Before, N, After) :-
    garbage_collect_atoms,
    statistics(table_space_used, Used),
    (   ( Used*10 < Before ; N == 0 )
    ->  After = Used
    ;   sleep(0.1),
        N2 is N-1,
        agc_settled(Before, N2, After)
    ).

:- end_tests(abolish_bulk).
