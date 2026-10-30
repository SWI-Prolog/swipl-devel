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

:- module(test_local_blobs,
          [ test_local_blobs/0
          ]).
:- use_module(library(plunit)).

/** <module> Test thread-local blobs

Uses '$local_test_blob'/1, which creates  a   blob  owned by the calling
thread, and '$local_atoms'/2, which  provides   the  statistics  of the
local sweep.
*/

test_local_blobs :-
    run_tests([ local_blobs
              ]).

:- dynamic
    kept/1.

churn(N) :-
    forall(between(1, N, _), '$local_test_blob'(_)).

reclaimed(N, Collected) :-
    '$local_atoms'(collected, C0),
    churn(N),
    '$local_atoms'(collected, C1),
    Collected is C1-C0.

%   Create N local blobs that stay alive until the thread terminates.

hold(N, Parent) :-
    length(L, N),
    maplist('$local_test_blob', L),
    thread_self(Me),
    thread_send_message(Parent, ready(Me)),
    thread_get_message(release),
    length(L, N).

hold_in_thread(N, Id) :-
    thread_self(Me),
    thread_create(hold(N, Me), Id),
    thread_get_message(ready(Id)).

release_thread(Id) :-
    thread_send_message(Id, release),
    thread_join(Id).

%   Create N local blobs, sweep while they are alive and return the
%   number of chunks we own.

held_chunks(N, Chunks) :-
    length(L, N),
    maplist('$local_test_blob', L),
    garbage_collect_atoms,
    '$local_atoms'(chunks, Chunks),
    length(L, N).			% keep L alive

%   Run Goal in a new thread, which has fresh sweep statistics.

:- meta_predicate in_thread(0).

in_thread(Goal) :-
    current_prolog_flag(threads, true),
    !,
    thread_self(Me),
    thread_create(( call(Goal)
                  -> thread_send_message(Me, done(Goal))
                  ;  thread_send_message(Me, failed)
                  ), Id),
    thread_join(Id),
    thread_get_message(Reply),
    Reply = done(Goal).
in_thread(Goal) :-                      % single threaded: use an engine
    engine_create(Goal, Goal, E),
    call_cleanup(engine_next(E, Goal), engine_destroy(E)).

:- begin_tests(local_blobs, [cleanup(retractall(kept(_)))]).

test(reclaim, [true(Collected > 15_000), true(Live < 2_048)]) :-
    reclaimed(20_000, Collected),
    '$local_atoms'(live, Live).
test(no_global_atoms, true(Grown < 100)) :-
    statistics(atoms, A0),
    churn(20_000),
    statistics(atoms, A1),
    Grown is A1-A0.
test(on_stack, Type == local_test) :-
    '$local_test_blob'(Keep),
    churn(20_000),
    garbage_collect_atoms,
    blob(Keep, Type).
test(registered, Types == [local_test]) :-
    assert_local_blob,			% only the clause refers to it
    churn(20_000),
    garbage_collect_atoms,
    findall(Type, (kept(K), blob(K, Type)), Types),
    retractall(kept(_)).
test(global_agc_skips, true(Collected > 400)) :-
    garbage_collect_atoms,		% start with a fresh threshold
    churn(500),				% below the threshold: no sweep
    '$local_atoms'(collected, C0),
    in_thread(garbage_collect_atoms),	% global AGC only
    garbage_collect_atoms,		% our sweep
    '$local_atoms'(collected, C1),
    Collected is C1-C0.
test(empty_chunks_returned, [true(Held > 300), true(Chunks < 50)]) :-
    held_chunks(20_000, Held),		% about 313 chunks
    garbage_collect,			% remove the list from the stack
    garbage_collect_atoms,
    '$local_atoms'(chunks, Chunks).
test(exit_releases, true(Grown < 100)) :-
    statistics(atoms, A0),
    in_thread(churn(500)),		% below the threshold: no sweep
    statistics(atoms, A1),
    Grown is A1-A0.
test(pool_reuse, [condition(current_prolog_flag(threads, true)),
                  true(Pool2 < Pool1 + Pool1//2)]) :-
    hold_in_thread(20_000, Id1),
    release_thread(Id1),
    '$local_atoms'(pool, Pool1),
    Pool1 > 300,
    hold_in_thread(20_000, Id2),
    release_thread(Id2),
    '$local_atoms'(pool, Pool2).
test(findall, Type == local_test) :-
    findall(B, '$local_test_blob'(B), [B1]),
    churn(20_000),
    garbage_collect_atoms,
    blob(B1, Type).
test(findall_bag, true(Types == Expected)) :-
    findall(B, ( between(1, 50, _),
                 '$local_test_blob'(B),
                 churn(2_000)		% sweeps while B is in the bag
               ), Bs),
    maplist(blob, Bs, Types),
    length(Bs, Len),
    length(Expected, Len),
    maplist(=(local_test), Expected).
test(thread_exit, Type == local_test) :-
    in_thread(( churn(3_000),
                '$local_test_blob'(B),
                assertz(kept(B))
              )),
    garbage_collect_atoms,
    kept(K),
    blob(K, Type),
    retractall(kept(_)).
test(local, true) :-
    '$local_test_blob'(B),
    '$local_atom'(B).
test(gvar_local, [true(Local == true), true(Type == local_test)]) :-
    set_gvar_blob,			% global variables are thread-local
    nb_getval(local_blob_test, B0),
    local_flag(B0, Local),
    churn(20_000),
    garbage_collect_atoms,
    nb_getval(local_blob_test, B),
    blob(B, Type),
    nb_delete(local_blob_test).
test(promote_assert, [true(Local == false), true(Type == local_test)]) :-
    retractall(kept(_)),
    assert_local_blob,			% promotes
    kept(B),
    local_flag(B, Local),
    churn(20_000),
    garbage_collect_atoms,
    kept(K),
    blob(K, Type),
    retractall(kept(_)).
test(promote_message, [condition(current_prolog_flag(threads, true)),
                       true(Local == false), true(Type == local_test)]) :-
    promote_by_message(B),
    local_flag(B, Local),
    blob(B, Type).
local_flag(Atom, Local) :-
    (   '$local_atom'(Atom)
    ->  Local = true
    ;   Local = false
    ).

%   Whether the sweep would reclaim the blob depends on stale words that
%   the conservative stack scan may find, so we test the promotion itself.

test(promote_answer, [true(Locals == [false]), true(Types == [local_test])]) :-
    abolish_all_tables,
    \+ \+ tabled_blob(_),		% the answer trie is the only reference
    churn(20_000),
    garbage_collect,
    garbage_collect_atoms,
    findall(K, (tabled_blob(k(K)), blob(K, _)), Ks),
    maplist(local_flag, Ks, Locals),
    maplist(blob, Ks, Types).
test(concurrent_agc, [condition(current_prolog_flag(threads, true)),
                      Type == local_test]) :-
    thread_create(atoms_and_agc(20), Id),
    '$local_test_blob'(Keep),
    forall(between(1, 20, _),
           ( churn(5_000),
             garbage_collect_atoms
           )),
    thread_join(Id),
    blob(Keep, Type).

assert_local_blob :-
    '$local_test_blob'(B),
    assertz(kept(B)).

%   promote_by_message(-Blob)
%
%   A thread creates Blob, sends it to us and sweeps before we use it.

promote_by_message(B) :-
    thread_self(Me),
    thread_create(send_blob(Me), Id),
    thread_get_message(blob(B)),
    thread_get_message(swept),
    thread_join(Id).

send_blob(To) :-
    '$local_test_blob'(B),
    thread_send_message(To, blob(B)),
    churn(5_000),
    garbage_collect,
    garbage_collect_atoms,
    thread_send_message(To, swept).

%   The answer trie of a table is private, so it uses chunked nodes once it
%   is large enough.  The keys of chunked nodes are not registered.

:- table tabled_blob/1.

tabled_blob(k(X)) :- between(1, 1_000, X).
tabled_blob(k(B)) :- '$local_test_blob'(B).

set_gvar_blob :-
    '$local_test_blob'(B),
    nb_setval(local_blob_test, B).

atoms_and_agc(N) :-
    forall(between(1, N, I),
           ( forall(between(1, 1_000, J),
                    atom_concat(I, J, _)),
             garbage_collect_atoms
           )).

:- end_tests(local_blobs).
