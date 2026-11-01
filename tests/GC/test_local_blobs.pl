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
    run_tests([ local_blobs,
                local_margin,
                local_find,
                local_streams,
                local_text,
                local_thread_local
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
    in_thread(( reclaimed(20_000, Collected), % fresh sweep threshold
                '$local_atoms'(live, Live)
              )).
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


%   gc_margin for local blobs.  '$local_unique_test_blob'(+Units, -B)
%   creates a PL_BLOB_UNIQUE|PL_BLOB_NOCOPY|PL_BLOB_LOCAL blob as
%   foreign code referencing external objects does, counting Units
%   towards the gc_margin of its type.  20 blobs are far below the
%   count that triggers a local sweep.  Each test runs in a new thread
%   for fresh sweep statistics.

%   Local is true if B is a local atom.

local_flag(B, Local) :-
    (   '$local_atom'(B)
    ->  Local = true
    ;   Local = false
    ).

:- begin_tests(local_margin,
               [ setup('$local_unique_test_blob'(1, _)), % registers the type
                 cleanup(set_blob_type(local_unique_test, gc_margin(0)))
               ]).

margin_churn(Margin, Units, N, Sweeps, Collected) :-
    set_blob_type(local_unique_test, gc_margin(Margin)),
    in_thread(margin_churn(Units, N, Sweeps, Collected)).

margin_churn(Units, N, Sweeps, Collected) :-
    '$local_atoms'(sweeps, S0),
    '$local_atoms'(collected, C0),
    forall(between(1, N, _), '$local_unique_test_blob'(Units, _)),
    '$local_atoms'(sweeps, S1),
    '$local_atoms'(collected, C1),
    Sweeps is S1-S0,
    Collected is C1-C0.

margin_hold(Margin, Units, N, Sweeps, Collected) :-
    set_blob_type(local_unique_test, gc_margin(Margin)),
    in_thread(margin_hold(Units, N, Sweeps, Collected)).

margin_hold(Units, N, Sweeps, Collected) :-
    '$local_atoms'(sweeps, S0),
    '$local_atoms'(collected, C0),
    length(L, N),
    maplist('$local_unique_test_blob'(Units), L),
    '$local_atoms'(sweeps, S1),
    '$local_atoms'(collected, C1),
    Sweeps is S1-S0,
    Collected is C1-C0,
    length(L, N).			% keep L alive

test(no_margin, [Sweeps == 0, Collected == 0]) :-
    margin_churn(0, 1_000_000, 20, Sweeps, Collected).
test(margin, [true(Sweeps >= 4), true(Sweeps =< 6), true(Collected >= 15)]) :-
    margin_churn(4_000_000, 1_000_000, 20, Sweeps, Collected).
test(margin_kept, [true(Sweeps >= 4), true(Sweeps =< 6), Collected == 0]) :-
    margin_hold(4_000_000, 1_000_000, 20, Sweeps, Collected).
test(margin_units, Units == 3_000_000) :-
    set_blob_type(local_unique_test, gc_margin(100_000_000)),
    in_thread(( length(L, 3),
                maplist('$local_unique_test_blob'(1_000_000), L),
                '$local_atoms'(units, Units),
                length(L, 3)
              )).

:- end_tests(local_margin).


%   Finding local blobs by enumeration and by their printed form.  The
%   owner enumerates its local blobs with current_blob/2 and they stay
%   local.  Other threads do not see them: enumerating would promote
%   them and their slots may be reused under our feet.  read_term/2,3
%   with blob(resolve) finds the local PL_BLOB_UNIQUE|PL_BLOB_NOCOPY blob
%   of another thread in the atom table, which promotes it.

:- begin_tests(local_find).

test(enum_own, [Found == true, Local == true]) :-
    in_thread(enum_own(Found, Local)).

enum_own(Found, Local) :-
    '$local_unique_test_blob'(1, B),
    (   current_blob(B2, local_unique_test), B2 == B
    ->  Found = true
    ;   Found = false
    ),
    local_flag(B, Local).

test(enum_other, [condition(current_prolog_flag(threads, true)),
                  Local == true, Found == false]) :-
    thread_self(Me),
    thread_create(owner(Me), Id),
    thread_get_message(created(_Text)),
    findall(B, current_blob(B, local_unique_test), Bs),
    thread_send_message(Id, check),     % before sending Bs promotes them
    thread_get_message(local(Local)),
    thread_send_message(Id, member(Bs)),
    thread_get_message(member(Found)),
    thread_send_message(Id, done),
    thread_join(Id).

test(resolve_own, [Same == true, Local == true]) :-
    in_thread(resolve_own(Same, Local)).

resolve_own(Same, Local) :-
    '$local_unique_test_blob'(1, B),
    format(string(Text), '~q', [B]),
    term_string(B2, Text, [blob(resolve)]),
    ( B2 == B -> Same = true ; Same = false ),
    local_flag(B, Local).

test(resolve_other, [condition(current_prolog_flag(threads, true)),
                     Type == local_unique_test, Same == true,
                     Local == false]) :-
    thread_self(Me),
    thread_create(owner(Me), Id),
    thread_get_message(created(Text)),
    term_string(B, Text, [blob(resolve)]),
    blob(B, Type),
    thread_send_message(Id, same(B)),
    thread_get_message(same(Same)),
    thread_send_message(Id, check),
    thread_get_message(local(Local)),
    thread_send_message(Id, done),
    thread_join(Id).

%   Create a local blob, tell its printed form and answer questions
%   about it until told we are done.

owner(Parent) :-
    '$local_unique_test_blob'(1, B),
    format(string(Text), '~q', [B]),
    thread_send_message(Parent, created(Text)),
    owner_loop(Parent, B).

owner_loop(Parent, B) :-
    thread_get_message(Msg),
    (   Msg = same(B2)
    ->  ( B2 == B -> Same = true ; Same = false ),
        thread_send_message(Parent, same(Same)),
        owner_loop(Parent, B)
    ;   Msg = member(Bs)
    ->  ( memberchk_eq(B, Bs) -> Found = true ; Found = false ),
        thread_send_message(Parent, member(Found)),
        owner_loop(Parent, B)
    ;   Msg == check
    ->  local_flag(B, Local),
        thread_send_message(Parent, local(Local)),
        owner_loop(Parent, B)
    ;   Msg == done
    ->  true
    ).

memberchk_eq(X, [Y|T]) :-
    (   X == Y
    ->  true
    ;   memberchk_eq(X, T)
    ).

:- end_tests(local_find).


%   Stream handles are local blobs.  The sweep of the thread reclaims
%   them.  Another thread that finds the stream promotes its handle.
%   The local sweep does not close open streams: if agc_close_streams
%   is true it promotes the handle and leaves closing to global atom
%   garbage collection, as closing may call Prolog.

:- begin_tests(local_streams).

stream_churn(N) :-
    forall(between(1, N, _),
           ( open_string("a.", S), read(S, _), close(S) )).

test(local_handle, Local == true) :-
    setup_call_cleanup(
        open_null_stream(S),
        local_flag(S, Local),
        close(S)).
test(reclaimed, true(Collected >= 4_000)) :-
    in_thread(stream_reclaimed(5_000, Collected)).

stream_reclaimed(N, Collected) :-
    '$local_atoms'(collected, C0),
    stream_churn(N),
    '$local_atoms'(collected, C1),
    Collected is C1-C0.

test(alias_promotes, [condition(current_prolog_flag(threads, true)),
                      Before == true, After == false]) :-
    thread_self(Me),
    thread_create(alias_owner(Me), Id),
    thread_get_message(local(Before)),
    stream_property(_, alias(local_stream_test)),
    thread_send_message(Id, check),
    thread_get_message(local(After)),
    thread_join(Id).

alias_owner(Parent) :-
    open_null_stream(S),
    set_stream(S, alias(local_stream_test)),
    local_flag(S, L0),
    thread_send_message(Parent, local(L0)),
    thread_get_message(check),
    local_flag(S, L1),
    thread_send_message(Parent, local(L1)),
    close(S).

test(no_local_close, [Open == true, Local == false]) :-
    current_prolog_flag(agc_close_streams, Old),
    setup_call_cleanup(
        set_prolog_flag(agc_close_streams, true),
        in_thread(dropped_open_stream(Open, Local)),
        set_prolog_flag(agc_close_streams, Old)).

%   Open a stream and drop its handle, without closing.  Sweep locally
%   until the handle was considered, then find the stream back from its
%   printed form.

dropped_open_stream(Open, Local) :-
    drop_open_stream(Text),
    '$local_atoms'(sweeps, S0),
    sweep_until(S0),
    term_string(S, Text, [blob(resolve)]),
    (   is_stream(S)
    ->  Open = true,
        local_flag(S, Local),
        close(S)
    ;   Open = false,
        Local = unknown
    ).

drop_open_stream(Text) :-		% backtracking drops the handle
    findall(T, ( open_null_stream(S),
                 format(string(T), '~q', [S])
               ), [Text]).

sweep_until(S0) :-
    stream_churn(2_000),
    '$local_atoms'(sweeps, S1),
    (   S1 > S0+1
    ->  true
    ;   sweep_until(S0)
    ).

:- end_tests(local_streams).


%   Text atoms that PL_unify_text() creates, e.g., by atom_concat/3, are
%   local atoms.  They are unique: if another thread creates the same
%   text it finds the local atom and promotes it.  Storing them in shared
%   data promotes them.  Atoms created with a reference that the caller
%   may keep, such as by the reader, are ordinary atoms.

:- begin_tests(local_text).

:- dynamic lt_fact/1.

text_local(Prefix, I, A, Local) :-
    atom_concat(Prefix, I, A),
    local_flag(A, Local).

test(atom_concat, Local == true) :-
    in_thread(text_local(lt_concat_, 1, _, Local)).
test(same_text, [condition(current_prolog_flag(threads, true)),
                 Mine == false, Theirs == false]) :-
    thread_self(Me),
    thread_create(text_owner(Me, lt_shared_, 42), Id),
    thread_get_message(created),
    text_local(lt_shared_, 42, _, Mine),
    thread_send_message(Id, check),
    thread_get_message(local(Theirs)),
    thread_join(Id).
test(assert, [Before == true, After == false,
              cleanup(retractall(lt_fact(_)))]) :-
    in_thread(( text_local(lt_assert_, 1, A, Before),
                assertz(lt_fact(A)),
                local_flag(A, After)
              )).
test(functor, [Before == true, After == false]) :-
    in_thread(( text_local(lt_functor_, 1, A, Before),
                functor(_, A, 2),
                local_flag(A, After)
              )).
test(reclaimed, [true(Collected > 15_000), true(Grown < 1_000)]) :-
    in_thread(( statistics(atoms, A0),
                '$local_atoms'(collected, C0),
                forall(between(1, 20_000, I), atom_concat(lt_churn_, I, _)),
                '$local_atoms'(collected, C1),
                statistics(atoms, A1),
                Collected is C1-C0,
                Grown is A1-A0
              )).
test(read, Local == true) :-
    in_thread(( term_string(A, "lt_read_atom_xyz"),
                local_flag(A, Local)
              )).
test(read_quoted, Local == true) :-
    in_thread(( term_string(A, "'lt read quoted'"),
                local_flag(A, Local)
              )).
test(read_arg, Local == true) :-
    in_thread(( term_string(f(A), "f(lt_read_arg)"),
                local_flag(A, Local)
              )).
test(read_functor, Local == false) :-
    in_thread(( term_string(T, "lt_read_functor(x)"),
                functor(T, A, _),
                local_flag(A, Local)
              )).
test(read_assert, [Before == true, After == false,
                   cleanup(retractall(lt_fact(_)))]) :-
    in_thread(( term_string(A, "lt_read_assert"),
                local_flag(A, Before),
                assertz(lt_fact(A)),
                local_flag(A, After)
              )).
test(read_reclaimed, [true(Collected > 15_000), true(Grown < 1_000)]) :-
    in_thread(( statistics(atoms, A0),
                '$local_atoms'(collected, C0),
                forall(between(1, 20_000, I),
                       ( format(string(S), "lt_read_churn_~d", [I]),
                         term_string(_, S)
                       )),
                '$local_atoms'(collected, C1),
                statistics(atoms, A1),
                Collected is C1-C0,
                Grown is A1-A0
              )).
test(sig_atomic, true(Collected > 15_000)) :-
    in_thread(sig_atomic(( '$local_atoms'(collected, C0),
                           forall(between(1, 20_000, I),
                                  atom_concat(lt_atomic_, I, _)),
                           '$local_atoms'(collected, C1),
                           Collected is C1-C0
                         ))).
test(promoted, [true(Sweeps < 3), cleanup(retractall(lt_fact(_)))]) :-
    in_thread(( '$local_atoms'(sweeps, S0),
                forall(between(1, 20_000, I),
                       ( atom_concat(lt_promoted_, I, A),
                         assertz(lt_fact(A))
                       )),
                '$local_atoms'(sweeps, S1),
                Sweeps is S1-S0
              )).

%   Create the atom Prefix+I, tell Parent and tell whether it is still
%   local when asked.

text_owner(Parent, Prefix, I) :-
    text_local(Prefix, I, A, _),
    thread_send_message(Parent, created),
    thread_get_message(check),
    local_flag(A, Local),
    thread_send_message(Parent, local(Local)).

:- end_tests(local_text).


%   Only the owner can access the clauses of thread-local predicates.
%   Another thread that gets hold of a reference to such a clause gets a
%   permission error.

:- begin_tests(local_thread_local).

:- thread_local tl_fact/1, tl_rule/0.

test(clause, [ condition(current_prolog_flag(threads, true)),
               setup(other_ref(Ref, Id)), cleanup(end_ref(Id)),
               error(permission_error(access, thread_local_clause, _))
             ]) :-
    clause(_, _, Ref).
test(clause_property,
     [ condition(current_prolog_flag(threads, true)),
       setup(other_ref(Ref, Id)), cleanup(end_ref(Id)),
       error(permission_error(access, thread_local_clause, _))
     ]) :-
    clause_property(Ref, _).
test(nth_clause, [ condition(current_prolog_flag(threads, true)),
                   setup(other_ref(Ref, Id)), cleanup(end_ref(Id)),
                   error(permission_error(access, thread_local_clause, _))
                 ]) :-
    nth_clause(_, _, Ref).
test(erase, [ condition(current_prolog_flag(threads, true)),
              setup(other_ref(Ref, Id)), cleanup(end_ref(Id)),
              error(permission_error(access, thread_local_clause, _))
            ]) :-
    erase(Ref).
test(continuation, [condition(current_prolog_flag(threads, true)),
                    error(permission_error(call, thread_local_clause, _))]) :-
    thread_self(Me),
    thread_create(cont_owner(Me), Id),
    thread_get_message(cont(Cont)),
    call_cleanup(call(Cont),
                 ( thread_send_message(Id, done),
                   thread_join(Id)
                 )).
%   Get a reference to a clause of tl_fact/1 of thread Id.  Id stays
%   alive until end_ref/1: if it terminates, its clauses are erased.

other_ref(Ref, Id) :-
    thread_self(Me),
    thread_create(ref_owner(Me), Id),
    thread_get_message(ref(Ref)).

end_ref(Id) :-
    thread_send_message(Id, done),
    thread_join(Id).

ref_owner(Parent) :-
    assertz(tl_fact(tl_other_ref), Ref),
    thread_send_message(Parent, ref(Ref)),
    thread_get_message(done).

cont_owner(Parent) :-
    assertz((tl_rule :- shift(here), atom_length(tl_cont_atom, _))),
    reset(tl_rule, here, Cont),
    thread_send_message(Parent, cont(Cont)),
    thread_get_message(done).

:- end_tests(local_thread_local).
