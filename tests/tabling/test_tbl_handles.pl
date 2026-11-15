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

:- module(test_tbl_handles,
          [ test_tbl_handles/0
          ]).
:- use_module(library(plunit)).

/** <module> Handles to worklists, components and private answer tries

Prolog refers to tabling worklists and components using integer
handles.  A handle to a freed object raises an existence error, also if
a new object reuses its slot, and other
threads see a table that is being completed as `incomplete`.  Private
answer tries also have thread-local handles.  Another thread that reaches
such a trie, e.g., through an IDG edge, gets the same handle, promoted.
*/

test_tbl_handles :-
    run_tests([ tbl_handles
              ]).

threads :-
    current_prolog_flag(threads, true).

:- dynamic
    saved/2.

:- table t/0, w/0, same/1, kind/0.

t :-
    '$tbl_scc'(SCC),
    assertz(saved(scc, SCC)).

same(Same) :-				% the component keeps its handle
    '$tbl_scc'(S1),
    garbage_collect_atoms,
    '$tbl_scc'(S2),
    (   S1 == S2 -> Same = true ; Same = false ).

kind :-					% a live worklist is no component
    '$tbl_variant_table'(VT),
    trie_gen(VT, test_tbl_handles:kind, ATrie),
    '$tbl_table_status'(ATrie, WL),
    '$tbl_scc_data'(WL, _).

w :-
    '$tbl_variant_table'(VT),
    trie_gen(VT, test_tbl_handles:w, ATrie),
    '$tbl_table_status'(ATrie, WL),
    assertz(saved(wl, WL)).

:- table w2/0.

w2 :-					% reuses the slot of w/0
    saved(wl, WL),
    '$tbl_wkl_table'(WL, _).

%   Promote a handle whose object is gone.  It must not get a reference
%   for the object: Refs is the number of references besides the clause.

promote_stale(Handle, Refs) :-
    assertz(saved(stale, Handle)),
    '$atom_references'(Handle, Refs0),
    Refs is Refs0-1,
    retractall(saved(_,_)).

:- table s/0 as shared.

s :-
    thread_send_message(main, started),
    thread_get_message(go).

shared_status(Status) :-
    thread_create(s, Id),
    thread_get_message(started),
    '$tbl_global_variant_table'(VT),
    trie_gen(VT, test_tbl_handles:s, ATrie),
    '$tbl_table_status'(ATrie, Status),
    thread_send_message(Id, go),
    thread_join(Id).

%   wipe(+Depth)
%
%   Overwrite the local stack above its top.  The sweep scans a margin
%   above the top for the frame of a call that is being built.

wipe(0) :- !.
wipe(N) :- N1 is N-1, wipe(N1), true.

%   Private answer tries

:- table p/1, q/1.

p(X) :- between(1, 3, X).
q(X) :- X > 0.

answer_trie(Goal, Trie) :-
    '$tbl_local_variant_table'(VT),
    once(trie_gen(VT, test_tbl_handles:Goal, Trie)).

local_flag(Atom, Local) :-
    (   '$local_atom'(Atom)
    ->  Local = true
    ;   Local = false
    ).

tables_with_handles(N) :-
    forall(between(1, N, I),
           ( q(I),
             answer_trie(q(I), _)
           )).

answers(Trie, Count) :-
    aggregate_all(count, trie_gen(Trie, _), Count).

%   A shared incremental table with a private dependent, so the shared
%   table has an IDG edge into the tables of the thread that calls pr/1.

:- dynamic([d/1], [incremental(true)]).
:- table s2/1 as (shared, incremental).
:- table pr/1 as incremental.

d(1).
s2(X) :- d(X).
pr(X) :- s2(X).

%   steal(+Hold, -Stolen, -Same, -Local)
%
%   A thread completes pr/1, optionally creating the handle of its answer
%   trie first.  We take the handle through the IDG edge.  Stolen tells
%   whether this handle is local, before sending it promotes it.  Same is
%   true if the thread sees the same handle afterwards and Local tells
%   whether it is still local.

steal(Hold, Stolen, Same, Local) :-
    thread_self(Me),
    abolish_all_tables,
    thread_create(own_pr(Hold, Me), Id),
    thread_get_message(ready),
    '$tbl_global_variant_table'(GVT),
    trie_gen(GVT, test_tbl_handles:s2(_), STrie),
    once('$idg_edge'(STrie, affected, Dep)),
    local_flag(Dep, Stolen),
    thread_send_message(Id, dep(Dep)),
    thread_get_message(result(Same, Local)),
    thread_join(Id),
    abolish_all_tables.

own_pr(Hold, Parent) :-
    forall(pr(_), true),
    (   Hold == true
    ->  answer_trie(pr(_), H0)
    ;   wipe(1_000),			% reclaim the symbol of the call
        garbage_collect_atoms
    ),
    thread_send_message(Parent, ready),
    thread_get_message(dep(Dep)),
    answer_trie(pr(_), H),
    (   Hold == true
    ->  ( H0 == H -> true ; Same = false )
    ;   true
    ),
    (   var(Same)
    ->  ( Dep == H -> Same = true ; Same = false )
    ;   true
    ),
    local_flag(H, Local),
    thread_send_message(Parent, result(Same, Local)).

:- begin_tests(tbl_handles, [cleanup(retractall(saved(_,_)))]).

test(handles, [true(integer(SCC)), true(integer(WL))]) :-
    abolish_all_tables,
    t, w,
    saved(scc, SCC),
    saved(wl, WL).
test(stale_component, error(existence_error(table_component, _))) :-
    abolish_all_tables,
    retractall(saved(_,_)),
    t,
    saved(scc, SCC),
    '$tbl_scc_data'(SCC, _).
test(stale_worklist, error(existence_error(worklist, _))) :-
    abolish_all_tables,
    retractall(saved(_,_)),
    w,
    saved(wl, WL),
    '$tbl_wkl_table'(WL, _).
test(identity, Same == true) :-
    abolish_all_tables,
    same(Same).
test(kind, error(type_error(table_component, _))) :-
    abolish_all_tables,
    kind.
test(incomplete, [condition(threads), Status == incomplete]) :-
    abolish_all_tables,
    shared_status(Status).
test(reused_slot, error(existence_error(worklist, _))) :-
    abolish_all_tables,
    retractall(saved(_,_)),
    w,
    w2.
test(trie_disowned, Refs == 0) :-
    abolish_all_tables,
    forall(p(_), true),
    answer_trie(p(_), T),
    abolish_all_tables,
    promote_stale(T, Refs).
test(trie_local, [Local == true, Answers == 3]) :-
    abolish_all_tables,
    forall(p(_), true),
    answer_trie(p(_), T),
    local_flag(T, Local),
    answers(T, Answers).
test(trie_no_global_atoms, true(Grown < 100)) :-
    abolish_all_tables,
    statistics(atoms, A0),
    tables_with_handles(5_000),
    statistics(atoms, A1),
    Grown is A1-A0,
    abolish_all_tables.
test(trie_reclaimed, [true(Collected > 15_000), Answers == 3]) :-
    abolish_all_tables,
    forall(p(_), true),
    '$local_atoms'(collected, C0),
    tables_with_handles(20_000),
    garbage_collect_atoms,
    '$local_atoms'(collected, C1),
    Collected is C1-C0,
    answer_trie(p(_), T),		% a new handle
    answers(T, Answers),
    abolish_all_tables.
test(trie_abolished, [true(is_trie(T)), Answers == 0]) :-
    abolish_all_tables,
    forall(p(_), true),
    answer_trie(p(_), T),
    abolish_all_tables,			% T now owns the trie
    tables_with_handles(20_000),
    garbage_collect_atoms,
    answers(T, Answers).
test(trie_promoted, [true(Local == false), true(is_trie(T))]) :-
    abolish_all_tables,
    retractall(saved(_,_)),
    forall(p(_), true),
    answer_trie(p(_), T0),
    assertz(saved(trie, T0)),		% promotes
    local_flag(T0, Local),
    abolish_all_tables,
    tables_with_handles(20_000),
    garbage_collect_atoms,
    saved(trie, T),
    retractall(saved(_,_)).
test(trie_steal_existing, [condition(threads),
                           Stolen-Same-Local == false-true-false]) :-
    steal(true, Stolen, Same, Local).
test(trie_steal_new, [condition(threads),
                      Stolen-Same-Local == false-true-false]) :-
    steal(false, Stolen, Same, Local).

:- end_tests(tbl_handles).
