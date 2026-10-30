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

/** <module> Handles to worklists and components

Prolog refers to tabling worklists and components using thread-local
blobs.  A handle to a freed object raises an existence error, and other
threads see a table that is being completed as `incomplete`.
*/

test_tbl_handles :-
    run_tests([ tbl_handles
              ]).

threads :-
    current_prolog_flag(threads, true).

:- dynamic
    saved/2.

:- table t/0, w/0.

t :-
    '$tbl_scc'(SCC),
    assertz(saved(scc, SCC)).

w :-
    '$tbl_variant_table'(VT),
    trie_gen(VT, test_tbl_handles:w, ATrie),
    '$tbl_table_status'(ATrie, WL),
    assertz(saved(wl, WL)).

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

:- begin_tests(tbl_handles, [cleanup(retractall(saved(_,_)))]).

test(handles, [true(blob(SCC, table_component)), true(blob(WL, worklist))]) :-
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
test(incomplete, [condition(threads), Status == incomplete]) :-
    abolish_all_tables,
    shared_status(Status).

:- end_tests(tbl_handles).
