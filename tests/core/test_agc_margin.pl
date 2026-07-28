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

:- module(test_agc_margin,
          [ test_agc_margin/0
          ]).
:- use_module(library(plunit)).

/** <module> Test the per blob type atom-GC budget

agc_margin counts atoms, which says nothing about what they keep alive: an
atom of a hundred kilobytes costs the same as one of five.  A type may
declare a gc_margin, against which a blob counts the length it was created
with.  See set_blob_gc_margin/2.

The `text` type is used throughout because its length is the size of the
text, so the budget can be exercised from Prolog without a foreign type.
*/

test_agc_margin :-
    run_tests([ agc_margin_api,
                agc_margin_trigger,
                agc_unregister
              ]).

%!  atom_of(+Bytes, +Seq, -Atom) is det.
%
%   A distinct atom of about Bytes bytes.

atom_of(Bytes, Seq, Atom) :-
    length(Codes, Bytes),
    maplist(=(0'x), Codes),
    atom_codes(Base, Codes),
    atom_concat(Base, Seq, Atom).

make_atoms(Count, Bytes) :-
    forall(between(1, Count, I),
           ( atom_of(Bytes, I, A),
             atom_length(A, _) )).

%  The tests need a collection to happen at a point we can observe, so they
%  run without the GC thread.  Everything is restored afterwards: the text
%  margin is global state shared with the rest of the suite.

setup_agc(state(Thread, Margin)) :-
    current_prolog_flag(gc_thread, Thread),
    '$blob_gc_margin'(text, Margin, _),
    set_prolog_flag(gc_thread, false),
    garbage_collect_atoms.

cleanup_agc(state(Thread, Margin)) :-
    set_blob_gc_margin(text, Margin),
    set_prolog_flag(gc_thread, Thread).


:- begin_tests(agc_margin_api).

test(default_is_zero) :-
    forall('$blob_gc_margin'(_, Margin, _),
           assertion(Margin == 0)).

test(round_trip, [cleanup(set_blob_gc_margin(text, 0))]) :-
    set_blob_gc_margin(text, 4 000 000),
    '$blob_gc_margin'(text, Margin, _),
    assertion(Margin =:= 4 000 000).

test(enumerates_every_type) :-
    findall(T, '$blob_gc_margin'(T, _, _), Types),
    assertion(memberchk(text, Types)),
    assertion(memberchk(clause, Types)),
    sort(Types, Sorted),
    assertion(same_length(Types, Sorted)).      % no duplicates

test(unknown_type, [throws(error(existence_error(blob_type, _), _))]) :-
    set_blob_gc_margin(no_such_blob_type, 10).

:- end_tests(agc_margin_api).


:- begin_tests(agc_margin_trigger).

%  200 atoms of 100kB is 20MB but only 200 atoms, so the count based margin
%  never notices.  A byte margin does.

test(count_margin_ignores_size,
     [ setup(setup_agc(S)), cleanup(cleanup_agc(S)) ]) :-
    set_blob_gc_margin(text, 0),
    statistics(agc, C0),
    make_atoms(200, 100 000),
    statistics(agc, C1),
    assertion(C1 =:= C0).

test(byte_margin_collects,
     [ setup(setup_agc(S)), cleanup(cleanup_agc(S)) ]) :-
    set_blob_gc_margin(text, 4 000 000),
    statistics(agc, C0),
    make_atoms(200, 100 000),
    statistics(agc, C1),
    assertion(C1 > C0),
    '$blob_gc_margin'(text, _, Unregistered),
    assertion(Unregistered < 4 000 000).        % kept near the budget

%  A type whose blobs are live must stop asking rather than spin: the
%  survivors raise non_garbage, so the next request needs a further margin
%  worth of candidates.  20MB against a 4MB budget is 5 collections, not one
%  per atom.

test(live_atoms_do_not_thrash,
     [ setup(setup_agc(S)), cleanup(cleanup_agc(S)) ]) :-
    set_blob_gc_margin(text, 4 000 000),
    statistics(agc, C0),
    keep_atoms(200, 100 000, Atoms),
    statistics(agc, C1),
    length(Atoms, N),
    assertion(N == 200),
    Runs is C1-C0,
    assertion(Runs =< 10).

keep_atoms(0, _, []) :- !.
keep_atoms(N, Bytes, [A|T]) :-
    atom_of(Bytes, N, A),
    N1 is N-1,
    keep_atoms(N1, Bytes, T).

%  agc_margin 0 disables atom-GC; a type budget must not resurrect it.
%  library(semweb/rdf_db) relies on this.

test(global_disable_wins,
     [ setup(setup_agc(S)), cleanup(cleanup_agc(S)) ]) :-
    setup_call_cleanup(
        ( current_prolog_flag(agc_margin, Old),
          set_prolog_flag(agc_margin, 0) ),
        ( set_blob_gc_margin(text, 1000),
          statistics(agc, C0),
          make_atoms(100, 100 000),
          statistics(agc, C1),
          assertion(C1 =:= C0) ),
        set_prolog_flag(agc_margin, Old)).

:- end_tests(agc_margin_trigger).


:- begin_tests(agc_unregister).

:- dynamic fact/1.

%  The margin is exceeded where references are dropped, which used to be a
%  place that never tested it: considerAGC() was only called when a new atom
%  was created.  Retracting the clauses that hold the only reference to each
%  atom reclaimed nothing until something else allocated.

test(dropping_references_collects,
     [ setup(setup_agc(S)),
       cleanup(( cleanup_agc(S), retractall(fact(_)) )) ]) :-
    forall(between(1, 30 000, I),
           ( atom_concat(agc_bulk_, I, A),
             assertz(fact(A)) )),
    garbage_collect_atoms,
    statistics(atoms, A0),
    retractall(fact(_)),
    garbage_collect_clauses,
    forall(between(1, 50, _), true),            % safe points for the signal
    statistics(atoms, A1),
    Reclaimed is A0-A1,
    assertion(Reclaimed > 20 000).

:- end_tests(agc_unregister).
