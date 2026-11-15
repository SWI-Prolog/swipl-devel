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


:- module(test_table_symbols,
          [ test_table_symbols/0
          ]).
:- use_module(library(plunit)).
:- use_module(library(lists)).

/** <module> Answer tries without atoms

The variant table of a thread refers to its answer tries directly.  A
trie gets its symbol (atom) only when Prolog needs a handle to it.  The
pseudo tables that record calls to incremental dynamic predicates never
need one during evaluation.
*/

test_table_symbols :-
    run_tests([ table_symbols
              ]).

threads :-
    current_prolog_flag(threads, true).

:- dynamic d/1 as incremental.
:- table p/1 as incremental.

d(1).

p(I) :- d(X), X >= 0, I >= 0, d(I).

add_d(X) :- assertz(d(X)).
del_d(X) :- retract(d(X)).

%   new_atoms(:Goal, -Atoms)
%
%   Atoms is the number of atoms created by Goal in a fresh thread, so
%   the tables of other tests do not interfere.

new_atoms(Goal, Atoms) :-
    thread_create(new_atoms_(Goal), Id),
    thread_join(Id, exited(Atoms)).

:- meta_predicate new_atoms_(0).
new_atoms_(Goal) :-
    garbage_collect_atoms,
    statistics(atoms, A0),
    call(Goal),
    statistics(atoms, A1),
    Atoms is A1-A0,
    thread_exit(Atoms).

call_p(N) :-
    forall(between(1, N, I), ignore(p(I))).

dyn_entry(Call, Trie) :-
    '$tbl_variant_table'(VT),
    trie_gen(VT, test_table_symbols:Call, Trie),
    Call = d(_).

:- begin_tests(table_symbols, [cleanup(abolish_all_tables)]).

test(no_atoms_for_dynamic, [condition(threads), true(Atoms < 1500)]) :-
    new_atoms(call_p(1000), Atoms).	% 1,000 tables, 1,001 dynamic nodes
test(lazy_symbol, [Status == dynamic]) :-
    abolish_all_tables,
    call_p(3),
    dyn_entry(d(2), Trie),
    is_trie(Trie),
    '$tbl_table_status'(Trie, Status).
test(update_after_lazy_symbol, [L1-L2 == [p(1)]-[p(1),p(2)]]) :-
    abolish_all_tables,
    findall(p(I), (between(1,2,I), p(I)), L1),
    forall(dyn_entry(_, _), true),	% create the symbols
    add_d(2),
    findall(p(I), (between(1,2,I), p(I)), L2),
    del_d(2).
test(update_without_symbol, [L1-L2 == [p(1)]-[p(1),p(3)]]) :-
    abolish_all_tables,
    findall(p(I), (member(I,[1,3]), p(I)), L1),
    add_d(3),
    findall(p(I), (member(I,[1,3]), p(I)), L2),
    del_d(3).
test(abolish_frees, [condition(threads), true(After*10 < Before)]) :-
    thread_create(abolish_frees, Id),
    thread_join(Id, exited(Before-After)).

abolish_frees :-
    call_p(1000),
    forall(dyn_entry(_, _), true),	% some with symbols
    statistics(table_space_used, Before),
    abolish_all_tables,
    agc_settled(Before, 100, After),
    thread_exit(Before-After).

%   agc_settled(+Before, +N, -After)
%
%   Atom-GC frees the answer tries that have a symbol.  Wait at most N
%   tenths of a second until it did.

agc_settled(Before, N, After) :-
    garbage_collect_atoms,
    statistics(table_space_used, Used),
    (   ( Used*10 < Before ; N == 0 )
    ->  After = Used
    ;   sleep(0.1),
        N2 is N-1,
        agc_settled(Before, N2, After)
    ).

:- end_tests(table_symbols).
