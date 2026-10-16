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

:- module(test_answer_discard,
          [ test_answer_discard/0
          ]).
:- use_module(library(plunit)).
:- use_module(library(lists)).
:- use_module(library(debug)).
:- use_module(library(apply)).
:- use_module(library(aggregate)).

/** <module> Test lazy compilation and discarding answer tries

A completed answer trie is compiled when it is called after its leader
completed.  If the table is private, not incremental and has no
conditional answers, the trie nodes are discarded after compilation.
Predicates that need the nodes restore them from the clause.
*/

test_answer_discard :-
    run_tests([ answer_discard
              ]).

:- meta_predicate
    all(?, 0, -).

all(T, G, L) :- findall(T, G, L0), msort(L0, L).

answer_trie(G, T) :-                    % enumerate: also finds shared
    current_table(test_answer_discard:V, T),
    V =@= G,
    !.

nodes(G, N) :-
    answer_trie(G, T),
    trie_property(T, node_count(N)).

compiled(G) :-
    answer_trie(G, T),
    trie_property(T, compiled_size(_)).

%   call_twice(:Goal, -Answers)
%
%   Complete the table for Goal and call the complete table, which
%   compiles it.

call_twice(G, L) :-
    all(G, test_answer_discard:G, _),
    all(G, test_answer_discard:G, L).

add_d(X) :- assertz(d(X)).
del_d(X) :- retract(d(X)).

:- table p/1, mix/1, ng/1, tz/0, tp/0, gen/1, deep/1.
p(X) :- between(1, 100, X).
mix(X) :- member(X, [a, 1, 1.5, "str", 100000000000000000000, f(a,g(b)),
                     [x,y], -3]).
ng(X) :- member(X, [f(_), g(_,_), h(A,A)]).
tz :- fail.
tp.
gen(X) :- between(1, 1000, I), atom_concat(discard_test_, I, X).
deep(L) :- numlist(1, 100, L).

%   with_compile_threshold(+N, :Goal)
%
%   Run Goal with the Prolog flag table_compile_threshold set to N.

with_compile_threshold(N, G) :-
    current_prolog_flag(table_compile_threshold, Old),
    setup_call_cleanup(
        set_prolog_flag(table_compile_threshold, N),
        G,
        set_prolog_flag(table_compile_threshold, Old)).

%   thread_join_true(:Goal)
%
%   Run Goal in a new thread and succeed if it succeeds there.

thread_join_true(G) :-
    thread_create(G, Id),
    thread_join(Id, Status),
    Status == true.

:- table und/1, und0/0.
und(X) :- member(X, [1,2]), tnot(und0).
und0 :- tnot(und0).

:- dynamic([d/1], [incremental(true)]).
:- table ip/1 as incremental.
d(1).
ip(X) :- d(X).

%   Atoms that only the tables refer to, to verify that compiled tables
%   keep their atoms and release them when abolished.

:- table ga/1, gal/1.
:- table gi/1 as incremental.
:- dynamic([di/1], [incremental(true)]).

ga(X) :- between(1, 1000, I), atom_concat(borrow_a_, I, X).
gal(L) :- findall(X, (between(1, 100, I), atom_concat(borrow_l_, I, X)), L).
gi(X) :- di(I), atom_concat(borrow_i_, I, X).

atom_count(Prefix, N) :-
    garbage_collect_atoms,
    aggregate_all(count,
                  ( current_atom(A),
                    sub_atom(A, 0, _, After, Prefix),
                    After > 0
                  ), N).

set_di(N) :- retractall(di(_)), forall(between(1, N, I), assertz(di(I))).
add_di(I) :- assertz(di(I)).

%   Abolish all tables and reclaim the erased clauses.  Clause GC only
%   reclaims clauses erased before the current generation.

abolish_and_gc :-
    abolish_all_tables,
    assertz(di(-1)), retract(di(-1)),
    garbage_collect_clauses.

ga_ok :-
    findall(X, ga(X), L),
    length(L, 1000),
    forall(member(X, L), sub_atom(X, 0, _, _, borrow_a_)).

:- table sh/1 as shared.
sh(X) :- between(1, 10, X).

:- table mo(_,max).
mo(a, 1).
mo(a, 3).
mo(a, 2).

:- table su/1 as subsumptive.
su(X) :- between(1, 10, X).

:- begin_tests(answer_discard).

test(lazy, Compiled == false) :-
    abolish_all_tables,
    all(X, p(X), L),
    assertion(length(L, 100)),
    ( compiled(p(_)) -> Compiled = true ; Compiled = false ).
test(lazy_deep, N == 1) :-             % deep answers: compile and discard
    abolish_all_tables,
    all(X, deep(X), [_]),
    nodes(deep(_), N).
test(lazy_deep_flag, Compiled == false) :-
    abolish_all_tables,
    with_compile_threshold(1000000, all(X, deep(X), [_])),
    ( compiled(deep(_)) -> Compiled = true ; Compiled = false ).
test(lazy_flag_zero, N == 1) :-         % 0: always compile at completion
    abolish_all_tables,
    with_compile_threshold(0, all(X, p(X), L)),
    assertion(length(L, 100)),
    nodes(p(_), N).
test(flag_inherit) :-                  % new threads inherit the flag
    current_prolog_flag(threads, true),
    with_compile_threshold(1000000,
                           thread_join_true(( all(X, deep(X), [_]),
                                              \+ compiled(deep(_)) ))).
test(flag_thread_local, V == Old) :-   % setting in a thread is local
    current_prolog_flag(threads, true),
    current_prolog_flag(table_compile_threshold, Old),
    thread_join_true(set_prolog_flag(table_compile_threshold, 7)),
    current_prolog_flag(table_compile_threshold, V).
test(discard, N == 1) :-
    abolish_all_tables,
    call_twice(p(_), L),
    assertion(length(L, 100)),
    answer_trie(p(_), T),
    assertion(trie_property(T, value_count(100))),
    nodes(p(_), N),
    all(p(X), p(X), L2),
    assertion(L2 == L).
test(restore, L == L0) :-
    abolish_all_tables,
    call_twice(p(_), L0),
    answer_trie(p(_), T),
    all(p(X), trie_gen(T, ret(X)), L),
    assertion(trie_property(T, value_count(100))),
    nodes(p(_), N),
    assertion(N > 1),
    all(p(X), p(X), L2),
    assertion(L2 == L0).
test(restore_lookup) :-
    abolish_all_tables,
    call_twice(p(_), _),
    answer_trie(p(_), T),
    assertion(trie_lookup(T, ret(42), _)),
    assertion(\+ trie_lookup(T, ret(142), _)).
test(kinds, L == L0) :-
    abolish_all_tables,
    all(X, mix(X), L0),
    call_twice(mix(_), _),
    assertion(nodes(mix(_), 1)),
    all(X, mix(X), L1),
    assertion(L1 == L0),
    answer_trie(mix(_), T),
    all(X, trie_gen(T, ret(X)), L).
test(nonground) :-
    abolish_all_tables,
    call_twice(ng(_), _),
    assertion(nodes(ng(_), 1)),
    findall(X, ng(X), L),
    assertion(length(L, 3)),
    memberchk(h(A,B), L),
    assertion(A == B).
test(atom_gc, L == L0) :-
    abolish_all_tables,
    findall(X, (between(1, 1000, I), atom_concat(discard_test_, I, X)), L0),
    \+ \+ call_twice(gen(_), _),
    assertion(nodes(gen(_), 1)),
    garbage_collect_atoms,
    findall(X, gen(X), L).
test(tnot) :-
    abolish_all_tables,
    call_twice(tz, _),
    call_twice(tp, _),
    assertion(tnot(tz)),
    assertion(\+ tnot(tp)).
test(abolish, L == L0) :-
    abolish_all_tables,
    call_twice(p(_), L0),
    abolish_all_tables,
    all(p(X), p(X), L).
test(conditional, true(N > 1)) :-
    abolish_all_tables,
    call_twice(und(_), _),
    nodes(und(_), N),
    call_delays(und(1), Delays),
    assertion(Delays \== true).
test(incremental, L == [1,2]) :-
    abolish_all_tables,
    call_twice(ip(_), _),
    nodes(ip(_), N),
    assertion(N > 1),
    add_d(2),
    all(X, ip(X), L),
    del_d(2).
test(shared, true(N > 1)) :-
    abolish_all_tables,
    call_twice(sh(_), _),
    call_twice(sh(_), _),
    nodes(sh(_), N).
test(moded, L == [a-3]) :-
    abolish_all_tables,
    call_twice(mo(_,_), _),
    nodes(mo(_,_), N),
    assertion(N > 1),
    all(X-Y, mo(X,Y), L).
test(subsumptive, L == [su(3)]) :-
    abolish_all_tables,
    call_twice(su(_), _),
    nodes(su(_), N),
    assertion(N > 1),
    all(su(3), su(3), L).

test(atom_keep_discarded) :-           % nodes discarded: clause owns atoms
    abolish_and_gc,
    \+ \+ call_twice(ga(_), _),
    assertion(nodes(ga(_), 1)),
    atom_count(borrow_a_, N),
    assertion(N == 1000),
    assertion(ga_ok),
    abolish_and_gc,
    atom_count(borrow_a_, N1),
    assertion(N1 == 0).
test(atom_keep_deep) :-                 % compiled at completion
    abolish_and_gc,
    \+ \+ gal(_),
    assertion(nodes(gal(_), 1)),
    atom_count(borrow_l_, N),
    assertion(N == 100),
    \+ \+ ( gal(L), length(L, 100) ),
    abolish_and_gc,
    atom_count(borrow_l_, N1),          % AGC is conservative: stale stack
    assertion(N1 < 10).                 % cells may keep a few atoms
test(atom_keep_reeval) :-               % clause discarded: nodes stay
    abolish_and_gc,
    set_di(200),
    \+ \+ call_twice(gi(_), _),
    add_di(201),
    \+ \+ call_twice(gi(_), _),
    abolish_and_gc,                     % reclaims the discarded clause
    set_di(0),
    atom_count(borrow_i_, N0),
    assertion(N0 == 0),
    set_di(200),
    \+ \+ call_twice(gi(_), _),
    add_di(201),
    garbage_collect_clauses,
    atom_count(borrow_i_, N),
    assertion(N == 200),                % the nodes keep the atoms
    \+ \+ ( findall(X, gi(X), L), length(L, 201) ),
    set_di(0),
    abolish_and_gc,
    atom_count(borrow_i_, N1),
    assertion(N1 == 0).

:- end_tests(answer_discard).
