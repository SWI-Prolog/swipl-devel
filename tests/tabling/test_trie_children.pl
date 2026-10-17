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

:- module(test_trie_children,
          [ test_trie_children/0
          ]).
:- use_module(library(plunit)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(debug)).
:- if(current_prolog_flag(threads, true)).
:- use_module(library(thread)).
:- endif.

/** <module> Test the representation of the children of trie nodes

Nodes have no children, a single child or  a block of children. Small
blocks are searched linearly, larger ones  have   an  index. These tests
exercise growing and compacting blocks,   deletion  and enumeration, and
concurrent insertion into tries created by trie_new/1.

Tries without concurrent writers, i.e., tables, allocate their nodes
from chunks.  The trie_node_chunks tests delete and re-add many nodes
of such tries and re-evaluate large incremental tables.
*/

test_trie_children :-
    run_tests([ trie_children,
                trie_node_chunks,
                trie_children_threads
              ]).

%   fill(+Trie, +Keys)
%   check(+Trie, +Keys)
%
%   Insert Keys with their position as value and verify that the trie
%   holds exactly Keys.

fill(Trie, Keys) :-
    foldl(insert(Trie), Keys, 1, _).

insert(Trie, Key, I0, I) :-
    trie_insert(Trie, Key, I0),
    I is I0+1.

check(Trie, Keys) :-
    findall(K, trie_gen(Trie, K, _), Found0),
    msort(Found0, Found),
    msort(Keys, Expected),
    assertion(Found == Expected),
    forall(member(K, Keys),
           assertion(trie_lookup(Trie, K, _))).

keys(N, Keys) :-
    numlist(1, N, Keys).

delete_keys(Trie, Keys) :-
    forall(member(K, Keys), trie_delete(Trie, K, _)).

even_odd(Keys, Even, Odd) :-
    partition([K]>>(K mod 2 =:= 0), Keys, Even, Odd).

:- begin_tests(trie_children).

test(sizes) :-
    forall(member(N, [1,2,3,8,9,16,17,100,1000]),
           ( trie_new(T),
             keys(N, Keys),
             fill(T, Keys),
             check(T, Keys)
           )).
test(delete_half) :-
    forall(member(N, [2,8,9,100,1000]),
           ( trie_new(T),
             keys(N, Keys),
             fill(T, Keys),
             even_odd(Keys, Even, Odd),
             delete_keys(T, Even),
             check(T, Odd),
             forall(member(K, Even), assertion(\+ trie_lookup(T, K, _)))
           )).
test(delete_reinsert) :-
    forall(member(N, [2,9,100,1000]),
           ( trie_new(T),
             keys(N, Keys),
             fill(T, Keys),
             even_odd(Keys, Even, Odd),
             delete_keys(T, Even),
             fill(T, Even),
             check(T, Keys),
             delete_keys(T, Odd),
             check(T, Even)
           )).
test(delete_all, Found == []) :-
    trie_new(T),
    keys(100, Keys),
    fill(T, Keys),
    delete_keys(T, Keys),
    findall(K, trie_gen(T, K, _), Found),
    keys(10, Keys10),
    fill(T, Keys10),
    check(T, Keys10).
test(delete_all_nested, Found == []) :-
    trie_new(T),
    findall(f(X,Y), (between(1, 20, X), between(1, 20, Y)), Keys),
    fill(T, Keys),
    delete_keys(T, Keys),
    findall(K, trie_gen(T, K, _), Found).
test(nested) :-
    trie_new(T),
    findall(f(X,g(Y),Z),
            ( between(1, 30, X), between(1, X, Y), member(Z, [a,b]) ),
            Keys),
    fill(T, Keys),
    check(T, Keys),
    include([f(X,_,_)]>>(X mod 3 =:= 0), Keys, Del),
    subtract(Keys, Del, Rest),
    delete_keys(T, Del),
    check(T, Rest).
test(kinds) :-
    trie_new(T),
    Big is 2^100,
    numlist(1, 20, Ints),
    findall(A, (between(1, 20, I), atom_concat(a, I, A)), Atoms),
    findall(F, (between(1, 20, I), F is I/3.0), Floats),
    findall(S, (between(1, 20, I), number_string(I, S0),
                string_concat("s", S0, S)), Strings),
    append([Ints, Atoms, Floats, Strings, [Big, -Big, f(a), f(_), g(_,_)]],
           Keys0),
    maplist([K,k(K)]>>true, Keys0, Keys),
    fill(T, Keys),
    findall(K, trie_gen(T, K, _), Found),
    length(Keys, Len),
    assertion(length(Found, Len)),
    forall(member(K, Keys),
           assertion((member(F, Found), F =@= K))).
test(vars) :-
    trie_new(T),
    Keys = [f(_,_), f(X,X), f(a,_), f(_,a), f(a,a), f(_,b), f(Y,Y,Y)],
    fill(T, Keys),
    findall(K, trie_gen(T, K, _), Found),
    length(Found, 7),
    forall(member(K, Keys),
           assertion((member(F, Found), F =@= K))),
    findall(Y1, trie_gen(T, f(a,Y1), _), Ys),
    length(Ys, 6).                      % all but f(Y,Y,Y)
test(gen_bound) :-                      % trie_gen/3 with a bound argument
    trie_new(T),
    findall(f(X,Y), (between(1, 50, X), between(1, 50, Y)), Keys),
    fill(T, Keys),
    findall(Y, trie_gen(T, f(17,Y), _), Ys),
    assertion(Ys == Ys),
    length(Ys, 50),
    trie_insert(T, f(_,1), var),
    findall(V, trie_gen(T, f(17,1), V), Vs),
    msort(Vs, VsS),
    assertion(VsS = [_,var]).
test(compiled) :-
    trie_new(T),
    keys(200, Keys),
    fill(T, Keys),
    even_odd(Keys, Even, Odd),
    delete_keys(T, Even),
    findall(K, trie_gen_compiled(T, K, _), Found0),
    msort(Found0, Found),
    assertion(Found == Odd).
test(property) :-
    trie_new(T),
    keys(1000, Keys),
    fill(T, Keys),
    trie_property(T, node_count(Nodes)),
    trie_property(T, value_count(Values)),
    trie_property(T, size(Bytes)),
    assertion(Values == 1000),
    assertion(Nodes == 1001),
    assertion(Bytes > 0).

:- end_tests(trie_children).

:- dynamic d/1 as incremental.
:- table q/1 as incremental.
:- table v/2.

q(X) :- d(X).
v(X, Y) :- Y is X*2.

q_answers(L) :-
    findall(X, q(X), L0),
    msort(L0, L).

d_facts(L) :-
    findall(X, d(X), L0),
    msort(L0, L).

v_tables(N) :-
    aggregate_all(count,
                  ( current_table(test_trie_children:V, _),
                    V = v(_,_)
                  ), N).

add_d(I)  :- assertz(d(f(I, g(I)))).
del_d(I)  :- retract(d(f(I, g(I)))).
clear_d   :- retractall(d(_)).

:- begin_tests(trie_node_chunks, [cleanup(abolish_all_tables)]).

test(incremental) :-                    % re-evaluate a large answer trie
    abolish_all_tables,
    clear_d,
    forall(between(1, 2000, I), add_d(I)),
    forall(between(1, 3, _),
           ( q_answers(L1), d_facts(D1), assertion(L1 == D1),
             forall(( between(1, 2000, I), I mod 2 =:= 0 ),
                    del_d(I)),
             q_answers(L2), d_facts(D2), assertion(L2 == D2),
             assertion(length(L2, 1000)),
             forall(( between(1, 2000, I), I mod 2 =:= 0 ),
                    add_d(I))
           )),
    q_answers(L), assertion(length(L, 2000)),
    clear_d.
test(registered) :-                     % atom-GC registers full chunks
    abolish_all_tables,
    \+ \+ reg_atom(_),
    reg_atom_count(N0),                 % first scan: chunks are full
    assertion(N0 == 5000),
    reg_atom_count(N1),                 % second scan: register them
    assertion(N1 == 5000),
    aggregate_all(count, reg_atom(_), C),
    assertion(C == 5000),
    abolish_all_tables,                 % releases the registered keys
    assertz(chunk_gen_dummy), retract(chunk_gen_dummy),
    garbage_collect_clauses,            % and the compiled clause
    reg_atom_count(N2),
    (   if_reliable_agc
    ->  assertion(N2 < 10)
    ;   true
    ).
test(variants) :-                       % variant table nodes are pruned
    abolish_all_tables,
    forall(between(1, 3, _),
           ( forall(between(1, 2000, I), v(I, _)),
             v_tables(N1), assertion(N1 == 2000),
             forall(( between(1, 2000, I), I mod 2 =:= 1 ),
                    abolish_table_subgoals(v(I, _))),
             v_tables(N2), assertion(N2 == 1000)
           )),
    forall(between(1, 2000, I), ( v(I, Y), assertion(Y =:= I*2) )).

:- end_tests(trie_node_chunks).

%   Tables with fresh atoms.  Atom-GC marks the keys of the chunk
%   nodes rather than the nodes registering the atoms.

:- table agc_atom/3, agc_var/2, reg_atom/1.
:- dynamic chunk_gen_dummy/0.

reg_atom(X) :-
    between(1, 5000, I),
    atom_concat(rga_, I, X).

%   Whether we can expect AGC to reclaim the released atoms.  AGC
%   may find stale references on the stacks, so only test this if
%   `SWIPL_TEST_FAIL_ON_UNLIKELY` is `y`.

if_reliable_agc :-
    getenv('SWIPL_TEST_FAIL_ON_UNLIKELY', y).

reg_atom_count(N) :-
    garbage_collect_atoms,
    aggregate_all(count,
                  ( current_atom(A),
                    sub_atom(A, 0, _, After, rga_),
                    After > 0
                  ), N).

agc_atom(Id, R, X) :-
    between(1, 300, I),
    agc_name(Id, R, I, X).

agc_name(Id, R, I, X) :-
    format(atom(X), 'agc_~w_~w_~w', [Id, R, I]).

agc_var(_, _).

%   agc_worker(+Id)
%
%   Create tables with fresh atoms and verify them a few times, such
%   that atom-GC registers the full chunks.  Then delete and re-add
%   variants with fresh atoms, deleting and reusing nodes of registered
%   chunks of the variant table.

agc_worker(Id) :-
    forall(between(1, 10, R), \+ \+ agc_atom(Id, R, _)),
    forall(between(1, 3, _), agc_check_answers(Id)),
    agc_var_names(Id, Names),
    forall(member(N, Names), agc_var(Id, N)),
    forall(between(1, 3, _), agc_check_variants(Id, Names)),
    agc_odd(Names, Odd, Even),
    forall(member(N, Odd), abolish_table_subgoals(agc_var(Id, N))),
    agc_check_variants(Id, Even),
    forall(member(N, Odd), agc_var(Id, N)),
    forall(between(1, 3, _), agc_check_variants(Id, Names)),
    agc_check_answers(Id).

agc_check_answers(Id) :-
    forall(between(1, 10, R),
           ( findall(X, agc_atom(Id, R, X), Xs),
             msort(Xs, Sorted),
             findall(X, (between(1, 300, I), agc_name(Id, R, I, X)), Exp0),
             msort(Exp0, Exp),
             Sorted == Exp
           )).

agc_var_names(Id, Names) :-
    findall(N, ( between(1, 400, I),
                 format(atom(N), 'agc_var_~w_~w', [Id, I])
               ), Names).

agc_check_variants(Id, Names) :-
    findall(N, ( current_table(test_trie_children:V, _),
                 V = agc_var(Id, N)
               ), Found0),
    msort(Found0, Found),
    findall(N, (member(N0, Names), atom_string(N0, S), atom_string(N, S)),
            Exp0),
    msort(Exp0, Exp),
    Found == Exp.

agc_odd([], [], []).
agc_odd([O], [O], []).
agc_odd([O,E|T0], [O|O1], [E|E1]) :-
    agc_odd(T0, O1, E1).

agc_loop(Stop) :-
    (   thread_peek_message(Stop)
    ->  true
    ;   garbage_collect_atoms,
        agc_loop(Stop)
    ).

:- begin_tests(trie_children_threads,
               [ condition(current_prolog_flag(threads, true))
               ]).

test(agc_concurrent) :-
    thread_create(agc_loop(stop), AGC, []),
    numlist(1, 4, Ids),
    findall(T, ( member(Id, Ids),
                 thread_create(agc_worker(Id), T, [])
               ), Workers),
    maplist(thread_join, Workers, Statuses),
    thread_send_message(AGC, stop),
    thread_join(AGC, AStatus),
    assertion(maplist(==(true), Statuses)),
    assertion(AStatus == true).

thread_keys(Id, Keys) :-
    findall(f(K), (between(1, 2000, I), K is (I*Id) mod 3001), Keys0),
    sort(Keys0, Keys).

test(concurrent_insert) :-
    trie_new(T),
    numlist(1, 8, Ids),
    concurrent_forall(member(Id, Ids),
                      ( thread_keys(Id, Keys),
                        forall(member(K, Keys), trie_update(T, K, Id))
                      ),
                      [threads(8)]),
    findall(Ks, (member(Id, Ids), thread_keys(Id, Ks)), Kss),
    append(Kss, All0),
    sort(All0, All),
    check(T, All).
test(concurrent_nested) :-
    trie_new(T),
    numlist(1, 8, Ids),
    concurrent_forall(member(Id, Ids),
                      forall(( between(1, 50, X), between(1, 50, Y) ),
                             trie_update(T, g(X,Id,Y), Id)),
                      [threads(8)]),
    aggregate_all(count, trie_gen(T, _, _), Count),
    assertion(Count == 20000).
test(insert_while_enumerating) :-
    trie_new(T),
    numlist(1, 1000, Before),
    forall(member(K, Before), trie_insert(T, f(K), old)),
    thread_create(forall(between(1001, 20000, K), trie_insert(T, f(K), new)),
                  Id, []),
    findall(K, trie_gen(T, f(K), _), Seen),
    thread_join(Id, Status),
    assertion(Status == true),
    msort(Seen, SeenS),
    assertion(ord_subset(Before, SeenS)),
    length(SeenS, Len), sort(SeenS, Unique),
    assertion(length(Unique, Len)),
    aggregate_all(count, trie_gen(T, _, _), Count),
    assertion(Count == 20000).

test(shared_variants) :-                % concurrent shared variant table
    abolish_all_tables,
    numlist(1, 8, Ids),
    concurrent_forall(member(Id, Ids),
                      forall(between(1, 500, I),
                             ( K is (I*Id) mod 701,
                               findall(X, sv(K, X), Xs),
                               assertion(Xs == [K]) )),
                      [threads(8)]),
    aggregate_all(count, (current_table(_:V, _), V = sv(_,_)), Count),
    findall(K, (member(Id, Ids), between(1, 500, I), K is (I*Id) mod 701),
            Ks0),
    sort(Ks0, Ks),
    length(Ks, Count).

:- table sv/2 as shared.
sv(K, K).

:- end_tests(trie_children_threads).
