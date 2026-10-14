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
:- use_module(library(thread)).

/** <module> Test the representation of the children of trie nodes

Nodes have no children, a single child or  a block of children. Small
blocks are searched linearly, larger ones  have   an  index. These tests
exercise growing and compacting blocks,   deletion  and enumeration, and
concurrent insertion into tries created by trie_new/1.
*/

test_trie_children :-
    run_tests([ trie_children,
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

:- begin_tests(trie_children_threads,
               [ condition(current_prolog_flag(threads, true))
               ]).

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
