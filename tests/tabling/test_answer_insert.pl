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

:- module(test_answer_insert,
          [ test_answer_insert/0
          ]).
:- use_module(library(plunit)).
:- use_module(library(lists)).
:- use_module(library(debug)).
:- use_module(library(apply)).

/** <module> Test adding answers to answer tries

Answers whose arguments are atomic or plain variables are added by the
fast path trie_insert_flat().  Others use trie_lookup_abstract().  These
tests verify that both produce the same answer tries.
*/

test_answer_insert :-
    run_tests([ answer_insert
              ]).

:- meta_predicate
    all(?, 0, -).

all(T, G, L) :- findall(T, G, L0), msort(L0, L).

%   in_table(:Variant, +Answer) is semidet.
%
%   True when Answer (a ret/N term) is in the answer table of Variant,
%   found using trie_lookup/3, i.e., through trie_lookup_abstract().

in_table(M:Variant, Answer) :-
    current_table(M:Variant, Trie),
    trie_lookup(Trie, Answer, _).

:- table
    ground_dups/1,
    shared/2,
    indirect/1,
    mixed/1,
    zero/0,
    wide/40.

ground_dups(X) :- member(X, [a,1,b,a,1,c,b]).

shared(X, Y) :- member(X-Y, [A-A, a-_, _-b, B-B, a-_, C-D, D-C]),
    ignore(A=A), ignore(B=B), ignore(C=C).

indirect(X) :-
    Big is 2^100,
    member(X, [1.5, Big, "str", 1.5, Big, "str", 2.5]).

mixed(X) :- member(X, [1, g(1), 1, _, g(_), g(1), h(a,_), a]).

zero :- member(_, [1,2,3]).

wide(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,
     A21,A22,A23,A24,A25,A26,A27,A28,A29,A30,A31,A32,A33,A34,A35,A36,A37,A38,
     A39,A40) :-
    member(_, [1,2]),
    A1 = A40, A2 = x,
    maplist(=(_), [A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,
                   A19,A20,A21,A22,A23,A24,A25,A26,A27,A28,A29,A30,A31,A32,
                   A33,A34,A35,A36,A37,A38,A39]).

:- begin_tests(answer_insert).

test(ground, L == [1,a,b,c]) :-
    abolish_all_tables,
    all(X, ground_dups(X), L).
test(ground_lookup) :-
    abolish_all_tables,
    forall(ground_dups(_), true),
    forall(member(X, [a,1,b,c]),
           assertion(in_table(test_answer_insert:ground_dups(_), ret(X)))).
test(shared, Len == 4) :-
    abolish_all_tables,
    findall(X-Y, shared(X, Y), L),
    length(L, Len),
    forall(member(E, [A-A, _-b, a-_, _-_]),
           assertion((member(E2, L), E2 =@= E))).
test(shared_lookup) :-
    abolish_all_tables,
    forall(shared(_,_), true),
    assertion(in_table(test_answer_insert:shared(_,_), ret(A,A))),
    assertion(in_table(test_answer_insert:shared(_,_), ret(a,_))),
    assertion(in_table(test_answer_insert:shared(_,_), ret(_,_))),
    assertion(\+ in_table(test_answer_insert:shared(_,_), ret(b,_))).
test(unbound, X-Y =@= _-_) :-          % bindings are undone
    abolish_all_tables,
    shared(X, Y), var(X), var(Y), X \== Y, !.
test(indirect, L == [1.5, 2.5, Big, "str"]) :-
    abolish_all_tables,
    Big is 2^100,
    all(X, indirect(X), L).
test(indirect_lookup) :-
    abolish_all_tables,
    forall(indirect(_), true),
    Big is 2^100,
    forall(member(X, [1.5, Big, "str"]),
           assertion(in_table(test_answer_insert:indirect(_), ret(X)))).
test(mixed, Len == 6) :-
    abolish_all_tables,
    findall(X, mixed(X), L),
    length(L, Len),
    forall(member(E, [_, 1, a, g(_), g(1), h(a,_)]),
           assertion((member(E2, L), E2 =@= E))).
test(mixed_lookup) :-
    abolish_all_tables,
    forall(mixed(_), true),
    forall(member(X, [1, g(1), _, g(_), h(a,_), a]),
           assertion(in_table(test_answer_insert:mixed(_), ret(X)))).
test(zero, L == [true]) :-
    abolish_all_tables,
    findall(true, zero, L).
test(wide, Len == 1) :-                 % 38 variables: generic path
    abolish_all_tables,
    length(Args, 40),
    G =.. [wide|Args],
    findall(G, G, L),
    length(L, Len),
    L = [W],
    arg(1, W, A1), arg(40, W, A40), arg(2, W, A2),
    assertion(A1 == A40),
    assertion(A2 == x).

:- end_tests(answer_insert).
