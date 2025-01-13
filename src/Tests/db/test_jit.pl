/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           www.swi-prolog.org
    Copyright (c)  2011-2025, University of Amsterdam
                              VU University Amsterdam
                              SWI-Prolog Solutions b.v.
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

:- module(test_jit,
          [ test_jit/0
          ]).
:- use_module(library(plunit)).
:- use_module(library(debug)).
:- use_module(library(apply)).
:- use_module(library(lists)).

test_jit :-
    run_tests([ jit,
                jit_static
              ]).

/** <module> Test unit for Just-In-Time indexing

This module tests behaviour of the just-in-time indexes.
*/

:- begin_tests(jit).

:- dynamic
    d/2.

:- meta_predicate
    has_hashes(:, ?),
    not_hashed(:).

%!  has_hashes(+Pred, +Hashes:List) is semidet.
%
%   Verify that Pred hash single  argument   hashes  on the arguments in
%   List.
%
%   @tbd Deal with deep and multi argument hashes.

has_hashes(P, Hashes) :-
    sort(Hashes, SHashes),
    predicate_property(P, indexed(Indexed)),
    maplist(hash_arg, Indexed, Args),
    sort(Args, SHashes).

hash_arg(Dict, Arg) :-
    [Arg] = Dict.arguments.

not_hashed(P) :-
    \+ predicate_property(P, indexed(_)).


test(remove, [cleanup(retractall(d(_,_)))]) :-
    forall(between(1,50,X), assertz(d(X,X))),
    d(_,30),
    assertion(has_hashes(d(_,_), [2])),
    forall(between(51,125,X), assertz(d(X,X))),
    assertion(not_hashed(p(_,_))),
    d(30,_),
    assertion(has_hashes(d(_,_), [1])).
test(remove, [cleanup(retractall(d(_,_)))]) :-
    forall(between(1,40,X), assertz(d(X,a))),
    forall(between(41,50,X), assertz(d(X,X))),
    d(30,a),
    assertion(has_hashes(d(_,_), [1])),
    retractall(d(_,a)),
    assertion(not_hashed(p(_,_))),
    d(_,45),
    assertion(has_hashes(d(_,_), [2])).
test(retract, [cleanup(retractall(d(_,_))), Xs == Xsok]) :-
    forall(between(1,10,X), assertz(d(X,X))),
    forall(between(11,100,X), assertz(d(a,X))),
    findall(X, retract(d(a,X)), Xs),
    numlist(11, 100, Xsok).
test(retract2, [cleanup(retractall(d(_,_))), Xs == Xsok]) :-
    forall(between(1,10,X), assertz(d(X,X))),
    forall(between(11,100,X), assertz(d(a,X))),
    findall(X, rmd(a,X), Xs),
    numlist(11, 100, Xsok).
test(clause, [cleanup(retractall(d(_,_))), Xs == Xsok]) :-
    forall(between(1,10,X), assertz(d(X,X))),
    forall(between(11,100,X), assertz(d(a,X))),
    findall(X, claused(a,X), Xs),
    numlist(11, 100, Xsok).
test(string, [cleanup(retractall(d(_,_)))]) :-
    test_index_1(string_concat("a")).
test(bigint, [condition(current_prolog_flag(bounded, false)),
              cleanup(retractall(d(_,_)))]) :-
    test_index_1(mkbigint(100)).
test(midint, [cleanup(retractall(d(_,_)))]) :-
    test_index_1(mkbigint(60)).
test(float, [cleanup(retractall(d(_,_)))]) :-
    test_index_1(mkfloat).
test(string, [cleanup(retractall(d(_,_)))]) :-
    test_index_2(string_concat("a")).
test(bigint, [condition(current_prolog_flag(bounded, false)),
              cleanup(retractall(d(_,_)))]) :-
    test_index_2(mkbigint(100)).
test(midint, [cleanup(retractall(d(_,_)))]) :-
    test_index_2(mkbigint(60)).
test(float, [cleanup(retractall(d(_,_)))]) :-
    test_index_2(mkfloat).

rmd(X,Y) :-
    retract(d(X, Y)),
    (   Y == 89
    ->  garbage_collect_clauses
    ;   true
    ).

claused(X,Y) :-
    clause(d(X, Y), true),
    (   Y == 89
    ->  garbage_collect_clauses
    ;   true
    ).

mkbigint(Shift, I, Big) :-
    Big is 1<<Shift+I.
mkfloat(I, Float) :-
    Float is float(I).

:- meta_predicate
    test_index_1(2),
    test_index_2(2).

test_index_1(Convert) :-
    retractall(d(_,_)),
    forall(between(1, 1000, I),
           (   call(Convert, I, D),
               assertz(d(D, I))
           )),
    forall(between(1, 1000, I),
           (   call(Convert, I, D),
               assertion((d(D, I2), I2 == I))
           )),
    assertion(has_hashes(d(_,_), [1])).

test_index_2(Convert) :-
    retractall(d(_,_)),
    forall(between(1, 1000, I),
           (   call(Convert, I, D),
               assertz(d(I, D))
           )),
    forall(between(1, 1000, I),
           (   call(Convert, I, D),
               assertion((d(I2, D), I2 == I))
           )),
    assertion(has_hashes(d(_,_), [2])).

p1(a(b(c(d(e(f(g(1)))))))).
p1(a(b(c(d(e(f(g(2)))))))).

p2(a(b(c(d(e(f(g(h(1))))))))).
p2(a(b(c(d(e(f(g(h(2))))))))).

test(depth) :-
    p1(a(b(c(d(e(f(g(1)))))))),
    p1(a(b(c(d(e(f(g(2)))))))).
test(depth_exceeded, nondet) :-
    p2(a(b(c(d(e(f(g(h(1))))))))),
    p2(a(b(c(d(e(f(g(h(2))))))))).

:- end_tests(jit).

:- begin_tests(jit_static).

x(x,x,x,x,[]).
x(x,x,x,x,[_|_]).

a(_,_,_,_,[]).
a(_,_,_,_,[_|_]).

:- mode(m(?,?,?,?,-)).
m(_,_,_,_,[]).
m(_,_,_,_,[_|_]).

b(_,_,_,_,[]).
b(_,_,_,[_|_],_).

pa(_, _).
pa(_, x).

test(x) :-				% must use S_LIST
    x(_,_,_,_,[]).
test(a) :-				% must use S_LIST (test H_VOID_N)
    a(_,_,_,_,[]).
test(b) :-				% may not use S_LIST (test H_VOID_N)
    call_cleanup(b(_,_,_,_,[]), Det=true),
    assertion(var(Det)),
    !.
test(m) :-				% may not use S_LIST (test mode/1)
    call_cleanup(m(_,_,_,_,[]), Det=true),
    assertion(var(Det)),
    !.
test(pa) :-                             % primary index should be on arg 2
    pa(_,y).

:- end_tests(jit_static).
