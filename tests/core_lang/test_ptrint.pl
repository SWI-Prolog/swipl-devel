/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org
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

:- module(test_ptrint,
	  [ test_ptrint/0
	  ]).
:- use_module(library(plunit)).
:- use_module(library(lists)).
:- use_module(library(error)).

/** <module> Test pointer integers

PL_put_pointer() and PL_unify_pointer() create _pointer integers_:
integers for Prolog that are never identical to a plain integer.  The
test-only predicate '$fast_record'/2 returns one.
*/

test_ptrint :-
	run_tests([ ptrint
		  ]).

:- dynamic
	stored/1,
	idx/2,
	bp/1.
:- table tp/1.

tp(X) :- nb_getval(test_ptrint, X).

ptr(P, I) :-
	'$fast_record'(f(x), P),
	I is P+0.

%   A pointer that does not fit in a small integer is a plain integer
%   (64-bit Android, see O_BIG_ADDRESS).  Tests that tell a pointer from
%   a plain integer only apply if it fits.

small_address :-
	ptr(P, _),
	current_prolog_flag(min_tagged_integer, Min),
	current_prolog_flag(max_tagged_integer, Max),
	between(Min, Max, P).

:- begin_tests(ptrint, [cleanup(cleanup)]).

cleanup :-
	retractall(stored(_)),
	retractall(idx(_,_)),
	retractall(bp(_)),
	abolish_all_tables,
	nb_delete(test_ptrint).

test(integer, [true(integer(P)), true(number(P)), I =:= P]) :-
	ptr(P, I).
test(not_identical, [condition(small_address), true(I \== P), true(\+ I = P)]) :-
	ptr(P, I).
test(compare, [condition(small_address), O1-O2 == (<)-(>)]) :-
	ptr(P, I),
	compare(O1, I, P),
	compare(O2, P, I).
test(msort, L == [I, P]) :-
	ptr(P, I),
	msort([P, I], L).
test(arith_is_plain, J == I) :-
	ptr(P, I),
	J is P*1.
test(assert, Q == P) :-
	ptr(P, _),
	assertz(stored(P)),
	stored(Q).
test(index, [condition(small_address), Xs-Ys == [a]-[b]]) :-
	ptr(P, I),
	assertz(idx(P, a)),
	assertz(idx(I, b)),
	findall(X, idx(P, X), Xs),
	findall(Y, idx(I, Y), Ys).
test(clause_body, Z == P) :-
	ptr(P, _),
	assertz((bp(Z0) :- Z0 = P)),
	bp(Z).
test(record, R == P) :-
	ptr(P, _),
	recorda(test_ptrint, P, Ref),
	recorded(test_ptrint, R, Ref),
	erase(Ref).
test(findall, F == P) :-
	ptr(P, _),
	findall(X, member(X, [P]), [F]).
test(copy_term, C == P) :-
	ptr(P, _),
	copy_term(g(P,_), g(C,_)).
test(global_var, G == P) :-
	ptr(P, _),
	nb_setval(test_ptrint, P),
	nb_getval(test_ptrint, G).
test(tabling, T == P) :-
	ptr(P, _),
	nb_setval(test_ptrint, P),
	tp(T).
test(trie, [condition(small_address), K == P, true(\+ trie_lookup(Tr, I, _))]) :-
	ptr(P, I),
	trie_new(Tr),
	trie_insert(Tr, P),
	trie_gen(Tr, K).
test(trie_compiled, [K-V == P-P]) :-	% T_PTRINT
	ptr(P, _),
	trie_new(Tr),
	trie_insert(Tr, k(P), P),
	trie_gen_compiled(Tr, k(K), V).
test(get_pointer, T == f(x)) :-
	ptr(P, _),
	'$fast_recorded'(P, T).
test(get_pointer_int, [condition(small_address), error(type_error(address, I))]) :-
	ptr(_, I),
	'$fast_recorded'(I, _).
test(is_address) :-
	ptr(P, _),
	is_address(P).
test(is_address_int, [condition(small_address), fail]) :-
	ptr(_, I),
	is_address(I).
test(is_address_other, fail) :-
	member(X, [_, a, 1.0, "s", f(x), 42]),
	is_address(X).
test(must_be_address) :-
	ptr(P, _),
	must_be(address, P).
test(must_be_address_int, [condition(small_address), error(type_error(address, I))]) :-
	ptr(_, I),
	must_be(address, I).
test(must_be_address_var, error(instantiation_error)) :-
	must_be(address, _).
test(read_is_plain, N == I) :-
	ptr(P, I),
	term_to_atom(P, A),
	term_to_atom(N, A).
test(external_is_plain, E == I) :-
	ptr(P, I),
	fast_term_serialized(P, S),
	fast_term_serialized(E, S).
test(gc, G == P) :-
	ptr(P, _),
	nb_setval(test_ptrint, P),
	garbage_collect,
	nb_getval(test_ptrint, G).

:- end_tests(ptrint).
