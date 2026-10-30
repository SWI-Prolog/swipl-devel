/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2010-2011, University of Amsterdam
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

:- module(test_dbref, [test_dbref/0]).
:- use_module(library(plunit)).

/** <module> Test Prolog database references

@author	Jan Wielemaker
*/

test_dbref :-
	run_tests([ assert2,
		    recorded,
		    dbref_blobs
		  ]).

:- begin_tests(assert2).

:- dynamic
	term/1.

test(bound, error(_)) :-
	assert(term(a), noref).
test(erase, true) :-
	assert(term(a), Ref),
	erase(Ref),
	\+ term(a).
test(double_erase, true) :-
	assert(term(a), Ref),
	erase(Ref),
	\+ erase(Ref).
test(retract_erase, true) :-
	assert(term(a), Ref),
	retractall(term(_)),
	\+ erase(Ref).

:- end_tests(assert2).

:- begin_tests(recorded).

test(erase, true) :-
	recorda(test, a, Ref),
	recorded(test, a),
	erase(Ref),
	\+ recorded(test, a).
test(double_erase, true) :-
	recorda(test, a, Ref),
	recorded(test, a),
	erase(Ref),
	\+ erase(Ref).
test(recorded_vt, [true([K1,K2] == [test_1,test_2]), nondet]) :-
	recorda(test_1, a1, R1),
	recorda(test_2, a2, R2),
	recorded(K2, a2),
	recorded(K1, a1),
	erase(R1),
	erase(R2).

:- end_tests(recorded).

:- begin_tests(dbref_blobs).

:- dynamic
	fact/1.

test(clause_unique, [R1 == R2, Type == clause]) :-
	assertz(fact(u), R1),
	clause(fact(u), true, R2),
	blob(R1, Type),
	erase(R1).
test(record_unique, [R1 == R2, Type == record]) :-
	recordz(dbref_test, a, R1),
	recorded(dbref_test, a, R2),
	blob(R1, Type),
	erase(R1).
test(order_stable, S1 == S2) :-
	assertz(fact(o1), R1),
	assertz(fact(o2), R2),
	assertz(fact(o3), R3),
	msort([R3,R1,R2], S1),
	erase(R2),
	msort([R3,R1,R2], S2),
	erase(R1),
	erase(R3).
test(erased_ref, true) :-
	assertz(fact(e), R),
	erase(R),
	garbage_collect_atoms,
	\+ clause(_, _, R).
test(trie_gen_compiled_clause, error(type_error(trie_or_clause, _))) :-
	assertz(fact(t), R),
	call_cleanup(trie_gen_compiled(R, _), erase(R)).
test(local_clause, [L1-L2 == true-true, R1 == R2]) :-
	assertz(fact(l), R1),
	local_flag(R1, L1),
	clause(fact(l), true, R2),
	local_flag(R2, L2),
	erase(R1).
test(local_record, [L1-L2 == true-true, R1 == R2]) :-
	recordz(dbref_test, l, R1),
	local_flag(R1, L1),
	recorded(dbref_test, l, R2),
	local_flag(R2, L2),
	erase(R1).
test(shared_identity, [condition(threads), Same-Local == true-false]) :-
	assertz(fact(s), R),
	thread_self(Me),
	thread_create(( clause(fact(s), true, R2),
			thread_send_message(Me, ref(R2))
		      ), Id),
	thread_get_message(ref(R3)),
	thread_join(Id),
	same(R, R3, Same),
	local_flag(R, Local),
	erase(R).
test(local_reclaimed, [ true(Collected > 15_000), true(Live < 5_000),
			true(Grown < 100), Same == true ]) :-
	retractall(fact(_)),
	forall(between(1, 20_000, I), assertz(fact(I))),
	'$local_atoms'(collected, C0),
	statistics(atoms, A0),
	forall(clause(fact(_), true, _), true),
	garbage_collect_atoms,
	'$local_atoms'(collected, C1),
	'$local_atoms'(live, Live),
	statistics(atoms, A1),
	Collected is C1-C0,
	Grown is A1-A0,
	clause(fact(1), true, R1),		% a new reference
	nth_clause(fact(_), 1, R2),
	same(R1, R2, Same),
	retractall(fact(_)).
test(local_slots_reused, true(Grown < 5_000)) :-
	retractall(fact(_)),
	forall(between(1, 20_000, I), assertz(fact(I))),
	enum_refs,
	'$local_atoms'(highest, H0),
	enum_refs,
	enum_refs,
	'$local_atoms'(highest, H1),
	Grown is H1-H0,
	retractall(fact(_)).
test(promoted_kept, [Local == false, Same == true]) :-
	retractall(saved(_)),
	assertz(fact(p), R),
	assertz(saved(R)),			% promotes
	local_flag(R, Local),
	forall(between(1, 20_000, I), assertz(fact(I))),
	forall(clause(fact(_), true, _), true),
	garbage_collect_atoms,
	saved(R2),
	clause(fact(p), true, R3),
	same(R2, R3, Same),
	retractall(saved(_)),
	retractall(fact(_)).

threads :-
	current_prolog_flag(threads, true).

enum_refs :-
	forall(clause(fact(_), true, _), true),
	garbage_collect_atoms.

local_flag(Atom, Local) :-
	(   '$local_atom'(Atom)
	->  Local = true
	;   Local = false
	).

same(X, Y, Same) :-
	(   X == Y
	->  Same = true
	;   Same = false
	).

:- dynamic
	saved/1.

:- end_tests(dbref_blobs).
