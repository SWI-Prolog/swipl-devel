/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2009-2017, University of Amsterdam
                              VU University Amsterdam
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

:- module(test_db, [test_db/0]).
:- use_module(library(plunit)).

/** <module> Test Prolog core database functions

@author	Jan Wielemaker
*/

test_db :-
	run_tests([ assert,
		    retract,
		    retractall,
		    dynamic,
		    protect,
		    res_compiler
		  ]).

:- begin_tests(assert).

:- dynamic
	term/0,
	f/1, f/2, f/0.

test(right_cyclic_head, [ sto(rational_trees),
			  error(representation_error(cyclic_term))
			]) :-
	X = f(X),
	assert(X).
test(cyclic_head, [ sto(rational_trees),
			  error(representation_error(cyclic_term))
			]) :-
	X = f(X, 1),
	assert(X).
test(cyclic_body, [ sto(rational_trees),
		    error(representation_error(cyclic_term))
		  ]) :-
	X = f(X),
	assert((f(a) :- X)).

test(cut_cond, Body = (! -> fail)) :-
	assert(f :- (! -> fail)),
	clause(f, Body),
	retractall(f).

:- end_tests(assert).

:- begin_tests(retract).

:- dynamic foo/1, insect/1, icopy/1.

test(theorist) :-
	(   assert((foo(A) :- bar(A))),
	    retract(foo(1) :- B)
	->  B == bar(1)
	).
test(theorist, [cleanup(retractall(foo(_)))]) :-
	assert((foo(A) :- bar(A))),
	\+ retract(foo(1) :- bar(2)).
test(update_view, L == [ant,bee]) :-
	retractall(insect(_)),
	retractall(icopy(_)),
	assertz(insect(ant)),
	assertz(insect(bee)),
	(   retract(insect(I)),
	    assertz(icopy(I)),
	    retract(insect(bee)),
	    fail
	;   findall(I, retract(icopy(I)), L)
	).
test(concurrent, Sum == ConcurrentSum) :-
	N = 10000,
	numlist(0, N, List),
	sum_list(List, Sum),
	forall(between(0, N, X),
	       assertz(a(X))),
	thread_self(Me),
	thread_create(collect(Me), Id1, []),
	thread_create(collect(Me), Id2, []),
	thread_join(Id1, true),
	thread_join(Id2, true),
	thread_get_message(collected(N1)),
	thread_get_message(collected(N2)),
	ConcurrentSum is N1+N2.

collect(Main) :-
	collect(0, N),
	thread_send_message(Main, collected(N)).

collect(N0, N) :-
	retract(a(A)), !,
	N1 is N0 + A,
	collect(N1, N).
collect(N, N).


:- end_tests(retract).

:- begin_tests(retractall).

:- dynamic
	db/2.

init_db :- forall(between(1,2,X),
		  forall(between(1,2,Y),
			 assert(db(X,Y)))).
clear_db :-
	retractall(db(_,_)).

test(all, [setup(init_db),cleanup(clear_db),All=[]]) :-
	retractall(db(_,_)),
	findall(db(X,Y), db(X,Y), All).
test(one, [setup(init_db),cleanup(clear_db),All=[db(2,1),db(2,2)]]) :-
	retractall(db(1,_)),
	findall(db(X,Y), db(X,Y), All).
test(shared, [setup(init_db),cleanup(clear_db),All=[db(1,2),db(2,1)]]) :-
	retractall(db(X,X)),
	findall(db(X,Y), db(X,Y), All).
test(type, error(type_error(callable, _))) :-
	retractall(3).
test(type, error(permission_error(modify, static_procedure, _))) :-
	retractall(retractall(_)).

:- end_tests(retractall).


:- begin_tests(dynamic).

test(make_dynamic, [true, cleanup(abolish(Name, 1))]) :-
	gensym(somepred, Name),
	Term =.. [Name, a],
	catch(Term, _, true),
	assertz(Term),
	Term.

:- end_tests(dynamic).

:- begin_tests(protect, [ setup(set_prolog_flag(protect_static_code, true)),
			  setup(set_prolog_flag(protect_static_code, false))
			]).

p1.
:- '$clausable'(p2/0).
p2.

test(clause, error(permission_error(access, private_procedure, _))) :-
	clause(p1, _).
test(clause, Body == true) :-
	clause(p2, Body).
test(dynamic, error(permission_error(modify, static_procedure, _))) :-
	dynamic(p1/0).
test(clausable, error(permission_error(modify, static_procedure, _))) :-
	'$clausable'(p1/0).

:- end_tests(protect).


:- begin_tests(res_compiler).

:- dynamic
        tmp/2.

test_big_clause(N) :-
        link_clause(N, V0, V, Body),
        retractall(tmp(_,_)),
        Clause = (tmp(V0, V) :- Body),
        assert(Clause, Ref),
        tmp(0, X),
        erase(Ref),
        assertion(X == N).

link_clause(1, V0, V, succ(V0, V)) :- !.
link_clause(N, V0, V, (succ(V0, V1), G)) :-
        N2 is N - 1,
        link_clause(N2, V1, V, G).

test(big_clause,
     [ true,
       cleanup(trim_stacks)
     ]) :-
	test_big_clause(60000).

:- end_tests(res_compiler).
