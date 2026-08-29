/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2007-2024, University of Amsterdam
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

:- module(test_occurs_check, [test_occurs_check/0]).
:- use_module(library(plunit)).

/** <module> Test occurs check

This module is a test-frame for the occurs_check flag.
*/

test_occurs_check :-
        run_tests([ unify_with_occurs_check,
                    occurs_check_fail,
		    occurs_check_error
		  ]).

has_occurs_check_flag :-
	catch(current_prolog_flag(occurs_check, _), _, fail).

		 /*******************************
		 *	   UTILITY PREDS	*
		 *******************************/

unify(X, X).

		 /*******************************
		 *	OCCURS-CHECK TESTS	*
		 *******************************/

:- begin_tests(unify_with_occurs_check).

test(simple_1) :-
    \+ unify_with_occurs_check(A, list(A)).
test(simple_2) :-
    unify_with_occurs_check(_A, _B).
test(attvar_1) :-               % test wakeup
    freeze(X, X = Y),
    unify_with_occurs_check(X, a),
    Y == a.
test(attvar_2) :-               % test occurs-check
    freeze(A, true),
    \+ unify_with_occurs_check(A, list(A)).
test(attvar_3) :-
    freeze(A, true),
    unify_with_occurs_check(A, A).
test(attvar_4) :-
    freeze(A, true),
    freeze(B, true),
    unify_with_occurs_check(A, B).

:- end_tests(unify_with_occurs_check).

:- begin_tests(occurs_check_fail,[]).

test(unify, [sto(finite_trees),fail]) :-
	X = f(X).
test(unify, [sto(finite_trees),fail]) :-
	unify(X, f(X)).
test(unifiable, [sto(finite_trees),fail]) :-
	unifiable(X, f(X), _).

:- end_tests(occurs_check_fail).


:- begin_tests(occurs_check_error,[condition(has_occurs_check_flag)]).

error_unification :-
	current_prolog_flag(occurs_check,error).

test(unify, [condition(error_unification),error(occurs_check(X, f(X)))]) :-
	X = f(X).
test(unify, [condition(error_unification),error(occurs_check(X, f(X)))]) :-
	unify(X, f(X)).
test(unifiable, [condition(error_unification),error(occurs_check(X, f(X)))]) :-
	unifiable(X, f(X), _).
test(?=,  [condition(error_unification),error(occurs_check(X, f(X)))]) :-
	?=(X, f(X)).
test(head, [condition(error_unification),error(occurs_check(X, s(X)))]) :-
        my_unify(X,X).

my_unify(X,s(X)) :-
        fail.
my_unify(_,_) :-
        fail.

:- end_tests(occurs_check_error).


