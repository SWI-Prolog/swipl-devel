/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           www.swi-prolog.org
    Copyright (c)  2010-2011, University of Amsterdam
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

:- module(test_factorize,
	  [ test_factorize/0
	  ]).
:- use_module(library(plunit)).
:- use_module(library(terms)).
:- use_module(library(apply)).

test_factorize :-
	run_tests([ factorize
		  ]).

:- meta_predicate ok(+, 0).

factorize_term(Term, Skeleton, Substitution) :-
	'$factorize_term'(Term, Skeleton, Substitution).

test_factor(Term) :-
	copy_term(Term, Save),
	term_factorized(Term, FOK, _BOK),
	(   factorize_term(Term, FT, BT),
	    ok(skeleton, variant(FOK, FT)),
	    maplist(call, BT),
	    ok(rebind, variant(FT, Save)),
	    garbage_collect,
	    fail
	;   ok(backtrack, variant(Term, Save))
	).

ok(Id, G) :-
	(   G
	->  true
	;   throw(failed(Id, G))
	).

fumo(0,fumo) :- !.
fumo(N,[F|F]) :-
        N1 is N-1,
        fumo(N1,F).

:- begin_tests(factorize, [sto(rational_trees)]).

test(simple, true) :-
	X = a, test_factor(X).
test(simple, true) :-
	X = a(1), test_factor(X).
test(cyclic, true) :-
	X = a(X), test_factor(X).
test(double, true) :-
	X = a(X,X), test_factor(X).
test(double_cyclic, true) :-
	A = a(X), X = a(A), test_factor(X).
test(double_cyclic, true) :-
	A = a(A), X = x(A,A), test_factor(X).
test(double_cyclic, true) :-
	X = x(A,A), A = a(A), test_factor(X).
test(double_cyclic, true) :-
	A = a(A), X = x(b(A),A), test_factor(X).
test(fumo, true) :-
	fumo(20, X), test_factor(X).

:- end_tests(factorize).
