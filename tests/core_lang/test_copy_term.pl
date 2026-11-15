/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           www.swi-prolog.org
    Copyright (c)  2021, SWI-Prolog Solutions b.v.
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

:- module(test_copy_term,
	  [ test_copy_term/0
	  ]).
:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(ordsets)).
:- use_module(library(plunit)).

test_copy_term :-
    run_tests([ copy_term,
		copy_term_4,
                copy_term_nat_4
	      ]).

/** <module> Test unit for copy_term/2 and friends
*/

:- begin_tests(copy_term).

test(share, Shared == [x(1)]) :-
    test_copy(t(_, x(1)), Shared).
test(cycle, [sto(rational_trees)]) :-
    f(X) = X,
    test_copy(X, _Shared).

:- end_tests(copy_term).

test_copy(Term, Shared) :-
    copy_term(Term, Copy),
    assertion(Term =@= Copy),
    shared_subterms(Term, Copy, Shared).

		 /*******************************
		 *         COPY_TERM/4		*
		 *******************************/

:- begin_tests(copy_term_4).

test(share, Shared == [x(Y)]) :-
    test_copy([X], q(X, x(Y)), Shared).
test(share, Shared == [x(Y)]) :-
    test_copy([X], q(X, t(X), x(Y)), Shared).
test(attvar, Shared == [x(Y)]) :-
    freeze(X, writeln(X)),
    test_copy([X], q(X, x(Y)), Shared).
test(attvar, Shared == [x(Y)]) :-
    freeze(X, writeln(X)),
    test_copy([X], q(X, t(X), x(Y)), Shared).
test(attvar, Shared == [x(Y)]) :-
    freeze(Y, writeln(Y)),
    test_copy([X], q(X, x(Y)), Shared).
test(attvar,  CQ == q(CX,Y)) :-
    freeze(Y, writeln(Y)),
    copy_term(X, q(X,Y), CX, CQ).
test(cycle, [sto(rational_trees), O == f(D,C)]) :-
    A = f(A,B),
    copy_term(B, f(B,C), D, O).

:- end_tests(copy_term_4).

:- begin_tests(copy_term_nat_4).

test(attvar,  CQ == q(CX,Y)) :-
    freeze(X, writeln(X)),
    copy_term_nat(X, q(X,Y), CX, CQ),
    assertion(\+attvar(CX)).
test(attvar,  CQ == q(CX,Y)) :-
    freeze(Y, writeln(Y)),
    copy_term_nat(X, q(X,Y), CX, CQ),
    assertion(attvar(Y)).

:- end_tests(copy_term_nat_4).



test_copy(Vs, Term, Shared) :-
    must_copy(Vs, Term, CopyVars, _NoCopyVars),
    copy_term(Vs, Term, VsCopy, Copy),
    assertion(Vs+Term =@= VsCopy+Copy),
    (   CopyVars \== []
    ->  assertion(Vs \== VsCopy)
    ;   true
    ),
    shared_subterms(Term, Copy, Shared).

must_copy(Vs, Term, Copy, NoCopy) :-
    term_variables(Vs, VsVars),
    term_variables(Term, TermVars),
    sort(VsVars, VsVarsSet),
    sort(TermVars, TermVarsSet),
    ord_intersection(VsVarsSet, TermVarsSet, Copy),
    ord_subtract(TermVarsSet, VsVarsSet, NoCopy).

shared_subterms(T, Copy, Shared) :-
    factorize_term(cp(T, Copy), _Skel, Subst),
    maplist(arg(2), Subst, Shared0),
    msort(Shared0, Shared).

factorize_term(Term, Skeleton, Substitution) :-
    '$factorize_term'(Term, Skeleton, Substitution).
