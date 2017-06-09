/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2008-2013, University of Amsterdam
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

:- module(test_prolog_clause,
	  [ test_prolog_clause/0
	  ]).
:- use_module(library(plunit)).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(prolog_clause)).

/** <module> Test source-information on clauses

This  module  tests  clause_info/4,  a  vital  part  of  the  SWI-Prolog
source-level debugger.
*/

%t :-
%	debug(clause_info),
%	test_prolog_clause.

test_prolog_clause :-
	run_tests([ prolog_clause
		  ]).

:- begin_tests(prolog_clause).

:- style_check(-no_effect).
:- '$clausable'((t0/0,t1/0,t2/0,t3/0,t4/0,t5/0)).
t0 :-	a(X), b(Y), X=Y.
t1 :-	a, _ = hello, b.
t2 :-	a(X), a(b) = X, b(X).
t3 :-	a, _ == hello, b.
t4 :-	a(X), x == X, b(X).
t5 :-	i(A), B is A-1, b(B).
:- style_check(+no_effect).

a.
b.
a(_).
b(_).
i(10).

test_ci(Head) :-
	clause(Head, _Body, Ref),
	clause_info(Ref, File, _TermPos, _NameOffset),
	atom(File).

test(t0) :- test_ci(t0).
test(t1) :- test_ci(t1).
test(t2) :- test_ci(t2).
test(t3) :- test_ci(t3).
test(t4) :- test_ci(t4).

:- end_tests(prolog_clause).
