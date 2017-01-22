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
		    recorded
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
