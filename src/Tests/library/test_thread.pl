/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           www.swi-prolog.org
    Copyright (c)  2007-2008, University of Amsterdam
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

:- module(test_thread, [test_thread/0]).

test_thread :-
	run_tests(thread).

:- use_module(library(plunit)).
:- use_module(library(thread)).

:- begin_tests(thread, [condition(current_prolog_flag(threads,true))]).

test(true, true) :-
	concurrent(2, [true], []).
test(unify, true(A==3)) :-
	concurrent(2, [A=3], []).
test(unify, true([A,B]==[3,4])) :-
	concurrent(2, [A=3, B = 4], []).
test(fail, fail) :-
	concurrent(2, [_A=3, fail, _B = 4], []).
test(error, throws(x)) :-
	concurrent(2, [_A=3, throw(x), _B = 4], []).
test(concur, true) :-
	forall(between(0, 20, _),
	       (   concurrent(2, [X=1,Y=2], []),
		   ground(X-Y))).

test(first, true(X==1)) :-
	first_solution(X, [X=1,X=1], []).
test(first, fail) :-
	first_solution(X, [fail,(sleep(1),X=1)], []).
test(first, true(X==1)) :-
	first_solution(X, [fail,(sleep(0.01),X=1)], [on_fail(continue)]).
test(first, true(X==1)) :-
	first_solution(X, [(repeat,fail), X=1], []).

:- end_tests(thread).
