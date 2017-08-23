/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2014-2015, VU University Amsterdam
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

:- module(test_sandbox,
	  [ test_sandbox/0
	  ]).
:- use_module(library(plunit)).

/** <module> Test set for library(sandbox)


@tbd	A lot more tests.  In particular test and precise the relation
	between sandboxs and modules.
*/

test_sandbox :-
	run_tests([ sandbox
		  ]).

:- begin_tests(sandbox).
:- use_module(library(sandbox)).
:- use_module(library(aggregate)).

my_call(G) :-
	phrase({G},_).

test(cleanup) :-
	safe_goal(setup_call_cleanup(true,true,true)).
test(time) :-
	safe_goal(time(true)).
test(setof) :-
	safe_goal(setof(X, Y^between(1, Y, X), _Xs)).
test(phrase) :-
	safe_goal(phrase("hello", `hello`, [])).
test(apply) :-
	safe_goal(forall(true,true)).
test(aggregate) :-
	safe_goal(aggregate(count, between(1,10,_), _Count)).
test(aggregate) :-
	safe_goal(aggregate(sum(I), X^between(1,X,I), _Count)).
test(dcg, error(permission_error(call, sandboxed, open(_,_,_)))) :-
	safe_goal(my_call(open(_,_,_))).
test(contexr, error(permission_error(call, sandboxed, @(_,_)))) :-
	safe_goal(@(open(_,_,_), user)).

:- end_tests(sandbox).
