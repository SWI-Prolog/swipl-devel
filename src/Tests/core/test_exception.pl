/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2007-2011, University of Amsterdam
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

:- module(test_exception, [test_exception/0]).
:- use_module(library(plunit)).

/** <module> Test Prolog core exception processing primitives

This module is a Unit test for   Prolog built-ins that handle exceptions
Please define a test-set for each predicate.

@author	Jan Wielemaker
*/

test_exception :-
	run_tests([ throw,
		    ex_coroutining
		  ]).

:- begin_tests(throw).

test(error, error(instantiation_error)) :-
	throw(_).

test(ground, throws(hello(world))) :-
	throw(hello(world)).

test(unbound, [ setup(Ball = hello(_)),
		throws(Ball)
	      ]) :-
	throw(Ball).

test(cyclic, [ sto(rational_trees), setup(Ball = hello(Ball)),
	       throws(Ball)
	     ]) :-
	throw(Ball).

:- end_tests(throw).


:- begin_tests(ex_coroutining).

test(not, error(foo)) :-
	freeze(X, throw(error(foo, bar))),
	\+ X = x.
test(non_unify, error(foo)) :-		% verifies error handling in
					% foreignWakeup()
	freeze(X, throw(error(foo, bar))),
	X \= x.

:- end_tests(ex_coroutining).
