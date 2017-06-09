/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2014, University of Amsterdam
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

:- module(test_inflimit,
	  [ test_inflimit/0
	  ]).
:- use_module(library(random)).
:- use_module(library(plunit)).
:- use_module(library(debug)).

test_inflimit :-
	run_tests([ call_with_inference_limit
		  ]).

outer :-
	call_with_inference_limit(inner(100), 500, Result),
	(   Result == inference_limit_exceeded
	->  p(+)
	;   format(user_error, '~NOops, no timeout (~w)!!~n', [Result]),
	    fail
	).

inner(N) :-
	forall(between(0, N, _),
	       forall(inner, true)).

inner :-
	Limit is random(100),
	call_with_inference_limit(goal, Limit, Result),
	(   Result == inference_limit_exceeded
	->  p(!)
	;   p(.)
	).

goal :-
	forall(between(1,25,_), true),
	maybe(0.3),				% may fail
	(   maybe(0.5)				% may succeed nondet
	;   true				% or det
	).

p(_) :- !.
p(C) :-	put_char(user_error, C).

:- begin_tests(call_with_inference_limit).

test(nesting, true) :-
	forall(between(1, 100, _), outer).

test(retry, true) :-
	call_with_inference_limit(length(_L,N), 1000, Result),
	forall(between(1,100,_), true),
	assertion(Result == true),
	N =:= 100, !.

:- end_tests(call_with_inference_limit).
