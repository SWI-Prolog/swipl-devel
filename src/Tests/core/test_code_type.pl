/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2015, University of Amsterdam
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

:- module(test_code_type, [test_code_type/0]).
:- use_module(library(plunit)).
:- use_module(library(apply)).

/** <module> Test Prolog text code_typeting primitives

This module is a Unit test for  Prolog code_type/2, etc.

@author	Jan Wielemaker
*/

test_code_type :-
	run_tests([ code_type
		  ]).

:- begin_tests(code_type).

test(code_type, true) :-
	assert_ct,
	gen.

:- end_tests(code_type).

:- thread_local ct/2.

assert_ct :-
	retractall(ct(_,_)),
	forall(( between(0, 255, C),
		 code_type(C, T)
	       ),
	       assertz(ct(C,T))).

gen_t(T) :-
	ct(_C,T0),
	(   atom(T0)
	->  T = T0
	;   functor(T0,F,A),
	    functor(T,F,A)
	).

gen :-
	setof(T, gen_t(T), TL),
	maplist(gen, TL).

gen(T) :-
	(setof(C, code_type(C,T), CL) -> true ; CL = []),
	(setof(C, ct(C,T), CL2) -> true ; CL2 = []),
	(   CL == CL2
	->  true
	;   format('ERROR: code_type ~p: ~p \\== ~p~n', [T, CL, CL2]),
	    fail
	).
