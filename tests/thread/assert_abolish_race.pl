/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org
    Copyright (c)  2026, SWI-Prolog Solutions b.v.
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

:- module(assert_abolish_race,
	  [ assert_abolish_race/0
	  ]).
:- use_module(library(plunit)).
:- use_module(library(debug)).

/** <module> Assert clauses while the predicate is abolished

abolish/1 could erase a clause that assert/1 was adding such that it
remained visible and was never reclaimed.
*/

assert_abolish_race :-
	run_tests([ assert_abolish_race
		  ]).

:- begin_tests(assert_abolish_race).

test(erased, fail) :-
	race(q, true),
	predicate_property(assert_abolish_race:q(_), defined).

:- end_tests(assert_abolish_race).

%   race(+Name, +Catch)
%
%   Assert clauses for Name/1 while another thread abolishes it.  If
%   Catch is true, ignore errors from assert/1.

race(Name, Catch) :-
	forall(between(1, 10, _),
	       ( thread_create(assert_n(Name, 10000, Catch), A, []),
		 thread_create(abolish_n(Name, 10000), B, []),
		 thread_join(A, SA),
		 thread_join(B, SB),
		 assertion(SA-SB == true-true),
		 abolish(Name/1)
	       )).

assert_n(Name, N, Catch) :-
	forall(between(1, N, I),
	       ( G =.. [Name, I],
		 (   Catch == true
		 ->  catch(assertz(G), _, true)
		 ;   assertz(G)
		 )
	       )).

abolish_n(Name, N) :-
	forall(between(1, N, _), abolish(Name/1)).
