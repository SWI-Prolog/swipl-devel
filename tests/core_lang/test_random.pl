/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           www.swi-prolog.org
    Copyright (c)  2011, University of Amsterdam
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

:- module(test_random,
	  [ test_random/0
	  ]).
:- use_module(library(dialect/sicstus4/lists), [subseq/3]).
:- use_module(library(lists), [numlist/3]).
:- use_module(library(plunit)).
:- use_module(library(random), [random_subseq/3, random_numlist/4]).

test_random :-
	run_tests([ random
		  ]).

/** <module> Test unit for random number handling
*/

:- begin_tests(random).

:- if(current_predicate(random_property/1)).

tr(N, L1, L2) :-
	set_random(seed(random)),
	random_property(state(State)),
	random_seq(N, L1),
	set_random(state(State)),
	random_seq(N, L2).

random_seq(N, [H|T]) :-
	succ(N2, N), !,
	H is random_float,
	random_seq(N2, T).
random_seq(0, []).

test(state, [X==Y]) :-
	tr(100, X,Y).

test(random_subseq, [
	forall((between(-3, 3, U), numlist(-3, U, List))),
	true(subseq(List, Subseq, Complement))
]) :-
	random_subseq(List, Subseq, Complement).

test(random_numlist, [
	forall(between(-3, 3, U)),
	true((numlist(-3, U, NumList), subseq(NumList, List, _)))
]) :-
	random_numlist(0.5, -3, U, List).

:- endif.

:- end_tests(random).
