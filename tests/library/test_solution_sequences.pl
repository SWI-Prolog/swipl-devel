/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2015, VU University Amsterdam
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

:- module(test_solution_sequences,
	  [ test_solution_sequences/0
	  ]).


:- use_module(library(plunit)).
:- use_module(library(solution_sequences)).

test_solution_sequences :-
	run_tests([ test_solution_sequences
		  ]).

:- begin_tests(test_solution_sequences).

data(1, a, a1).
data(1, a, ax).
data(1, b, a2).
data(2, a, n1).
data(2, b, n2).
data(2, b, n0).

test(distinct, all(A-B-C == [1-a-a1,2-a-n1])) :-
	distinct(A, data(A,B,C)).

test(limit, all(X == [1,2,3])) :-
	limit(3, between(1, 10, X)).

test(offset, all(X == [2,3,4,5])) :-
	offset(1, between(1, 5, X)).

% Order by

test(a, all(A-B-C ==
	    [1-a-a1, 1-a-ax, 1-b-a2, 2-a-n1, 2-b-n2, 2-b-n0])) :-
	order_by([asc(A)], data(A,B,C)).
test(d, all(A-B-C ==
            [2-a-n1, 2-b-n2, 2-b-n0, 1-a-a1, 1-a-ax, 1-b-a2])) :-
	order_by([desc(A)], data(A,B,C)).
test(ad, all(A-B-C ==
	    [1-b-a2, 1-a-a1, 1-a-ax, 2-b-n2, 2-b-n0, 2-a-n1])) :-
	order_by([asc(A),desc(B)], data(A,B,C)).

test(group_by, all(A-Bag == [1-[a,a,b],2-[a,b,b]])) :-
	group_by(A, B, data(A,B,_), Bag).

:- end_tests(test_solution_sequences).
