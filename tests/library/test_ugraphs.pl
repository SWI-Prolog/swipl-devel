/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2023, SWI-Prolog Solutions b.v.
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

:- module(test_ugraphs,
	  [ test_ugraphs/0
	  ]).
:- use_module(library(plunit)).
:- use_module(library(ugraphs)).

test_ugraphs :-
    run_tests([ top_sort
		  ]).

:- begin_tests(top_sort).

test(linear, L == [1,2,3]) :-
    top_sort([1-[2], 2-[3], 3-[]], L).
test(linear, L == [[1],[2],[3]]) :-
    ugraph_layers([1-[2], 2-[3], 3-[]], L).
test(multi, L == [[1], [2, 4], [3]]) :-
    ugraph_layers([1-[2,4], 2-[3], 3-[], 4-[]], L).
test(multi,  L == [[1], [2, 3], [4]]) :-
    ugraph_layers([1-[2,3], 2-[4], 3-[], 4-[]], L).
test(disconnected, L = [[1, 4], [2], [3]]) :-
    ugraph_layers([1-[2,3], 2-[3], 3-[], 4-[]], L).
test(cyclic, fail) :-
    ugraph_layers([1-[2,3], 2-[3], 3-[2], 4-[5], 5-[]], _L).

:- end_tests(top_sort).
