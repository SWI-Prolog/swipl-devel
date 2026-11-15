/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           www.swi-prolog.org
    Copyright (c)  2016, University of Amsterdam
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

:- module(test_skip_list, [test_skip_list/0]).
:- use_module(library(plunit)).


test_skip_list :-
	run_tests([ skip_list ]).



:- begin_tests(skip_list).

test(empty_list) :-
	'$skip_list'(Len, [], Diff),
	Len == 0,
	Diff == [].

test(proper_list_small) :-
	'$skip_list'(Len, [_], Diff),
	Len == 1,
	Diff == [].

test(proper_list_long) :-
	length(List, 10000),
	'$skip_list'(Len, List, Diff),
	Len == 10000,
	Diff == [].

test(partial_list) :-
	'$skip_list'(Len, [_|X], Diff),
	Len == 1,
	Diff == X.

test(cyclic_list, [sto(rational_trees)]) :-
	List = [_|List],
	'$skip_list'(Len, List, Diff),
	Len >= 1,
	nonvar(Diff),
	Diff = [_|_].

test(cyclic_list_long, [sto(rational_trees)]) :-
	List = [_|X],
	length(List0, 10000),
	append(List0, List, X),
	'$skip_list'(Len, List, Diff),
	Len >= 10001,
	nonvar(Diff),
	Diff = [_|_].

test(not_a_list_1, [sto(rational_trees)]) :-
	'$skip_list'(Len, X, Diff),
	Len == 0,
	Diff == X.

test(not_a_list_2, [sto(rational_trees)]) :-
	'$skip_list'(Len, x, Diff),
	Len == 0,
	Diff == x.

test(not_a_list_3, [sto(rational_trees)]) :-
	'$skip_list'(Len, [_|x], Diff),
	Len == 1,
	Diff == x.

:- end_tests(skip_list).
