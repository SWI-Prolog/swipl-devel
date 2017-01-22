/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           www.swi-prolog.org
    Copyright (c)  2010-2015, University of Amsterdam
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

:- module(test_assoc,
	  [ test_assoc/0
	  ]).
:- use_module(library(plunit)).
:- use_module(library(assoc)).

% Test new predicates of assoc library
% Author: Glenn Burgess May 2010
% Modified for using PlUnit by Jan Wielemaker

test_assoc :-
	run_tests([ assoc
		  ]).

:- begin_tests(assoc).

% Test is_assoc first as all other tests rely on this

test(wirth_85, true) :-			% Test tree from Wirth 85
	is_assoc(t(5, 1, -, t(3, 1, <, t(2, 1, <, t(1, 1, -, t, t), t), t(4, 1, -, t, t)), t(8, 1, -, t(7, 1, <, t(6, 1, -, t, t), t), t(10, 1, -, t(9, 1, -, t, t), t(11, 1, -, t, t))))).

test(unbalanced_1, fail) :-
	is_assoc(t(5, 1, <, t(3, 1, <, t(2, 1, <, t(1, 1, -, t, t), t), t(4, 1, -, t, t)), t(8, 1, -, t(7, 1, <, t(6, 1, -, t, t), t), t(10, 1, -, t(9, 1, -, t, t), t(11, 1, -, t, t))))).

test(unbalanced_2, fail) :-
	is_assoc(t(5, 1, -, t(3, 1, -, t(2, 1, <, t(1, 1, -, t, t), t), t(4, 1, -, t, t)), t(8, 1, -, t(7, 1, <, t(6, 1, -, t, t), t), t(10, 1, -, t(9, 1, -, t, t), t(11, 1, -, t, t))))).

test(unbalanced_3, fail) :-
	is_assoc(t(5, 1, -, t(3, 1, <, t(2, 1, <, t(1, 1, -, t, t), t), t(4, 1, -, t, t)), t(8, 1, -, t(7, 1, <, t(6, 1, -, t, t), t), t(10, 1, -, t(9, 1, -, t, t), t(11, 1, >, t, t))))).

test(unordered_1, fail) :-
	is_assoc(t(5, 1, -, t(3, 1, <, t(2, 1, >, t, t(1, 1, -, t, t)), t(4, 1, -, t, t)), t(8, 1, -, t(7, 1, <, t(6, 1, -, t, t), t), t(10, 1, -, t(9, 1, -, t, t), t(11, 1, -, t, t))))).

test(unordered_2, fail) :-
	is_assoc(t(5, 1, -, t(4, 1, <, t(2, 1, <, t(1, 1, -, t, t), t), t(4, 1, -, t, t)), t(8, 1, -, t(7, 1, <, t(6, 1, -, t, t), t), t(10, 1, -, t(9, 1, -, t, t), t(11, 1, -, t, t))))).

test(unordered_3, fail) :-
	is_assoc(t(5, 1, -, t(3, 1, <, t(2, 1, <, t(1, 1, -, t, t), t), t(6, 1, -, t, t)), t(8, 1, -, t(7, 1, <, t(6, 1, -, t, t), t), t(10, 1, -, t(9, 1, -, t, t), t(11, 1, -, t, t))))).

test(bad_structure_1, fail) :-
	is_assoc(t(5, 1, =, t(3, 1, <, t(2, 1, <, t(1, 1, -, t, t), t), t(4, 1, -, t, t)), t(8, 1, -, t(7, 1, <, t(6, 1, -, t, t), t), t(10, 1, -, t(9, 1, -, t, t), t(11, 1, -, t, t))))).

test(bad_structure_2, fail) :-
	is_assoc(t(5, 1, -, t(3, 1, <, t(2, 1, <, t(1, 1, -, t, t), t), t(4, 1, -, t, t)), t(8, 1, -, t(7, 1, <, t(6, 1, -, t, t), t), t(10, 1, -, t(9, 1, -, t, t), t(11, 1, -, f, t))))).

test(bad_structure_keys_not_ground, fail) :-
	is_assoc(t(5, 1, -, t(3-_X, 1, <, t(2, 1, <, t(1, 1, -, t, t), t), t(4, 1, -, t, t)), t(8, 1, -, t(7, 1, <, t(6, 1, -, t, t), t), t(10, 1, -, t(9, 1, -, t, t), t(11, 1, -, t, t))))).

% Test del_min_assoc.
% Recursively extract all elements (first to last)
% At each step check that returned association list is still valid (balanced etc.)
del_min_assoc_to_list( A, L ) :-
	del_min_assoc_to_list( A, [], L ).

del_min_assoc_to_list( t, L, L) :- !.

del_min_assoc_to_list( A, Rest, [Key-Val | Result]) :-
	del_min_assoc( A, Key, Val, A1 ) ->
	is_assoc(A1),
	del_min_assoc_to_list(A1, Rest, Result).

% Test del_max_assoc.
% Recursively extract all elements (last to first)
% At each step check that returned association list is still valid (balanced etc.)
del_max_assoc_to_list( A, L ) :-
	del_max_assoc_to_list( A, [], L ).

del_max_assoc_to_list( t, L, L) :- !.

del_max_assoc_to_list( A, Rest, Result) :-
	del_max_assoc( A, Key, Val, A1 ) ->
	is_assoc(A1),
	del_max_assoc_to_list(A1, [Key-Val | Rest], Result).

test(del_max_min, true) :-
	Wirth = t(5, 1, -, t(3, 1, <, t(2, 3, <, t(1, 4, -, t, t), t), t(4, 5, -, t, t)), t(8, 6, -, t(7, 7, <, t(6, 8, -, t, t), t), t(10, 9, -, t(9, 10, -, t, t), t(11, 11, -, t, t)))),
	del_max_assoc_to_list(Wirth,L),
	assoc_to_list(Wirth,L),
	del_min_assoc_to_list(Wirth,L).

% Test del_assoc
% Extract elements one at a time, comparing results to expected result from Wirth 85.
test_del_assoc( K, Win, V, Wout ) :-
	del_assoc(K, Win, V1, W1) ->
	V = V1,
	Wout = W1,
	is_assoc(W1).

test(wirth_del, true) :-
	Wirtha = t(5, 1, -, t(3, 1, <, t(2, 3, <, t(1, 4, -, t, t), t), t(4, 5, -, t, t)), t(8, 6, -, t(7, 7, <, t(6, 8, -, t, t), t), t(10, 9, -, t(9, 10, -, t, t), t(11, 11, -, t, t)))) ->
	test_del_assoc(4, Wirtha, 5, Wb),
	Wb = t(5, 1, >, t(2, 3, -, t(1, 4, -, t, t), t(3, 1, -, t, t)), t(8, 6, -, t(7, 7, <, t(6, 8, -, t, t), t), t(10, 9, -, t(9, 10, -, t, t), t(11, 11, -, t, t)))),
	test_del_assoc(8, Wb, 6, Wc),
	Wc = t(5, 1, >, t(2, 3, -, t(1, 4, -, t, t), t(3, 1, -, t, t)), t(7, 7, >, t(6, 8, -, t, t), t(10, 9, -, t(9, 10, -, t, t), t(11, 11, -, t, t)))),
	test_del_assoc(6, Wc, 8, Wd),
	Wd = t(5, 1, >, t(2, 3, -, t(1, 4, -, t, t), t(3, 1, -, t, t)), t(10, 9, <, t(7, 7, >, t, t(9, 10, -, t, t)), t(11, 11, -, t, t))),
	test_del_assoc(5, Wd, 1, We1),
	We1 = t(7, 7, -, t(2, 3, -, t(1, 4, -, t, t), t(3, 1, -, t, t)), t(10, 9, -, t(9, 10, -, t, t), t(11, 11, -, t, t))),
   % Wirth's balance algorithm yields a different result for the above. Ours is 'more balanced' in this case.
   % but for the rest of the tests a closer structure is achieved by deleting node 3 (and changing 5 to 3
   % which I haven't done)
	test_del_assoc(3, Wd, 1, We),
	We = t(5, 1, >, t(2, 3, <, t(1, 4, -, t, t), t), t(10, 9, <, t(7, 7, >, t, t(9, 10, -, t, t)), t(11, 11, -, t, t))),
	test_del_assoc(2, We, 3, Wf),   % double rotation to rebalance
	Wf = t(7, 7, -, t(5, 1, <, t(1, 4, -, t, t), t), t(10, 9, -, t(9, 10, -, t, t), t(11, 11, -, t, t))),
	test_del_assoc(1, Wf, 4, Wg),
	Wg = t(7, 7, >, t(5, 1, -, t, t), t(10, 9, -, t(9, 10, -, t, t), t(11, 11, -, t, t))),
	test_del_assoc(7, Wg, 7, Wh),
   % Again, Wirth's algorithm balances the tree differently, although both are correct.
	Wh = t(9, 10, >, t(5, 1, -, t, t), t(10, 9, >, t, t(11, 11, -, t, t))).

% Ah, but the above doesn't test enough cases, possibly because our algorithm is slightly different from Wirth's. So try this:

rand_list( N, L ) :-
	rand_list( N, [], L0 ),
	sort(L0, L).

rand_list( 0, L, L ) :- !.
rand_list( N, Lin, Lout ) :-
	N1 is N - 1,
	R is random(100),
	rand_list(N1, [R-1 | Lin], Lout).

% Exercise (in deletion) avl_geq clauses 1,2 and 5
test(random_min, true) :-
	rand_list(100,L),
	list_to_assoc(L,A),
	del_min_assoc_to_list(A,_L1).

% Exercise (in deletion) avl_geq clauses 3,4 and 6
test(random_max, true) :-
	rand_list(100,L),
	list_to_assoc(L,A),
	del_max_assoc_to_list(A,_L1).

test(del_empty, true) :-
	empty_assoc(A),
	\+ del_min_assoc(A, _, _, _).

test(del_no_member, true) :-
	list_to_assoc([1-a,2-b,4-c], A),
	\+ del_min_assoc(3, A, _, _).

:- end_tests(assoc).
