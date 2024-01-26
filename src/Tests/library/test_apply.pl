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

:- module(test_apply,
          [ test_apply/0
          ]).
:- use_module(library(apply)).
:- use_module(library(plunit)).
:- use_module(library(solution_sequences)).
:- use_module(library(yall)).

% Test foldl of library(apply)
% Author: David Tonhofer Dec 2020

test_apply :-
    run_tests([ foldl
              ]).


:- begin_tests(foldl).

false(_,_,_) :-
        false.

test("foldl empty, starter is atom", true(V == foo)) :-
   foldl(false, [], foo, V).

% an unbound variable as starter is actually not supported according to
% the mode flags

test("foldl empty, starter is unbound", VA == VB) :-
   foldl(false, [], VA, VB).

test("foldl construction to verify order of arguments, 1 list",
     V == startabcd) :-
   foldl([E,FL,TR]>>atom_concat(FL,E,TR),
         [a,b,c,d],
         start, V).

test("foldl construction to verify order of arguments, 2 lists",
     V == 'start(a1)(b2)(c3)(d4)') :-
   foldl([E1,E2,FL,TR]>>atomic_list_concat([FL,'(',E1,E2,')'],TR),
         [a,b,c,d],
         [1,2,3,4],
         start, V).

test("foldl construction to verify order of arguments, 3 lists",
     V == 'start(a1x)(b2y)(c3z)(d4k)') :-
   foldl([E1,E2,E3,FL,TR]>>atomic_list_concat([FL,'(',E1,E2,E3,')'],TR),
         [a,b,c,d],
         [1,2,3,4],
         [x,y,z,k],
         start, V).

test("foldl construction to verify order of arguments, 4 lists",
     V == 'start(a1xq)(b2yw)(c3ze)(d4kr)') :-
    foldl([E1,E2,E3,E4,FL,TR]>>atomic_list_concat([FL,'(',E1,E2,E3,E4,')'],TR),
          [a,b,c,d],
          [1,2,3,4],
          [x,y,z,k],
          [q,w,e,r],
          start, V).

% an unbound variable as list is actually not supported according to
% the mode flags

test("foldl construction, unifying unbound variables",
     [L,V] == [[k,k,k,k,k,k],k]) :-
    length(L,6),
    foldl([X,X,X]>>true, L, K, V),
    K = k.

test("foldl building sequence of monotonically increasing ints",
     [L,Final] == [[0, 1, 2, 3, 4, 5],6]) :-
    length(L,6),
    foldl([E,FL,TR]>>(succ(FL,TR),FL=E), L, 0, Final).

test("foldl building sequence of monotonically increasing ints, just verify") :-
    foldl([E,FL,TR]>>(succ(FL,TR),FL=E), [0,1,2,3,4,5], 0, 6).

test("foldl breakoff with failure", fail) :-
    foldl([E,_,_]>>call(E), [true,false,type_error(_,_)], _, _).

test("foldl breakoff with exception", error(type_error(_,_))) :-
    foldl([E,_,_]>>call(E), [true,type_error(_,_)], _, _).

% an unbound variable as list is actually not supported according to the mode flags
test("foldl on list that is initially an unbound variable",
     Bag = [[[], 0], [[0], 1], [[0, 1], 2], [[0, 1, 2], 3]]) :-
    findall(
        [L,Final],
        limit(4,foldl([E,FL,TR]>>(succ(FL,TR),FL=E), L, 0, Final)),
        Bag).

:- end_tests(foldl).


