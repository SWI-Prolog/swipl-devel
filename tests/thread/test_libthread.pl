/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2020, University of Amsterdam
                         VU University Amsterdam
		         CWI, Amsterdam
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

:- module(test_libthread,
          [ test_libthread/0
          ]).
:- use_module(library(plunit)).
:- use_module(library(debug)).
:- use_module(library(thread)).

test_libthread :-
    run_tests([ concurrent_and
              ]).

:- meta_predicate
    reclaims_threads(0).

:- begin_tests(concurrent_and, [sto(rational_trees)]).

test(basic, Answer == [1-2, 2-4, 3-6]) :-
    reclaims_threads(
        setof(X-Y, concurrent_and(between(1, 3, X), Y is X*2), Answer)).
test(select, Answer == [2-4]) :-
    reclaims_threads(
        setof(X-Y, concurrent_and(between(1, 3, X), (X == 2, Y is X*2)), Answer)).
test(cut, true) :-
    reclaims_threads(
        setof(X-Y,
              (concurrent_and(between(1, 5, X), Y is X^2), (X==3->!;true)),
              _Ans)).
test(error, error(evaluation_error(zero_divisor))) :-
    reclaims_threads(
        setof(X-Y, (concurrent_and(between(1, 5, X), Y is 1/(3-X))), _Answer)).
test(gen_error, error(evaluation_error(zero_divisor))) :-
    reclaims_threads(
        setof(X-Z,
              concurrent_and((between(1, 5, X),Y is 1/(3-X)), Z is 2*Y),
              _Answer)).

:- end_tests(concurrent_and).

reclaims_threads(Goal) :-
    findall(T, anon_thread(T), Before),
    Goal,
    findall(T, (anon_thread(T), \+ memberchk(T, Before)), StillRunning),
    assertion(StillRunning == []).

anon_thread(T) :-
    thread_property(T, id(_)),
    \+ thread_property(T, alias(_)).
