/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2020, VU University Amsterdam
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

:- module(test_intercept,
          [ test_intercept/0
          ]).
:- use_module(library(plunit)).
:- use_module(library(intercept)).
:- use_module(library(debug)).

test_intercept :-
    run_tests([ intercept
              ]).

enum_b(F, T) :-
    forall(between(F, T, I),
           send_signal(emit(I))).

enum(I, Max) :-
    I =< Max,
    !,
    send_signal(emit(I)),
    I2 is I+1,
    enum(I2, Max).
enum(_, _).


:- begin_tests(intercept).

test(assign) :-
    intercept(send_signal(x), X, Y=X),
    assertion(var(Y)).
test(assign, Y == x) :-
    intercept(send_signal(x), X, =(X), Y).
test(no_univ, Xs == [x,y]) :-
    intercept((send_signal(x),send_signal(y)), X, assert(d(X))),
    findall(X, retract(d(X)), Xs).
test(all, all(List = [[1], [1,2]])) :-
    F = 1, T = 2,
    intercept_all(I, (between(F,T,Max),enum(F,Max)), emit(I), List).
test(all_empty, List == []) :-
    intercept_all(I, enum_b(1,3), emit(I), List).
test(all_nb, List == [1,2,3]) :-
    nb_intercept_all(I, enum_b(1,3), emit(I), List).

:- end_tests(intercept).
