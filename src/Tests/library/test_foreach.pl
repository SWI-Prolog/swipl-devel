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

:- module(test_foreach,
          [ test_foreach/0
          ]).
:- use_module(library(plunit)).
:- use_module(library(aggregate)).
:- use_module(library(dif)).
:- use_module(library(hashtable)).
:- use_module(library(lists)).

test_foreach :-
    run_tests([ foreach
              ]).

:- begin_tests(foreach).

test(dif1, Y == 5) :-
    foreach(between(1,4,X), dif(X,Y)), Y = 5.
test(dif1, fail) :-
    foreach(between(1,4,X), dif(X,Y)), Y = 3.
test(member, L =@= [1,2,3,4|_]) :-
    foreach(between(1,4,X), member(X,L)),
    !.
test(ht, Pairs == [1-1,2-2,3-3,4-4,5-5]) :-
    ht_new(HT),
    foreach(between(1,5,X),
            ht_put(HT,X,X)),
    ht_pairs(HT, Pairs).

:- end_tests(foreach).
