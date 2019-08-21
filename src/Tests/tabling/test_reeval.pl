/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2019, VU University Amsterdam
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

:- module(test_reeval,
          [ test_reeval/0
          ]).
:- use_module(library(plunit)).

/** <module> Incremental tabling reevaluation tests
*/

test_reeval :-
    run_tests(tabling_reeval).

:- begin_tests(tabling_reeval, [ sto(rational_trees),
                                 cleanup(abolish_all_tables)
                               ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Two mutual dependent goals depending on   the same dynamic predicate. We
should _not_ create a sub-environment for solving the dependent goal.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- table (p/1, q/1) as incremental.
:- dynamic([max/1], [incremental(true)]).

max(2).

set_max(Max) :-
    retractall(max(_)),
    assert(max(Max)).

p(Y) :- q(X), max(Max), X < Max, Y is X+1.
p(0).
q(Y) :- p(X), max(Max), X < Max, Y is X+1.
q(0).

test(mutal_dependent, Ys = [0,1,2,3]) :-
    set_max(2),
    setof(X, p(X), Xs),
    assertion(Xs == [0,1,2]),
    set_max(3),
    setof(Y, p(Y), Ys).

:- end_tests(tabling_reeval).
