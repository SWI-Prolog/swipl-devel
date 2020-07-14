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

:- module(test_transaction_tabling,
          [ test_transaction_tabling/0
          ]).
:- use_module(library(plunit)).
:- use_module(library(debug)).

test_transaction_tabling :-
    run_tests([ tr_incremental_tabling
              ]).

:- begin_tests(tr_incremental_tabling).

:- table p/1 as incremental.
:- dynamic d/1 as incremental.

p(X) :- d(X).

cleanup :-
    retractall(d(_)),
    abolish_all_tables.

test(assert, cleanup(cleanup)) :-
    assertz(d(1)),
    assertion(setof(X, p(X), [1])),
    snapshot(add_to_p),
    assertion(setof(X, p(X), [1])).
test(tmp_assert, cleanup(cleanup)) :-
    snapshot(add_to_p2),
    assertion(\+ p(_)).
test(retract, cleanup(cleanup)) :-
    assertz(d(1)),
    assertz(d(2)),
    assertion(setof(X, p(X), [1,2])),
    snapshot(del_from_p),
    assertion(setof(X, p(X), [1,2])).

add_to_p :-
    assertz(d(2)),
    assertion(setof(X, p(X), [1,2])).

add_to_p2 :-
    assertz(d(1)),
    assertion(setof(X, p(X), [1])),
    retractall(d(_)).

del_from_p :-
    retract(d(2)),
    assertion(setof(X, p(X), [1])).

:- end_tests(tr_incremental_tabling).
