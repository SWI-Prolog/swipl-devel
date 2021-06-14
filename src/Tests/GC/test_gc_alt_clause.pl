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

:- module(test_gc_alt_clause,
          [ test_gc_alt_clause/0
          ]).
:- use_module(library(plunit)).

/** <module> Test mark_alt_clauses() in pl-gc.c
*/

test_gc_alt_clause :-
    run_tests([ gc_alt_clause
              ]).

:- begin_tests(gc_alt_clause,
               [ condition(current_prolog_flag(bounded, false))
               ]).

alt(_X) :- garbage_collect, fail.
alt(X)  :- rat(Rat), use(Rat), X = 1.
alt(X)  :- use("hello"),       X = 2.
alt(X)  :- use(42.0),          X = 3.

:- if(current_prolog_flag(bounded, false)).
rat(R) :-
    R is rdiv(1, 3).
:- else.
rat(0.33).
:- endif.


use(_).

test(alt_rat, set(X == [1,2,3])) :-
    alt(X).

:- end_tests(gc_alt_clause).
