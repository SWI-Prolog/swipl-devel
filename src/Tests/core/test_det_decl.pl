/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2021, SWI-Prolog Solutions b.v.
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

:- module(test_det_decl,
          [ test_det_decl/0
          ]).
:- use_module(library(plunit)).

/** <module> Test determinism declarations

*/

test_det_decl :-
    run_tests([ det_decl
              ]).

:- meta_predicate
    with_debug(0).

:- begin_tests(det_decl).

:- det((p1/1,
        p2/1,
        np/1,
        cdet/0,
        cdet2/0)).

p1(1).
p1(2).
p1(2).

p2(X) :- sp2(X).

sp2(1).

np(X) :- np2(X).

np2(1).
np2(2).

cdet :-
    np2(_), !.

cdet2 :-
    np2(X),
    !,
    np2(X).

test(det) :-
    p1(1).
test(cdet) :-
    cdet.
test(cdet2) :-
    cdet2.
test(nondet_1, error(determinism_error(_:p1/1,det,nondet))) :-
    p1(2).
test(nondet_2, error(determinism_error(_:np2/1,det,nondet))) :-
    np(_).
test(nondet_3, error(determinism_error(_:np/1,det,nondet))) :-
    with_debug(np(_)).
test(fail_1, error(determinism_error(_:p1/1,det,fail))) :-
    p1(3).
test(fail_2, error(determinism_error(_:np2/1,det,fail))) :-
    np(3).
test(fail_3, error(determinism_error(_:np/1,det,fail))) :-
    with_debug(np(3)).

:- end_tests(det_decl).

with_debug(Goal) :-
    current_prolog_flag(debug, Old),
    setup_call_cleanup(
        set_prolog_flag(debug, true),
        Goal,
        set_prolog_flag(debug, Old)).


