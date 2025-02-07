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
    run_tests([ det_decl,
                det_guard,
                det_goal
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

np(X) :- np1(X).
np1(X) :- np2(X).

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
test(nondet_1, error(determinism_error(_:p1/1,det,nondet,property))) :-
    p1(2).
test(nondet_2, error(determinism_error(_:np2/1,det,nondet,property))) :-
    np(_).
test(nondet_3, error(determinism_error(_:np/1,det,nondet,property))) :-
    with_debug(np(_)).
test(fail_1, error(determinism_error(_:p1/1,det,fail,property))) :-
    p1(3).
test(fail_2, error(determinism_error(_:np2/1,det,fail,property))) :-
    np(3).
test(fail_3, error(determinism_error(_:np/1,det,fail,property))) :-
    with_debug(np(3)).

:- end_tests(det_decl).

:- begin_tests(det_guard).

nd(X) :- nd2(X).
nd2(X) :- nd3(X).

nd3(1).
nd3(2).
nd3(2).

d(X) :- $, nd(X).

test(det) :-
    d(1).
test(ndet, error(determinism_error(_:nd3/1,det,nondet,guard_in_caller))) :-
    d(2).
test(ndet, error(determinism_error(_:d/1,det,nondet,guard_in_caller))) :-
    with_debug(d(2)).
test(fail, error(determinism_error(_:nd3/1,det,fail,guard_in_caller))) :-
    d(3).
test(fail, error(determinism_error(_:d/1,det,fail,guard_in_caller))) :-
    with_debug(d(3)).

:- end_tests(det_guard).

:- begin_tests(det_goal).

nd(X) :- nd2(X), garbage_collect.
nd2(X) :- nd3(X).

nd3(1).
nd3(2).
nd3(2).

end.

d(X) :- $nd(X), end.

test(det) :-
    d(1).
test(ndet, error(determinism_error(nd(2),det,nondet,goal))) :-
    d(2).
test(ndet, error(determinism_error(nd(2),det,nondet,goal))) :-
    with_debug(d(2)).
test(fail, error(determinism_error(nd(3),det,fail,goal))) :-
    d(3).
test(fail, error(determinism_error(nd(3),det,fail,goal))) :-
    with_debug(d(3)).

d1 :- $ndd(X), u(X).
d2 :- $nd3(X), u(X).
d3 :- $ndf(X), u(X).
ndd(1) :- garbage_collect.
ndf(1) :- fail.
u(_).

test(ndetv, true) :-
    d1.
test(ndetv, error(determinism_error(nd3(1),det,nondet,goal))) :-
    d2.
test(ndetv, error(determinism_error(ndf(_),det,fail,goal))) :-
    d3.
test(ndetvc, Body =@= $ndd(X), u(X)) :-
    clause(d1, Body).

:- end_tests(det_goal).


with_debug(Goal) :-
    current_prolog_flag(debug, Old),
    setup_call_cleanup(
        set_prolog_flag(debug, true),
        Goal,
        set_prolog_flag(debug, Old)).


