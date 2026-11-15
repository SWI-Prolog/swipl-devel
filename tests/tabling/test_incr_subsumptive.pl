/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2026, SWI-Prolog Solutions b.v.
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


:- module(test_incr_subsumptive,
          [ test_incr_subsumptive/0
          ]).
:- use_module(library(plunit)).

/** <module> Incremental subsumptive tabling

Test `:- table p/2 as (subsumptive,incremental)`.  Re-evaluating the
invalidated general table used to raise a tabling dependency error and
calls  that  reused  an  existing  table  did  not  record  an  IDG
dependency, so incremental tables depending on them were not updated.
*/

test_incr_subsumptive :-
    run_tests([ incr_subsumptive
              ]).

:- begin_tests(incr_subsumptive).

:- dynamic d/2 as incremental.
:- table p/2 as (subsumptive,incremental).
:- table q/1 as incremental.
:- table r/1 as incremental.

p(X,Y) :- d(X,Y).
q(Y) :- p(1,Y).                         % subsumed by p(_,_)
r(Y) :- p(_,Y).                         % variant of p(_,_)

init :-
    abolish_all_tables,
    retractall(d(_,_)),
    assertz(d(1,a)),
    assertz(d(2,b)),
    forall(p(_,_), true).               % create the general table

test(general, L == [1-a,2-b,3-c]) :-
    init,
    assertz(d(3,c)),
    findall(X-Y, p(X,Y), L0),
    msort(L0, L).
test(subsumed, L == [a,z]) :-
    init,
    findall(Y, p(1,Y), _),
    assertz(d(1,z)),
    findall(Y, p(1,Y), L0),
    msort(L0, L).
test(dependent_subsumed, [L1-L2 == [a,z]-[z]]) :-
    init,
    findall(Y, q(Y), _),
    assertz(d(1,z)),
    findall(Y, q(Y), L10),
    msort(L10, L1),
    retract(d(1,a)),
    findall(Y, q(Y), L2).
test(dependent_variant, [L1-L2 == [a,b,z]-[b,z]]) :-
    init,
    findall(Y, r(Y), _),
    assertz(d(1,z)),
    findall(Y, r(Y), L10),
    msort(L10, L1),
    retract(d(1,a)),
    findall(Y, r(Y), L20),
    msort(L20, L2).

:- end_tests(incr_subsumptive).
