/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2017-2020, University of Amsterdam
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

:- module(test_answer_subsumption,
	  [ test_answer_subsumption/0
	  ]).
:- use_module(library(plunit)).
:- use_module(library(wfs)).

test_answer_subsumption :-
    run_tests([ answer_subsumption
	      ]).

:- begin_tests(answer_subsumption).

:- table p/2, pas(_,lattice(join(_X,_Y,_Z))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Mixed answer subsumption and normal tabling

p(X, Y) :-
    pas(X, Y).
p(_, 1).

pas(X, Z) :-
    p(X, Y),
    Y < 5,
    Z is Y+1.

join(A,B,C):-
    (   A > B
    ->  C = A
    ;   C = B
    ).

test(as_with_no_as, set(X == [1,2,3,4,5])) :-
    p(1,X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Test max aggregation

:- table test1(+,+,max).
test1(L,L2, Val) :-
        member(Val1,L),
        member(Val2,L2),
        Val is Val1+Val2.

test(max, Max == 206) :-
    L1 = [1,2,3,5,98,3,103,4,4,21],
    test1(L1, L1, Max).

:- end_tests(answer_subsumption).
