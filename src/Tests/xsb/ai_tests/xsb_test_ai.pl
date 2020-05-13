/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2019, University of Amsterdam
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

:- module(xsb_test_ai,
          [ xsb_test_ai/0
          ]).
:- use_module('../xsb_test').
:- use_module(library(plunit)).

xsb_test_ai :-
    run_tests(xsb_ai_tests).

:- begin_tests(xsb_ai_tests, [sto(rational_trees)]).

goal_expansion(xsb_test(Test, Goal),
               xsb_test(ai_tests, Test, Goal)).

test(cs_o)    :- xsb_test(cs_o, test).
test(cs_r)    :- xsb_test(cs_r, test).
test(disj)    :- xsb_test(disj, test).
test(gabriel) :- xsb_test(gabriel, test).
test(kalah)   :- xsb_test(kalah, test).
test(peep)    :- xsb_test(peep, test).
test(pg)      :- xsb_test(pg, test).
test(plan)    :- xsb_test(plan, test).
test(qsort)   :- xsb_test(qsort, test).
test(queens)  :- xsb_test(queens, test).
test(read)    :- xsb_test(read, test).

:- end_tests(xsb_ai_tests).
