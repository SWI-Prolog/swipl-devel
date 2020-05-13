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

:- module(xsb_test_basics,
          [ xsb_test_basics/0
          ]).
:- use_module('../xsb_test').
:- use_module(library(plunit)).

xsb_test_basics :-
    run_tests(xsb_basic_tests).

:- begin_tests(xsb_basic_tests, [sto(rational_trees)]).

goal_expansion(xsb_test(Test, Goal),
               xsb_test(basic_tests, Test, Goal)).

test(tsstr13) :- xsb_test(tsstr13, testcombo).
test(tsstr23) :- xsb_test(tsstr23, testcombo).
test(tsstr33) :- xsb_test(tsstr33, testcombo).
test(tstr11)  :- xsb_test(tstr11, (tw,fail)).
test(tstr21)  :- xsb_test(tstr21, (tw,fail)).
test(tstr31)  :- xsb_test(tstr31, (tw,fail)).
test(tstr51)  :- xsb_test(tstr51, (tw,fail)).
test(tstr61)  :- xsb_test(tstr61, (tw,fail)).
test(tstr12)  :- xsb_test(tstr12, testcombo).
test(tstr22)  :- xsb_test(tstr22, testcombo).
test(tstr32)  :- xsb_test(tstr32, testcombo).
test(tstr52)  :- xsb_test(tstr52, testcombo).
test(tstr62)  :- xsb_test(tstr62, testcombo).
test(tstr13)  :- xsb_test(tstr13, tw).
test(tstr23)  :- xsb_test(tstr23, tw).
test(tstr33)  :- xsb_test(tstr33, tw).
test(tstr53)  :- xsb_test(tstr53, tw).
test(tstr63)  :- xsb_test(tstr63, tw).
test(thstr13) :- xsb_test(thstr13, tw).
test(thstr43) :- xsb_test(thstr43, tw).
test(thstr23) :- xsb_test(thstr23, tw1).
test(tcyl11)  :- xsb_test(tcyl11, tw(1)).
test(tcyl12)  :- xsb_test(tcyl12, tw(1)).
test(testsg)  :- xsb_test(testsg, tw).
test(interp)  :- xsb_test(interp, test).
test(tsing1)  :- xsb_test(tsing1, (a(X,Y),write(X),write(' '),write(Y),nl,fail)).

:- end_tests(xsb_basic_tests).
