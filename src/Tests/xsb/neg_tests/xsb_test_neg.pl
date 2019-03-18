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

:- module(xsb_test_neg,
          [ xsb_test_neg/0
          ]).
:- use_module('../xsb_test').
:- use_module(library(plunit)).

xsb_test_neg :-
    run_tests([ tabled_basic_negation,
                tabled_lr_stratified_negation,
                tabled_dynakicallly_stratified_negation,
                tabled_stratified_negation,
                tabled_misc_negation
              ]).

term_expansion(xsb_test(Test),
               (   test(Test) :-
                       xsb_test(neg_tests, Test, test)
               )).

:- begin_tests(tabled_basic_negation, [sto(rational_trees)]).

xsb_test(neg1).
xsb_test(neg2).
xsb_test(neg3).
xsb_test(ullman2).

:- end_tests(tabled_basic_negation).

:- begin_tests(tabled_lr_stratified_negation, [sto(rational_trees)]).

xsb_test(lmod1).
xsb_test(lmod2).
xsb_test(lmod3).
xsb_test(lmod4).
xsb_test(lmod5).
xsb_test(lmod6).
xsb_test(lmod7).
xsb_test(lmod8).
xsb_test(lmod9).
xsb_test(lmod10).
xsb_test(lmod11).
xsb_test(ullman1).

:- end_tests(tabled_lr_stratified_negation).

:- begin_tests(tabled_dynakicallly_stratified_negation, [sto(rational_trees)]).

xsb_test(przy2).
xsb_test(ldynstrat0).
xsb_test(ldynstrat1).
xsb_test(ldynstrat2).
xsb_test(ldynstrat3).
xsb_test(ldynstrat4).

:- end_tests(tabled_dynakicallly_stratified_negation).

:- begin_tests(tabled_stratified_negation, [sto(rational_trees)]).

xsb_test(mod1).

:- end_tests(tabled_stratified_negation).

:- begin_tests(tabled_misc_negation, [sto(rational_trees)]).

xsb_test(q7).

:- end_tests(tabled_misc_negation).

