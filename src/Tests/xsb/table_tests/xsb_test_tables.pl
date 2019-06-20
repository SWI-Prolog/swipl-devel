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

:- module(xsb_test_tables,
          [ xsb_test_tables/0
          ]).
:- use_module('../xsb_test').
:- use_module(library(plunit)).

xsb_test_tables :-
    run_tests([ xsb_test_tables
              ]).

:- meta_predicate
    swi_test(:).

term_expansion(table_test(Test),
               (   test(Test) :-
                       run_table_test(Test)
               )).

run_table_test(Test) :-
    xsb_test(table_tests,Test,xsb_test_tables:swi_test(Test)).

swi_test(M:_P) :-
    abolish_all_tables,
    M:test.

:- begin_tests(xsb_test_tables, [sto(rational_trees)]).

table_test(abol_test).          % atp/atc basic
table_test(abol_test2).         % a module t basic
table_test(abol_test2a).        % a module t gc diff preds
table_test(abol_test3).         % atp gc same preds
table_test(abol_test3a).        % atp gc diff preds
table_test(abol_test3b).        % atp gc diff preds + valid
table_test(abol_test3c).        % atp gc diff preds + valid + multiple gcs
table_test(atc_test).
table_test(abolish_cascade).	% cascading abolish for subgoals with gc etc.
/*
*/

:- end_tests(xsb_test_tables).
