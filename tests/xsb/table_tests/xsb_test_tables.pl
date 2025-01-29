/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2019-2020, University of Amsterdam
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
    reset_table_defaults,
    xsb_test(table_tests,Test,xsb_test_tables:swi_test(Test)).

swi_test(M:_P) :-
    abolish_all_tables,
    M:test,
    reset_table_defaults.

reset_table_defaults :-
    set_prolog_flag(max_table_answer_size, infinite),
    set_prolog_flag(max_table_answer_size_action, error),
    set_prolog_flag(max_table_subgoal_size, infinite),
    set_prolog_flag(max_table_subgoal_size_action, error),
    set_prolog_flag(max_answers_for_subgoal, infinite),
    set_prolog_flag(max_answers_for_subgoal_action, error).


:- begin_tests(xsb_test_tables, [sto(rational_trees)]).

table_test(abol_test).          % atp/atc basic
table_test(abol_test2).         % a module t basic
table_test(abol_test2a).        % a module t gc diff preds
table_test(abol_test3).         % atp gc same preds
table_test(abol_test3a).        % atp gc diff preds
table_test(abol_test3b).        % atp gc diff preds + valid
table_test(abol_test3c).        % atp gc diff preds + valid + multiple gcs
table_test(abolish_cascade).	% cascading abolish for subgoals with gc etc.
table_test(abolish_cascade_pred).
table_test(abolish_cycle).
table_test(abolish_dag).
table_test(abolish_neg_cycle).
table_test(abolish_neg_dag).
table_test(atc_test).
table_test(concomp).
table_test(expand).
table_test(ins).
table_test(large_arity_tables).
table_test(lrbug).
table_test(pps).
table_test(pred_abolish_cycle).
table_test(pred_abolish_dag).
table_test(recursive_aboltest).
table_test(tabbug1).
table_test(test_3vwfs_1).       % WFS with answer subsumption
table_test(test_calldepth).
table_test(test_cyclic_tabling).% Cyclic term handling
table_test(test_large_tabled_terms).
table_test(test_maxans_decl).   % max_answers(Count) restraint
table_test(test_negcycle).
table_test(test_tda).		% subgoal_abstract restraint (abstract)
table_test(test_tda_i).		% subgoal_abstract restraint (abstract)

:- end_tests(xsb_test_tables).
