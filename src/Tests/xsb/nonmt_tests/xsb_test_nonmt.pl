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

:- module(xsb_test_nonmt,
          [ xsb_test_nonmt/0
          ]).
:- use_module('../xsb_test').
:- use_module(library(plunit)).

xsb_test_nonmt :-
    run_tests([ xsb_test_nonmt
              ]).

:- meta_predicate
    swi_test(:).

term_expansion(nonmt_test(Test),
               (   test(Test) :-
                       run_nonmt_test(Test)
               )).

run_nonmt_test(Test) :-
    xsb_test(nonmt_tests,Test,xsb_test_nonmt:swi_test(Test)).

swi_test(M:_P) :-
    abolish_all_tables,
    M:test.

:- begin_tests(xsb_test_nonmt, [sto(rational_trees)]).

% nonmt_test(test_tc).			% relies on timed_call/2
nonmt_test(test_iso_basic).
nonmt_test(test_iso_mult_visit).
nonmt_test(test_iso_hash).
nonmt_test(test_iso_undef).
% nonmt_test(test_iso_attr).		% relies on attributed variables
nonmt_test(test_lazy).
nonmt_test(test_introspection).
nonmt_test(test_invalidate).            % modified: force order, get_residual/2
% nonmt_test(test_goal_interrupt).	% tests interrupt_with_goal/1
% nonmt_test(test_trip).		% tests answer/variant limits
% nonmt_test(test_trip_mem).
% nonmt_test(test_trip_susp_reset).
% nonmt_test(test_maxans).
% nonmt_test(cmu_sei_083).              % probably ok, but order dependent
nonmt_test(cmu_sei_0050).
% nonmt_test(cmu_sei_a46d).		% contains highlog
nonmt_test(test_recomputable).
% nonmt_test(test_set_pp).		% too specific
% nonmt_test(test_dynopq).
% nonmt_test(test_memory_ovrflw_1).
% nonmt_test(test_memory_ovrflw_2).

:- end_tests(xsb_test_nonmt).

