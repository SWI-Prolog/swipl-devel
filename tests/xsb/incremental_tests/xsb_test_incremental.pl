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

:- module(xsb_test_incremental,
          [ xsb_test_incremental/0
          ]).
:- use_module('../xsb_test').
:- use_module(library(plunit)).

xsb_test_incremental :-
    run_tests([ xsb_test_incremental
              ]).

:- meta_predicate
    swi_test(:).

term_expansion(incremental_test(Test),
               (   test(Test) :-
                       run_incremental_test(Test)
               )).

run_incremental_test(Test) :-
    xsb_test(incremental_tests,Test,xsb_test_incremental:swi_test(Test)).

swi_test(M:_P) :-
    abolish_all_tables,
    M:test.

:- begin_tests(xsb_test_incremental, [sto(rational_trees)]).

incremental_test(incremental).          % general incremental tests
incremental_test(incremental1).         % same as above, but with changed decls.
%incremental_test(incremental_trie).	% incremental tests with interned tries
incremental_test(inc_trie_dyn).         % incremental tests with trie asserts
%incremental_test(inc_trie_alt).        % inc with interned tries - for storage.P
incremental_test(incremental_rule).
incremental_test(incremental_rule_alt). % inc_rule with initial empty dyn pred
incremental_test(test_incr_depends).    % inc trans depends (cyclic)
incremental_test(test_incr_depends_2).  % inc trans depends (non cyclic)
incremental_test(test_inc_switch).      % test incremental <-> opaque
%incremental_test(test_visitors).       % testing executable incremental decls.
incremental_test(test_sound_updates).
incremental_test(test_wfs_update).
%incremental_test(incr_scc).
incremental_test(incr_test_romero).
incremental_test(test_abolish_nonincremental).
%incremental_test(test_abolish_incremental_call_single).
%incremental_test(inc_abol).            % inc tests with abolish_all_tables
incremental_test(inc_atc).              % inc tests with abolish_table_call
incremental_test(inc_atc_gc).           % inc tests with abolish_table_call + gc
incremental_test(inc_atc_gc_tricky).    % inc tests with abolish_table_call + gc
/*
incremental_test(test_directives).
incremental_test(test_declarations).
incremental_test(test_table_errors).    % testing executable incremental dirs.
% Below are disabled in XSB suite
incremental_test(test_set_pp).
incremental_test(test_tc).
incremental_test(test_iso_basic).
incremental_test(test_iso_mult_visit).
incremental_test(test_iso_hash).
incremental_test(test_iso_undef).
incremental_test(test_iso_attr).
incremental_test(test_lazy).            % testing executable incremental decls.
incremental_test(inc_atc_single_gc_deps). % yet more inc tests with abolish_table_call_single + gc
incremental_test(test_introspection).
incremental_test(test_invalidate).      % yet more tests of invalidation
incremental_test(test_goal_interrupt).
incremental_test(test_trip).
incremental_test(test_trip_mem).
incremental_test(test_trip_susp_reset).
incremental_test(test_maxans).
*/
:- end_tests(xsb_test_incremental).
