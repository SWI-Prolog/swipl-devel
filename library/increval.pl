/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2019-2021, VU University Amsterdam
                              SWI-Prolog Solutions b.v.
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

:- module(increval,
          [ incr_assert/1,                      % :Clause
            incr_asserta/1,                     % :Clause
            incr_assertz/1,                     % :Clause
            incr_retractall/1,                  % :Head
            incr_retract/1,                     % :Clause

            is_incremental_subgoal/1,           % :Goal
            incr_directly_depends/2,            % :Goal1, :Goal2
            incr_trans_depends/2,		% :Goal1, :Goal2
            incr_invalid_subgoals/1,            % -List
            incr_is_invalid/1,                  % :Goal

            incr_invalidate_call/1,		% :Goal
            incr_invalidate_calls/1,		% :Goal
            incr_table_update/0,

            incr_propagate_calls/1              % :Answer
          ]).
:- use_module(library(tables)).

/** <module> Incremental dynamic predicate modification

This module emulates the XSB module   `increval`. This module serves two
goals: (1) provide alternatives  for   the  dynamic  clause manipulation
predicates that propagate into the incremental  tables and (2) query the
dynamically maintained _Incremental Depency Graph_ (IDG).

The change propagation for incremental   dynamic  predicates. SWI-Prolog
relies in prolog_listen/2 to forward any change to dynamic predicates to
the table IDG  and  incr_assert/1  and   friends  thus  simply  call the
corresponding database update.

@compat XSB
*/

:- meta_predicate
    incr_assert(:),
    incr_asserta(:),
    incr_assertz(:),
    incr_retractall(:),
    incr_retract(:),
    is_incremental_subgoal(:),
    incr_directly_depends(:,:),
    incr_trans_depends(:, :),
    incr_is_invalid(:),
    incr_invalidate_call(:),
    incr_invalidate_calls(:),
    incr_propagate_calls(:).

incr_assert(T)     :- assertz(T).
incr_asserta(T)    :- asserta(T).
incr_assertz(T)    :- assertz(T).
incr_retractall(T) :- retractall(T).
incr_retract(T)    :- retract(T).

%!  is_incremental_subgoal(?SubGoal) is nondet.
%
%   This   predicate   non-deterministically   unifies    Subgoal   with
%   incrementally tabled subgoals that are currently table entries.

is_incremental_subgoal(SubGoal) :-
    '$tbl_variant_table'(VTrie),
    trie_gen(VTrie, SubGoal, ATrie),
    (   '$idg_edge'(ATrie, _Dir, _Value)
    ->  true
    ).

%!  incr_directly_depends(:Goal1, :Goal2) is nondet.
%
%   True if Goal1 depends on Goal2 in the IDG.
%
%   @compat: In XSB, at least one of Goal 1 or Goal 2 must be bound.
%   This implementation may be used with both arguments unbound.

incr_directly_depends(Goal1, Goal2) :-
    '$tbl_variant_table'(VTrie),
    (   nonvar(Goal2)
    ->  trie_gen(VTrie, Goal2, ATrie2),
        '$idg_edge'(ATrie2, affected, ATrie1),
        '$tbl_table_status'(ATrie1, _Status, Goal1, _Return)
    ;   trie_gen(VTrie, Goal1, ATrie1),
        '$idg_edge'(ATrie1, dependent, ATrie2),
        '$tbl_table_status'(ATrie2, _Status, Goal2, _Return)
    ).

%!  incr_trans_depends(:Goal1, Goal2) is nondet.
%
%   True   for   each   pair    in     the    transitive    closure   of
%   incr_directly_depends(G1, G2).

incr_trans_depends(Goal1, Goal2) :-
    '$tbl_variant_table'(VTrie),
    (   nonvar(Goal1)
    ->  trie_gen(VTrie, Goal1, ATrie1),
        incr_trans_depends(ATrie1, dependent, ATrie2, []),
        '$tbl_table_status'(ATrie2, _Status, Goal2, _Return)
    ;   trie_gen(VTrie, Goal2, ATrie2),
        incr_trans_depends(ATrie2, affected, ATrie1, []),
        '$tbl_table_status'(ATrie1, _Status, Goal1, _Return)
    ).

incr_trans_depends(ATrie1, Dir, ATrie, Done) :-
    '$idg_edge'(ATrie1, Dir, ATrie2),
    \+ memberchk(ATrie2, Done),
    (   ATrie = ATrie2
    ;   incr_trans_depends(ATrie2, Dir, ATrie, [ATrie2|Done])
    ).

%!  incr_invalid_subgoals(-List) is det.
%
%   List is a sorted list (set)  of   the  incremental subgoals that are
%   currently invalid.

incr_invalid_subgoals(List) :-
    findall(Invalid, invalid_subgoal(Invalid, _), List0),
    sort(List0, List).

invalid_subgoal(Goal, ATrie) :-
    '$tbl_variant_table'(VTrie),
    trie_gen(VTrie, Goal, ATrie),
    (   '$idg_edge'(ATrie, _Dir, _Value)
    ->  true
    ),
    '$idg_falsecount'(ATrie, Count),
    Count > 0,
    \+ '$tbl_table_status'(ATrie, dynamic, _Goal, _Return).

%!  incr_is_invalid(:Subgoal) is semidet.
%
%   True when Subgoal's table is marked as invalid.

incr_is_invalid(Subgoal) :-
    get_calls(Subgoal, Table, _Return),
    '$idg_falsecount'(Table, Count),
    Count > 0.

%!  incr_invalidate_calls(:Goal) is det.
%
%   Invalidate all tables for subgoals of Goal   as  well as tables that
%   are affected by these.

incr_invalidate_calls(Goal) :-
    '$tbl_variant_table'(VTable),
    !,
    forall(trie_gen(VTable, Goal, ATrie),
           '$idg_changed'(ATrie)).
incr_invalidate_calls(_).

%!  incr_invalidate_call(:Goal) is det.
%
%   This is the XSB name, but   the  manual says incr_invalidate_calls/1
%   and the comment with the code suggests this is misnamed.
%
%   @deprecated Use incr_invalidate_calls/1.

incr_invalidate_call(Goal) :-
    incr_invalidate_calls(Goal).

%!  incr_table_update
%
%   Updated all invalid tables

incr_table_update :-
    repeat,
    (   invalid_subgoal(Goal, ATrie)
    ->  '$tabling':reeval(ATrie, Goal, _Return),
        fail
    ;   !
    ).

%!  incr_propagate_calls(:Answer) is det.
%
%   Activate the monotonic answer propagation similarly   to  when a new
%   fact is asserted for a monotonic  dynamic predicate. The Answer term
%   must match a monotonic dynamic predicate.

incr_propagate_calls(Answer) :-
    setup_call_cleanup(
        '$tbl_propagate_start'(Old),
        '$tabling':incr_propagate_assert(Answer),
        '$tbl_propagate_end'(Old)).
