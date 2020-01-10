/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2019, VU University Amsterdam
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

:- module(tables,
          [ abolish_all_tables/0,
            abolish_module_tables/1,            % +Module
            abolish_table_pred/1,               % :CallableOrPI
            abolish_table_call/1,               % :Callable
            abolish_table_call/2,               % :Callable, +Options
            abolish_table_subgoals/2,           % :Callable, +Options

            tfindall/3,                         % +Template, :Goal, -Answers
            't not'/1,                          % :Goal

            get_call/3,				% :CallTerm, -AnswerTrie, -Templ
            get_calls/3,			% :CallTerm, -AnswerTrie, -Templ
            get_returns/2,			% +AnswerTrie, -Return
            get_returns/3,			% +AnswerTrie, -Return, -NodeID
            get_returns_and_dls/3,		% +AnswerTrie, -Return, -DL
            get_returns_and_tvs/3,		% +AnswerTrie, -Return, -TVs
            get_returns_for_call/2,             % :CallTerm, ?AnswerTerm
            get_residual/2,			% :CallTerm, -DelayList

            set_pil_on/0,
            set_pil_off/0
          ]).
:- use_module(library(debug)).
:- use_module(library(error)).
:- use_module(library(dialect/xsb)).

:- meta_predicate
    abolish_table_pred(:),
    abolish_table_call(:),
    abolish_table_call(:, +),
    abolish_table_subgoals(:, +),
    tfindall(+, 0, -),
    't not'(0),
    get_call(:, -, -),
    get_calls(:, -, -),
    get_returns_for_call(:, :),
    get_returns_and_dls(+, -, :),
    get_residual(:, -).

%!  't not'(:Goal)
%
%   Tabled negation.

't not'(Goal) :-
    tnot(Goal).

%!  tfindall(+Template, :Goal, -Answers)
%
%   More safe findall wrt. tabling.  For now just findall/3.

tfindall(Template, Goal, Answers) :-
    findall(Template, Goal, Answers).

%!  set_pil_on.
%!  set_pil_off.
%
%   Dummy predicates

set_pil_on.
set_pil_off.

%!  get_call(:CallTerm, -Trie, -Skeleton) is semidet.
%
%   True when Trie is an answer trie for a variant of CallTerm. See also
%   get_calls/3.

get_call(Goal0, Trie, Skeleton) :-
    '$tbl_implementation'(Goal0, M:Goal),
    M:'$table_mode'(Goal, Table, _),
    current_table(M:Table, Trie),
    '$tbl_table_status'(Trie, _Status, M:Table, Skeleton).


%!  get_calls(:CallTerm, -Trie, -Skeleton) is nondet.
%
%   True when Trie is an answer  trie   for  a variant that unifies with
%   CallTerm and Skeleton is the answer skeleton.
%
%   @arg Skeleton is a term ret(...) with as many arguments as there are
%   variables in the answer template. The   `ret`  functor is compatible
%   with XSB.

get_calls(Goal0, Trie, Skeleton) :-
    '$tbl_variant_table'(VariantTrie),
    '$tbl_implementation'(Goal0, M:Goal),
    M:'$table_mode'(Goal, Table, _),
    trie_gen(VariantTrie, M:Table, Trie),
    '$tbl_table_status'(Trie, _Status, M:Table, Skeleton).

%!  get_returns(+AnswerTrie, -Return) is nondet.
%
%   True when Return is an answer template for the AnswerTrie.
%
%   @arg Return is a term ret(...).  See get_calls/3.

get_returns(AnswerTrie, Return) :-
    trie_gen(AnswerTrie, Return).

%!  get_returns(+AnswerTrie, -Return, -NodeID) is nondet.
%
%   True when Return is an answer template for the AnswerTrie and the
%   answer is represented by the trie node NodeID.
%
%   @arg Return is a term ret(...).  See get_calls/3.

get_returns(AnswerTrie, Return, NodeID) :-
    '$trie_gen_node'(AnswerTrie, Return, NodeID).

%!  get_returns_and_tvs(+AnswerTrie, -Return, -TruthValue) is nondet.
%
%   Identical to get_returns/2, but also obtains   the  truth value of a
%   given  answer,  setting  TruthValue  to  `t`    if   the  answer  is
%   unconditional and to `u` if  it   is  conditional.  If a conditional
%   answer has multiple delay lists, this   predicate  will succeed only
%   once, so that using  this  predicate   may  be  more  efficient than
%   get_residual/2 (although less informative)

get_returns_and_tvs(AnswerTrie, Return, TruthValue) :-
    '$tbl_answer_dl'(AnswerTrie, Return, AN),
    (   AN == true
    ->  TruthValue = t
    ;   TruthValue = u
    ).

%!  get_returns_and_dls(+AnswerTrie, -Return, :DelayLists) is nondet.
%
%   True when Return appears in AnswerTrie   with  the given DelayLists.
%   DelayLists is a list of lists,  where   the  inner lists expresses a
%   conjunctive condition and and outer list a disjunction.

get_returns_and_dls(AnswerTrie, Return, M:DelayLists) :-
    '$tbl_answer'(AnswerTrie, Return, Condition),
    condition_delay_lists(Condition, M, DelayLists).

condition_delay_lists(true, _, []) :-
    !.
condition_delay_lists((A;B), M, List) :-
    !,
    phrase(semicolon_list((A;B)), L0),
    maplist(conj_list(M), L0, List).
condition_delay_lists(One, M, [List]) :-
    conj_list(M, One, List).

semicolon_list((A;B)) -->
    !,
    semicolon_list(A),
    semicolon_list(B).
semicolon_list(G) -->
    [G].


%!  get_residual(:CallTerm, -DelayList) is nondet.
%
%   True if CallTerm appears in a  table and has DelayList. SWI-Prolog's
%   representation for a delay  is  a   body  term,  more specifically a
%   disjunction   of   conjunctions.   The     XSB   representation   is
%   non-deterministic and uses a list to represent the conjunction.

get_residual(Goal0, DelayList) :-
    '$tbl_implementation'(Goal0, Goal),
    Goal = M:Head,
    '$tbl_trienode'(Reserved),
    M:'$table_mode'(Head, Variant, Moded),
    '$tbl_variant_table'(VariantTrie),
    trie_gen(VariantTrie, M:Variant, Trie),
    '$tbl_table_status'(Trie, _Status, M:Variant, Skeleton),
    (   Reserved == Moded
    ->  '$tbl_answer'(Trie, Skeleton, Condition)
    ;   '$tbl_answer'(Trie, Skeleton, Moded, Condition)
    ),
    condition_delay_list(Condition, M, DelayList).

condition_delay_list(true, _, List) :-
    !,
    List = [].
condition_delay_list((A;B), M, List) :-
    !,
    (   condition_delay_list(A, M, List)
    ;   condition_delay_list(B, M, List)
    ).
condition_delay_list(Conj, M, List) :-
    !,
    conj_list(M, Conj, List).

conj_list(M, Conj, List) :-
    phrase(comma_list(Conj, M), List).

comma_list((A,B), M) -->
    !,
    comma_list(A, M),
    comma_list(B, M).
comma_list(M:G, M) -->
    !,
    [G].
comma_list(tnot(M:G), M) -->
    !,
    [tnot(G)].
comma_list(G, _) -->
    [G].


%!  get_returns_for_call(:CallTerm, -AnswerTerm) is nondet.
%
%   True if AnswerTerm appears in the tables for the _variant_ CallTerm.

get_returns_for_call(M:CallTerm, AnswerTerm) :-
    current_table(M:CallTerm, Trie),
    '$tbl_table_status'(Trie, _Status, AnswerTerm, Skeleton),
    trie_gen(Trie, Skeleton, _).


		 /*******************************
		 *             TABLES		*
		 *******************************/

%!  abolish_table_pred(:CallTermOrPI)
%
%   Invalidates all tabled subgoals for  the   predicate  denoted by the
%   predicate or term indicator Pred.
%
%   @tbd If Pred has a subgoal that   contains a conditional answer, the
%   default  behavior  will  be  to   transitively  abolish  any  tabled
%   predicates  with  subgoals  having  answers    that  depend  on  any
%   conditional answers of S.

abolish_table_pred(M:Name/Arity) :-
    !,
    functor(Head, Name, Arity),
    abolish_table_subgoals(M:Head).
abolish_table_pred(M:Head) :-
    callable(Head),
    !,
    functor(Head, Name, Arity),
    functor(Generic, Name, Arity),
    abolish_table_subgoals(M:Generic).
abolish_table_pred(PI) :-
    type_error(callable_or_predicate_indicator, PI).

%!  abolish_table_call(+Head) is det.
%!  abolish_table_call(+Head, +Options) is det.
%
%   Same as abolish_table_subgoals/1.  See also abolish_table_pred/1.
%
%   @deprecated Use abolish_table_subgoals/[1,2].

abolish_table_call(Head) :-
    abolish_table_subgoals(Head).

abolish_table_call(Head, Options) :-
    abolish_table_subgoals(Head, Options).

%!  abolish_table_subgoals(:Head, +Options)
%
%   Behaves  as  abolish_table_subgoals/1,  but    allows   the  default
%   `table_gc_action` to be over-ridden with a flag, which can be either
%   `abolish_tables_transitively` or `abolish_tables_singly`.
%
%   @compat Options is compatible with XSB, but does not follow the ISO
%   option handling conventions.

abolish_table_subgoals(Head, Options) :-
    must_be(list, Options),
    (   Options == []
    ->  abolish_table_subgoals(Head)
    ;   memberchk(abolish_tables_transitively, Options)
    ->  abolish_table_subgoals(Head)
    ;   memberchk(abolish_tables_singly, Options)
    ->  abolish_table_subgoals(Head)
    ;   domain_error([abolish_tables_transitively,abolish_tables_singly], Options)
    ).
