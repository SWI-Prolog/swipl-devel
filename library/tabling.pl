/*  Part of SWI-Prolog

    Author:        Benoit Desouter <Benoit.Desouter@UGent.be>
                   Jan Wielemaker (SWI-Prolog port)
                   Fabrizio Riguzzi (mode directed tabling)
    Copyright (c)  2016, Benoit Desouter, Jan Wielemaker, Fabrizio Riguzzi
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

:- module(tabling,
          [ (table)/1,                  % +PI ...

            current_table/2,            % :Variant, ?Table
            abolish_all_tables/0,
            abolish_table_subgoals/1,   % :Subgoal

            start_tabling/2,            % +Wrapper, :Worker.

            op(1150, fx, table)
          ]).
:- use_module(library(error)).
:- set_prolog_flag(generate_debug_info, false).

:- meta_predicate
    start_tabling(+, 0),
    current_table(:, -),
    abolish_table_subgoals(:).

/** <module> Tabled execution (SLG WAM)

This  library  handled  _tabled_  execution   of  predicates  using  the
characteristics if the _SLG WAM_. The   required  suspension is realised
using _delimited continuations_ implemented by  reset/3 and shift/1. The
table space and work lists are part of the SWI-Prolog core.

@author Benoit Desouter
*/

%!  table(+PredicateIndicators)
%
%   Prepare the given PredicateIndicators for   tabling. Can only be
%   used as a directive. The example   below  prepares the predicate
%   edge/2 and the non-terminal statement//1 for tabled execution.
%
%     ==
%     :- table edge/2, statement//1.
%     ==
%
%   In addition to using _predicate  indicators_,   a  predicate  can be
%   declared for _mode  directed  tabling_  using   a  term  where  each
%   argument declares the intended mode.  For example:
%
%     ==
%     :- table connection(_,_,min).
%     ==
%
%   _Mode directed tabling_ is  discussed   in  the general introduction
%   section about tabling.

table(PIList) :-
    throw(error(context_error(nodirective, table(PIList)), _)).

%!  start_tabling(+Variant, +Implementation)
%
%   Execute Implementation using tabling. This  predicate should not
%   be called directly. The table/1 directive  causes a predicate to
%   be translated into a renamed implementation   and a wrapper that
%   involves this predicate.
%
%   @compat This interface may change or disappear without notice
%           from future versions.

start_tabling(Wrapper, Worker) :-
    get_wrapper_no_mode_args(Wrapper, WrapperNoModes, ModeArgs),
    '$tbl_variant_table'(WrapperNoModes, Trie, Status),
    (   Status == complete
    ->  trie_gen(Trie, WrapperNoModes, ModeArgs)
    ;   (   '$tbl_scheduling_component'(false, true)
        ->  catch(run_leader(Wrapper, WrapperNoModes, Worker, Trie),
                  E, true),
            (   var(E)
            ->  trie_gen(Trie, WrapperNoModes, ModeArgs)
            ;   '$tbl_table_discard_all',
                throw(E)
            )
        ;   run_follower(Status, Wrapper, WrapperNoModes, Worker, Trie)
        )
    ).

get_wrapper_no_mode_args(M:Wrapper, M:WrapperNoModes, ModeArgs) :-
    M:'$table_mode'(Wrapper, WrapperNoModes, ModeArgs).

run_follower(fresh, Wrapper, WrapperNoModes, Worker, Trie) :-
    !,
    activate(Wrapper, WrapperNoModes, Worker, Trie, Worklist),
    shift(call_info(Wrapper, Worklist)).
run_follower(Worklist, Wrapper, _WrapperNoModes, _Worker, _Trie) :-
    shift(call_info(Wrapper, Worklist)).

run_leader(Wrapper, WrapperNoModes, Worker, Trie) :-
    activate(Wrapper, WrapperNoModes, Worker, Trie, _Worklist),
    completion,
    '$tbl_scheduling_component'(_, false).

activate(Wrapper, WrapperNoModes, Worker, Trie, WorkList) :-
    '$tbl_new_worklist'(WorkList, Trie),
    (   delim(Wrapper, WrapperNoModes, Worker, WorkList),
        fail
    ;   true
    ).

%!  delim(+Wrapper, +Worker, +WorkList).
%!  delim(+Wrapper, +WrapperNoModes, +Worker, +WorkList).
%
%   Call/resume Worker

delim(Wrapper, Worker, WorkList) :-
    reset(Worker, SourceCall, Continuation),
    add_answer_or_suspend(Continuation, Wrapper,
                          WorkList, SourceCall).

add_answer_or_suspend(0, Wrapper, WorkList, _) :-
    !,
    '$tbl_wkl_add_answer'(WorkList, Wrapper).
add_answer_or_suspend(Continuation, Wrapper, WorkList,
                      call_info(SrcWrapper, SourceWL)) :-
    '$tbl_wkl_add_suspension'(
        SourceWL,
        dependency(SrcWrapper, Continuation, Wrapper, WorkList)).

delim(Wrapper, WrapperNoModes, Worker, WorkList) :-
    reset(Worker, SourceCall, Continuation),
    add_answer_or_suspend(Continuation, Wrapper, WrapperNoModes,
                          WorkList, SourceCall).

add_answer_or_suspend(0, Wrapper, WrapperNoModes, WorkList, _) :-
    !,
    get_wrapper_no_mode_args(Wrapper, _, ModeArgs),
    '$tbl_wkl_mode_add_answer'(WorkList, WrapperNoModes,
                               ModeArgs, Wrapper).
add_answer_or_suspend(Continuation, Wrapper, _WrapperNoModes, WorkList,
                      call_info(SrcWrapper, SourceWL)) :-
    '$tbl_wkl_add_suspension'(
        SourceWL,
        dependency(SrcWrapper, Continuation, Wrapper, WorkList)).

%!  update(+Wrapper, +A1, +A2, -A3) is det.
%
%   Update the aggregated value for  an   answer.  Wrapper is the tabled
%   goal, A1 is the aggregated value so far, A2 is the new answer and A3
%   should be unified with the new aggregated value.

:- public
    update/4.

update(M:Wrapper, A1, A2, A3) :-
    M:'$table_update'(Wrapper, A1, A2, A3).


%!  completion
%
%   Wakeup suspended goals until no new answers are generated.

completion :-
    '$tbl_pop_worklist'(WorkList),
    !,
    completion_step(WorkList),
    completion.
completion :-
    '$tbl_table_complete_all'.

completion_step(SourceTable) :-
    (   '$tbl_trienode'(Reserved),
        '$tbl_wkl_work'(SourceTable,
                        Answer, ModeArgs,
                        Goal, Continuation, Wrapper, TargetTable),
        (   ModeArgs == Reserved
        ->  Goal = Answer,
            delim(Wrapper, Continuation, TargetTable)
        ;   get_wrapper_no_mode_args(Goal, Answer, ModeArgs),
            get_wrapper_no_mode_args(Wrapper, WrapperNoModes, _),
            delim(Wrapper, WrapperNoModes, Continuation, TargetTable)
        ),
        fail
    ;   true
    ).

                 /*******************************
                 *            CLEANUP           *
                 *******************************/

%!  abolish_all_tables
%
%   Remove all tables. This is normally used to free up the space or
%   recompute the result after predicates on   which  the result for
%   some tabled predicates depend.
%
%   @error  permission_error(abolish, table, all) if tabling is
%           in progress.

abolish_all_tables :-
    '$tbl_abolish_all_tables'.

%!  abolish_table_subgoals(:Subgoal) is det.
%
%   Abolish all tables that unify with SubGoal.

abolish_table_subgoals(M:SubGoal) :-
    '$tbl_variant_table'(VariantTrie),
    current_module(M),
    forall(trie_gen(VariantTrie, M:SubGoal, Trie),
           '$tbl_destroy_table'(Trie)).


                 /*******************************
                 *        EXAMINE TABLES        *
                 *******************************/

%!  current_table(:Variant, -Trie) is nondet.
%
%   True when Trie is the answer table for Variant.

current_table(M:Variant, Trie) :-
    '$tbl_variant_table'(VariantTrie),
    (   (var(Variant) ; var(M))
    ->  trie_gen(VariantTrie, M:Variant, Trie)
    ;   trie_lookup(VariantTrie, M:Variant, Trie)
    ).


                 /*******************************
                 *      WRAPPER GENERATION      *
                 *******************************/

:- multifile
    system:term_expansion/2,
    prolog:rename_predicate/2,
    tabled/2.
:- dynamic
    system:term_expansion/2.

wrappers(Var) -->
    { var(Var),
      !,
      instantiation_error(Var)
    }.
wrappers((A,B)) -->
    !,
    wrappers(A),
    wrappers(B).
wrappers(Name//Arity) -->
    { atom(Name), integer(Arity), Arity >= 0,
      !,
      Arity1 is Arity+2
    },
    wrappers(Name/Arity1).
wrappers(Name/Arity) -->
    { atom(Name), integer(Arity), Arity >= 0,
      !,
      functor(Head, Name, Arity),
      atom_concat(Name, ' tabled', WrapName),
      Head =.. [Name|Args],
      WrappedHead =.. [WrapName|Args],
      prolog_load_context(module, Module),
      '$tbl_trienode'(Reserved)
    },
    [ '$tabled'(Head),
      '$table_mode'(Head, Head, Reserved),
      (   Head :-
             start_tabling(Module:Head, WrappedHead)
      )
    ].
wrappers(ModeDirectedSpec) -->
    { callable(ModeDirectedSpec),
      !,
      functor(ModeDirectedSpec, Name, Arity),
      functor(Head, Name, Arity),
      atom_concat(Name, ' tabled', WrapName),
      Head =.. [Name|Args],
      WrappedHead =.. [WrapName|Args],
      extract_modes(ModeDirectedSpec, Head, Variant, Modes, Moded),
      updater_clauses(Modes, Head, UpdateClauses),
      prolog_load_context(module, Module)
    },
    [ '$tabled'(Head),
      '$table_mode'(Head, Variant, Moded),
      (   Head :-
             start_tabling(Module:Head, WrappedHead)
      )
    | UpdateClauses
    ].
wrappers(TableSpec) -->
    { type_error(table_desclaration, TableSpec)
    }.


%!  extract_modes(+ModeSpec, +Head, -Variant, -Modes, -ModedAnswer) is det.
%
%   Split Head into  its  variant  and   term  that  matches  the  moded
%   arguments.
%
%   @arg ModedAnswer is a term that  captures   that  value of all moded
%   arguments of an answer. If there  is   only  one,  this is the value
%   itself. If there are multiple, this is a term s(A1,A2,...)

extract_modes(ModeSpec, Head, Variant, Modes, ModedAnswer) :-
    compound_name_arguments(ModeSpec, Name, ModeSpecArgs),
    compound_name_arguments(Head, Name, HeadArgs),
    separate_args(ModeSpecArgs, HeadArgs, VariantArgs, Modes, ModedArgs),
    Variant =.. [Name|VariantArgs],
    (   ModedArgs == []
    ->  '$tbl_trienode'(ModedAnswer)
    ;   ModedArgs = [ModedAnswer]
    ->  true
    ;   ModedAnswer =.. [s|ModedArgs]
    ).

%!  separate_args(+ModeSpecArgs, +HeadArgs,
%!		  -NoModesArgs, -Modes, -ModeArgs) is det.
%
%   Split the arguments in those that  need   to  be part of the variant
%   identity (NoModesArgs) and those that are aggregated (ModeArgs).
%
%   @arg Args seems a copy of ModeArgs, why?

separate_args([], [], [], [], []).
separate_args([HM|TM], [H|TA], [H|TNA], Modes, TMA):-
    indexed_mode(HM),
    !,
    separate_args(TM, TA, TNA, Modes, TMA).
separate_args([M|TM], [H|TA], TNA, [M|Modes], [H|TMA]):-
    separate_args(TM, TA, TNA, Modes, TMA).

indexed_mode(Mode) :-                           % XSB
    var(Mode),
    !.
indexed_mode(index).                            % YAP
indexed_mode(+).                                % B

%!  updater_clauses(+Modes, +Head, -Clauses)
%
%   Generates a clause to update the aggregated state.  Modes is
%   a list of predicate names we apply to the state.

updater_clauses([], _, []) :- !.
updater_clauses([P], Head, [('$table_update'(Head, S0, S1, S2) :- Body)]) :- !,
    update_goal(P, S0,S1,S2, Body).
updater_clauses(Modes, Head, [('$table_update'(Head, S0, S1, S2) :- Body)]) :-
    length(Modes, Len),
    functor(S0, s, Len),
    functor(S1, s, Len),
    functor(S2, s, Len),
    S0 =.. [_|Args0],
    S1 =.. [_|Args1],
    S2 =.. [_|Args2],
    update_body(Modes, Args0, Args1, Args2, true, Body).

update_body([], _, _, _, Body, Body).
update_body([P|TM], [A0|Args0], [A1|Args1], [A2|Args2], Body0, Body) :-
    update_goal(P, A0,A1,A2, Goal),
    mkconj(Body0, Goal, Body1),
    update_body(TM, Args0, Args1, Args2, Body1, Body).

update_goal(Var, _,_,_, _) :-
    var(Var),
    !,
    instantiation_error(Var).
update_goal(lattice(M:PI), S0,S1,S2, M:Goal) :-
    !,
    must_be(atom, M),
    update_goal(lattice(PI), S0,S1,S2, Goal).
update_goal(lattice(Name/Arity), S0,S1,S2, Goal) :-
    !,
    must_be(oneof([3]), Arity),
    must_be(atom, Name),
    Goal =.. [Name,S0,S1,S2].
update_goal(lattice(Name), S0,S1,S2, Goal) :-
    !,
    must_be(atom, Name),
    update_goal(lattice(Name/3), S0,S1,S2, Goal).
update_goal(po(Name/Arity), S0,S1,S2, Goal) :-
    !,
    must_be(oneof([2]), Arity),
    must_be(atom, Name),
    Call =.. [Name, S0, S1],
    Goal = (Call -> S2 = S0 ; S2 = S1).
update_goal(po(M:Name/Arity), S0,S1,S2, Goal) :-
    !,
    must_be(atom, M),
    must_be(oneof([2]), Arity),
    must_be(atom, Name),
    Call =.. [Name, S0, S1],
    Goal = (M:Call -> S2 = S0 ; S2 = S1).
update_goal(po(M:Name), S0,S1,S2, Goal) :-
    !,
    must_be(atom, M),
    must_be(atom, Name),
    update_goal(po(M:Name/2), S0,S1,S2, Goal).
update_goal(po(Name), S0,S1,S2, Goal) :-
    !,
    must_be(atom, Name),
    update_goal(po(Name/2), S0,S1,S2, Goal).
update_goal(Alias, S0,S1,S2, Goal) :-
    update_alias(Alias, Update),
    !,
    update_goal(Update, S0,S1,S2, Goal).
update_goal(Mode, _,_,_, _) :-
    domain_error(tabled_mode, Mode).

update_alias(first, lattice(tabling:first/3)).
update_alias(-,     lattice(tabling:first/3)).
update_alias(last,  lattice(tabling:last/3)).
update_alias(min,   lattice(tabling:min/3)).
update_alias(max,   lattice(tabling:max/3)).
update_alias(sum,   lattice(tabling:sum/3)).

mkconj(true, G,  G) :- !.
mkconj(G1,   G2, (G1,G2)).


		 /*******************************
		 *          AGGREGATION		*
		 *******************************/

%!  first(+S0, +S1, -S) is det.
%!  last(+S0, +S1, -S) is det.
%!  min(+S0, +S1, -S) is det.
%!  max(+S0, +S1, -S) is det.
%!  sum(+S0, +S1, -S) is det.
%
%   Implement YAP tabling modes.

:- public first/3, last/3, min/3, max/3, sum/3.

first(S, _, S).
last(_, S, S).
min(S0, S1, S) :- (S0 @< S1 -> S = S0 ; S = S1).
max(S0, S1, S) :- (S0 @> S1 -> S = S0 ; S = S1).
sum(S0, S1, S) :- S is S0+S1.


		 /*******************************
		 *         RENAME WORKER	*
		 *******************************/

%!  prolog:rename_predicate(:Head0, :Head) is semidet.
%
%   Hook into term_expansion for  post   processing  renaming of the
%   generated predicate.

prolog:rename_predicate(M:Head0, M:Head) :-
    '$flushed_predicate'(M:'$tabled'(_)),
    call(M:'$tabled'(Head0)),
    !,
    rename_term(Head0, Head).

rename_term(Compound0, Compound) :-
    compound(Compound0),
    !,
    compound_name_arguments(Compound0, Name, Args),
    atom_concat(Name, ' tabled', WrapName),
    compound_name_arguments(Compound, WrapName, Args).
rename_term(Name, WrapName) :-
    atom_concat(Name, ' tabled', WrapName).


system:term_expansion((:- table(Preds)),
                      [ (:- multifile('$tabled'/1)),
                        (:- multifile('$table_mode'/3)),
                        (:- multifile('$table_update'/4))
                      | Clauses
                      ]) :-
    phrase(wrappers(Preds), Clauses).


                 /*******************************
                 *           SANDBOX            *
                 *******************************/

:- multifile
    sandbox:safe_directive/1,
    sandbox:safe_primitive/1,
    sandbox:safe_meta/2.

%!  sandbox:safe_directive(+Directive) is semidet.
%
%   Allow tabling directives that affect locally defined predicates.

sandbox:safe_directive(Dir) :-
    ground(Dir),
    local_tabling_dir(Dir).

local_tabling_dir(table(Preds)) :-
    local_preds(Preds).

local_preds((A,B)) :-
    !,
    local_preds(A),
    local_preds(B).

local_preds(Name/Arity) :-
    atom(Name), integer(Arity).
local_preds(Name//Arity) :-
    atom(Name), integer(Arity).

sandbox:safe_meta_predicate(tabling:start_tabling/2).

sandbox:safe_primitive(tabling:abolish_all_tables).
sandbox:safe_meta(tabling:abolish_table_subgoals(V), []) :-
    \+ qualified(V).
sandbox:safe_meta(tabling:current_table(V, _), []) :-
    \+ qualified(V).

qualified(V) :-
    nonvar(V),
    V = _:_.
