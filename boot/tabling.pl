/*  Part of SWI-Prolog

    Author:        Benoit Desouter <Benoit.Desouter@UGent.be>
                   Jan Wielemaker (SWI-Prolog port)
                   Fabrizio Riguzzi (mode directed tabling)
    Copyright (c) 2016-2019, Benoit Desouter,
                             Jan Wielemaker,
                             Fabrizio Riguzzi
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

:- module('$tabling',
          [ (table)/1,                  % :PI ...
            untable/1,                  % :PI ...

            (tnot)/1,                   % :Goal
            undefined/0,

            current_table/2,            % :Variant, ?Table
            abolish_all_tables/0,
            abolish_table_subgoals/1,   % :Subgoal
            abolish_module_tables/1,    % +Module

            start_tabling/2,            % +Wrapper, :Worker
            start_subsumptive_tabling/2,% +Wrapper, :Worker
            start_tabling/4,            % +Wrapper, :Worker, :Variant, ?ModeArgs

            '$wrap_tabled'/2,		% :Head, +Mode
            '$moded_wrap_tabled'/4,	% :Head, +ModeTest, +Variant, +Moded
            '$wfs_call'/2,              % :Goal, -Delays

            '$wrap_incremental'/1       % :Head
          ]).

:- meta_predicate
    table(:),
    untable(:),
    tnot(0),
    start_tabling(+, 0),
    start_tabling(+, 0, +, ?),
    current_table(:, -),
    abolish_table_subgoals(:),
    '$wfs_call'(0, :).

/** <module> Tabled execution (SLG WAM)

This  library  handled  _tabled_  execution   of  predicates  using  the
characteristics if the _SLG WAM_. The   required  suspension is realised
using _delimited continuations_ implemented by  reset/3 and shift/1. The
table space and work lists are part of the SWI-Prolog core.

@author Benoit Desouter, Jan Wielemaker and Fabrizio Riguzzi
*/

% Enable debugging using debug(tabling(Topic)) when compiled with
% -DO_DEBUG
goal_expansion(tdebug(Topic, Fmt, Args), Expansion) :-
    (   current_prolog_flag(prolog_debug, true)
    ->  Expansion = debug(tabling(Topic), Fmt, Args)
    ;   Expansion = true
    ).
goal_expansion(tdebug(Goal), Expansion) :-
    (   current_prolog_flag(prolog_debug, true)
    ->  Expansion = (   debugging(tabling(_))
                    ->  (   Goal
                        ->  true
                        ;   print_message(error, goal_failed(Goal))
                        )
                    ;   true
                    )
    ;   Expansion = true
    ).

:- if(current_prolog_flag(prolog_debug, true)).
wl_goal(tnot(WorkList), ~(Goal), Skeleton) :-
    !,
    '$tbl_worklist_data'(WorkList, worklist(_SCC,Trie,_,_,_)),
    '$tbl_table_status'(Trie, _Status, Wrapper, Skeleton),
    unqualify_goal(Wrapper, user, Goal).
wl_goal(WorkList, Goal, Skeleton) :-
    '$tbl_worklist_data'(WorkList, worklist(_SCC,Trie,_,_,_)),
    '$tbl_table_status'(Trie, _Status, Wrapper, Skeleton),
    unqualify_goal(Wrapper, user, Goal).

trie_goal(ATrie, Goal, Skeleton) :-
    '$tbl_table_status'(ATrie, _Status, Wrapper, Skeleton),
    unqualify_goal(Wrapper, user, Goal).

delay_goals(List, Goal) :-
    delay_goals(List, user, Goal).

user_goal(Goal, UGoal) :-
    unqualify_goal(Goal, user, UGoal).

:- endif.

%!  table(:PredicateIndicators)
%
%   Prepare the given PredicateIndicators for tabling. This predicate is
%   normally used as a directive,  but   SWI-Prolog  also allows runtime
%   conversion of non-tabled predicates to  tabled predicates by calling
%   table/1. The example below prepares  the   predicate  edge/2 and the
%   non-terminal statement//1 for tabled execution.
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

table(M:PIList) :-
    setup_call_cleanup(
        '$set_source_module'(OldModule, M),
        expand_term((:- table(PIList)), Clauses),
        '$set_source_module'(OldModule)),
    dyn_tabling_list(Clauses, M).

dyn_tabling_list([], _).
dyn_tabling_list([H|T], M) :-
    dyn_tabling(H, M),
    dyn_tabling_list(T, M).

dyn_tabling((:- multifile(PI)), M) :-
    !,
    multifile(M:PI),
    dynamic(M:PI).
dyn_tabling(:- initialization(Wrap, now), M) :-
    !,
    M:Wrap.
dyn_tabling('$tabled'(Head, TMode), M) :-
    (   clause(M:'$tabled'(Head, OMode), true, Ref),
        (   OMode \== TMode
        ->  erase(Ref),
            fail
        ;   true
        )
    ->  true
    ;   assertz(M:'$tabled'(Head, TMode))
    ).
dyn_tabling('$table_mode'(Head, Variant, Moded), M) :-
    (   clause(M:'$table_mode'(Head, Variant0, Moded0), true, Ref)
    ->  (   t(Head, Variant, Moded) =@= t(Head, Variant0, Moded0)
        ->  true
        ;   erase(Ref),
            assertz(M:'$table_mode'(Head, Variant, Moded))
        )
    ;   assertz(M:'$table_mode'(Head, Variant, Moded))
    ).

%!  untable(M:PIList) is det.
%
%   Remove tabling for the predicates in  PIList.   This  can be used to
%   undo the effect of table/1 at runtime.   In addition to removing the
%   tabling instrumentation this also removes possibly associated tables
%   using abolish_table_subgoals/1.
%
%   @arg PIList is a comma-list that is compatible ith table/1.

untable(M:PIList) :-
    untable(PIList, M).

untable(Var, _) :-
    var(Var),
    !,
    '$instantiation_error'(Var).
untable((A,B), M) :-
    !,
    untable(A, M),
    untable(B, M).
untable(Name//Arity, M) :-
    atom(Name), integer(Arity), Arity >= 0,
    !,
    Arity1 is Arity+2,
    untable(Name/Arity1, M).
untable(Name/Arity, M) :-
    !,
    functor(Head, Name, Arity),
    (   predicate_property(M:Head, tabled(_))
    ->  abolish_table_subgoals(M:Head),
        dynamic(M:'$tabled'/2),
        dynamic(M:'$table_mode'/3),
        retractall(M:'$tabled'(Head, _TMode)),
        retractall(M:'$table_mode'(Head, _Variant, _Moded)),
        unwrap_predicate(M:Name/Arity, table),
        '$set_predicate_attribute'(M:Head, tabled, false)
    ;   true
    ).
untable(Head, M) :-
    callable(Head),
    !,
    functor(Head, Name, Arity),
    untable(Name/Arity, M).
untable(TableSpec, _) :-
    '$type_error'(table_desclaration, TableSpec).


%!  start_tabling(:Wrapper, :Implementation)
%
%   Execute Implementation using tabling. This  predicate should not
%   be called directly. The table/1 directive  causes a predicate to
%   be translated into a renamed implementation   and a wrapper that
%   involves this predicate.
%
%   @compat This interface may change or disappear without notice
%           from future versions.

'$wrap_tabled'(Head, Options) :-
    get_dict(mode, Options, subsumptive),
    !,
    set_pattributes(Head, Options),
    '$wrap_predicate'(Head, table, Wrapped,
                      start_subsumptive_tabling(Head, Wrapped)).
'$wrap_tabled'(Head, Options) :-
    !,
    set_pattributes(Head, Options),
    '$wrap_predicate'(Head, table, Wrapped,
                      start_tabling(Head, Wrapped)).

set_pattributes(Head, Options) :-
    '$set_predicate_attribute'(Head, tabled, true),
    (   get_dict(incremental, Options, true)
    ->  '$set_predicate_attribute'(Head, incremental, true)
    ;   true
    ).

start_tabling(Wrapper, Worker) :-
    '$tbl_variant_table'(Wrapper, Trie, Status, Skeleton),
    (   Status == complete
    ->  '$idg_add_edge'(Trie),
        '$tbl_answer_update_dl'(Trie, Skeleton)
    ;   Status == fresh
    ->  '$tbl_create_subcomponent'(SCC, Trie),
        tdebug(user_goal(Wrapper, Goal)),
        tdebug(schedule, 'Created component ~d for ~p', [SCC, Goal]),
        '$idg_add_edge'(OldCurrent, Trie),
        setup_call_catcher_cleanup(
            true,
            run_leader(Skeleton, Worker, Trie, SCC, LStatus),
            Catcher,
            finished_leader(Catcher, SCC, Wrapper)),
        tdebug(schedule, 'Leader ~p done, status = ~p', [Goal, LStatus]),
        '$idg_set_current'(OldCurrent),
        done_leader(LStatus, SCC, Skeleton, Trie)
    ;   Status == invalid
    ->  user:reeval(Trie),                      % debug in user land
        '$idg_add_edge'(Trie),
        '$tbl_answer_update_dl'(Trie, Skeleton)
    ;   % = run_follower, but never fresh and Status is a worklist
        shift(call_info(Skeleton, Status))
    ).


%!  start_subsumptive_tabling(:Wrapper, :Implementation)

start_subsumptive_tabling(Wrapper, Worker) :-
    (   '$tbl_existing_variant_table'(Wrapper, Trie, Status, Skeleton)
    ->  (   Status == complete
        ->  '$idg_add_edge'(Trie),
            '$tbl_answer_update_dl'(Trie, Skeleton)
        ;    shift(call_info(Skeleton, Status))
        )
    ;   more_general_table(Wrapper, ATrie),
        '$tbl_table_status'(ATrie, complete, Wrapper, Skeleton)
    ->  '$idg_add_edge'(ATrie),
        '$tbl_answer_update_dl'(ATrie, Skeleton)
    ;   '$tbl_variant_table'(Wrapper, Trie, _0Status, Skeleton),
        '$idg_add_edge'(Trie),
        tdebug(_0Status == fresh),
        '$tbl_create_subcomponent'(SCC, Trie),
        tdebug(user_goal(Wrapper, Goal)),
        tdebug(schedule, 'Created component ~d for ~p', [SCC, Goal]),
        setup_call_catcher_cleanup(
            true,
            run_leader(Skeleton, Worker, Trie, SCC, LStatus),
            Catcher,
            finished_leader(Catcher, SCC, Wrapper)),
        tdebug(schedule, 'Leader ~p done, status = ~p', [Goal, LStatus]),
        done_leader(LStatus, SCC, Skeleton, Trie)
    ).


done_leader(complete, _SCC, Skeleton, Trie) :-
    !,
    '$tbl_answer_update_dl'(Trie, Skeleton).
done_leader(final, SCC, Skeleton, Trie) :-
    !,
    '$tbl_free_component'(SCC),
    '$tbl_answer_update_dl'(Trie, Skeleton).
done_leader(_,_,_,_).

finished_leader(exit, _, _) :-
    !.
finished_leader(fail, _, _) :-
    !.
finished_leader(Catcher, SCC, Wrapper) :-
    '$tbl_table_discard_all'(SCC),
    (   Catcher = exception(_)
    ->  true
    ;   print_message(error, tabling(unexpected_result(Wrapper, Catcher)))
    ).

%!  run_leader(+Wrapper, +Worker, +Trie, +SCC, -Status) is det.
%
%   Run the leader of  a  (new)   SCC,  storing  instantiated  copies of
%   Wrapper into Trie. Status  is  the  status   of  the  SCC  when this
%   predicate terminates. It is one of   `complete`, in which case local
%   completion finished or `merged` if running   the completion finds an
%   open (not completed) active goal that resides in a parent component.
%   In this case, this SCC has been merged with this parent.
%
%   If the SCC is merged, the answers   it already gathered are added to
%   the worklist and we shift  (suspend),   turning  our  leader into an
%   internal node for the upper SCC.

run_leader(Skeleton, Worker, Trie, SCC, Status) :-
    tdebug('$tbl_table_status'(Trie, _Status, Wrapper, Skeleton)),
    tdebug(user_goal(Wrapper, Goal)),
    tdebug(schedule, '-> Activate component ~p for ~p', [SCC, Goal]),
    activate(Skeleton, Worker, Trie, Worklist),
    tdebug(schedule, '-> Complete component ~p for ~p', [SCC, Goal]),
    completion(SCC),
    tdebug(schedule, '-> Completed component ~p for ~p', [SCC, Goal]),
    '$tbl_component_status'(SCC, Status),
    (   Status == merged
    ->  tdebug(merge, 'Turning leader ~p into follower', [Goal]),
        '$tbl_wkl_make_follower'(Worklist),
        shift(call_info(Skeleton, Worklist))
    ;   true                                    % completed
    ).

activate(Wrapper, Worker, Trie, WorkList) :-
    '$tbl_new_worklist'(WorkList, Trie),
    tdebug(activate, '~p: created wl=~p, trie=~p',
           [Wrapper, WorkList, Trie]),
    (   reset_delays,
        delim(Wrapper, Worker, WorkList, []),
        fail
    ;   true
    ).

%!  delim(+Wrapper, +Worker, +WorkList, +Delays)
%
%   Call/resume Worker for non-mode directed tabled predicates.

delim(Wrapper, Worker, WorkList, Delays) :-
    reset(Worker, SourceCall, Continuation),
    tdebug(wl_goal(WorkList, Goal, _)),
    (   Continuation == 0
    ->  tdebug('$tbl_add_global_delays'(Delays, AllDelays)),
        tdebug(delay_goals(AllDelays, Cond)),
        tdebug(answer, 'New answer ~p for ~p (delays = ~p)',
               [Wrapper, Goal, Cond]),
        '$tbl_wkl_add_answer'(WorkList, Wrapper, Delays, Complete),
        Complete == !,
        !
    ;   SourceCall = call_info(SrcSkeleton, SourceWL),
        '$tbl_add_global_delays'(Delays, AllDelays),
        tdebug(wl_goal(SourceWL, SrcGoal, _)),
        tdebug(wl_goal(WorkList, DstGoal, _)),
        tdebug(schedule, 'Suspended ~p, for solving ~p', [SrcGoal, DstGoal]),
        '$tbl_wkl_add_suspension'(
            SourceWL,
            dependency(SrcSkeleton, Continuation, Wrapper, WorkList, AllDelays))
    ).

%!  start_tabling(:Wrapper, :Implementation, +Variant, +ModeArgs)
%
%   As start_tabling/2, but in addition separates the data stored in the
%   answer trie in the Variant and ModeArgs.

'$moded_wrap_tabled'(Head, ModeTest, WrapperNoModes, ModeArgs) :-
    '$set_predicate_attribute'(Head, tabled, true),
    '$wrap_predicate'(Head, table, Wrapped,
                      (   ModeTest,
                          start_tabling(Head, Wrapped, WrapperNoModes, ModeArgs)
                      )).


start_tabling(Wrapper, Worker, WrapperNoModes, ModeArgs) :-
    '$tbl_variant_table'(WrapperNoModes, Trie, Status, _Skeleton),
    '$idg_add_edge'(Trie),
    (   Status == complete
    ->  trie_gen(Trie, WrapperNoModes, ModeArgs)
    ;   Status == fresh
    ->  '$tbl_create_subcomponent'(SubComponent, Trie),
        setup_call_catcher_cleanup(
            true,
            run_leader(Wrapper, WrapperNoModes, ModeArgs,
                       Worker, Trie, SubComponent, LStatus),
            Catcher,
            finished_leader(Catcher, SubComponent, Wrapper)),
        tdebug(schedule, 'Leader ~p done, modeargs = ~p, status = ~p',
               [Wrapper, ModeArgs, LStatus]),
        moded_done_leader(LStatus, SubComponent, WrapperNoModes, ModeArgs, Trie)
    ;   % = run_follower, but never fresh and Status is a worklist
        shift(call_info(Wrapper, Status))
    ).

moded_done_leader(complete, _SCC, WrapperNoModes, ModeArgs, Trie) :-
    !,
    trie_gen(Trie, WrapperNoModes, ModeArgs).
moded_done_leader(final, SCC, WrapperNoModes, ModeArgs, Trie) :-
    !,
    '$tbl_free_component'(SCC),
    trie_gen(Trie, WrapperNoModes, ModeArgs).
moded_done_leader(_, _, _, _, _).


get_wrapper_no_mode_args(M:Wrapper, M:WrapperNoModes, ModeArgs) :-
    M:'$table_mode'(Wrapper, WrapperNoModes, ModeArgs).

run_leader(Wrapper, WrapperNoModes, ModeArgs, Worker, Trie, SCC, Status) :-
    moded_activate(Wrapper, WrapperNoModes, ModeArgs, Worker, Trie, Worklist),
    completion(SCC),
    '$tbl_component_status'(SCC, Status),
    (   Status == merged
    ->  tdebug(scc, 'Turning leader ~p into follower', [Wrapper]),
        (   trie_gen(Trie, WrapperNoModes1, ModeArgs1),
            tdebug(scc, 'Adding old answer ~p+~p to worklist ~p',
                   [ WrapperNoModes1, ModeArgs1, Worklist]),
            '$tbl_wkl_mode_add_answer'(Worklist, WrapperNoModes1,
                                       ModeArgs1, Wrapper),
            fail
        ;   true
        ),
        shift(call_info(Wrapper, Worklist))
    ;   true                                    % completed
    ).


moded_activate(Wrapper, WrapperNoModes, _ModeArgs, Worker, Trie, WorkList) :-
    '$tbl_new_worklist'(WorkList, Trie),
    (   moded_delim(Wrapper, WrapperNoModes, Worker, WorkList, []), % FIXME: Delay list
        fail
    ;   true
    ).

%!  moded_delim(+Wrapper, +WrapperNoModes, +Worker, +WorkList, +Delays).
%
%   Call/resume Worker for mode directed tabled predicates.

moded_delim(Wrapper, WrapperNoModes, Worker, WorkList, Delays) :-
    reset(Worker, SourceCall, Continuation),
    moded_add_answer_or_suspend(Continuation, Wrapper, WrapperNoModes,
                                WorkList, SourceCall, Delays).

moded_add_answer_or_suspend(0, Wrapper, WrapperNoModes, WorkList, _, _) :-
    !,
    get_wrapper_no_mode_args(Wrapper, _, ModeArgs),
    '$tbl_wkl_mode_add_answer'(WorkList, WrapperNoModes,
                               ModeArgs, Wrapper). % FIXME: Add Delays
moded_add_answer_or_suspend(Continuation, Wrapper, _WrapperNoModes, WorkList,
                      call_info(SrcWrapper, SourceWL),
                      Delays) :-
    '$tbl_wkl_add_suspension'(
        SourceWL,
        dependency(SrcWrapper, Continuation, Wrapper, WorkList, Delays)).


%!  update(+Wrapper, +A1, +A2, -A3) is semidet.
%
%   Update the aggregated value for  an   answer.  Wrapper is the tabled
%   goal, A1 is the aggregated value so far, A2 is the new answer and A3
%   should be unified with the new   aggregated value. The new aggregate
%   is ignored if it is the same as the old one.

:- public
    update/4.

update(M:Wrapper, A1, A2, A3) :-
    M:'$table_update'(Wrapper, A1, A2, A3),
    A1 \=@= A3.


%!  completion(+Component)
%
%   Wakeup suspended goals until  no  new   answers  are  generated. The
%   second argument of completion/2 keeps the current heap _delay list_,
%   called the _D_ register in th XSB   literature.  It is modified from
%   the C core (negative_worklist())   using (backtrackable) destructive
%   assignment. The C core walks the   environment  to find completion/2
%   and from there the delay list.

completion(SCC) :-
    (   reset_delays,
        completion_(SCC),
        fail
    ;   true
    ).

completion_(SCC) :-
    repeat,
    '$tbl_component_status'(SCC, Status),
    (   Status == active
    ->  (   '$tbl_pop_worklist'(SCC, WorkList)
        ->  tdebug(wl_goal(WorkList, Goal, _)),
            tdebug(schedule, 'Complete ~p in ~p', [Goal, scc(SCC)]),
            completion_step(WorkList),
            fail
        ;   tdebug(schedule, 'Completed ~p', [scc(SCC)]),
            '$tbl_table_complete_all'(SCC)
        )
    ;   Status == merged
    ->  tdebug(schedule, 'Aborted completion of ~p', [scc(SCC)])
    ;   true
    ),
    !.

completion_step(WorkList) :-
    (   '$tbl_trienode'(Reserved),
        '$tbl_wkl_work'(WorkList,
                        Answer, ModeArgs,
                        Goal, Continuation, Wrapper, TargetWorklist, Delays),
        '$idg_set_current_wl'(TargetWorklist),
        tdebug(wl_goal(WorkList, SourceGoal, _)),
        tdebug(wl_goal(TargetWorklist, TargetGoal, _Skeleton)),
        (   ModeArgs == Reserved
        ->  tdebug('$tbl_add_global_delays'(Delays, AllDelays)),
            tdebug(delay_goals(AllDelays, Cond)),
            tdebug(schedule, 'Resuming ~p, calling ~p with ~p (delays = ~p)',
                   [TargetGoal, SourceGoal, Answer, Cond]),
            Goal = Answer,
            delim(Wrapper, Continuation, TargetWorklist, Delays)
        ;   get_wrapper_no_mode_args(Goal, Answer, ModeArgs),
            get_wrapper_no_mode_args(Wrapper, WrapperNoModes, _),
            moded_delim(Wrapper, WrapperNoModes, Continuation, TargetWorklist,
                        Delays)
        ),
        fail
    ;   true
    ).


		 /*******************************
		 *     STRATIFIED NEGATION	*
		 *******************************/

%!  tnot(:Goal)
%
%   Tabled negation.

tnot(Goal0) :-
    '$tbl_implementation'(Goal0, Goal),         % verifies Goal is tabled
    '$tbl_variant_table'(Goal, Trie, Status, Skeleton),
    \+ \+ '$idg_add_edge'(Trie),                % do not update current node
    (   '$tbl_answer_dl'(Trie, _, true)
    ->  fail
    ;   '$tbl_answer_dl'(Trie, _, _)
    ->  add_delay(Trie)
    ;   Status == complete
    ->  true
    ;   Status == fresh
    ->  tdebug(tnot, 'tnot: ~p: fresh', [Goal]),
        (   call(Goal),
            fail
        ;   '$tbl_variant_table'(Goal, Trie, NewStatus, NewSkeleton),
            tdebug(tnot, 'tnot: fresh ~p now ~p', [Goal, NewStatus]),
            (   '$tbl_answer_dl'(Trie, _, true)
            ->  fail
            ;   '$tbl_answer_dl'(Trie, _, _)
            ->  add_delay(Trie)
            ;   NewStatus == complete
            ->  true
            ;   negation_suspend(Goal, NewSkeleton, NewStatus)
            )
        )
    ;   negation_suspend(Goal, Skeleton, Status)
    ).


%!  negation_suspend(+Goal, +Skeleton, +Worklist)
%
%   Suspend Worklist due to negation. This marks the worklist as dealing
%   with a negative literal and suspend.
%
%   The completion step will resume  negative   worklists  that  have no
%   solutions, causing this to succeed.

negation_suspend(Wrapper, Skeleton, Worklist) :-
    tdebug(tnot, 'negation_suspend ~p (wl=~p)', [Wrapper, Worklist]),
    '$tbl_wkl_negative'(Worklist),
    shift(call_info(Skeleton, tnot(Worklist))),
    tdebug(tnot, 'negation resume ~p (wl=~p)', [Wrapper, Worklist]),
    '$tbl_wkl_is_false'(Worklist).


		 /*******************************
		 *           DELAY LISTS	*
		 *******************************/

add_delay(Delay) :-
    '$tbl_delay_list'(DL0),
    '$tbl_set_delay_list'([Delay|DL0]).

reset_delays :-
    '$tbl_set_delay_list'([]).

%!  '$wfs_call'(:Goal, :Delays)
%
%   Call Goal and provide WFS delayed goals  as a conjunction in Delays.
%   This  predicate  is  teh  internal  version  of  call_delays/2  from
%   library(wfs).

'$wfs_call'(Goal, M:Delays) :-
    '$tbl_delay_list'(DL0),
    reset_delays,
    call(Goal),
    '$tbl_delay_list'(DL1),
    (   delay_goals(DL1, M, Delays)
    ->  true
    ;   Delays = undefined
    ),
    '$append'(DL0, DL1, DL),
    '$tbl_set_delay_list'(DL).

delay_goals([], _, true) :-
    !.
delay_goals([AT+AN|T], M, Goal) :-
    !,
    (   integer(AN)
    ->  at_delay_goal(AT, M, G0, Answer),
        trie_term(AN, Answer)
    ;   '$tbl_table_status'(AT, _Status, G0, AN)
    ),
    GN = G0,
    (   T == []
    ->  Goal = GN
    ;   Goal = (GN,GT),
        delay_goals(T, M, GT)
    ).
delay_goals([AT|T], M, Goal) :-
    at_delay_goal(AT, M, G0, _Skeleton),
    GN = tnot(G0),
    (   T == []
    ->  Goal = GN
    ;   Goal = (GN,GT),
        delay_goals(T, M, GT)
    ).

at_delay_goal(tnot(Trie), M, tnot(Goal), Skeleton) :-
    is_trie(Trie),
    !,
    '$tbl_table_status'(Trie, _Status, Wrapper, Skeleton),
    unqualify_goal(Wrapper, M, Goal).
at_delay_goal(Trie, M, Goal, Skeleton) :-
    is_trie(Trie),
    !,
    '$tbl_table_status'(Trie, _Status, Wrapper, Skeleton),
    unqualify_goal(Wrapper, M, Goal).

unqualify_goal(M:Goal, M, Goal0) :-
    !,
    Goal0 = Goal.
unqualify_goal(Goal, _, Goal).


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

abolish_table_subgoals(SubGoal0) :-
    '$tbl_implementation'(SubGoal0, M:SubGoal),
    '$tbl_variant_table'(VariantTrie),
    !,
    current_module(M),
    forall(trie_gen(VariantTrie, M:SubGoal, Trie),
           '$tbl_destroy_table'(Trie)).
abolish_table_subgoals(_).

%!  abolish_module_tables(+Module) is det.
%
%   Abolish all tables for predicates associated with the given module.

abolish_module_tables(Module) :-
    '$must_be'(atom, Module),
    '$tbl_variant_table'(VariantTrie),
    current_module(Module),
    !,
    forall(trie_gen(VariantTrie, Module:_, Trie),
           '$tbl_destroy_table'(Trie)).
abolish_module_tables(_).


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
    tabled/2.
:- dynamic
    system:term_expansion/2.

wrappers(Spec) -->
    wrappers(Spec, #{}).

wrappers(Var, _) -->
    { var(Var),
      !,
      '$instantiation_error'(Var)
    }.
wrappers(Spec as Options, Opts0) -->
    !,
    { table_options(Options, Opts0, Opts) },
    wrappers(Spec, Opts).
wrappers((A,B), Opts) -->
    !,
    wrappers(A, Opts),
    wrappers(B, Opts).
wrappers(Name//Arity, Opts) -->
    { atom(Name), integer(Arity), Arity >= 0,
      !,
      Arity1 is Arity+2
    },
    wrappers(Name/Arity1, Opts).
wrappers(Name/Arity, Opts) -->
    { '$option'(mode(TMode), Opts, variant),
      atom(Name), integer(Arity), Arity >= 0,
      !,
      functor(Head, Name, Arity),
      prolog_load_context(module, Module),
      '$tbl_trienode'(Reserved)
    },
    [ '$tabled'(Head, TMode),
      '$table_mode'(Head, Head, Reserved),
      (:- initialization('$wrap_tabled'(Module:Head, Opts), now))
    ].
wrappers(ModeDirectedSpec, Opts) -->
    { '$option'(mode(TMode), Opts, variant),
      callable(ModeDirectedSpec),
      !,
      functor(ModeDirectedSpec, Name, Arity),
      functor(Head, Name, Arity),
      extract_modes(ModeDirectedSpec, Head, Variant, Modes, Moded),
      updater_clauses(Modes, Head, UpdateClauses),
      prolog_load_context(module, Module),
      mode_check(Moded, ModeTest),
      (   ModeTest == true
      ->  WrapClause = '$wrap_tabled'(Module:Head, Opts)
      ;   WrapClause = '$moded_wrap_tabled'(Module:Head, ModeTest,
          Module:Variant, Moded)
      )
    },
    [ '$tabled'(Head, TMode),
      '$table_mode'(Head, Variant, Moded),
      (:- initialization(WrapClause, now))
    | UpdateClauses
    ].
wrappers(TableSpec, _Opts) -->
    { '$type_error'(table_desclaration, TableSpec)
    }.

%!  table_options(+Options, +OptDictIn, -OptDictOut)
%
%   Handler the ... as _options_ ... construct.

table_options(Options, _Opts0, _Opts) :-
    var(Options),
    '$instantiation_error'(Options).
table_options((A,B), Opts0, Opts) :-
    table_options(A, Opts0, Opts1),
    table_options(B, Opts1, Opts).
table_options(subsumptive, Opts0, Opts1) :-
    put_dict(mode, Opts0, subsumptive, Opts1).
table_options(variant, Opts0, Opts1) :-
    put_dict(mode, Opts0, variant, Opts1).
table_options(incremental, Opts0, Opts1) :-
    put_dict(incremental, Opts0, true, Opts1).


%!  mode_check(+Moded, -TestCode)
%
%   Enforce the output arguments of a  mode-directed tabled predicate to
%   be unbound.

mode_check(Moded, Check) :-
    var(Moded),
    !,
    Check = (var(Moded)->true;'$uninstantiation_error'(Moded)).
mode_check(Moded, true) :-
    '$tbl_trienode'(Moded),
    !.
mode_check(Moded, (Test->true;'$tabling':instantiated_moded_arg(Vars))) :-
    Moded =.. [s|Vars],
    var_check(Vars, Test).

var_check([H|T], Test) :-
    (   T == []
    ->  Test = var(H)
    ;   Test = (var(H),Rest),
        var_check(T, Rest)
    ).

:- public
    instantiated_moded_arg/1.

instantiated_moded_arg(Vars) :-
    '$member'(V, Vars),
    \+ var(V),
    '$uninstantiation_error'(V).


%!  extract_modes(+ModeSpec, +Head, -Variant, -Modes, -ModedAnswer) is det.
%
%   Split Head into  its  variant  and   term  that  matches  the  moded
%   arguments.
%
%   @arg ModedAnswer is a term that  captures   that  value of all moded
%   arguments of an answer. If there  is   only  one,  this is the value
%   itself. If there are multiple, this is a term s(A1,A2,...)

extract_modes(ModeSpec, Head, Variant, Modes, ModedAnswer) :-
    compound(ModeSpec),
    !,
    compound_name_arguments(ModeSpec, Name, ModeSpecArgs),
    compound_name_arguments(Head, Name, HeadArgs),
    separate_args(ModeSpecArgs, HeadArgs, VariantArgs, Modes, ModedArgs),
    length(ModedArgs, Count),
    atomic_list_concat([$,Name,$,Count], VName),
    Variant =.. [VName|VariantArgs],
    (   ModedArgs == []
    ->  '$tbl_trienode'(ModedAnswer)
    ;   ModedArgs = [ModedAnswer]
    ->  true
    ;   ModedAnswer =.. [s|ModedArgs]
    ).
extract_modes(Atom, Atom, Variant, [], ModedAnswer) :-
    atomic_list_concat([$,Atom,$,0], Variant),
    '$tbl_trienode'(ModedAnswer).

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
    '$instantiation_error'(Var).
update_goal(lattice(M:PI), S0,S1,S2, M:Goal) :-
    !,
    '$must_be'(atom, M),
    update_goal(lattice(PI), S0,S1,S2, Goal).
update_goal(lattice(Name/Arity), S0,S1,S2, Goal) :-
    !,
    '$must_be'(oneof(integer, lattice_arity, [3]), Arity),
    '$must_be'(atom, Name),
    Goal =.. [Name,S0,S1,S2].
update_goal(lattice(Head), S0,S1,S2, Goal) :-
    compound(Head),
    !,
    compound_name_arity(Head, Name, Arity),
    '$must_be'(oneof(integer, lattice_arity, [3]), Arity),
    Goal =.. [Name,S0,S1,S2].
update_goal(lattice(Name), S0,S1,S2, Goal) :-
    !,
    '$must_be'(atom, Name),
    update_goal(lattice(Name/3), S0,S1,S2, Goal).
update_goal(po(Name/Arity), S0,S1,S2, Goal) :-
    !,
    '$must_be'(oneof(integer, po_arity, [2]), Arity),
    '$must_be'(atom, Name),
    Call =.. [Name, S0, S1],
    Goal = (Call -> S2 = S0 ; S2 = S1).
update_goal(po(M:Name/Arity), S0,S1,S2, Goal) :-
    !,
    '$must_be'(atom, M),
    '$must_be'(oneof(integer, po_arity, [2]), Arity),
    '$must_be'(atom, Name),
    Call =.. [Name, S0, S1],
    Goal = (M:Call -> S2 = S0 ; S2 = S1).
update_goal(po(M:Name), S0,S1,S2, Goal) :-
    !,
    '$must_be'(atom, M),
    '$must_be'(atom, Name),
    update_goal(po(M:Name/2), S0,S1,S2, Goal).
update_goal(po(Name), S0,S1,S2, Goal) :-
    !,
    '$must_be'(atom, Name),
    update_goal(po(Name/2), S0,S1,S2, Goal).
update_goal(Alias, S0,S1,S2, Goal) :-
    update_alias(Alias, Update),
    !,
    update_goal(Update, S0,S1,S2, Goal).
update_goal(Mode, _,_,_, _) :-
    '$domain_error'(tabled_mode, Mode).

update_alias(first, lattice('$tabling':first/3)).
update_alias(-,     lattice('$tabling':first/3)).
update_alias(last,  lattice('$tabling':last/3)).
update_alias(min,   lattice('$tabling':min/3)).
update_alias(max,   lattice('$tabling':max/3)).
update_alias(sum,   lattice('$tabling':sum/3)).

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
		 *      INCREMENTAL TABLING	*
		 *******************************/

%!  '$wrap_incremental'(:Head) is det.
%
%   Wrap an incremental dynamic predicate to be added to the IDG.

'$wrap_incremental'(Head) :-
    abstract_goal(Head, Abstract),
    '$wrap_predicate'(Head, incremental, Wrapped,
                      (   '$idg_add_dyncall'(Abstract),
                          Wrapped
                      )),
    '$pi_head'(PI, Head),
    (   Head == Abstract
    ->  prolog_listen(PI, dyn_update)
    ;   prolog_listen(PI, dyn_update(Abstract))
    ).

abstract_goal(M:Head, M:Abstract) :-
    compound(Head),
    '$get_predicate_attribute'(M:Head, abstract, 1),
    !,
    compound_name_arity(Head, Name, Arity),
    functor(Abstract, Name, Arity).
abstract_goal(Head, Head).

dyn_update(_Action, ClauseRef) :-
    clause(Head, _Body, ClauseRef),
    dyn_changed_pattern(Head).
dyn_update(Abstract, _, _) :-
    dyn_changed_pattern(Abstract).

dyn_changed_pattern(Term) :-
    '$tbl_variant_table'(VTable),
    !,
    forall(trie_gen(VTable, Term, ATrie),
           '$idg_changed'(ATrie)).
dyn_changed_pattern(_).


		 /*******************************
		 *      EXPAND DIRECTIVES	*
		 *******************************/

system:term_expansion((:- table(Preds)),
                      [ (:- multifile('$tabled'/2)),
                        (:- multifile('$table_mode'/3)),
                        (:- multifile('$table_update'/4))
                      | Clauses
                      ]) :-
    \+ current_prolog_flag(xref, true),
    phrase(wrappers(Preds), Clauses).


		 /*******************************
		 *      ANSWER COMPLETION	*
		 *******************************/

:- public answer_completion/2.

%!  answer_completion(+AnswerTrie, +Return) is det.
%
%   Find  positive  loops  in  the  residual   program  and  remove  the
%   corresponding answers, possibly causing   additional simplification.
%   This is called from C  if   simplify_component()  detects  there are
%   conditional answers after simplification.
%
%   Note that we are called recursively from   C.  Our caller prepared a
%   clean new tabling environment and restores   the  old one after this
%   predicate terminates.
%
%   @author This code is by David Warren as part of XSB.
%   @see called from C, pl-tabling.c, answer_completion()

answer_completion(AnswerTrie, Return) :-
    tdebug(trie_goal(AnswerTrie, Goal, _Return)),
    tdebug(ac(start), 'START: Answer completion for ~p', [Goal]),
    call_cleanup(answer_completion_guarded(AnswerTrie, Return, Propagated),
                 abolish_table_subgoals(eval_subgoal_in_residual(_,_))),
    (   Propagated > 0
    ->  answer_completion(AnswerTrie, Return)
    ;   true
    ).

answer_completion_guarded(AnswerTrie, Return, Propagated) :-
    (   eval_subgoal_in_residual(AnswerTrie, Return),
        fail
    ;   true
    ),
    delete_answers_for_failing_calls(Propagated),
    (   Propagated == 0
    ->  mark_succeeding_calls_as_answer_completed
    ;   true
    ).

%!  delete_answers_for_failing_calls(-Propagated)
%
%   Delete answers whose condition  is  determined   to  be  `false` and
%   return the number of additional  answers   that  changed status as a
%   consequence of additional simplification propagation.

delete_answers_for_failing_calls(Propagated) :-
    State = state(0),
    (   subgoal_residual_trie(ASGF, ESGF),
        \+ trie_gen(ESGF, _ETmp),
        tdebug(trie_goal(ASGF, Goal0, _)),
        tdebug(trie_goal(ASGF, Goal, _0Return)),
        '$trie_gen_node'(ASGF, _0Return, ALeaf),
        tdebug(ac(prune), '  Removing answer ~p from ~p', [Goal, Goal0]),
	'$tbl_force_truth_value'(ALeaf, false, Count),
        arg(1, State, Prop0),
        Prop is Prop0+Count-1,
        nb_setarg(1, State, Prop),
	fail
    ;   arg(1, State, Propagated)
    ).

mark_succeeding_calls_as_answer_completed :-
    (   subgoal_residual_trie(ASGF, _ESGF),
        (   '$tbl_answer_dl'(ASGF, _0Return, _True)
        ->  tdebug(trie_goal(ASGF, Answer, _0Return)),
            tdebug(trie_goal(ASGF, Goal, _0Return)),
            tdebug(ac(prune), '  Completed ~p on ~p', [Goal, Answer]),
            '$tbl_set_answer_completed'(ASGF)
        ),
        fail
    ;   true
    ).

subgoal_residual_trie(ASGF, ESGF) :-
    '$tbl_variant_table'(VariantTrie),
    context_module(M),
    trie_gen(VariantTrie, M:eval_subgoal_in_residual(ASGF, _), ESGF).

%!  eval_dl_in_residual(+Condition)
%
%   Evaluate a condition by only looking at   the  residual goals of the
%   involved calls.

eval_dl_in_residual(true) :-
    !.
eval_dl_in_residual((A;B)) :-
    !,
    (   eval_dl_in_residual(A)
    ;   eval_dl_in_residual(B)
    ).
eval_dl_in_residual((A,B)) :-
    !,
    eval_dl_in_residual(A),
    eval_dl_in_residual(B).
eval_dl_in_residual(tnot(G)) :-
    !,
    tdebug(ac, ' ? tnot(~p)', [G]),
    current_table(G, SGF),
    '$tbl_table_status'(SGF, _Status, _Wrapper, Return),
    tnot(eval_subgoal_in_residual(SGF, Return)).
eval_dl_in_residual(G) :-
    tdebug(ac, ' ? ~p', [G]),
    (   current_table(G, SGF)
    ->	true
    ;   more_general_table(G, SGF)
    ->	true
    ;	writeln(user_error, 'MISSING CALL? '(G)),
        fail
    ),
    '$tbl_table_status'(SGF, _Status, _Wrapper, Return),
    eval_subgoal_in_residual(SGF, Return).

more_general_table(G, Trie) :-
    term_variables(G, Vars),
    length(Vars, Len),
    '$tbl_variant_table'(VariantTrie),
    trie_gen(VariantTrie, G, Trie),
    all_vars(Vars),
    sort(Vars, V2),
    length(V2, Len).

all_vars([]).
all_vars([H|T]) :-
    var(H),
    all_vars(T).

:- table eval_subgoal_in_residual/2.

%!  eval_subgoal_in_residual(+AnswerTrie, ?Return)
%
%   Derive answers for the variant represented   by  AnswerTrie based on
%   the residual goals only.

eval_subgoal_in_residual(AnswerTrie, _Return) :-
    '$tbl_is_answer_completed'(AnswerTrie),
    !,
    undefined.
eval_subgoal_in_residual(AnswerTrie, Return) :-
    '$tbl_answer'(AnswerTrie, Return, Condition),
    tdebug(trie_goal(AnswerTrie, Goal, Return)),
    tdebug(ac, 'Condition for ~p is ~p', [Goal, Condition]),
    eval_dl_in_residual(Condition).

%!  undefined
%
%   Expresses the value _bottom_ from the well founded semantics.

:- table
    undefined/0.

undefined :-
    tnot(undefined).
