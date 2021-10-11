/*  Part of SWI-Prolog

    Author:        Benoit Desouter <Benoit.Desouter@UGent.be>
                   Jan Wielemaker (SWI-Prolog port)
                   Fabrizio Riguzzi (mode directed tabling)
    Copyright (c) 2016-2021, Benoit Desouter,
                             Jan Wielemaker,
                             Fabrizio Riguzzi
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

:- module('$tabling',
          [ (table)/1,                  % :PI ...
            untable/1,                  % :PI ...

            (tnot)/1,                   % :Goal
            not_exists/1,               % :Goal
            undefined/0,
            answer_count_restraint/0,
            radial_restraint/0,

            current_table/2,            % :Variant, ?Table
            abolish_all_tables/0,
            abolish_private_tables/0,
            abolish_shared_tables/0,
            abolish_table_subgoals/1,   % :Subgoal
            abolish_module_tables/1,    % +Module
            abolish_nonincremental_tables/0,
            abolish_nonincremental_tables/1, % +Options
            abolish_monotonic_tables/0,

            start_tabling/3,            % +Closure, +Wrapper, :Worker
            start_subsumptive_tabling/3,% +Closure, +Wrapper, :Worker
            start_abstract_tabling/3,   % +Closure, +Wrapper, :Worker
            start_moded_tabling/5,      % +Closure, +Wrapper, :Worker,
                                        % :Variant, ?ModeArgs

            '$tbl_answer'/4,            % +Trie, -Return, -ModeArgs, -Delay

            '$wrap_tabled'/2,		% :Head, +Mode
            '$moded_wrap_tabled'/5,	% :Head, +Opts, +ModeTest, +Varnt, +Moded
            '$wfs_call'/2,              % :Goal, -Delays

            '$set_table_wrappers'/1,    % :Head
            '$start_monotonic'/2        % :Head, :Wrapped
          ]).

:- meta_predicate
    table(:),
    untable(:),
    tnot(0),
    not_exists(0),
    tabled_call(0),
    start_tabling(+, +, 0),
    start_abstract_tabling(+, +, 0),
    start_moded_tabling(+, +, 0, +, ?),
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
                        ;   print_message(error,
                                          format('goal_failed: ~q', [Goal]))
                        )
                    ;   true
                    )
    ;   Expansion = true
    ).

:- if(current_prolog_flag(prolog_debug, true)).
wl_goal(tnot(WorkList), ~(Goal), Skeleton) :-
    !,
    '$tbl_wkl_table'(WorkList, ATrie),
    trie_goal(ATrie, Goal, Skeleton).
wl_goal(WorkList, Goal, Skeleton) :-
    '$tbl_wkl_table'(WorkList, ATrie),
    trie_goal(ATrie, Goal, Skeleton).

trie_goal(ATrie, Goal, Skeleton) :-
    '$tbl_table_status'(ATrie, _Status, M:Variant, Skeleton),
    (   M:'$table_mode'(Goal0, Variant, _Moded)
    ->  true
    ;   Goal0 = Variant                 % dynamic IDG nodes
    ),
    unqualify_goal(M:Goal0, user, Goal).

delay_goals(List, Goal) :-
    delay_goals(List, user, Goal).

user_goal(Goal, UGoal) :-
    unqualify_goal(Goal, user, UGoal).

:- multifile
    prolog:portray/1.

user:portray(ATrie) :-
    '$is_answer_trie'(ATrie, _),
    trie_goal(ATrie, Goal, _Skeleton),
    (   '$idg_falsecount'(ATrie, FalseCount)
    ->  (   '$idg_forced'(ATrie)
        ->  format('~q [fc=~d/F] for ~p', [ATrie, FalseCount, Goal])
        ;   format('~q [fc=~d] for ~p', [ATrie, FalseCount, Goal])
        )
    ;   format('~q for ~p', [ATrie, Goal])
    ).
user:portray(Cont) :-
    compound(Cont),
    compound_name_arguments(Cont, '$cont$', [_Context, Clause, PC | Args]),
    clause_property(Clause, file(File)),
    file_base_name(File, Base),
    clause_property(Clause, line_count(Line)),
    clause_property(Clause, predicate(PI)),
    format('~q at ~w:~d @PC=~w, ~p', [PI, Base, Line, PC, Args]).

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

dyn_tabling(M:Clause, _) :-
    !,
    dyn_tabling(Clause, M).
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
dyn_tabling(('$table_update'(Head, S0, S1, S2) :- Body), M) :-
    (   clause(M:'$table_update'(Head, S00, S10, S20), Body0, Ref)
    ->  (   t(Head, S0, S1, S2, Body) =@= t(Head, S00, S10, S20, Body0)
        ->  true
        ;   erase(Ref),
            assertz(M:('$table_update'(Head, S0, S1, S2) :- Body))
        )
    ;   assertz(M:('$table_update'(Head, S0, S1, S2) :- Body))
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
untable(M:Spec, _) :-
    !,
    '$must_be'(atom, M),
    untable(Spec, M).
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
    (   '$get_predicate_attribute'(M:Head, tabled, 1)
    ->  abolish_table_subgoals(M:Head),
        dynamic(M:'$tabled'/2),
        dynamic(M:'$table_mode'/3),
        retractall(M:'$tabled'(Head, _TMode)),
        retractall(M:'$table_mode'(Head, _Variant, _Moded)),
        unwrap_predicate(M:Name/Arity, table),
        '$set_predicate_attribute'(M:Head, tabled, false),
        '$set_predicate_attribute'(M:Head, opaque, false),
        '$set_predicate_attribute'(M:Head, incremental, false),
        '$set_predicate_attribute'(M:Head, monotonic, false),
        '$set_predicate_attribute'(M:Head, lazy, false)
    ;   true
    ).
untable(Head, M) :-
    callable(Head),
    !,
    functor(Head, Name, Arity),
    untable(Name/Arity, M).
untable(TableSpec, _) :-
    '$type_error'(table_desclaration, TableSpec).

untable_reconsult(PI) :-
    print_message(informational, untable(PI)),
    untable(PI).

:- initialization
   prolog_listen(untable, untable_reconsult).


'$wrap_tabled'(Head, Options) :-
    get_dict(mode, Options, subsumptive),
    !,
    set_pattributes(Head, Options),
    '$wrap_predicate'(Head, table, Closure, Wrapped,
                      start_subsumptive_tabling(Closure, Head, Wrapped)).
'$wrap_tabled'(Head, Options) :-
    get_dict(subgoal_abstract, Options, _Abstract),
    !,
    set_pattributes(Head, Options),
    '$wrap_predicate'(Head, table, Closure, Wrapped,
                      start_abstract_tabling(Closure, Head, Wrapped)).
'$wrap_tabled'(Head, Options) :-
    !,
    set_pattributes(Head, Options),
    '$wrap_predicate'(Head, table, Closure, Wrapped,
                      start_tabling(Closure, Head, Wrapped)).

%!  set_pattributes(:Head, +Options) is det.
%
%   Set all tabling attributes for Head. These have been collected using
%   table_options/3 from the `:- table Head as (Attr1,...)` directive.

set_pattributes(Head, Options) :-
    '$set_predicate_attribute'(Head, tabled, true),
    (   tabled_attribute(Attr),
        get_dict(Attr, Options, Value),
        '$set_predicate_attribute'(Head, Attr, Value),
        fail
    ;   current_prolog_flag(table_monotonic, lazy),
        '$set_predicate_attribute'(Head, lazy, true),
        fail
    ;   true
    ).

tabled_attribute(incremental).
tabled_attribute(dynamic).
tabled_attribute(tshared).
tabled_attribute(max_answers).
tabled_attribute(subgoal_abstract).
tabled_attribute(answer_abstract).
tabled_attribute(monotonic).
tabled_attribute(opaque).
tabled_attribute(lazy).

%!  start_tabling(:Closure, :Wrapper, :Implementation)
%
%   Execute Implementation using tabling. This   predicate should not be
%   called directly. The table/1 directive  causes   a  predicate  to be
%   translated into a renamed implementation and a wrapper that involves
%   this predicate.
%
%   @arg Closure is the wrapper closure   to find the predicate quickly.
%   It is also allowed to pass nothing.   In that cases the predicate is
%   looked up using Wrapper.  We suggest to pass `0` in this case.
%
%   @compat This interface may change or disappear without notice
%           from future versions.

start_tabling(Closure, Wrapper, Worker) :-
    '$tbl_variant_table'(Closure, Wrapper, Trie, Status, Skeleton, IsMono),
    (   IsMono == true
    ->  shift(dependency(Skeleton, Trie, Mono)),
        (   Mono == true
        ->  tdebug(monotonic, 'Monotonic new answer: ~p', [Skeleton])
        ;   start_tabling_2(Closure, Wrapper, Worker, Trie, Status, Skeleton)
        )
    ;   start_tabling_2(Closure, Wrapper, Worker, Trie, Status, Skeleton)
    ).

start_tabling_2(Closure, Wrapper, Worker, Trie, Status, Skeleton) :-
    tdebug(deadlock, 'Got table ~p, status ~p', [Trie, Status]),
    (   Status == complete
    ->  trie_gen_compiled(Trie, Skeleton)
    ;   functor(Status, fresh, 2)
    ->  catch(create_table(Trie, Status, Skeleton, Wrapper, Worker),
              deadlock,
              restart_tabling(Closure, Wrapper, Worker))
    ;   Status == invalid
    ->  reeval(Trie, Wrapper, Skeleton)
    ;   % = run_follower, but never fresh and Status is a worklist
        shift_for_copy(call_info(Skeleton, Status))
    ).

create_table(Trie, Fresh, Skeleton, Wrapper, Worker) :-
    tdebug(Fresh = fresh(SCC, WorkList)),
    tdebug(wl_goal(WorkList, Goal, _)),
    tdebug(schedule, 'Created component ~d for ~p', [SCC, Goal]),
    setup_call_catcher_cleanup(
        '$idg_set_current'(OldCurrent, Trie),
        run_leader(Skeleton, Worker, Fresh, LStatus, Clause),
        Catcher,
        finished_leader(OldCurrent, Catcher, Fresh, Wrapper)),
    tdebug(schedule, 'Leader ~p done, status = ~p', [Goal, LStatus]),
    done_leader(LStatus, Fresh, Skeleton, Clause).

%!  restart_tabling(+Closure, +Wrapper, +Worker)
%
%   We were aborted due to a  deadlock.   Simply  retry. We sleep a very
%   tiny amount to give the thread against  which we have deadlocked the
%   opportunity to grab our table. Without, it is common that we re-grab
%   the table within our time slice  and   before  the kernel managed to
%   wakeup the other thread.

restart_tabling(Closure, Wrapper, Worker) :-
    tdebug(user_goal(Wrapper, Goal)),
    tdebug(deadlock, 'Deadlock running ~p; retrying', [Goal]),
    sleep(0.000001),
    start_tabling(Closure, Wrapper, Worker).

restart_abstract_tabling(Closure, Wrapper, Worker) :-
    tdebug(user_goal(Wrapper, Goal)),
    tdebug(deadlock, 'Deadlock running ~p; retrying', [Goal]),
    sleep(0.000001),
    start_abstract_tabling(Closure, Wrapper, Worker).

%!  start_subsumptive_tabling(:Closure, :Wrapper, :Implementation)
%
%   (*) We should __not__ use  trie_gen_compiled/2   here  as  this will
%   enumerate  all  answers  while  '$tbl_answer_update_dl'/2  uses  the
%   available trie indexing to only fetch the relevant answer(s).
%
%   @tbd  In  the  end  '$tbl_answer_update_dl'/2  is  problematic  with
%   incremental and shared tabling  as  we   do  not  get the consistent
%   update view from the compiled result.

start_subsumptive_tabling(Closure, Wrapper, Worker) :-
    (   '$tbl_existing_variant_table'(Closure, Wrapper, Trie, Status, Skeleton)
    ->  (   Status == complete
        ->  trie_gen_compiled(Trie, Skeleton)
        ;   Status == invalid
        ->  reeval(Trie, Wrapper, Skeleton),
            trie_gen_compiled(Trie, Skeleton)
        ;   shift_for_copy(call_info(Skeleton, Status))
        )
    ;   more_general_table(Wrapper, ATrie),
        '$tbl_table_status'(ATrie, complete, Wrapper, Skeleton)
    ->  '$tbl_answer_update_dl'(ATrie, Skeleton) % see (*)
    ;   more_general_table(Wrapper, ATrie),
        '$tbl_table_status'(ATrie, Status, GenWrapper, GenSkeleton)
    ->  (   Status == invalid
        ->  reeval(ATrie, GenWrapper, GenSkeleton),
            Wrapper = GenWrapper,
            '$tbl_answer_update_dl'(ATrie, GenSkeleton)
        ;   wrapper_skeleton(GenWrapper, GenSkeleton, Wrapper, Skeleton),
            shift_for_copy(call_info(GenSkeleton, Skeleton, Status)),
            unify_subsumptive(Skeleton, GenSkeleton)
        )
    ;   start_tabling(Closure, Wrapper, Worker)
    ).

%!  wrapper_skeleton(+GenWrapper, +GenSkeleton, +Wrapper, -Skeleton)
%
%   Skeleton is a specialized version of   GenSkeleton  for the subsumed
%   new consumer.

wrapper_skeleton(GenWrapper, GenSkeleton, Wrapper, Skeleton) :-
    copy_term(GenWrapper+GenSkeleton, Wrapper+Skeleton),
    tdebug(call_subsumption, 'GenSkeleton+Skeleton = ~p',
           [GenSkeleton+Skeleton]).

unify_subsumptive(X,X).

%!  start_abstract_tabling(:Closure, :Wrapper, :Worker)
%
%   Deal with ``table p/1 as  subgoal_abstract(N)``.   This  is  a merge
%   between  variant  and  subsumptive  tabling.  If  the  goal  is  not
%   abstracted this is simple variant tabling. If the goal is abstracted
%   we must solve the  more  general  goal   and  use  answers  from the
%   abstract table.
%
%   Wrapper is e.g., user:p(s(s(s(X))),Y)
%   Worker  is e.g., call(<closure>(p/2)(s(s(s(X))),Y))

start_abstract_tabling(Closure, Wrapper, Worker) :-
    '$tbl_abstract_table'(Closure, Wrapper, Trie, _Abstract, Status, Skeleton),
    tdebug(abstract, 'Wrapper=~p, Worker=~p, Skel=~p',
           [Wrapper, Worker, Skeleton]),
    (   is_most_general_term(Skeleton)           % TBD: Fill and test Abstract
    ->  start_tabling_2(Closure, Wrapper, Worker, Trie, Status, Skeleton)
    ;   Status == complete
    ->  '$tbl_answer_update_dl'(Trie, Skeleton)
    ;   functor(Status, fresh, 2)
    ->  '$tbl_table_status'(Trie, _, GenWrapper, GenSkeleton),
        abstract_worker(Worker, GenWrapper, GenWorker),
        catch(create_abstract_table(Trie, Status, Skeleton, GenSkeleton, GenWrapper,
                                    GenWorker),
              deadlock,
              restart_abstract_tabling(Closure, Wrapper, Worker))
    ;   Status == invalid
    ->  '$tbl_table_status'(Trie, _, GenWrapper, GenSkeleton),
        reeval(ATrie, GenWrapper, GenSkeleton),
        Wrapper = GenWrapper,
        '$tbl_answer_update_dl'(ATrie, Skeleton)
    ;   shift_for_copy(call_info(GenSkeleton, Skeleton, Status)),
        unify_subsumptive(Skeleton, GenSkeleton)
    ).

create_abstract_table(Trie, Fresh, Skeleton, GenSkeleton, Wrapper, Worker) :-
    tdebug(Fresh = fresh(SCC, WorkList)),
    tdebug(wl_goal(WorkList, Goal, _)),
    tdebug(schedule, 'Created component ~d for ~p', [SCC, Goal]),
    setup_call_catcher_cleanup(
        '$idg_set_current'(OldCurrent, Trie),
        run_leader(GenSkeleton, Worker, Fresh, LStatus, _Clause),
        Catcher,
        finished_leader(OldCurrent, Catcher, Fresh, Wrapper)),
    tdebug(schedule, 'Leader ~p done, status = ~p', [Goal, LStatus]),
    Skeleton = GenSkeleton,
    done_abstract_leader(LStatus, Fresh, GenSkeleton, Trie).

abstract_worker(_:call(Term), _M:GenWrapper, call(GenTerm)) :-
    functor(Term, Closure, _),
    GenWrapper =.. [_|Args],
    GenTerm =.. [Closure|Args].

:- '$hide'((done_abstract_leader/4)).

done_abstract_leader(complete, _Fresh, Skeleton, Trie) :-
    !,
    '$tbl_answer_update_dl'(Trie, Skeleton).
done_abstract_leader(final, fresh(SCC, _Worklist), Skeleton, Trie) :-
    !,
    '$tbl_free_component'(SCC),
    '$tbl_answer_update_dl'(Trie, Skeleton).
done_abstract_leader(_,_,_,_).

%!  done_leader(+Status, +Fresh, +Skeleton, -Clause)
%
%   Called on completion of a table. Possibly destroys the component and
%   generates the answers from the complete  table. The last cases deals
%   with leaders that are merged into a higher SCC (and thus no longer a
%   leader).

:- '$hide'((done_leader/4, finished_leader/4)).

done_leader(complete, _Fresh, Skeleton, Clause) :-
    !,
    trie_gen_compiled(Clause, Skeleton).
done_leader(final, fresh(SCC, _Worklist), Skeleton, Clause) :-
    !,
    '$tbl_free_component'(SCC),
    trie_gen_compiled(Clause, Skeleton).
done_leader(_,_,_,_).

finished_leader(OldCurrent, Catcher, Fresh, Wrapper) :-
    '$idg_set_current'(OldCurrent),
    (   Catcher == exit
    ->  true
    ;   Catcher == fail
    ->  true
    ;   Catcher = exception(_)
    ->  Fresh = fresh(SCC, _),
        '$tbl_table_discard_all'(SCC)
    ;   print_message(error, tabling(unexpected_result(Wrapper, Catcher)))
    ).

%!  run_leader(+Skeleton, +Worker, +Fresh, -Status, -Clause) is det.
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

run_leader(Skeleton, Worker, fresh(SCC, Worklist), Status, Clause) :-
    tdebug(wl_goal(Worklist, Goal, Skeleton)),
    tdebug(schedule, '-> Activate component ~p for ~p', [SCC, Goal]),
    activate(Skeleton, Worker, Worklist),
    tdebug(schedule, '-> Complete component ~p for ~p', [SCC, Goal]),
    completion(SCC, Status, Clause),
    tdebug(schedule, '-> Completed component ~p for ~p: ~p', [SCC, Goal, Status]),
    (   Status == merged
    ->  tdebug(merge, 'Turning leader ~p into follower', [Goal]),
        '$tbl_wkl_make_follower'(Worklist),
        shift_for_copy(call_info(Skeleton, Worklist))
    ;   true                                    % completed
    ).

activate(Skeleton, Worker, WorkList) :-
    tdebug(activate, '~p: created wl=~p', [Skeleton, WorkList]),
    (   reset_delays,
        delim(Skeleton, Worker, WorkList, []),
        fail
    ;   true
    ).

%!  delim(+Skeleton, +Worker, +WorkList, +Delays)
%
%   Call WorkList and  add  all  instances   of  Skeleton  as  answer to
%   WorkList, conditional according to Delays.
%
%   @arg Skeleton is the return skeleton (ret/N term)
%   @arg Worker is either the (wrapped) tabled goal or a _continuation_
%   @arg WorkList is the work list associated with Worker (or its
%        continuation).
%   @arg Delays is the current delay list.  Note that the actual delay
%        also include the internal global delay list.
%        '$tbl_wkl_add_answer'/4 joins the two.  For a dependency we
%        join the two explicitly.

delim(Skeleton, Worker, WorkList, Delays) :-
    reset(Worker, SourceCall, Continuation),
    tdebug(wl_goal(WorkList, Goal, _)),
    (   Continuation == 0
    ->  tdebug('$tbl_add_global_delays'(Delays, AllDelays)),
        tdebug(delay_goals(AllDelays, Cond)),
        tdebug(answer, 'New answer ~p for ~p (delays = ~p)',
               [Skeleton, Goal, Cond]),
        '$tbl_wkl_add_answer'(WorkList, Skeleton, Delays, Complete),
        Complete == !,
        !
    ;   SourceCall = call_info(SrcSkeleton, SourceWL)
    ->  '$tbl_add_global_delays'(Delays, AllDelays),
        tdebug(wl_goal(SourceWL, SrcGoal, _)),
        tdebug(wl_goal(WorkList, DstGoal, _)),
        tdebug(schedule, 'Suspended ~p, for solving ~p', [SrcGoal, DstGoal]),
        '$tbl_wkl_add_suspension'(
            SourceWL,
            dependency(SrcSkeleton, Continuation, Skeleton, WorkList, AllDelays))
    ;   SourceCall = call_info(SrcSkeleton, InstSkeleton, SourceWL)
    ->  '$tbl_add_global_delays'(Delays, AllDelays),
        tdebug(wl_goal(SourceWL, SrcGoal, _)),
        tdebug(wl_goal(WorkList, DstGoal, _)),
        tdebug(schedule, 'Suspended ~p, for solving ~p', [SrcGoal, DstGoal]),
        '$tbl_wkl_add_suspension'(
            SourceWL,
            InstSkeleton,
            dependency(SrcSkeleton, Continuation, Skeleton, WorkList, AllDelays))
    ;   '$tbl_wkl_table'(WorkList, ATrie),
        mon_assert_dep(SourceCall, Continuation, Skeleton, ATrie)
    ->  delim(Skeleton, Continuation, WorkList, Delays)
    ).

%!  start_moded_tabling(+Closure, :Wrapper, :Implementation, +Variant, +ModeArgs)
%
%   As start_tabling/2, but in addition separates the data stored in the
%   answer trie in the Variant and ModeArgs.

'$moded_wrap_tabled'(Head, Options, ModeTest, WrapperNoModes, ModeArgs) :-
    set_pattributes(Head, Options),
    '$wrap_predicate'(Head, table, Closure, Wrapped,
                      (   ModeTest,
                          start_moded_tabling(Closure, Head, Wrapped,
                                              WrapperNoModes, ModeArgs)
                      )).


start_moded_tabling(Closure, Wrapper, Worker, WrapperNoModes, ModeArgs) :-
    '$tbl_moded_variant_table'(Closure, WrapperNoModes, Trie,
                               Status, Skeleton, IsMono),
    (   IsMono == true
    ->  shift(dependency(Skeleton/ModeArgs, Trie, Mono)),
        (   Mono == true
        ->  tdebug(monotonic, 'Monotonic new answer: ~p', [Skeleton])
        ;   start_moded_tabling_2(Closure, Wrapper, Worker, ModeArgs,
                                  Trie, Status, Skeleton)
        )
    ;   start_moded_tabling_2(Closure, Wrapper, Worker, ModeArgs,
                              Trie, Status, Skeleton)
    ).

start_moded_tabling_2(_Closure, Wrapper, Worker, ModeArgs,
                      Trie, Status, Skeleton) :-
    (   Status == complete
    ->  moded_gen_answer(Trie, Skeleton, ModeArgs)
    ;   functor(Status, fresh, 2)
    ->  setup_call_catcher_cleanup(
            '$idg_set_current'(OldCurrent, Trie),
            moded_run_leader(Wrapper, Skeleton/ModeArgs,
                             Worker, Status, LStatus),
            Catcher,
            finished_leader(OldCurrent, Catcher, Status, Wrapper)),
        tdebug(schedule, 'Leader ~p done, modeargs = ~p, status = ~p',
               [Wrapper, ModeArgs, LStatus]),
        moded_done_leader(LStatus, Status, Skeleton, ModeArgs, Trie)
    ;   Status == invalid
    ->  reeval(Trie, Wrapper, Skeleton),
        moded_gen_answer(Trie, Skeleton, ModeArgs)
    ;   % = run_follower, but never fresh and Status is a worklist
        shift_for_copy(call_info(Skeleton/ModeArgs, Status))
    ).

:- public
    moded_gen_answer/3.                         % XSB tables.pl

moded_gen_answer(Trie, Skeleton, ModedArgs) :-
    trie_gen(Trie, Skeleton),
    '$tbl_answer_update_dl'(Trie, Skeleton, ModedArgs).

'$tbl_answer'(ATrie, Skeleton, ModedArgs, Delay) :-
    trie_gen(ATrie, Skeleton),
    '$tbl_answer_c'(ATrie, Skeleton, ModedArgs, Delay).

moded_done_leader(complete, _Fresh, Skeleton, ModeArgs, Trie) :-
    !,
    moded_gen_answer(Trie, Skeleton, ModeArgs).
moded_done_leader(final, fresh(SCC, _WorkList), Skeleton, ModeArgs, Trie) :-
    !,
    '$tbl_free_component'(SCC),
    moded_gen_answer(Trie, Skeleton, ModeArgs).
moded_done_leader(_, _, _, _, _).

moded_run_leader(Wrapper, SkeletonMA, Worker, fresh(SCC, Worklist), Status) :-
    tdebug(wl_goal(Worklist, Goal, _)),
    tdebug(schedule, '-> Activate component ~p for ~p', [SCC, Goal]),
    moded_activate(SkeletonMA, Worker, Worklist),
    tdebug(schedule, '-> Complete component ~p for ~p', [SCC, Goal]),
    completion(SCC, Status, _Clause),           % TBD: propagate
    tdebug(schedule, '-> Completed component ~p for ~p: ~p', [SCC, Goal, Status]),
    (   Status == merged
    ->  tdebug(merge, 'Turning leader ~p into follower', [Wrapper]),
        '$tbl_wkl_make_follower'(Worklist),
        shift_for_copy(call_info(SkeletonMA, Worklist))
    ;   true                                    % completed
    ).

moded_activate(SkeletonMA, Worker, WorkList) :-
    (   reset_delays,
        delim(SkeletonMA, Worker, WorkList, []),
        fail
    ;   true
    ).

%!  update(+Flags, +Head, +Module, +A1, +A2, -A3, -Action) is semidet.
%
%   Update the aggregated value  for  an   answer.  Iff  this  predicate
%   succeeds, the aggregated value is updated to   A3. If Del is unified
%   with `true`, A1 should be deleted.
%
%   @arg Flags is a bit mask telling which of A1 and A2 are unconditional
%   @arg Head is the head of the predicate
%   @arg Module is the module of the predicate
%   @arg A1 is the currently aggregated value
%   @arg A2 is the newly produced value
%   @arg Action is one of
%	 - `delete` to replace the old answer with the new
%	 - `keep`   to keep the old answer and add the new
%	 - `done`   to stop the update process

:- public
    update/7.

% both unconditional
update(0b11, Wrapper, M, Agg, New, Next, delete) :-
    !,
    M:'$table_update'(Wrapper, Agg, New, Next),
    Agg \=@= Next.
% old unconditional, new conditional
update(0b10, Wrapper, M, Agg, New, Next, keep) :-
    !,
    M:'$table_update'(Wrapper, Agg, New, Next0),
    (   Next0 =@= Agg
    ->  Next = Agg
    ;   Next = Next0
    ).
% old conditional, new unconditional,
update(0b01, Wrapper, M, Agg, New, Next, keep) :-
    !,
    M:'$table_update'(Wrapper, New, Agg, Next0),
    (   Next0 =@= Agg
    ->  Next = Agg
    ;   Next = Next0
    ).
% both conditional
update(0b00, _Wrapper, _M, _Agg, New, New, keep) :-
    !.

%!  completion(+Component, -Status, -Clause) is det.
%
%   Wakeup suspended goals until no new answers are generated. Status is
%   one of `merged`, `completed` or `final`.  If Status is not `merged`,
%   Clause is a compiled  representation  for   the  answer  trie of the
%   Component leader.

completion(SCC, Status, Clause) :-
    (   reset_delays,
        completion_(SCC),
        fail
    ;   '$tbl_table_complete_all'(SCC, Status, Clause),
        tdebug(schedule, 'SCC ~p: ~p', [scc(SCC), Status])
    ).

completion_(SCC) :-
    repeat,
    (   '$tbl_pop_worklist'(SCC, WorkList)
    ->  tdebug(wl_goal(WorkList, Goal, _)),
        tdebug(schedule, 'Complete ~p in ~p', [Goal, scc(SCC)]),
        completion_step(WorkList)
    ;   !
    ).

%!  '$tbl_wkl_work'(+WorkList,
%!                  -Answer,
%!                  -Continuation, -Wrapper, -TargetWorklist,
%!                  -Delays) is nondet.
%
%   True when Continuation needs to run with Answer and possible answers
%   need to be added to  TargetWorklist.   The  remaining  arguments are
%   there to restore variable bindings and restore the delay list.
%
%   The  suspension  added  by  '$tbl_wkl_add_suspension'/2  is  a  term
%   dependency(SrcWrapper,  Continuation,  Wrapper,  WorkList,  Delays).
%   Note that:
%
%     - Answer and Goal must be unified to rebind the _input_ arguments
%       for the continuation.
%     - Wrapper is stored in TargetWorklist on successful completion
%       of the Continuation.
%     - If Answer Subsumption is in effect, the story is a bit more
%       complex and ModeArgs provide the binding over which we do
%       _aggregation_. Otherwise, ModeArgs is the the
%       reserved trie node produced by '$tbl_trienode'/1.
%
%   @arg Answer is the answer term from the answer cluster (node in
%   the answer trie).  For answer subsumption it is a term Ret/ModeArgs
%   @arg Goal to Delays are extracted from the dependency/5 term in
%   the same order.

%!  completion_step(+Worklist) is fail.

completion_step(SourceWL) :-
    '$tbl_wkl_work'(SourceWL,
                    Answer, Continuation, TargetSkeleton, TargetWL, Delays),
    tdebug(wl_goal(SourceWL, SourceGoal, _)),
    tdebug(wl_goal(TargetWL, TargetGoal, _Skeleton)),
    tdebug('$tbl_add_global_delays'(Delays, AllDelays)),
    tdebug(delay_goals(AllDelays, Cond)),
    tdebug(schedule, 'Resuming ~p, calling ~p with ~p (delays = ~p)',
           [TargetGoal, SourceGoal, Answer, Cond]),
    delim(TargetSkeleton, Continuation, TargetWL, Delays),
    fail.


		 /*******************************
		 *     STRATIFIED NEGATION	*
		 *******************************/

%!  tnot(:Goal)
%
%   Tabled negation.
%
%   (*): Only variant tabling is allowed under tnot/1.

tnot(Goal0) :-
    '$tnot_implementation'(Goal0, Goal),        % verifies Goal is tabled
    (   '$tbl_existing_variant_table'(_, Goal, Trie, Status, Skeleton),
        Status \== invalid
    ->  '$idg_add_edge'(Trie),
        (   '$tbl_answer_dl'(Trie, _, true)
        ->  fail
        ;   '$tbl_answer_dl'(Trie, _, _)
        ->  tdebug(tnot, 'tnot: adding ~p to delay list', [Goal]),
            add_delay(Trie)
        ;   Status == complete
        ->  true
        ;   negation_suspend(Goal, Skeleton, Status)
        )
    ;   tdebug(tnot, 'tnot: ~p: fresh', [Goal]),
        (   '$wrapped_implementation'(Goal, table, Implementation), % see (*)
            functor(Implementation, Closure, _),
            start_tabling(Closure, Goal, Implementation),
            fail
        ;   '$tbl_existing_variant_table'(_, Goal, Trie, NewStatus, NewSkeleton),
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
    ).

floundering(Goal) :-
    format(string(Comment), 'Floundering goal in tnot/1: ~p', [Goal]),
    throw(error(instantiation_error, context(_Stack, Comment))).


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
    shift_for_copy(call_info(Skeleton, tnot(Worklist))),
    tdebug(tnot, 'negation resume ~p (wl=~p)', [Wrapper, Worklist]),
    '$tbl_wkl_is_false'(Worklist).

%!  not_exists(:P) is semidet.
%
%   Tabled negation for non-ground goals. This predicate uses the tabled
%   meta-predicate tabled_call/1. The tables  for xsb:tabled_call/1 must
%   be cleared if `the world changes' as   well  as to avoid aggregating
%   too many variants.

not_exists(Goal) :-
    ground(Goal),
    '$get_predicate_attribute'(Goal, tabled, 1),
    !,
    tnot(Goal).
not_exists(Goal) :-
    (   tabled_call(Goal), fail
    ;   tnot(tabled_call(Goal))
    ).

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
%   This  predicate  is  the  internal  version  of  call_delays/2  from
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
    ->  at_delay_goal(AT, M, G0, Answer, Moded),
        (   '$tbl_is_trienode'(Moded)
        ->  trie_term(AN, Answer)
        ;   true                        % TBD: Generated moded answer
        )
    ;   AN = Skeleton/ModeArgs
    ->  '$tbl_table_status'(AT, _, M1:GNoModes, Skeleton),
        M1:'$table_mode'(G0plain, GNoModes, ModeArgs),
        G0 = M1:G0plain
    ;   '$tbl_table_status'(AT, _, G0, AN)
    ),
    GN = G0,
    (   T == []
    ->  Goal = GN
    ;   Goal = (GN,GT),
        delay_goals(T, M, GT)
    ).
delay_goals([AT|T], M, Goal) :-
    atrie_goal(AT, G0),
    unqualify_goal(G0, M, G1),
    GN = tnot(G1),
    (   T == []
    ->  Goal = GN
    ;   Goal = (GN,GT),
        delay_goals(T, M, GT)
    ).

at_delay_goal(tnot(Trie), M, tnot(Goal), Skeleton, Moded) :-
    is_trie(Trie),
    !,
    at_delay_goal(Trie, M, Goal, Skeleton, Moded).
at_delay_goal(Trie, M, Goal, Skeleton, Moded) :-
    is_trie(Trie),
    !,
    '$tbl_table_status'(Trie, _Status, M2:Variant, Skeleton),
    M2:'$table_mode'(Goal0, Variant, Moded),
    unqualify_goal(M2:Goal0, M, Goal).

atrie_goal(Trie, M:Goal) :-
    '$tbl_table_status'(Trie, _Status, M:Variant, _Skeleton),
    M:'$table_mode'(Goal, Variant, _Moded).

unqualify_goal(M:Goal, M, Goal0) :-
    !,
    Goal0 = Goal.
unqualify_goal(Goal, _, Goal).


                 /*******************************
                 *            CLEANUP           *
                 *******************************/

%!  abolish_all_tables
%
%   Remove all tables. This is normally  used   to  free up the space or
%   recompute the result after predicates on   which the result for some
%   tabled predicates depend.
%
%   Abolishes both local and shared   tables. Possibly incomplete tables
%   are marked for destruction upon   completion.  The dependency graphs
%   for incremental and monotonic tabling are reclaimed as well.

abolish_all_tables :-
    (   '$tbl_abolish_local_tables'
    ->  true
    ;   true
    ),
    (   '$tbl_variant_table'(VariantTrie),
        trie_gen(VariantTrie, _, Trie),
        '$tbl_destroy_table'(Trie),
        fail
    ;   true
    ).

abolish_private_tables :-
    (   '$tbl_abolish_local_tables'
    ->  true
    ;   (   '$tbl_local_variant_table'(VariantTrie),
            trie_gen(VariantTrie, _, Trie),
            '$tbl_destroy_table'(Trie),
            fail
        ;   true
        )
    ).

abolish_shared_tables :-
    (   '$tbl_global_variant_table'(VariantTrie),
        trie_gen(VariantTrie, _, Trie),
        '$tbl_destroy_table'(Trie),
        fail
    ;   true
    ).

%!  abolish_table_subgoals(:Subgoal) is det.
%
%   Abolish all tables that unify with SubGoal.
%
%   @tbd: SubGoal must be callable.  Should we allow for more general
%   patterns?

abolish_table_subgoals(SubGoal0) :-
    '$tbl_implementation'(SubGoal0, M:SubGoal),
    !,
    '$must_be'(acyclic, SubGoal),
    (   '$tbl_variant_table'(VariantTrie),
        trie_gen(VariantTrie, M:SubGoal, Trie),
        '$tbl_destroy_table'(Trie),
        fail
    ;   true
    ).
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

%!  abolish_nonincremental_tables is det.
%
%   Abolish all tables that are not related to incremental predicates.

abolish_nonincremental_tables :-
    (   '$tbl_variant_table'(VariantTrie),
        trie_gen(VariantTrie, _, Trie),
        '$tbl_table_status'(Trie, Status, Goal, _),
        (   Status == complete
        ->  true
        ;   '$permission_error'(abolish, incomplete_table, Trie)
        ),
        \+ predicate_property(Goal, incremental),
        '$tbl_destroy_table'(Trie),
        fail
    ;   true
    ).

%!  abolish_nonincremental_tables(+Options)
%
%   Allow for skipping incomplete tables while abolishing.
%
%   @tbd Mark tables for destruction such   that they are abolished when
%   completed.

abolish_nonincremental_tables(Options) :-
    (   Options = on_incomplete(Action)
    ->  Action == skip
    ;   '$option'(on_incomplete(skip), Options)
    ),
    !,
    (   '$tbl_variant_table'(VariantTrie),
        trie_gen(VariantTrie, _, Trie),
        '$tbl_table_status'(Trie, complete, Goal, _),
        \+ predicate_property(Goal, incremental),
        '$tbl_destroy_table'(Trie),
        fail
    ;   true
    ).
abolish_nonincremental_tables(_) :-
    abolish_nonincremental_tables.


                 /*******************************
                 *        EXAMINE TABLES        *
                 *******************************/

%!  current_table(:Variant, -Trie) is nondet.
%
%   True when Trie is the answer table   for  Variant. If Variant has an
%   unbound module or goal, all  possible   answer  tries are generated,
%   otherwise Variant is considered a fully instantiated variant and the
%   predicate is semidet.

current_table(Variant, Trie) :-
    ct_generate(Variant),
    !,
    current_table_gen(Variant, Trie).
current_table(Variant, Trie) :-
    current_table_lookup(Variant, Trie),
    !.

current_table_gen(M:Variant, Trie) :-
    '$tbl_local_variant_table'(VariantTrie),
    trie_gen(VariantTrie, M:NonModed, Trie),
    M:'$table_mode'(Variant, NonModed, _Moded).
current_table_gen(M:Variant, Trie) :-
    '$tbl_global_variant_table'(VariantTrie),
    trie_gen(VariantTrie, M:NonModed, Trie),
    \+ '$tbl_table_status'(Trie, fresh), % shared tables are not destroyed
    M:'$table_mode'(Variant, NonModed, _Moded).

current_table_lookup(M:Variant, Trie) :-
    M:'$table_mode'(Variant, NonModed, _Moded),
    '$tbl_local_variant_table'(VariantTrie),
    trie_lookup(VariantTrie, M:NonModed, Trie).
current_table_lookup(M:Variant, Trie) :-
    M:'$table_mode'(Variant, NonModed, _Moded),
    '$tbl_global_variant_table'(VariantTrie),
    trie_lookup(VariantTrie, NonModed, Trie),
    \+ '$tbl_table_status'(Trie, fresh).

ct_generate(M:Variant) :-
    (   var(Variant)
    ->  true
    ;   var(M)
    ).

                 /*******************************
                 *      WRAPPER GENERATION      *
                 *******************************/

:- multifile
    system:term_expansion/2,
    tabled/2.
:- dynamic
    system:term_expansion/2.

wrappers(Spec, M) -->
    { tabling_defaults(
          [ (table_incremental=true)            - (incremental=true),
            (table_shared=true)                 - (tshared=true),
            (table_subsumptive=true)            - ((mode)=subsumptive),
            call(subgoal_size_restraint(Level)) - (subgoal_abstract=Level)
          ],
          #{}, Defaults)
    },
    wrappers(Spec, M, Defaults).

wrappers(Var, _, _) -->
    { var(Var),
      !,
      '$instantiation_error'(Var)
    }.
wrappers(M:Spec, _, Opts) -->
    !,
    { '$must_be'(atom, M) },
    wrappers(Spec, M, Opts).
wrappers(Spec as Options, M, Opts0) -->
    !,
    { table_options(Options, Opts0, Opts) },
    wrappers(Spec, M, Opts).
wrappers((A,B), M, Opts) -->
    !,
    wrappers(A, M, Opts),
    wrappers(B, M, Opts).
wrappers(Name//Arity, M, Opts) -->
    { atom(Name), integer(Arity), Arity >= 0,
      !,
      Arity1 is Arity+2
    },
    wrappers(Name/Arity1, M, Opts).
wrappers(Name/Arity, Module, Opts) -->
    { '$option'(mode(TMode), Opts, variant),
      atom(Name), integer(Arity), Arity >= 0,
      !,
      functor(Head, Name, Arity),
      '$tbl_trienode'(Reserved)
    },
    qualify(Module,
            [ '$tabled'(Head, TMode),
              '$table_mode'(Head, Head, Reserved)
            ]),
    [ (:- initialization('$wrap_tabled'(Module:Head, Opts), now))
    ].
wrappers(ModeDirectedSpec, Module, Opts) -->
    { '$option'(mode(TMode), Opts, variant),
      callable(ModeDirectedSpec),
      !,
      functor(ModeDirectedSpec, Name, Arity),
      functor(Head, Name, Arity),
      extract_modes(ModeDirectedSpec, Head, Variant, Modes, Moded),
      updater_clauses(Modes, Head, UpdateClauses),
      mode_check(Moded, ModeTest),
      (   ModeTest == true
      ->  WrapClause = '$wrap_tabled'(Module:Head, Opts),
          TVariant = Head
      ;   WrapClause = '$moded_wrap_tabled'(Module:Head, Opts, ModeTest,
                                            Module:Variant, Moded),
          TVariant = Variant
      )
    },
    qualify(Module,
            [ '$tabled'(Head, TMode),
              '$table_mode'(Head, TVariant, Moded)
            ]),
    [ (:- initialization(WrapClause, now))
    ],
    qualify(Module, UpdateClauses).
wrappers(TableSpec, _M, _Opts) -->
    { '$type_error'(table_desclaration, TableSpec)
    }.

qualify(Module, List) -->
    { prolog_load_context(module, Module) },
    !,
    clist(List).
qualify(Module, List) -->
    qlist(List, Module).

clist([])    --> [].
clist([H|T]) --> [H], clist(T).

qlist([], _)    --> [].
qlist([H|T], M) --> [M:H], qlist(T, M).


tabling_defaults([], Dict, Dict).
tabling_defaults([Condition-(Opt=Value)|T], Dict0, Dict) :-
    (   tabling_default(Condition)
    ->  Dict1 = Dict0.put(Opt,Value)
    ;   Dict1 = Dict0
    ),
    tabling_defaults(T, Dict1, Dict).

tabling_default(Flag=FValue) :-
    !,
    current_prolog_flag(Flag, FValue).
tabling_default(call(Term)) :-
    call(Term).

% Called from wrappers//2.

subgoal_size_restraint(Level) :-
    current_prolog_flag(max_table_subgoal_size_action, abstract),
    current_prolog_flag(max_table_subgoal_size, Level).

%!  table_options(+Options, +OptDictIn, -OptDictOut)
%
%   Handler the ... as _options_ ... construct.

table_options(Options, _Opts0, _Opts) :-
    var(Options),
    '$instantiation_error'(Options).
table_options((A,B), Opts0, Opts) :-
    !,
    table_options(A, Opts0, Opts1),
    table_options(B, Opts1, Opts).
table_options(subsumptive, Opts0, Opts1) :-
    !,
    put_dict(mode, Opts0, subsumptive, Opts1).
table_options(variant, Opts0, Opts1) :-
    !,
    put_dict(mode, Opts0, variant, Opts1).
table_options(incremental, Opts0, Opts1) :-
    !,
    put_dict(#{incremental:true,opaque:false}, Opts0, Opts1).
table_options(monotonic, Opts0, Opts1) :-
    !,
    put_dict(monotonic, Opts0, true, Opts1).
table_options(opaque, Opts0, Opts1) :-
    !,
    put_dict(#{incremental:false,opaque:true}, Opts0, Opts1).
table_options(lazy, Opts0, Opts1) :-
    !,
    put_dict(lazy, Opts0, true, Opts1).
table_options(dynamic, Opts0, Opts1) :-
    !,
    put_dict(dynamic, Opts0, true, Opts1).
table_options(shared, Opts0, Opts1) :-
    !,
    put_dict(tshared, Opts0, true, Opts1).
table_options(private, Opts0, Opts1) :-
    !,
    put_dict(tshared, Opts0, false, Opts1).
table_options(max_answers(Count), Opts0, Opts1) :-
    !,
    restraint(max_answers, Count, Opts0, Opts1).
table_options(subgoal_abstract(Size), Opts0, Opts1) :-
    !,
    restraint(subgoal_abstract, Size, Opts0, Opts1).
table_options(answer_abstract(Size), Opts0, Opts1) :-
    !,
    restraint(answer_abstract, Size, Opts0, Opts1).
table_options(Opt, _, _) :-
    '$domain_error'(table_option, Opt).

restraint(Name, Value0, Opts0, Opts) :-
    '$table_option'(Value0, Value),
    (   Value < 0
    ->  Opts = Opts0
    ;   put_dict(Name, Opts0, Value, Opts)
    ).


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
		 *      DYNAMIC PREDICATES	*
		 *******************************/

%!  '$set_table_wrappers'(:Head)
%
%   Clear/add wrappers and notifications to trap dynamic predicates.
%   This is required both for incremental and monotonic tabling.

'$set_table_wrappers'(Pred) :-
    (   '$get_predicate_attribute'(Pred, incremental, 1),
        \+ '$get_predicate_attribute'(Pred, opaque, 1)
    ->  wrap_incremental(Pred)
    ;   unwrap_incremental(Pred)
    ),
    (   '$get_predicate_attribute'(Pred, monotonic, 1)
    ->  wrap_monotonic(Pred)
    ;   unwrap_monotonic(Pred)
    ).

		 /*******************************
		 *       MONOTONIC TABLING	*
		 *******************************/

%!  mon_assert_dep(+Dependency, +Continuation, +Skel, +ATrie) is det.
%
%   Create a dependency for monotonic tabling.   Skel  and ATrie are the
%   target trie for solutions of Continuation.

mon_assert_dep(dependency(Dynamic), Cont, Skel, ATrie) :-
    '$idg_add_mono_dyn_dep'(Dynamic,
                            dependency(Dynamic, Cont, Skel),
                            ATrie).
mon_assert_dep(dependency(SrcSkel, SrcTrie, IsMono), Cont, Skel, ATrie) :-
    '$idg_add_monotonic_dep'(SrcTrie,
                             dependency(SrcSkel, IsMono, Cont, Skel),
                             ATrie).

%!  monotonic_affects(+SrcTrie, +SrcReturn, -IsMono,
%!                    -Continuation, -Return, -Atrie)
%
%   Dependency between two monotonic tables. If   SrcReturn  is added to
%   SrcTrie we must add all answers for Return of Continuation to Atrie.
%   IsMono shares with Continuation and is   used  in start_tabling/3 to
%   distinguish normal tabled call from propagation.

monotonic_affects(SrcTrie, SrcSkel, IsMono, Cont, Skel, ATrie) :-
    '$idg_mono_affects_eager'(SrcTrie, ATrie,
                              dependency(SrcSkel, IsMono, Cont, Skel)).

%!  monotonic_dyn_affects(:Head, -Continuation, -Return, -ATrie)
%
%   Dynamic predicate that maintains  the   dependency  from a monotonic

monotonic_dyn_affects(Head, Cont, Skel, ATrie) :-
    dyn_affected(Head, DTrie),
    '$idg_mono_affects_eager'(DTrie, ATrie,
                              dependency(Head, Cont, Skel)).

%!  wrap_monotonic(:Head)
%
%   Prepare the dynamic predicate Head for monotonic tabling. This traps
%   calls to build the dependency graph and updates to propagate answers
%   from new clauses through the dependency graph.

wrap_monotonic(Head) :-
    '$wrap_predicate'(Head, monotonic, _Closure, Wrapped,
                      '$start_monotonic'(Head, Wrapped)),
    '$pi_head'(PI, Head),
    prolog_listen(PI, monotonic_update).

%!  unwrap_monotonic(+Head)
%
%   Remove the monotonic wrappers and dependencies.

unwrap_monotonic(Head) :-
    '$pi_head'(PI, Head),
    (   unwrap_predicate(PI, monotonic)
    ->  prolog_unlisten(PI, monotonic_update)
    ;   true
    ).

%!  '$start_monotonic'(+Head, +Wrapped)
%
%   This is called the monotonic wrapper   around a dynamic predicate to
%   collect the dependencies  between  the   dynamic  predicate  and the
%   monotonic tabled predicates.

'$start_monotonic'(Head, Wrapped) :-
    (   '$tbl_collect_mono_dep'
    ->  shift(dependency(Head)),
        tdebug(monotonic, 'Cont in $start_dynamic/2 with ~p', [Head]),
        Wrapped,
        tdebug(monotonic, '  --> ~p', [Head])
    ;   Wrapped
    ).

%!  monotonic_update(+Action, +ClauseRef)
%
%   Trap changes to the monotonic dynamic predicate and forward them.

:- public monotonic_update/2.
monotonic_update(Action, ClauseRef) :-
    (   atomic(ClauseRef)                       % avoid retractall, start(_)
    ->  '$clause'(Head, _Body, ClauseRef, _Bindings),
        mon_propagate(Action, Head, ClauseRef)
    ;   true
    ).

%!  mon_propagate(+Action, +Head, +ClauseRef)
%
%   Handle changes to a dynamic predicate as part of monotonic
%   updates.

mon_propagate(Action, Head, ClauseRef) :-
    assert_action(Action),
    !,
    setup_call_cleanup(
        '$tbl_propagate_start'(Old),
        propagate_assert(Head),                 % eager monotonic dependencies
        '$tbl_propagate_end'(Old)),
    forall(dyn_affected(Head, ATrie),
           '$mono_idg_changed'(ATrie, ClauseRef)). % lazy monotonic dependencies
mon_propagate(retract, Head, _) :-
    !,
    mon_invalidate_dependents(Head).
mon_propagate(rollback(Action), Head, _) :-
    mon_propagate_rollback(Action, Head).

mon_propagate_rollback(Action, _Head) :-
    assert_action(Action),
    !.
mon_propagate_rollback(retract, Head) :-
    mon_invalidate_dependents(Head).

assert_action(asserta).
assert_action(assertz).

%!  propagate_assert(+Head) is det.
%
%   Propagate assertion of a dynamic clause with head Head.

propagate_assert(Head) :-
    tdebug(monotonic, 'Asserted ~p', [Head]),
    (   monotonic_dyn_affects(Head, Cont, Skel, ATrie),
        tdebug(monotonic, 'Propagating dyn ~p to ~p', [Head, ATrie]),
        '$idg_set_current'(_, ATrie),
        pdelim(Cont, Skel, ATrie),
        fail
    ;   true
    ).

%!  incr_propagate_assert(+Head) is det.
%
%   Propagate assertion of a dynamic clause with head Head, both
%   through eager and dynamic tables.

incr_propagate_assert(Head) :-
    tdebug(monotonic, 'New dynamic answer ~p', [Head]),
    (   dyn_affected(Head, DTrie),
         '$idg_mono_affects'(DTrie, ATrie,
                             dependency(Head, Cont, Skel)),
        tdebug(monotonic, 'Propagating dyn ~p to ~p', [Head, ATrie]),
        '$idg_set_current'(_, ATrie),
        pdelim(Cont, Skel, ATrie),
        fail
    ;   true
    ).


%!  propagate_answer(+SrcTrie, +SrcSkel) is det.
%
%   Propagate the new answer SrcSkel to the answer table SrcTrie.

propagate_answer(SrcTrie, SrcSkel) :-
    (   monotonic_affects(SrcTrie, SrcSkel, true, Cont, Skel, ATrie),
        tdebug(monotonic, 'Propagating tab ~p to ~p', [SrcTrie, ATrie]),
        pdelim(Cont, Skel, ATrie),
        fail
    ;   true
    ).

%!  pdelim(+Worker, +Skel, +ATrie)
%
%   Call Worker (a continuation) and add   each  binding it provides for
%   Skel  to  ATrie.  If  a  new  answer    is  added  to  ATrie,  using
%   propagate_answer/2 to propagate this further. Note   that we may hit
%   new dependencies and thus we need to run this using reset/3.
%
%   @tbd Not sure whether we need full   tabling  here. Need to think of
%   test cases.

pdelim(Worker, Skel, ATrie) :-
    reset(Worker, Dep, Cont),
    (   Cont == 0
    ->  '$tbl_monotonic_add_answer'(ATrie, Skel),
        propagate_answer(ATrie, Skel)
    ;   mon_assert_dep(Dep, Cont, Skel, ATrie),
        pdelim(Cont, Skel, ATrie)
    ).

%!  mon_invalidate_dependents(+Head)
%
%   A non-monotonic operation was done on Head. Invalidate all dependent
%   tables, preparing for normal incremental   reevaluation  on the next
%   cycle.

mon_invalidate_dependents(Head) :-
    tdebug(monotonic, 'Invalidate dependents for ~p', [Head]),
    forall(dyn_affected(Head, ATrie),
           '$idg_mono_invalidate'(ATrie)).

%!  abolish_monotonic_tables
%
%   Abolish all monotonic tables and the monotonic dependency relations.
%
%   @tbd: just prepare for incremental reevaluation?

abolish_monotonic_tables :-
    (   '$tbl_variant_table'(VariantTrie),
        trie_gen(VariantTrie, Goal, ATrie),
        '$get_predicate_attribute'(Goal, monotonic, 1),
        '$tbl_destroy_table'(ATrie),
        fail
    ;   true
    ).

		 /*******************************
		 *      INCREMENTAL TABLING	*
		 *******************************/

%!  wrap_incremental(:Head) is det.
%
%   Wrap an incremental dynamic predicate to be added to the IDG.

wrap_incremental(Head) :-
    tdebug(monotonic, 'Wrapping ~p', [Head]),
    abstract_goal(Head, Abstract),
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

%!  dyn_update(+Action, +Context) is det.
%
%   Track changes to added or removed clauses. We use '$clause'/4
%   because it works on erased clauses.
%
%   @tbd Add a '$clause_head'(-Head, +ClauseRef) to only decompile the
%   head.

:- public dyn_update/2, dyn_update/3.

dyn_update(_Action, ClauseRef) :-
    (   atomic(ClauseRef)                       % avoid retractall, start(_)
    ->  '$clause'(Head, _Body, ClauseRef, _Bindings),
        dyn_changed_pattern(Head)
    ;   true
    ).

dyn_update(Abstract, _, _) :-
    dyn_changed_pattern(Abstract).

dyn_changed_pattern(Term) :-
    forall(dyn_affected(Term, ATrie),
           '$idg_changed'(ATrie)).

dyn_affected(Term, ATrie) :-
    '$tbl_variant_table'(VTable),
    trie_gen(VTable, Term, ATrie).

%!  unwrap_incremental(:Head) is det.
%
%   Remove dynamic predicate incremenal forwarding,   reset the possible
%   `abstract` property and remove possible tables.

unwrap_incremental(Head) :-
    '$pi_head'(PI, Head),
    abstract_goal(Head, Abstract),
    (   Head == Abstract
    ->  prolog_unlisten(PI, dyn_update)
    ;   '$set_predicate_attribute'(Head, abstract, 0),
        prolog_unlisten(PI, dyn_update(_))
    ),
    (   '$tbl_variant_table'(VariantTrie)
    ->  forall(trie_gen(VariantTrie, Head, ATrie),
               '$tbl_destroy_table'(ATrie))
    ;   true
    ).

%!  reeval(+ATrie, :Goal, ?Return) is nondet.
%
%   Called  if  the   table   ATrie    is   out-of-date   (has  non-zero
%   _falsecount_). The answers of this predicate are the answers to Goal
%   after re-evaluating the answer trie.
%
%   This finds all dependency  paths  to   dynamic  predicates  and then
%   evaluates the nodes in a breath-first  fashion starting at the level
%   just above the dynamic predicates  and   moving  upwards.  Bottom up
%   evaluation is used to profit from upward propagation of not-modified
%   events that may cause the evaluation to stop early.
%
%   Note that false paths either end  in   a  dynamic node or a complete
%   node. The latter happens if we have and  IDG   "D  -> P -> Q" and we
%   first re-evaluate P for some reason.  Now   Q  can  still be invalid
%   after P has been re-evaluated.
%
%   @arg ATrie is the answer trie.  When shared tabling, we own this
%   trie.
%   @arg Goal is tabled goal (variant).  If we run into a deadlock we
%   need to call this.
%   @arg Return is the return skeleton. We must run
%   trie_gen_compiled(ATrie, Return) to enumerate the answers

reeval(ATrie, Goal, Return) :-
    catch(try_reeval(ATrie, Goal, Return), deadlock,
          retry_reeval(ATrie, Goal)).

retry_reeval(ATrie, Goal) :-
    '$tbl_reeval_abandon'(ATrie),
    tdebug(deadlock, 'Deadlock re-evaluating ~p; retrying', [ATrie]),
    sleep(0.000001),
    call(Goal).

try_reeval(ATrie, Goal, Return) :-
    nb_current('$tbl_reeval', true),
    !,
    tdebug(reeval, 'Nested re-evaluation for ~p', [ATrie]),
    do_reeval(ATrie, Goal, Return).
try_reeval(ATrie, Goal, Return) :-
    tdebug(reeval, 'Planning reeval for ~p', [ATrie]),
    findall(Path, false_path(ATrie, Path), Paths0),
    sort(0, @>, Paths0, Paths1),
    clean_paths(Paths1, Paths),
    tdebug(forall('$member'(Path, Paths),
                  tdebug(reeval, '  Re-eval complete path: ~p', [Path]))),
    reeval_paths(Paths, ATrie),
    do_reeval(ATrie, Goal, Return).

do_reeval(ATrie, Goal, Return) :-
    '$tbl_reeval_prepare_top'(ATrie, Clause),
    (   Clause == 0                          % complete and answer subsumption
    ->  '$tbl_table_status'(ATrie, _Status, M:Variant, Return),
        M:'$table_mode'(Goal0, Variant, ModeArgs),
        Goal = M:Goal0,
        moded_gen_answer(ATrie, Return, ModeArgs)
    ;   nonvar(Clause)                       % complete
    ->  trie_gen_compiled(Clause, Return)
    ;   call(Goal)                           % actually re-evaluate
    ).


%!  clean_paths(+PathsIn, -Paths)
%
%   Clean the reevaluation paths. Get rid of   the head term for ranking
%   and remove duplicate paths. Note that  a   Path  is a list of tries,
%   ground terms.

clean_paths([], []).
clean_paths([[_|Path]|T0], [Path|T]) :-
    clean_paths(T0, Path, T).

clean_paths([], _, []).
clean_paths([[_|CPath]|T0], CPath, T) :-
    !,
    clean_paths(T0, CPath, T).
clean_paths([[_|Path]|T0], _, [Path|T]) :-
    clean_paths(T0, Path, T).

%!  reeval_paths(+Paths, +Atrie)
%
%   Make Atrie valid again by re-evaluating nodes   in Paths. We stop as
%   soon as Atrie  is  valid  again.  Note   that  we  may  not  need to
%   reevaluate all paths because evaluating the   head  of some path may
%   include other nodes in an SCC, making them valid as well.

reeval_paths([], _) :-
    !.
reeval_paths(BottomUp, ATrie) :-
    is_invalid(ATrie),
    !,
    reeval_heads(BottomUp, ATrie, BottomUp1),
    tdebug(assertion(BottomUp \== BottomUp1)),
    '$list_to_set'(BottomUp1, BottomUp2),
    reeval_paths(BottomUp2, ATrie).
reeval_paths(_, _).

reeval_heads(_, ATrie, []) :-                % target is valid again
    \+ is_invalid(ATrie),
    !.
reeval_heads([], _, []).
reeval_heads([[H]|B], ATrie, BT) :-          % Last one of a falsepath
    reeval_node(H),
    !,
    reeval_heads(B, ATrie, BT).
reeval_heads([[H|T]|B], ATrie, [T|BT]) :-
    reeval_node(H),
    !,
    reeval_heads(B, ATrie, BT).
reeval_heads([FP|B], ATrie, [FP|BT]) :-
    reeval_heads(B, ATrie, BT).


%!  false_path(+Atrie, -Path) is nondet.
%
%   True when Path is a list of   invalid  tries (bottom up, ending with
%   ATrie).   The   last   element   of    the     list    is   a   term
%   `s(Rank,Length,ATrie)` that is used for sorting the paths.
%
%   If we find a table along the  way   that  is being worked on by some
%   other thread we wait for it.

false_path(ATrie, BottomUp) :-
    false_path(ATrie, Path, []),
    '$reverse'(Path, BottomUp).

false_path(ATrie, [ATrie|T], Seen) :-
    \+ memberchk(ATrie, Seen),
    '$idg_false_edge'(ATrie, Dep, Status),
    tdebug(reeval, '    ~p has dependent ~p (~w)', [ATrie, Dep, Status]),
    (   Status == invalid
    ->  (   false_path(Dep, T, [ATrie|Seen])
        ->  true
        ;   length(Seen, Len),               % invalid has no dependencies:
            T = [s(2, Len, [])]              % dynamic and tabled or explicitly
        )                                    % invalidated
    ;   status_rank(Status, Rank),
        length(Seen, Len),
        T = [s(Rank,Len,Dep)]
    ).

status_rank(dynamic,   2) :- !.
status_rank(monotonic, 2) :- !.
status_rank(complete,  1) :- !.
status_rank(Status,    Rank) :-
    var(Rank),
    !,
    format(user_error, 'Re-eval from status ~p~n', [Status]),
    Rank = 0.
status_rank(Rank,   Rank) :-
    format(user_error, 'Re-eval from rank ~p~n', [Rank]).

is_invalid(ATrie) :-
    '$idg_falsecount'(ATrie, FalseCount),
    FalseCount > 0.

%!  reeval_node(+ATrie) is semidet.
%
%   Re-evaluate the invalid answer trie ATrie.  Initially this created a
%   nested tabling environment, but this is dropped:
%
%     - It is possible for the re-evaluating variant to call into outer
%       non/not-yet incremental tables, requiring a merge with this
%       outer SCC.  This doesn't work well with a sub-environment.
%     - We do not need one.  If this environment is not merged into the
%       outer one it will complete before we continue.
%
%   Fails if the node is not ready for   evaluation. This is the case if
%   it is valid or it is a lazy table that has invalid dependencies.

reeval_node(ATrie) :-
    '$tbl_reeval_prepare'(ATrie, M:Variant),
    !,
    M:'$table_mode'(Goal0, Variant, _Moded),
    Goal = M:Goal0,
    tdebug(reeval, 'Re-evaluating ~p', [Goal]),
    (   '$idg_reset_current',
        setup_call_cleanup(
            nb_setval('$tbl_reeval', true),
            ignore(Goal),                    % assumes local scheduling
            nb_delete('$tbl_reeval')),
        fail
    ;   tdebug(reeval, 'Re-evaluated ~p', [Goal])
    ).
reeval_node(ATrie) :-
    '$mono_reeval_prepare'(ATrie, Size),
    !,
    reeval_monotonic_node(ATrie, Size).
reeval_node(ATrie) :-
    \+ is_invalid(ATrie).

reeval_monotonic_node(ATrie, Size) :-
    setup_call_cleanup(
        '$tbl_propagate_start'(Old),
        reeval_monotonic_node(ATrie, Size, Deps),
        '$tbl_propagate_end'(Old)),
    (   Deps == []
    ->  tdebug(reeval, 'Re-evaluation for ~p complete', [ATrie])
    ;   Deps == false
    ->  tdebug(reeval, 'Re-evaluation for ~p queued new answers', [ATrie]),
        reeval_node(ATrie)
    ;   tdebug(reeval, 'Re-evaluation for ~p: new invalid deps: ~p',
               [ATrie, Deps]),
        reeval_nodes(Deps),
        reeval_node(ATrie)
    ).

%!  reeval_nodes(+Nodes:list(trie)) is det.
%
%   After pulling in the monotonic answers  into   some  node, this is a
%   list if invalid dependencies.  We must revaluate these and then pull
%   in possible queued answers before we are done.

reeval_nodes([]).
reeval_nodes([H|T]) :-
    reeval_node(H),
    reeval_nodes(T).

reeval_monotonic_node(ATrie, Size, Deps) :-
    tdebug(reeval, 'Re-evaluating lazy monotonic ~p', [ATrie]),
    (   '$idg_mono_affects_lazy'(ATrie, _0SrcTrie, Dep, DepRef, Answers),
        length(Answers, Count),
        '$idg_mono_empty_queue'(DepRef, Count),
        (   Dep = dependency(Head, Cont, Skel)
        ->  (   '$member'(ClauseRef, Answers),
                '$clause'(Head, _Body, ClauseRef, _Bindings),
                tdebug(monotonic, 'Propagating ~p from ~p to ~p',
                       [Head, _0SrcTrie, ATrie]),
                '$idg_set_current'(_, ATrie),
                pdelim(Cont, Skel, ATrie),
                fail
            ;   true
            )
        ;   Dep = dependency(SrcSkel, true, Cont, Skel)
        ->  (   '$member'(Node, Answers),
                '$tbl_node_answer'(Node, SrcSkel),
                tdebug(monotonic, 'Propagating ~p from ~p to ~p',
                       [Skel, _0SrcTrie, ATrie]),
                '$idg_set_current'(_, ATrie),
                pdelim(Cont, Skel, ATrie),
                fail
            ;   true
            )
        ;   tdebug(monotonic, 'Skipped queued ~p, answers ~p',
                   [Dep, Answers])
        ),
        fail
    ;   '$mono_reeval_done'(ATrie, Size, Deps)
    ).


		 /*******************************
		 *      EXPAND DIRECTIVES	*
		 *******************************/

system:term_expansion((:- table(Preds)), Expansion) :-
    \+ current_prolog_flag(xref, true),
    prolog_load_context(module, M),
    phrase(wrappers(Preds, M), Clauses),
    multifile_decls(Clauses, Directives0),
    sort(Directives0, Directives),
    '$append'(Directives, Clauses, Expansion).

multifile_decls([], []).
multifile_decls([H0|T0], [H|T]) :-
    multifile_decl(H0, H),
    !,
    multifile_decls(T0, T).
multifile_decls([_|T0], T) :-
    multifile_decls(T0, T).

multifile_decl(M:(Head :- _Body), (:- multifile(M:Name/Arity))) :-
    !,
    functor(Head, Name, Arity).
multifile_decl(M:Head, (:- multifile(M:Name/Arity))) :-
    !,
    functor(Head, Name, Arity).
multifile_decl((Head :- _Body), (:- multifile(Name/Arity))) :-
    !,
    functor(Head, Name, Arity).
multifile_decl(Head, (:- multifile(Name/Arity))) :-
    !,
    Head \= (:-_),
    functor(Head, Name, Arity).


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
    '$tbl_variant_table'(VariantTrie),
    trie_gen(VariantTrie, G, Trie),
    is_most_general_term(Vars).

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


		 /*******************************
		 *            TRIPWIRES		*
		 *******************************/

%!  tripwire(+Wire, +Action, +Context)
%
%   Called from the tabling engine of some  tripwire is exceeded and the
%   situation  is  not  handled  internally   (such  as  `abstract`  and
%   `bounded_rationality`.

:- public tripwire/3.
:- multifile prolog:tripwire/2.

tripwire(Wire, _Action, Context) :-
    prolog:tripwire(Wire, Context),
    !.
tripwire(Wire, Action, Context) :-
    Error = error(resource_error(tripwire(Wire, Context)), _),
    tripwire_action(Action, Error).

tripwire_action(warning, Error) :-
    print_message(warning, Error).
tripwire_action(error, Error) :-
    throw(Error).
tripwire_action(suspend, Error) :-
    print_message(warning, Error),
    break.


		 /*******************************
		 *   SYSTEM TABLED PREDICATES	*
		 *******************************/

:- table
    system:undefined/0,
    system:answer_count_restraint/0,
    system:radial_restraint/0,
    system:tabled_call/1.

%!  undefined is undefined.
%
%   Expresses the value _bottom_ from the well founded semantics.

system:(undefined :-
    tnot(undefined)).

%!  answer_count_restraint is undefined.
%!  radial_restraint is undefined.
%
%   Similar  to  undefined/0,  providing  a   specific  _undefined_  for
%   restraint violations.

system:(answer_count_restraint :-
    tnot(answer_count_restraint)).

system:(radial_restraint :-
    tnot(radial_restraint)).

system:(tabled_call(X) :- call(X)).

