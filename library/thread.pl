/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2007-2020, University of Amsterdam
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

:- module(thread,
          [ concurrent/3,               % +Threads, :Goals, +Options
            concurrent_maplist/2,       % :Goal, +List
            concurrent_maplist/3,       % :Goal, ?List1, ?List2
            concurrent_maplist/4,       % :Goal, ?List1, ?List2, ?List3
            concurrent_forall/2,        % :Generate, :Test
            concurrent_forall/3,        % :Generate, :Test, +Options
            concurrent_and/2,           % :Generator,:Test
            concurrent_and/3,           % :Generator,:Test,+Options
            first_solution/3,           % -Var, :Goals, +Options

            call_in_thread/2            % +Thread, :Goal
          ]).
:- autoload(library(apply), [maplist/2, maplist/3, maplist/4, maplist/5]).
:- autoload(library(error), [must_be/2]).
:- autoload(library(lists), [subtract/3, same_length/2, nth0/3]).
:- autoload(library(option), [option/2, option/3]).
:- autoload(library(ordsets), [ord_intersection/3, ord_union/3]).
:- autoload(library(debug), [debug/3, assertion/1]).

%:- debug(concurrent).

:- meta_predicate
    concurrent(+, :, +),
    concurrent_maplist(1, +),
    concurrent_maplist(2, ?, ?),
    concurrent_maplist(3, ?, ?, ?),
    concurrent_forall(0, 0),
    concurrent_forall(0, 0, +),
    concurrent_and(0, 0),
    concurrent_and(0, 0, +),
    first_solution(-, :, +),
    call_in_thread(+, 0).


:- predicate_options(concurrent/3, 3,
                     [ pass_to(system:thread_create/3, 3)
                     ]).
:- predicate_options(concurrent_forall/3, 3,
                     [ threads(nonneg)
                     ]).
:- predicate_options(concurrent_and/3, 3,
                     [ threads(nonneg)
                     ]).
:- predicate_options(first_solution/3, 3,
                     [ on_fail(oneof([stop,continue])),
                       on_error(oneof([stop,continue])),
                       pass_to(system:thread_create/3, 3)
                     ]).

/** <module> High level thread primitives

This  module  defines  simple  to  use   predicates  for  running  goals
concurrently.  Where  the  core  multi-threaded    API  is  targeted  at
communicating long-living threads, the predicates   here  are defined to
run goals concurrently without having to   deal with thread creation and
maintenance explicitely.

Note that these predicates run goals   concurrently  and therefore these
goals need to be thread-safe. As  the   predicates  in  this module also
abort branches of the computation that  are no longer needed, predicates
that have side-effect must act properly.  In   a  nutshell, this has the
following consequences:

  * Nice clean Prolog code without side-effects (but with cut) works
    fine.
  * Side-effects are bad news.  If you really need assert to store
    intermediate results, use the thread_local/1 declaration.  This
    also guarantees cleanup of left-over clauses if the thread is
    cancelled.  For other side-effects, make sure to use call_cleanup/2
    to undo them should the thread be cancelled.
  * Global variables are ok as they are thread-local and destroyed
    on thread cancellation.  Note however that global variables in
    the calling thread are *not* available in the threads that are
    created.  You have to pass the value as an argument and initialise
    the variable in the new thread.
  * Thread-cancellation uses thread_signal/2.  Using this code
    with long-blocking foreign predicates may result in long delays,
    even if another thread asks for cancellation.

@author Jan Wielemaker
*/

%!  concurrent(+N, :Goals, +Options) is semidet.
%
%   Run Goals in parallel using N   threads.  This call blocks until
%   all work has been done.  The   Goals  must  be independent. They
%   should not communicate using shared  variables   or  any form of
%   global data. All Goals must be thread-safe.
%
%   Execution succeeds if all goals  have   succeeded.  If  one goal
%   fails or throws an exception,  other   workers  are abandoned as
%   soon as possible and the entire   computation fails or re-throws
%   the exception. Note that if  multiple   goals  fail  or raise an
%   error it is not defined which error or failure is reported.
%
%   On successful completion, variable bindings   are returned. Note
%   however that threads have independent   stacks and therefore the
%   goal is copied to the worker  thread   and  the result is copied
%   back to the caller of concurrent/3.
%
%   Choosing the right number of threads is not always obvious. Here
%   are some scenarios:
%
%     * If the goals are CPU intensive and normally all succeeding,
%     typically the number of CPUs is the optimal number of
%     threads.  Less does not use all CPUs, more wastes time in
%     context switches and also uses more memory.
%
%     * If the tasks are I/O bound the number of threads is
%     typically higher than the number of CPUs.
%
%     * If one or more of the goals may fail or produce an error,
%     using a higher number of threads may find this earlier.
%
%   @arg N Number of worker-threads to create. Using 1, no threads
%        are created.  If N is larger than the number of Goals we
%        create exactly as many threads as there are Goals.
%   @arg Goals List of callable terms.
%   @arg Options Passed to thread_create/3 for creating the
%        workers.  Only options changing the stack-sizes can
%        be used. In particular, do not pass the detached or alias
%        options.
%   @see In many cases, concurrent_maplist/2 and friends
%        is easier to program and is tractable to program
%        analysis.

concurrent(1, M:List, _) :-
    !,
    maplist(once_in_module(M), List).
concurrent(N, M:List, Options) :-
    must_be(positive_integer, N),
    must_be(list(callable), List),
    length(List, JobCount),
    message_queue_create(Done),
    message_queue_create(Queue),
    WorkerCount is min(N, JobCount),
    create_workers(WorkerCount, Queue, Done, Workers, Options),
    submit_goals(List, 1, M, Queue, VarList),
    forall(between(1, WorkerCount, _),
           thread_send_message(Queue, done)),
    VT =.. [vars|VarList],
    concur_wait(JobCount, Done, VT, cleanup(Workers, Queue),
                Result, [], Exitted),
    subtract(Workers, Exitted, RemainingWorkers),
    concur_cleanup(Result, RemainingWorkers, [Queue, Done]),
    (   Result == true
    ->  true
    ;   Result = false
    ->  fail
    ;   Result = exception(Error)
    ->  throw(Error)
    ).

once_in_module(M, Goal) :-
    call(M:Goal), !.

%!  submit_goals(+List, +Id0, +Module, +Queue, -Vars) is det.
%
%   Send all jobs from List to Queue. Each goal is added to Queue as
%   a term goal(Id, Goal, Vars). Vars  is   unified  with  a list of
%   lists of free variables appearing in each goal.

submit_goals([], _, _, _, []).
submit_goals([H|T], I, M, Queue, [Vars|VT]) :-
    term_variables(H, Vars),
    thread_send_message(Queue, goal(I, M:H, Vars)),
    I2 is I + 1,
    submit_goals(T, I2, M, Queue, VT).


%!  concur_wait(+N, +Done:queue, +VT:compound, +Cleanup,
%!              -Result, +Exitted0, -Exitted) is semidet.
%
%   Wait for completion, failure or error.
%
%   @arg Exited List of thread-ids with threads that completed
%   before all work was done.

concur_wait(0, _, _, _, true, Exited, Exited) :- !.
concur_wait(N, Done, VT, Cleanup, Status, Exitted0, Exitted) :-
    debug(concurrent, 'Concurrent: waiting for workers ...', []),
    catch(thread_get_message(Done, Exit), Error,
          concur_abort(Error, Cleanup, Done, Exitted0)),
    debug(concurrent, 'Waiting: received ~p', [Exit]),
    (   Exit = done(Id, Vars)
    ->  debug(concurrent, 'Concurrent: Job ~p completed with ~p', [Id, Vars]),
        arg(Id, VT, Vars),
        N2 is N - 1,
        concur_wait(N2, Done, VT, Cleanup, Status, Exitted0, Exitted)
    ;   Exit = finished(Thread)
    ->  thread_join(Thread, JoinStatus),
        debug(concurrent, 'Concurrent: waiter ~p joined: ~p',
              [Thread, JoinStatus]),
        (   JoinStatus == true
        ->  concur_wait(N, Done, VT, Cleanup, Status, [Thread|Exitted0], Exitted)
        ;   Status = JoinStatus,
            Exitted = [Thread|Exitted0]
        )
    ).

concur_abort(Error, cleanup(Workers, Queue), Done, Exitted) :-
    debug(concurrent, 'Concurrent: got ~p', [Error]),
    subtract(Workers, Exitted, RemainingWorkers),
    concur_cleanup(Error, RemainingWorkers, [Queue, Done]),
    throw(Error).

create_workers(N, Queue, Done, [Id|Ids], Options) :-
    N > 0,
    !,
    thread_create(worker(Queue, Done), Id,
                  [ at_exit(thread_send_message(Done, finished(Id)))
                  | Options
                  ]),
    N2 is N - 1,
    create_workers(N2, Queue, Done, Ids, Options).
create_workers(_, _, _, [], _).


%!  worker(+WorkQueue, +DoneQueue) is det.
%
%   Process jobs from WorkQueue and send the results to DoneQueue.

worker(Queue, Done) :-
    thread_get_message(Queue, Message),
    debug(concurrent, 'Worker: received ~p', [Message]),
    (   Message = goal(Id, Goal, Vars)
    ->  (   Goal
        ->  thread_send_message(Done, done(Id, Vars)),
            worker(Queue, Done)
        )
    ;   true
    ).


%!  concur_cleanup(+Result, +Workers:list, +Queues:list) is det.
%
%   Cleanup the concurrent workers and message  queues. If Result is
%   not =true=, signal all workers to make them stop prematurely. If
%   result is true we assume  all   workers  have been instructed to
%   stop or have stopped themselves.

concur_cleanup(Result, Workers, Queues) :-
    !,
    (   Result == true
    ->  true
    ;   kill_workers(Workers)
    ),
    join_all(Workers),
    maplist(message_queue_destroy, Queues).

kill_workers([]).
kill_workers([Id|T]) :-
    debug(concurrent, 'Signalling ~w', [Id]),
    catch(thread_signal(Id, abort), _, true),
    kill_workers(T).

join_all([]).
join_all([Id|T]) :-
    thread_join(Id, _),
    join_all(T).


		 /*******************************
		 *             FORALL		*
		 *******************************/

%!  concurrent_forall(:Generate, :Action) is semidet.
%!  concurrent_forall(:Generate, :Action, +Options) is semidet.
%
%   True when Action is true for all solutions of Generate. This has the
%   same semantics as forall/2, but  the   Action  goals are executed in
%   multiple threads. Notable a failing Action   or a Action throwing an
%   exception signals the calling  thread  which   in  turn  aborts  all
%   workers and fails or re-throws the generated error. Options:
%
%     - threads(+Count)
%       Number of threads to use.  The default is determined by the
%       Prolog flag `cpu_count`.
%
%   @tbd Ideally we would grow the   set of workers dynamically, similar
%   to dynamic scheduling of  HTTP  worker   threads.  This  would avoid
%   creating threads that are never used if Generate is too slow or does
%   not provide enough answers and  would   further  raise the number of
%   threads if Action is I/O bound rather than CPU bound.

:- dynamic
    fa_aborted/1.

concurrent_forall(Generate, Test) :-
    concurrent_forall(Generate, Test, []).

concurrent_forall(Generate, Test, Options) :-
    jobs(Jobs, Options),
    Jobs > 1,
    !,
    term_variables(Generate, GVars),
    term_variables(Test, TVars),
    sort(GVars, GVarsS),
    sort(TVars, TVarsS),
    ord_intersection(GVarsS, TVarsS, Shared),
    Templ =.. [v|Shared],
    MaxSize is Jobs*4,
    message_queue_create(Q, [max_size(MaxSize)]),
    length(Workers, Jobs),
    thread_self(Me),
    maplist(thread_create(fa_worker(Q, Me, Templ, Test)), Workers),
    catch(( forall(Generate,
                   thread_send_message(Q, job(Templ))),
            forall(between(1, Jobs, _),
                   thread_send_message(Q, done)),
            maplist(thread_join, Workers),
            message_queue_destroy(Q)
          ),
          Error,
          fa_cleanup(Error, Workers, Q)).
concurrent_forall(Generate, Test, _) :-
    forall(Generate, Test).

fa_cleanup(Error, Workers, Q) :-
    maplist(safe_abort, Workers),
    debug(concurrent(fail), 'Joining workers', []),
    maplist(safe_join, Workers),
    debug(concurrent(fail), 'Destroying queue', []),
    retractall(fa_aborted(Q)),
    message_queue_destroy(Q),
    (   Error = fa_worker_failed(_0Test, Why)
    ->  debug(concurrent(fail), 'Test ~p failed: ~p', [_0Test, Why]),
        (   Why == false
        ->  fail
        ;   Why = error(E)
        ->  throw(E)
        ;   assertion(fail)
        )
    ;   throw(Error)
    ).

fa_worker(Queue, Main, Templ, Test) :-
    repeat,
    thread_get_message(Queue, Msg),
    (   Msg == done
    ->  !
    ;   Msg = job(Templ),
        debug(concurrent, 'Running test ~p', [Test]),
        (   catch_with_backtrace(Test, E, true)
        ->  (   var(E)
            ->  fail
            ;   fa_stop(Queue, Main, fa_worker_failed(Test, error(E)))
            )
        ;   !,
            fa_stop(Queue, Main, fa_worker_failed(Test, false))
        )
    ).

fa_stop(Queue, Main, Why) :-
    with_mutex('$concurrent_forall',
               fa_stop_sync(Queue, Main, Why)).

fa_stop_sync(Queue, _Main, _Why) :-
    fa_aborted(Queue),
    !.
fa_stop_sync(Queue, Main, Why) :-
    asserta(fa_aborted(Queue)),
    debug(concurrent(fail), 'Stop due to ~p. Signalling ~q', [Why, Main]),
    thread_signal(Main, throw(Why)).

jobs(Jobs, Options) :-
    (   option(threads(Jobs), Options)
    ->  true
    ;   current_prolog_flag(cpu_count, Jobs)
    ->  true
    ;   Jobs = 1
    ).

safe_abort(Thread) :-
    catch(thread_signal(Thread, abort), error(_,_), true).
safe_join(Thread) :-
    E = error(_,_),
    catch(thread_join(Thread, _Status), E, true).


		 /*******************************
		 *              AND		*
		 *******************************/

%!  concurrent_and(:Generator, :Test).
%!  concurrent_and(:Generator, :Test, +Options).
%
%   Concurrent version of `(Generator,Test)`. This   predicate creates a
%   thread providing solutions for Generator that   are handed to a pool
%   of threads that run Test for   the different instantiations provided
%   by Generator concurrently. The predicate  is logically equivalent to
%   a simple conjunction except for two  aspects: (1) terms are _copied_
%   from Generator to the test  Test   threads  while answers are copied
%   back to the calling thread and (2)   answers  may be produced out of
%   order.
%
%   If   the   evaluation   of   some    Test   raises   an   exception,
%   concurrent_and/2,3 is terminated with this  exception. If the caller
%   commits  after  a  given  answer  or    raises  an  exception  while
%   concurrent_and/2,3  is  active  with  pending   choice  points,  all
%   involved resources are reclaimed.
%
%   Options:
%
%     - threads(+Count)
%       Create a worker pool holding Count threads.  The default is
%       the Prolog flag `cpu_count`.
%
%   This    predicate    was    proposed     by      Jan     Burse    as
%   balance((Generator,Test)).

concurrent_and(Gen, Test) :-
    concurrent_and(Gen, Test, []).

concurrent_and(Gen, Test, Options) :-
    jobs(Jobs, Options),
    MaxSize is Jobs*4,
    message_queue_create(JobQueue, [max_size(MaxSize)]),
    message_queue_create(AnswerQueue, [max_size(MaxSize)]),
    ca_template(Gen, Test, Templ),
    term_variables(Gen+Test, AllVars),
    ReplyTempl =.. [v|AllVars],
    length(Workers, Jobs),
    Alive is 1<<Jobs-1,
    maplist(thread_create(ca_worker(JobQueue, AnswerQueue,
                                    Templ, Test, ReplyTempl)),
            Workers),
    thread_create(ca_generator(Gen, Templ, JobQueue, AnswerQueue),
                  GenThread),
    State = state(Alive),
    call_cleanup(
        ca_gather(State, AnswerQueue, ReplyTempl, Workers),
        ca_cleanup(GenThread, Workers, JobQueue, AnswerQueue)).

ca_gather(State, AnswerQueue, ReplyTempl, Workers) :-
    repeat,
       thread_get_message(AnswerQueue, Msg),
       (   Msg = true(ReplyTempl)
       ->  true
       ;   Msg = done(Worker)
       ->  nth0(Done, Workers, Worker),
           arg(1, State, Alive0),
           Alive1 is Alive0 /\ \(1<<Done),
           debug(concurrent(and), 'Alive = ~2r', [Alive1]),
           (   Alive1 =:= 0
           ->  !,
               fail
           ;   nb_setarg(1, State, Alive1),
               fail
           )
       ;   Msg = error(E)
       ->  throw(E)
       ).

ca_template(Gen, Test, Templ) :-
    term_variables(Gen,  GVars),
    term_variables(Test, TVars),
    sort(GVars, GVarsS),
    sort(TVars, TVarsS),
    ord_intersection(GVarsS, TVarsS, Shared),
    ord_union(GVarsS, Shared, TemplVars),
    Templ =.. [v|TemplVars].

ca_worker(JobQueue, AnswerQueue, Templ, Test, ReplyTempl) :-
    thread_self(Me),
    EG = error(existence_error(message_queue, _), _),
    repeat,
    catch(thread_get_message(JobQueue, Req), EG, Req=all_done),
    (   Req = job(Templ)
    ->  (   catch(Test, E, true),
            (   var(E)
            ->  thread_send_message(AnswerQueue, true(ReplyTempl))
            ;   thread_send_message(AnswerQueue, error(E))
            ),
            fail
        )
    ;   Req == done
    ->  !,
        message_queue_destroy(JobQueue),
        thread_send_message(AnswerQueue, done(Me))
    ;   assertion(Req == all_done)
    ->  !,
        thread_send_message(AnswerQueue, done(Me))
    ).

ca_generator(Gen, Templ, JobQueue, AnswerQueue) :-
    (   catch(Gen, E, true),
        (   var(E)
        ->  thread_send_message(JobQueue, job(Templ))
        ;   thread_send_message(AnswerQueue, error(E))
        ),
        fail
    ;   thread_send_message(JobQueue, done)
    ).

ca_cleanup(GenThread, Workers, JobQueue, AnswerQueue) :-
    safe_abort(GenThread),
    safe_join(GenThread),
    maplist(safe_abort, Workers),
    maplist(safe_join, Workers),
    message_queue_destroy(AnswerQueue),
    catch(message_queue_destroy(JobQueue), error(_,_), true).


                 /*******************************
                 *             MAPLIST          *
                 *******************************/

%!  concurrent_maplist(:Goal, +List) is semidet.
%!  concurrent_maplist(:Goal, +List1, +List2) is semidet.
%!  concurrent_maplist(:Goal, +List1, +List2, +List3) is semidet.
%
%   Concurrent version of maplist/2. This   predicate uses concurrent/3,
%   using multiple _worker_ threads.  The  number   of  threads  is  the
%   minimum of the list length and the   number  of cores available. The
%   number of cores is determined using  the prolog flag =cpu_count=. If
%   this flag is absent or 1 or List   has  less than two elements, this
%   predicate calls the corresponding maplist/N  version using a wrapper
%   based on once/1. Note that all goals   are executed as if wrapped in
%   once/1 and therefore these predicates are _semidet_.
%
%   Note that the the overhead  of   this  predicate is considerable and
%   therefore Goal must  be  fairly  expensive   before  one  reaches  a
%   speedup.

concurrent_maplist(Goal, List) :-
    workers(List, WorkerCount),
    !,
    maplist(ml_goal(Goal), List, Goals),
    concurrent(WorkerCount, Goals, []).
concurrent_maplist(M:Goal, List) :-
    maplist(once_in_module(M, Goal), List).

once_in_module(M, Goal, Arg) :-
    call(M:Goal, Arg), !.

ml_goal(Goal, Elem, call(Goal, Elem)).

concurrent_maplist(Goal, List1, List2) :-
    same_length(List1, List2),
    workers(List1, WorkerCount),
    !,
    maplist(ml_goal(Goal), List1, List2, Goals),
    concurrent(WorkerCount, Goals, []).
concurrent_maplist(M:Goal, List1, List2) :-
    maplist(once_in_module(M, Goal), List1, List2).

once_in_module(M, Goal, Arg1, Arg2) :-
    call(M:Goal, Arg1, Arg2), !.

ml_goal(Goal, Elem1, Elem2, call(Goal, Elem1, Elem2)).

concurrent_maplist(Goal, List1, List2, List3) :-
    same_length(List1, List2, List3),
    workers(List1, WorkerCount),
    !,
    maplist(ml_goal(Goal), List1, List2, List3, Goals),
    concurrent(WorkerCount, Goals, []).
concurrent_maplist(M:Goal, List1, List2, List3) :-
    maplist(once_in_module(M, Goal), List1, List2, List3).

once_in_module(M, Goal, Arg1, Arg2, Arg3) :-
    call(M:Goal, Arg1, Arg2, Arg3), !.

ml_goal(Goal, Elem1, Elem2, Elem3, call(Goal, Elem1, Elem2, Elem3)).

workers(List, Count) :-
    current_prolog_flag(cpu_count, Cores),
    Cores > 1,
    length(List, Len),
    Count is min(Cores,Len),
    Count > 1,
    !.

same_length([], [], []).
same_length([_|T1], [_|T2], [_|T3]) :-
    same_length(T1, T2, T3).


                 /*******************************
                 *             FIRST            *
                 *******************************/

%!  first_solution(-X, :Goals, +Options) is semidet.
%
%   Try  alternative  solvers  concurrently,   returning  the  first
%   answer. In a typical scenario, solving any of the goals in Goals
%   is satisfactory for the application to  continue. As soon as one
%   of the tried alternatives is  successful,   all  the others are
%   killed and first_solution/3 succeeds.
%
%   For example, if it is unclear whether   it is better to search a
%   graph breadth-first or depth-first we can use:
%
%   ==
%   search_graph(Grap, Path) :-
%            first_solution(Path, [ breadth_first(Graph, Path),
%                                   depth_first(Graph, Path)
%                                 ],
%                           []).
%   ==
%
%   Options include thread stack-sizes passed   to thread_create, as
%   well as the options =on_fail= and   =on_error= that specify what
%   to do if a  solver  fails  or   triggers  an  error.  By default
%   execution of all  solvers  is  terminated   and  the  result  is
%   returned. Sometimes one may wish to  continue. One such scenario
%   is if one of the solvers may run  out of resources or one of the
%   solvers is known to be incomplete.
%
%           * on_fail(Action)
%           If =stop= (default), terminate all threads and stop with
%           the failure.  If =continue=, keep waiting.
%           * on_error(Action)
%           As above, re-throwing the error if an error appears.
%
%   @bug    first_solution/3 cannot deal with non-determinism.  There
%           is no obvious way to fit non-determinism into it.  If multiple
%           solutions are needed wrap the solvers in findall/3.


first_solution(X, M:List, Options) :-
    message_queue_create(Done),
    thread_options(Options, ThreadOptions, RestOptions),
    length(List, JobCount),
    create_solvers(List, M, X, Done, Solvers, ThreadOptions),
    wait_for_one(JobCount, Done, Result, RestOptions),
    concur_cleanup(kill, Solvers, [Done]),
    (   Result = done(_, Var)
    ->  X = Var
    ;   Result = error(_, Error)
    ->  throw(Error)
    ).

create_solvers([], _, _, _, [], _).
create_solvers([H|T], M, X, Done, [Id|IDs], Options) :-
    thread_create(solve(M:H, X, Done), Id, Options),
    create_solvers(T, M, X, Done, IDs, Options).

solve(Goal, Var, Queue) :-
    thread_self(Me),
    (   catch(Goal, E, true)
    ->  (   var(E)
        ->  thread_send_message(Queue, done(Me, Var))
        ;   thread_send_message(Queue, error(Me, E))
        )
    ;   thread_send_message(Queue, failed(Me))
    ).

wait_for_one(0, _, failed, _) :- !.
wait_for_one(JobCount, Queue, Result, Options) :-
    thread_get_message(Queue, Msg),
    LeftCount is JobCount - 1,
    (   Msg = done(_, _)
    ->  Result = Msg
    ;   Msg = failed(_)
    ->  (   option(on_fail(stop), Options, stop)
        ->  Result = Msg
        ;   wait_for_one(LeftCount, Queue, Result, Options)
        )
    ;   Msg = error(_, _)
    ->  (   option(on_error(stop), Options, stop)
        ->  Result = Msg
        ;   wait_for_one(LeftCount, Queue, Result, Options)
        )
    ).


%!  thread_options(+Options, -ThreadOptions, -RestOptions) is det.
%
%   Split the option  list  over   thread(-size)  options  and other
%   options.

thread_options([], [], []).
thread_options([H|T], [H|Th], O) :-
    thread_option(H),
    !,
    thread_options(T, Th, O).
thread_options([H|T], Th, [H|O]) :-
    thread_options(T, Th, O).

thread_option(local(_)).
thread_option(global(_)).
thread_option(trail(_)).
thread_option(argument(_)).
thread_option(stack(_)).


%!  call_in_thread(+Thread, :Goal) is semidet.
%
%   Run Goal as an interrupt in the context  of Thread. This is based on
%   thread_signal/2. If waiting times  out,   we  inject  a stop(Reason)
%   exception into Goal. Interrupts can be   nested, i.e., it is allowed
%   to run a call_in_thread/2 while the target thread is processing such
%   an interrupt.
%
%   This predicate is primarily intended   for  debugging and inspection
%   tasks.

call_in_thread(Thread, Goal) :-
    thread_self(Thread),
    !,
    once(Goal).
call_in_thread(Thread, Goal) :-
    term_variables(Goal, Vars),
    thread_self(Me),
    A is random(1 000 000 000),
    thread_signal(Thread, run_in_thread(Goal,Vars,A,Me)),
    catch(thread_get_message(in_thread(A,Result)),
          Error,
          forward_exception(Thread, A, Error)),
    (   Result = true(Vars)
    ->  true
    ;   Result = error(Error)
    ->  throw(Error)
    ;   fail
    ).

run_in_thread(Goal, Vars, Id, Sender) :-
    (   catch_with_backtrace(call(Goal), Error, true)
    ->  (   var(Error)
        ->  thread_send_message(Sender, in_thread(Id, true(Vars)))
        ;   Error = stop(_)
        ->  true
        ;   thread_send_message(Sender, in_thread(Id, error(Error)))
        )
    ;   thread_send_message(Sender, in_thread(Id, false))
    ).

forward_exception(Thread, Id, Error) :-
    kill_with(Error, Kill),
    thread_signal(Thread, kill_task(Id, Kill)),
    throw(Error).

kill_with(time_limit_exceeded, stop(time_limit_exceeded)) :-
    !.
kill_with(_, stop(interrupt)).

kill_task(Id, Exception) :-
    prolog_current_frame(Frame),
    prolog_frame_attribute(Frame, parent_goal,
                           run_in_thread(_Goal, _Vars, Id, _Sender)),
    !,
    throw(Exception).
kill_task(_, _).
