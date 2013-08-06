/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2002-2013, University of Amsterdam
			      VU University Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(thread,
	  [ concurrent/3,		% +Threads, :Goals, +Options
	    concurrent_maplist/2,	% :Goal, +List
	    concurrent_maplist/3,	% :Goal, ?List1, ?List2
	    concurrent_maplist/4,	% :Goal, ?List1, ?List2, ?List3
	    first_solution/3		% -Var, :Goals, +Options
	  ]).
:- use_module(library(debug)).
:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(library(apply)).

%:- debug(concurrent).

:- meta_predicate
	concurrent(+, :, +),
	concurrent_maplist(1, +),
	concurrent_maplist(2, ?, ?),
	concurrent_maplist(3, ?, ?, ?),
	first_solution(-, :, +).

:- predicate_options(concurrent/3, 3,
		     [ pass_to(system:thread_create/3, 3)
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

@author	Jan Wielemaker
*/

%%	concurrent(+N, :Goals, Options) is semidet.
%
%	Run Goals in parallel using N   threads.  This call blocks until
%	all work has been done.  The   Goals  must  be independent. They
%	should not communicate using shared  variables   or  any form of
%	global data. All Goals must be thread-safe.
%
%	Execution succeeds if all goals  have   succeeded.  If  one goal
%	fails or throws an exception,  other   workers  are abandoned as
%	soon as possible and the entire   computation fails or re-throws
%	the exception. Note that if  multiple   goals  fail  or raise an
%	error it is not defined which error or failure is reported.
%
%	On successful completion, variable bindings   are returned. Note
%	however that threads have independent   stacks and therefore the
%	goal is copied to the worker  thread   and  the result is copied
%	back to the caller of concurrent/3.
%
%	Choosing the right number of threads is not always obvious. Here
%	are some scenarios:
%
%	  * If the goals are CPU intensive and normally all succeeding,
%	  typically the number of CPUs is the optimal number of
%	  threads.  Less does not use all CPUs, more wastes time in
%	  context switches and also uses more memory.
%
%	  * If the tasks are I/O bound the number of threads is
%	  typically higher than the number of CPUs.
%
%	  * If one or more of the goals may fail or produce an errors,
%	  using a higher number of threads may find this earlier.
%
%	@param N Number of worker-threads to create. Using 1, no threads
%	       are created.  If N is larger than the number of Goals we
%	       create exactly as many threads as there are Goals.
%	@param Goals List of callable terms.
%	@param Options Passed to thread_create/3 for creating the
%	       workers.  Only options changing the stack-sizes can
%	       be used. In particular, do not pass the detached or alias
%	       options.
%	@see In many cases, concurrent_maplist/2 and friends
%	     is easier to program and is tractable to program
%	     analysis.

concurrent(1, M:List, _) :- !,
	maplist(M:call, List).
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
	concur_wait(JobCount, Done, VT, Result, Exitted),
	subtract(Workers, Exitted, RemainingWorkers),
	concur_cleanup(Result, RemainingWorkers, [Queue, Done]),
	(   Result == true
	->  true
	;   Result = false
	->  fail
	;   Result = exception(Error)
	->  throw(Error)
	).

%%	submit_goals(+List, +Id0, +Module, +Queue, -Vars) is det.
%
%	Send all jobs from List to Queue. Each goal is added to Queue as
%	a term goal(Id, Goal, Vars). Vars  is   unified  with  a list of
%	lists of free variables appearing in each goal.

submit_goals([], _, _, _, []).
submit_goals([H|T], I, M, Queue, [Vars|VT]) :-
	term_variables(H, Vars),
	thread_send_message(Queue, goal(I, M:H, Vars)),
	I2 is I + 1,
	submit_goals(T, I2, M, Queue, VT).


%%	concur_wait(+N, +Done:queue, +VT:compound, -Result, -Exitted) is semidet.
%
%	Wait for completion, failure or error.
%
%	@param	Exited  List of thread-ids with threads that completed before
%			all work was done.

concur_wait(0, _, _, true, []) :- !.
concur_wait(N, Done, VT, Status, Exitted) :-
	debug(concurrent, 'Waiting: ...', []),
	thread_get_message(Done, Exit),
	debug(concurrent, 'Waiting: received ~p', [Exit]),
	(   Exit = done(Id, Vars)
	->  arg(Id, VT, Vars),
	    N2 is N - 1,
	    concur_wait(N2, Done, VT, Status, Exitted)
	;   Exit = finished(Thread)
	->  thread_join(Thread, JoinStatus),
	    debug(concurrent, 'Joined ~w with ~p', [Thread, JoinStatus]),
	    (	JoinStatus == true
	    ->	Exitted = [Thread|Exitted2],
	        concur_wait(N, Done, VT, Status, Exitted2)
	    ;	Status = JoinStatus,
		Exitted = [Thread]
	    )
	).


create_workers(N, Queue, Done, [Id|Ids], Options) :-
	N > 0, !,
	thread_create(worker(Queue, Done), Id,
		      [ at_exit(thread_send_message(Done, finished(Id)))
		      | Options
		      ]),
	N2 is N - 1,
	create_workers(N2, Queue, Done, Ids, Options).
create_workers(_, _, _, [], _).


%%	worker(+WorkQueue, +DoneQueue) is det.
%
%	Process jobs from WorkQueue and send the results to DoneQueue.

worker(Queue, Done) :-
	thread_get_message(Queue, Message),
	debug(concurrent, 'Worker: received ~p', [Message]),
	(   Message = goal(Id, Goal, Vars)
	->  (   Goal
	    ->	thread_send_message(Done, done(Id, Vars)),
		worker(Queue, Done)
	    )
	;   true
	).


%%	concur_cleanup(+Result, +Workers:list, +Queues:list) is det.
%
%	Cleanup the concurrent workers and message  queues. If Result is
%	not =true=, signal all workers to make them stop prematurely. If
%	result is true we assume  all   workers  have been instructed to
%	stop or have stopped themselves.

concur_cleanup(Result, Workers, Queues) :- !,
	(   Result == true
	->  true
	;   kill_workers(Workers)
	),
	join_all(Workers),
	maplist(message_queue_destroy, Queues).

kill_workers([]).
kill_workers([Id|T]) :-
	debug(concurrent, 'Signalling ~w', [Id]),
	catch(thread_signal(Id, throw(abort)), _, true),
	kill_workers(T).

join_all([]).
join_all([Id|T]) :-
	thread_join(Id, _),
	join_all(T).


		 /*******************************
		 *	       MAPLIST		*
		 *******************************/

%%	concurrent_maplist(:Goal, +List).
%%	concurrent_maplist(:Goal, +List1, +List2).
%%	concurrent_maplist(:Goal, +List1, +List2, +List3).
%
%	Concurrent   version   of   maplist/2.   This   predicate   uses
%	concurrent/3, using multiple _worker_  threads.   The  number of
%	threads is the minimum of the  list   length  and  the number of
%	cores available. The number of  cores   is  determined using the
%	prolog flag =cpu_count=. If this flag is absent or 1 or List has
%	less  than  two  elements,  this   predicate  simply  calls  the
%	corresponding maplist/N version.
%
%	Note that the the overhead of this predicate is considerable and
%	therefore Goal must be fairly  expensive   before  one reaches a
%	speedup.

concurrent_maplist(Goal, List) :-
	workers(List, WorkerCount), !,
	maplist(ml_goal(Goal), List, Goals),
	concurrent(WorkerCount, Goals, []).
concurrent_maplist(Goal, List) :-
	maplist(Goal, List).

ml_goal(Goal, Elem, call(Goal, Elem)).

concurrent_maplist(Goal, List1, List2) :-
	same_length(List1, List2),
	workers(List1, WorkerCount), !,
	maplist(ml_goal(Goal), List1, List2, Goals),
	concurrent(WorkerCount, Goals, []).
concurrent_maplist(Goal, List1, List2) :-
	maplist(Goal, List1, List2).

ml_goal(Goal, Elem1, Elem2, call(Goal, Elem1, Elem2)).

concurrent_maplist(Goal, List1, List2, List3) :-
	same_length(List1, List2, List3),
	workers(List1, WorkerCount), !,
	maplist(ml_goal(Goal), List1, List2, List3, Goals),
	concurrent(WorkerCount, Goals, []).
concurrent_maplist(Goal, List1, List2, List3) :-
	maplist(Goal, List1, List2, List3).

ml_goal(Goal, Elem1, Elem2, Elem3, call(Goal, Elem1, Elem2, Elem3)).

workers(List, Count) :-
	current_prolog_flag(cpu_count, Cores),
	Cores > 1,
	length(List, Len),
	Count is min(Cores,Len),
	Count > 1, !.

same_length([], [], []).
same_length([_|T1], [_|T2], [_|T3]) :-
	same_length(T1, T2, T3).


		 /*******************************
		 *	       FIRST		*
		 *******************************/

%%	first_solution(-X, :Goals, +Options) is semidet.
%
%	Try  alternative  solvers  concurrently,   returning  the  first
%	answer. In a typical scenario, solving any of the goals in Goals
%	is satisfactory for the application to  continue. As soon as one
%	of the tried alternatives is  successful,   all  the others are
%	killed and first_solution/3 succeeds.
%
%	For example, if it is unclear whether   it is better to search a
%	graph breadth-first or depth-first we can use:
%
%	==
%	search_graph(Grap, Path) :-
%		 first_solution(Path, [ breadth_first(Graph, Path),
%				        depth_first(Graph, Path)
%				      ]).
%	==
%
%	Options include thread stack-sizes passed   to thread_create, as
%	well as the options =on_fail= and   =on_error= that specify what
%	to do if a  solver  fails  or   triggers  an  error.  By default
%	exection  of  all  solvers  is  terminated  and  the  result  is
%	returned. Sometimes one may wish to  continue. One such scenario
%	is if one of the solvers may run  out of resources or one of the
%	solvers is known to be incomplete.
%
%		* on_fail(Action)
%		If =stop= (default), terminate all threads and stop with
%		the failure.  If =continue=, keep waiting.
%		* on_error(Action)
%		As above, re-throwing the error if an error appears.
%
%	@bug	first_solution/3 cannot deal with non-determinism.  There
%		is no obvious way to fit non-determinism into it.  If multiple
%		solutions are needed wrap the solvers in findall/3.


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
	    ->	Result = Msg
	    ;	wait_for_one(LeftCount, Queue, Result, Options)
	    )
	;   Msg = error(_, _)
	->  (   option(on_error(stop), Options, stop)
	    ->	Result = Msg
	    ;	wait_for_one(LeftCount, Queue, Result, Options)
	    )
	).


%%	thread_options(+Options, -ThreadOptions, -RestOptions) is det.
%
%	Split the option  list  over   thread(-size)  options  and other
%	options.

thread_options([], [], []).
thread_options([H|T], [H|Th], O) :-
	thread_option(H), !,
	thread_options(T, Th, O).
thread_options([H|T], Th, [H|O]) :-
	thread_options(T, Th, O).

thread_option(local(_)).
thread_option(global(_)).
thread_option(trail(_)).
thread_option(argument(_)).
thread_option(stack(_)).
