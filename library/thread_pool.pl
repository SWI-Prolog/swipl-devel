/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2008-2016, University of Amsterdam
                              VU University Amsterdam
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

:- module(thread_pool,
          [ thread_pool_create/3,       % +Pool, +Size, +Options
            thread_pool_destroy/1,      % +Pool
            thread_create_in_pool/4,    % +Pool, :Goal, -Id, +Options

            current_thread_pool/1,      % ?Pool
            thread_pool_property/2      % ?Pool, ?Property
          ]).
:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(rbtrees)).
:- use_module(library(debug)).


/** <module> Resource bounded thread management

The module library(thread_pool) manages threads in pools. A pool defines
properties of its member threads and the  maximum number of threads that
can coexist in the pool. The   call  thread_create_in_pool/4 allocates a
thread in the pool, just like  thread_create/3.   If  the  pool is fully
allocated it can be asked to wait or raise an error.

The library has been designed  to   deal  with  server applications that
receive a variety of requests, such as   HTTP servers. Simply starting a
thread for each request is a bit too simple minded for such servers:

    * Creating many CPU intensive threads often leads to a slow-down
    rather than a speedup.
    * Creating many memory intensive threads may exhaust resources
    * Tasks that require little CPU and memory but take long waiting
    for external resources can run many threads.

Using this library, one can define a  pool   for  each set of tasks with
comparable characteristics and create threads in   this pool. Unlike the
worker-pool model, threads are not started immediately. Depending on the
design, both approaches can be attractive.

The library is implemented by means of   a manager thread with the fixed
thread id =|__thread_pool_manager|=. All  state   is  maintained in this
manager thread, which receives and  processes   requests  to  create and
destroy pools, create  threads  in  a   pool  and  handle  messages from
terminated threads. Thread pools are _not_ saved   in  a saved state and
must therefore be recreated  using   the  initialization/1  directive or
otherwise during startup of the application.

@see http_handler/3 and http_spawn/2.
*/

:- meta_predicate
    thread_create_in_pool(+, 0, -, :).
:- predicate_options(thread_create_in_pool/4, 4,
                     [ wait(boolean),
                       pass_to(system:thread_create/3, 3)
                     ]).

:- multifile
    create_pool/1.

%!  thread_pool_create(+Pool, +Size, +Options) is det.
%
%   Create a pool of threads. A pool of threads is a declaration for
%   creating threads with shared  properties   (stack  sizes)  and a
%   limited  number  of  threads.   Threads    are   created   using
%   thread_create_in_pool/4. If all threads in the  pool are in use,
%   the   behaviour   depends   on    the     =wait=    option    of
%   thread_create_in_pool/4  and  the  =backlog=   option  described
%   below.  Options are passed to thread_create/3, except for
%
%       * backlog(+MaxBackLog)
%       Maximum number of requests that can be suspended.  Default
%       is =infinite=.  Otherwise it must be a non-negative integer.
%       Using backlog(0) will never delay thread creation for this
%       pool.
%
%   The pooling mechanism does _not_   interact  with the =detached=
%   state of a thread. Threads can   be  created both =detached= and
%   normal and must be joined using   thread_join/2  if they are not
%   detached.

thread_pool_create(Name, Size, Options) :-
    must_be(list, Options),
    pool_manager(Manager),
    thread_self(Me),
    thread_send_message(Manager, create_pool(Name, Size, Options, Me)),
    wait_reply.

%!  thread_pool_destroy(+Name) is det.
%
%   Destroy the thread pool named Name.
%
%   @error  existence_error(thread_pool, Name).

thread_pool_destroy(Name) :-
    pool_manager(Manager),
    thread_self(Me),
    thread_send_message(Manager, destroy_pool(Name, Me)),
    wait_reply.


%!  current_thread_pool(?Name) is nondet.
%
%   True if Name refers to a defined thread pool.

current_thread_pool(Name) :-
    pool_manager(Manager),
    thread_self(Me),
    thread_send_message(Manager, current_pools(Me)),
    wait_reply(Pools),
    (   atom(Name)
    ->  memberchk(Name, Pools)
    ;   member(Name, Pools)
    ).

%!  thread_pool_property(?Name, ?Property) is nondet.
%
%   True if Property is a property of thread pool Name. Defined
%   properties are:
%
%       * options(Options)
%       Thread creation options for this pool
%       * free(Size)
%       Number of free slots on this pool
%       * size(Size)
%       Total number of slots on this pool
%       * members(ListOfIDs)
%       ListOfIDs is the list or threads running in this pool
%       * running(Running)
%       Number of running threads in this pool
%       * backlog(Size)
%       Number of delayed thread creations on this pool

thread_pool_property(Name, Property) :-
    current_thread_pool(Name),
    pool_manager(Manager),
    thread_self(Me),
    thread_send_message(Manager, pool_properties(Me, Name, Property)),
    wait_reply(Props),
    (   nonvar(Property)
    ->  memberchk(Property, Props)
    ;   member(Property, Props)
    ).


%!  thread_create_in_pool(+Pool, :Goal, -Id, +Options) is det.
%
%   Create  a  thread  in  Pool.  Options  overrule  default  thread
%   creation options associated  to  the   pool.  In  addition,  the
%   following option is defined:
%
%       * wait(+Boolean)
%       If =true= (default) and the pool is full, wait until a
%       member of the pool completes.  If =false=, throw a
%       resource_error.
%
%   @error  resource_error(threads_in_pool(Pool)) is raised if wait
%           is =false= or the backlog limit has been reached.
%   @error  existence_error(thread_pool, Pool) if Pool does not
%           exist.

thread_create_in_pool(Pool, Goal, Id, QOptions) :-
    meta_options(is_meta, QOptions, Options),
    catch(thread_create_in_pool_(Pool, Goal, Id, Options),
          Error, true),
    (   var(Error)
    ->  true
    ;   Error = error(existence_error(thread_pool, Pool), _),
        create_pool_lazily(Pool)
    ->  thread_create_in_pool_(Pool, Goal, Id, Options)
    ;   throw(Error)
    ).

thread_create_in_pool_(Pool, Goal, Id, Options) :-
    select_option(wait(Wait), Options, ThreadOptions, true),
    pool_manager(Manager),
    thread_self(Me),
    thread_send_message(Manager,
                        create(Pool, Goal, Me, Wait, Id, ThreadOptions)),
    wait_reply(Id).

is_meta(at_exit).


%!  create_pool_lazily(+Pool) is semidet.
%
%   Call the hook create_pool/1 to create the pool lazily.

create_pool_lazily(Pool) :-
    with_mutex(Pool,
               ( mutex_destroy(Pool),
                 create_pool_sync(Pool)
               )).

create_pool_sync(Pool) :-
    current_thread_pool(Pool),
    !.
create_pool_sync(Pool) :-
    create_pool(Pool).


                 /*******************************
                 *        START MANAGER         *
                 *******************************/

%!  pool_manager(-ThreadID) is det.
%
%   ThreadID is the thread (alias) identifier of the manager. Starts
%   the manager if it is not running.

pool_manager(TID) :-
    TID = '__thread_pool_manager',
    (   thread_running(TID)
    ->  true
    ;   with_mutex('__thread_pool', create_pool_manager(TID))
    ).

thread_running(Thread) :-
    catch(thread_property(Thread, status(Status)),
          E, true),
    (   var(E)
    ->  (   Status == running
        ->  true
        ;   thread_join(Thread, _),
            print_message(warning, thread_pool(manager_died(Status))),
            fail
        )
    ;   E = error(existence_error(thread, Thread), _)
    ->  fail
    ;   throw(E)
    ).

create_pool_manager(Thread) :-
    thread_running(Thread),
    !.
create_pool_manager(Thread) :-
    thread_create(pool_manager_main, _,
                  [ alias(Thread),
                    inherit_from(main)
                  ]).


pool_manager_main :-
    rb_new(State0),
    manage_thread_pool(State0).


                 /*******************************
                 *        MANAGER LOGIC         *
                 *******************************/

%!  manage_thread_pool(+State)

manage_thread_pool(State0) :-
    thread_get_message(Message),
    (   update_thread_pool(Message, State0, State)
    ->  debug(thread_pool(state), 'Message ~p --> ~p', [Message, State]),
        manage_thread_pool(State)
    ;   format(user_error, 'Update failed: ~p~n', [Message])
    ).


update_thread_pool(create_pool(Name, Size, Options, For), State0, State) :-
    !,
    (   rb_insert_new(State0,
                      Name, tpool(Options, Size, Size, WP, WP, []),
                      State)
    ->  thread_send_message(For, thread_pool(true))
    ;   reply_error(For, permission_error(create, thread_pool, Name)),
        State = State0
    ).
update_thread_pool(destroy_pool(Name, For), State0, State) :-
    !,
    (   rb_delete(State0, Name, State)
    ->  thread_send_message(For, thread_pool(true))
    ;   reply_error(For, existence_error(thread_pool, Name)),
        State = State0
    ).
update_thread_pool(current_pools(For), State, State) :-
    !,
    rb_keys(State, Keys),
    debug(thread_pool(current), 'Reply to ~w: ~p', [For, Keys]),
    reply(For, Keys).
update_thread_pool(pool_properties(For, Name, P), State, State) :-
    !,
    (   rb_lookup(Name, Pool, State)
    ->  findall(P, pool_property(P, Pool), List),
        reply(For, List)
    ;   reply_error(For, existence_error(thread_pool, Name))
    ).
update_thread_pool(Message, State0, State) :-
    arg(1, Message, Name),
    (   rb_lookup(Name, Pool0, State0)
    ->  update_pool(Message, Pool0, Pool),
        rb_update(State0, Name, Pool, State)
    ;   State = State0,
        (   Message = create(Name, _, For, _, _, _)
        ->  reply_error(For, existence_error(thread_pool, Name))
        ;   true
        )
    ).

pool_property(options(Options),
              tpool(Options, _Free, _Size, _WP, _WPT, _Members)).
pool_property(backlog(Size),
              tpool(_, _Free, _Size, WP, WPT, _Members)) :-
    diff_list_length(WP, WPT, Size).
pool_property(free(Free),
              tpool(_, Free, _Size, _, _, _)).
pool_property(size(Size),
              tpool(_, _Free, Size, _, _, _)).
pool_property(running(Count),
              tpool(_, Free, Size, _, _, _)) :-
    Count is Size - Free.
pool_property(members(IDList),
              tpool(_, _, _, _, _, IDList)).

diff_list_length(List, Tail, Size) :-
    '$skip_list'(Length, List, Rest),
    (   Rest == Tail
    ->  Size = Length
    ;   type_error(difference_list, List/Tail)
    ).


%!  update_pool(+Message, +Pool0, -Pool) is det.
%
%   Deal with create requests and  completion   messages  on a given
%   pool.  There are two messages:
%
%       * create(PoolName, Goal, ForThread, Wait, Id, Options)
%       Create a new thread on behalf of ForThread.  There are
%       two cases:
%            * Free slots: create the thread
%            * No free slots: error or add to waiting
%       * exitted(PoolName, Thread)
%       A thread completed.  If there is a request waiting,
%       create a new one.

update_pool(create(Name, Goal, For, _, Id, MyOptions),
            tpool(Options, Free0, Size, WP, WPT, Members0),
            tpool(Options, Free, Size, WP, WPT, Members)) :-
    succ(Free, Free0),
    !,
    merge_options(MyOptions, Options, ThreadOptions),
    select_option(at_exit(AtExit), ThreadOptions, ThreadOptions1, true),
    catch(thread_create(Goal, Id,
                        [ at_exit(worker_exitted(Name, Id, AtExit))
                        | ThreadOptions1
                        ]),
          E, true),
    (   var(E)
    ->  Members = [Id|Members0],
        reply(For, Id)
    ;   reply_error(For, E),
        Members = Members0
    ).
update_pool(Create,
            tpool(Options, 0, Size, WP, WPT0, Members),
            tpool(Options, 0, Size, WP, WPT, Members)) :-
    Create = create(Name, _Goal, For, Wait, _, _Options),
    !,
    option(backlog(BackLog), Options, infinite),
    (   can_delay(Wait, BackLog, WP, WPT0)
    ->  WPT0 = [Create|WPT],
        debug(thread_pool, 'Delaying ~p', [Create])
    ;   WPT = WPT0,
        reply_error(For, resource_error(threads_in_pool(Name)))
    ).
update_pool(exitted(_Name, Id),
            tpool(Options, Free0, Size, WP0, WPT, Members0),
            Pool) :-
    succ(Free0, Free),
    delete(Members0, Id, Members1),
    Pool1 = tpool(Options, Free, Size, WP, WPT, Members1),
    (   WP0 == WPT
    ->  WP = WP0,
        Pool = Pool1
    ;   WP0 = [Waiting|WP],
        debug(thread_pool, 'Start delayed ~p', [Waiting]),
        update_pool(Waiting, Pool1, Pool)
    ).


can_delay(true, infinite, _, _) :- !.
can_delay(true, BackLog, WP, WPT) :-
    diff_list_length(WP, WPT, Size),
    BackLog > Size.

%!  worker_exitted(+PoolName, +WorkerId, :AtExit)
%
%   It is possible that  '__thread_pool_manager'   no  longer exists
%   while closing down the process because   the  manager was killed
%   before the worker.
%
%   @tbd Find a way to discover that we are terminating Prolog.

:- public
    worker_exitted/3.

worker_exitted(Name, Id, AtExit) :-
    catch(thread_send_message('__thread_pool_manager', exitted(Name, Id)),
          _, true),
    call(AtExit).


                 /*******************************
                 *             UTIL             *
                 *******************************/

reply(To, Term) :-
    thread_send_message(To, thread_pool(true(Term))).

reply_error(To, Error) :-
    thread_send_message(To, thread_pool(error(Error, _))).

wait_reply :-
    thread_get_message(thread_pool(Result)),
    (   Result == true
    ->  true
    ;   Result == fail
    ->  fail
    ;   throw(Result)
    ).

wait_reply(Value) :-
    thread_get_message(thread_pool(Reply)),
    (   Reply = true(Value0)
    ->  Value = Value0
    ;   Reply == fail
    ->  fail
    ;   throw(Reply)
    ).


                 /*******************************
                 *             HOOKS            *
                 *******************************/

%!  create_pool(+PoolName) is semidet.
%
%   Hook to create a thread  pool  lazily.   The  hook  is called if
%   thread_create_in_pool/4 discovers that the thread  pool does not
%   exist. If the  hook   succeeds,  thread_create_in_pool/4 retries
%   creating the thread. For  example,  we   can  use  the following
%   declaration to create threads in the pool =media=, which holds a
%   maximum of 20 threads.
%
%     ==
%     :- multifile thread_pool:create_pool/1.
%
%     thread_pool:create_pool(media) :-
%         thread_pool_create(media, 20, []).
%     ==

                 /*******************************
                 *            MESSAGES          *
                 *******************************/
:- multifile
    prolog:message/3.

prolog:message(thread_pool(Message)) -->
    message(Message).

message(manager_died(Status)) -->
    [ 'Thread-pool: manager died on status ~p; restarting'-[Status] ].
