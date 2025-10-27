/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1999-2025, University of Amsterdam
                              VU University Amsterdam
                              SWI-Prolog Solutions b.v.

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

:- module(thread_util,
          [ threads/0,                  % List available threads
            join_threads/0,             % Join all terminated threads
            with_stopped_threads/2,     % :Goal, +Options
            thread_has_console/0,       % True if thread has a console
            attach_console/0,           % Create a new console for thread.
            attach_console/1,           % ?Title

            tspy/1,                     % :Spec
            tspy/2,                     % :Spec, +ThreadId
            tdebug/0,
            tdebug/1,                   % +ThreadId
            tnodebug/0,
            tnodebug/1,                 % +ThreadId
            tprofile/1,                 % +ThreadId
            tbacktrace/1,               % +ThreadId,
            tbacktrace/2                % +ThreadId, +Options
          ]).
:- if(current_prolog_flag(xpce, true)).
:- export(( interactor/0,
            interactor/1                % ?Title
          )).
:- autoload(library(epilog),
            [ epilog/1,
              epilog_attach/1,
              ep_has_console/1
            ]).
:- endif.

:- meta_predicate
    with_stopped_threads(0, +).

:- autoload(library(apply),[maplist/3]).
:- autoload(library(backcomp),[thread_at_exit/1]).
:- autoload(library(edinburgh),[nodebug/0]).
:- autoload(library(lists),[max_list/2,append/2]).
:- autoload(library(option),[merge_options/3,option/3]).
:- autoload(library(prolog_stack),
	    [print_prolog_backtrace/2,get_prolog_backtrace/3]).
:- autoload(library(statistics),[thread_statistics/2]).
:- autoload(library(prolog_profile), [show_profile/1]).
:- autoload(library(thread),[call_in_thread/2]).

:- set_prolog_flag(generate_debug_info, false).

:- module_transparent
    tspy/1,
    tspy/2.

/** <module> Interactive thread utilities

This  library  provides  utilities  that   are  primarily  intended  for
interactive usage in a  threaded  Prolog   environment.  It  allows  for
inspecting threads, manage I/O of background   threads (depending on the
environment) and manipulating the debug status of threads.
*/

%!  threads
%
%   List currently known threads with their status.

threads :-
    threads(Threads),
    print_message(information, threads(Threads)).

threads(Threads) :-
    findall(Thread, thread_statistics(_,Thread), Threads).

%!  join_threads
%
%   Join all terminated threads.

join_threads :-
    findall(Ripped, rip_thread(Ripped), AllRipped),
    (   AllRipped == []
    ->  true
    ;   print_message(informational, joined_threads(AllRipped))
    ).

rip_thread(thread{id:id, status:Status}) :-
    thread_property(Id, status(Status)),
    Status \== running,
    \+ thread_self(Id),
    thread_join(Id, _).

%!  with_stopped_threads(:Goal, Options) is det.
%
%   Stop all threads except the caller   while  running once(Goal). Note
%   that this is in the thread user   utilities as this is not something
%   that should be used  by  normal   applications.  Notably,  this  may
%   _deadlock_ if the current thread  requires   input  from  some other
%   thread to complete Goal or one of   the  stopped threads has a lock.
%   Options:
%
%     - stop_nodebug_threads(+Boolean)
%       If `true` (default `false`), also stop threads created with
%       the debug(false) option.
%     - except(+List)
%       Do not stop threads from this list.
%
%   @bug Note that the threads are stopped when they process signals. As
%   signal handling may be  delayed,  this   implies  they  need  not be
%   stopped before Goal starts.

:- dynamic stopped_except/1.

with_stopped_threads(_, _) :-
    stopped_except(_),
    !.
with_stopped_threads(Goal, Options) :-
    thread_self(Me),
    setup_call_cleanup(
        asserta(stopped_except(Me), Ref),
        ( stop_other_threads(Me, Options),
          once(Goal)
        ),
        erase(Ref)).

stop_other_threads(Me, Options) :-
    findall(T, stop_thread(Me, T, Options), Stopped),
    broadcast(stopped_threads(Stopped)).

stop_thread(Me, Thread, Options) :-
    option(except(Except), Options, []),
    (   option(stop_nodebug_threads(true), Options)
    ->  thread_property(Thread, status(running))
    ;   debug_target(Thread)
    ),
    Me \== Thread,
    \+ memberchk(Thread, Except),
    catch(thread_signal(Thread, stopped_except), error(_,_), fail).

stopped_except :-
    thread_wait(\+ stopped_except(_),
                [ wait_preds([stopped_except/1])
                ]).

%!  thread_has_console is semidet.
%
%   True when the calling thread has an attached console.
%
%   @see attach_console/0

thread_has_console(main) :-
    !,
    \+ current_prolog_flag(epilog, true).
thread_has_console(Id) :-
    ep_has_console(Id).

thread_has_console :-
    current_prolog_flag(break_level, _),
    !.
thread_has_console :-
    thread_self(Id),
    thread_has_console(Id),
    !.

%!  attach_console is det.
%!  attach_console(?Title) is det.
%
%   Create a new console and make the   standard Prolog streams point to
%   it. If not provided, the title is   built  using the thread id. Does
%   nothing if the current thread already has a console attached.

attach_console :-
    attach_console(_).

attach_console(_) :-
    thread_has_console,
    !.
:- if(current_predicate(epilog_attach/1)).
attach_console(Title) :-
    thread_self(Me),
    console_title(Me, Title),
    epilog_attach([ title(Title)
                  ]).
:- endif.
attach_console(Title) :-
    print_message(error, cannot_attach_console(Title)),
    fail.

console_title(Thread, Title) :-
    current_prolog_flag(system_thread_id, SysId),
    human_thread_id(Thread, Id),
    format(atom(Title),
           'SWI-Prolog Thread ~w (~d) Interactor',
           [Id, SysId]).

human_thread_id(Thread, Alias) :-
    thread_property(Thread, alias(Alias)),
    !.
human_thread_id(Thread, Id) :-
    thread_property(Thread, id(Id)).

%!  interactor is det.
%!  interactor(?Title) is det.
%
%   Run a Prolog toplevel in another thread   with a new console window.
%   If Title is given, this will be used as the window title.

interactor :-
    interactor(_).

:- if(current_predicate(epilog/1)).
interactor(Title) :-
    !,
    (   nonvar(Title)
    ->  Options = [title(Title)]
    ;   Options = []
    ),
    epilog([ init(true)
           | Options
           ]).
:- endif.
interactor(Title) :-
    print_message(error, cannot_attach_console(Title)),
    fail.


                 /*******************************
                 *          DEBUGGING           *
                 *******************************/

%!  tspy(:Spec) is det.
%!  tspy(:Spec, +ThreadId) is det.
%
%   Trap the graphical debugger on reaching Spec in the specified or
%   any thread.

tspy(Spec) :-
    spy(Spec),
    tdebug.

tspy(Spec, ThreadID) :-
    spy(Spec),
    tdebug(ThreadID).


%!  tdebug is det.
%!  tdebug(+Thread) is det.
%
%   Enable debug-mode, trapping the graphical debugger on reaching
%   spy-points or errors.

tdebug :-
    forall(debug_target(Id), thread_signal(Id, debug_thread)).

tdebug(ThreadID) :-
    thread_signal(ThreadID, debug_thread).

debug_thread :-
    current_prolog_flag(gui, true),
    !,
    autoload_call(gdebug).
debug_thread :-
    debug.


%!  tnodebug is det.
%!  tnodebug(+Thread) is det.
%
%   Disable debug-mode in all threads or the specified Thread.

tnodebug :-
    forall(debug_target(Id), thread_signal(Id, nodebug)).

tnodebug(ThreadID) :-
    thread_signal(ThreadID, nodebug).


debug_target(Thread) :-
    thread_property(Thread, status(running)),
    thread_property(Thread, debug(true)).

%!  tbacktrace(+Thread) is det.
%!  tbacktrace(+Thread, +Options) is det.
%
%   Print a backtrace for  Thread  to   the  stream  `user_error` of the
%   calling thread. This is achieved  by   inserting  an  interrupt into
%   Thread using call_in_thread/2. Options:
%
%     - depth(+MaxFrames)
%       Number of stack frames to show.  Default is the current Prolog
%       flag `backtrace_depth` or 20.
%
%   Other options are passed to get_prolog_backtrace/3.
%
%   @bug call_in_thread/2 may not process the event.

tbacktrace(Thread) :-
    tbacktrace(Thread, []).

tbacktrace(Thread, Options) :-
    merge_options(Options, [clause_references(false)], Options1),
    (   current_prolog_flag(backtrace_depth, Default)
    ->  true
    ;   Default = 20
    ),
    option(depth(Depth), Options1, Default),
    call_in_thread(Thread, thread_get_prolog_backtrace(Depth, Stack, Options1)),
    print_prolog_backtrace(user_error, Stack).

%!  thread_get_prolog_backtrace(+Depth, -Stack, +Options)
%
%   As get_prolog_backtrace/3, but starts above   the C callback, hiding
%   the overhead inside call_in_thread/2.

thread_get_prolog_backtrace(Depth, Stack, Options) :-
    prolog_current_frame(Frame),
    signal_frame(Frame, SigFrame),
    get_prolog_backtrace(Depth, Stack, [frame(SigFrame)|Options]).

signal_frame(Frame, SigFrame) :-
    prolog_frame_attribute(Frame, clause, _),
    !,
    (   prolog_frame_attribute(Frame, parent, Parent)
    ->  signal_frame(Parent, SigFrame)
    ;   SigFrame = Frame
    ).
signal_frame(Frame, SigFrame) :-
    (   prolog_frame_attribute(Frame, parent, Parent)
    ->  SigFrame = Parent
    ;   SigFrame = Frame
    ).



                 /*******************************
                 *       REMOTE PROFILING       *
                 *******************************/

%!  tprofile(+Thread) is det.
%
%   Profile the operation of Thread until the user hits a key.

tprofile(Thread) :-
    init_pce,
    thread_signal(Thread,
                  (   reset_profiler,
                      profiler(_, true)
                  )),
    format('Running profiler in thread ~w (press RET to show results) ...',
           [Thread]),
    flush_output,
    get_code(_),
    thread_signal(Thread,
                  (   profiler(_, false),
                      show_profile([])
                  )).


%!  init_pce
%
%   Make sure XPCE is running if it is   attached, so we can use the
%   graphical display using in_pce_thread/1.

:- if(exists_source(library(pce))).
init_pce :-
    current_prolog_flag(gui, true),
    !,
    autoload_call(send(@(display), open)).
:- endif.
init_pce.


                 /*******************************
                 *             HOOKS            *
                 *******************************/

:- multifile
    prolog:message_action/2.

prolog:message_action(trace_mode(on), _Level) :-
    \+ thread_has_console,
    \+ current_prolog_flag(gui_tracer, true),
    catch(attach_console, error(_,_), fail).

:- multifile
    prolog:message/3.

prolog:message(thread_welcome) -->
    { thread_self(Self),
      human_thread_id(Self, Id)
    },
    [ 'SWI-Prolog console for thread ~w'-[Id],
      nl, nl
    ].
prolog:message(joined_threads(Threads)) -->
    [ 'Joined the following threads'-[], nl ],
    thread_list(Threads).
prolog:message(threads(Threads)) -->
    thread_list(Threads).
prolog:message(cannot_attach_console(_Title)) -->
    [ 'Cannot attach a console (requires xpce package)' ].

thread_list(Threads) -->
    { maplist(th_id_len, Threads, Lens),
      max_list(Lens, MaxWidth),
      LeftColWidth is max(6, MaxWidth),
      Threads = [H|_]
    },
    thread_list_header(H, LeftColWidth),
    thread_list(Threads, LeftColWidth).

th_id_len(Thread, IdLen) :-
    write_length(Thread.id, IdLen, [quoted(true)]).

thread_list([], _) --> [].
thread_list([H|T], CW) -->
    thread_info(H, CW),
    (   {T == []}
    ->  []
    ;   [nl],
        thread_list(T, CW)
    ).

thread_list_header(Thread, CW) -->
    { _{id:_, status:_, time:_, stacks:_} :< Thread,
      !,
      HrWidth is CW+18+13+13
    },
    [ '~|~tThread~*+ Status~tTime~18+~tStack use~13+~tallocated~13+'-[CW], nl ],
    [ '~|~`-t~*+'-[HrWidth], nl ].
thread_list_header(Thread, CW) -->
    { _{id:_, status:_} :< Thread,
      !,
      HrWidth is CW+7
    },
    [ '~|~tThread~*+ Status'-[CW], nl ],
    [ '~|~`-t~*+'-[HrWidth], nl ].

thread_info(Thread, CW) -->
    { _{id:Id, status:Status, time:Time, stacks:Stacks} :< Thread },
    !,
    [ '~|~t~q~*+ ~w~t~3f~18+~t~D~13+~t~D~13+'-
      [ Id, CW, Status, Time.cpu, Stacks.total.usage, Stacks.total.allocated
      ]
    ].
thread_info(Thread, CW) -->
    { _{id:Id, status:Status} :< Thread },
    !,
    [ '~|~t~q~*+ ~w'-
      [ Id, CW, Status
      ]
    ].

