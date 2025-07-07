/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1999-2024, University of Amsterdam
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
:- if((   current_predicate(win_open_console/5)
      ;   current_predicate('$open_xterm'/5))).
:- export(( thread_run_interactor/0,    % interactor main loop
            interactor/0,
            interactor/1                % ?Title
          )).
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

:- dynamic
    has_console/4.                  % Id, In, Out, Err

thread_has_console(main) :- !.                  % we assume main has one.
thread_has_console(Id) :-
    has_console(Id, _, _, _).

thread_has_console :-
    current_prolog_flag(break_level, _),
    !.
thread_has_console :-
    thread_self(Id),
    thread_has_console(Id),
    !.

%!  open_console(+Title, -In, -Out, -Err) is det.
%
%   Open a new console window and unify In,  Out and Err with the input,
%   output and error streams for the new console. This predicate is only
%   available  if  win_open_console/5  (Windows  or   Qt  swipl-win)  or
%   '$open_xterm'/5 (POSIX systems with pseudo terminal support).

:- multifile xterm_args/1.
:- dynamic   xterm_args/1.

:- if(current_predicate(win_open_console/5)).

can_open_console.

open_console(Title, In, Out, Err) :-
    thread_self(Id),
    regkey(Id, Key),
    win_open_console(Title, In, Out, Err,
                     [ registry_key(Key)
                     ]).

regkey(Key, Key) :-
    atom(Key).
regkey(_, 'Anonymous').

:- elif(current_predicate('$open_xterm'/5)).

%!  xterm_args(-List) is nondet.
%
%   Multifile and dynamic hook that  provides (additional) arguments for
%   the xterm(1) process opened  for   additional  thread consoles. Each
%   solution must bind List to a list   of  atomic values. All solutions
%   are concatenated using append/2 to form the final argument list.
%
%   The defaults set  the  colors   to  black-on-light-yellow,  enable a
%   scrollbar, set the font using  Xft   font  pattern  and prepares the
%   back-arrow key.

xterm_args(['-xrm', '*backarrowKeyIsErase: false']).
xterm_args(['-xrm', '*backarrowKey: false']).
xterm_args(['-fa', 'Ubuntu Mono', '-fs', 12]).
xterm_args(['-fg', '#000000']).
xterm_args(['-bg', '#ffffdd']).
xterm_args(['-sb', '-sl', 1000, '-rightbar']).

can_open_console :-
    getenv('DISPLAY', _),
    absolute_file_name(path(xterm), _XTerm, [access(execute)]).

open_console(Title, In, Out, Err) :-
    findall(Arg, xterm_args(Arg), Args),
    append(Args, Argv),
    '$open_xterm'(Title, In, Out, Err, Argv).

:- endif.

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
:- if(current_predicate(open_console/4)).
attach_console(Title) :-
    can_open_console,
    !,
    thread_self(Id),
    (   var(Title)
    ->  console_title(Id, Title)
    ;   true
    ),
    open_console(Title, In, Out, Err),
    assert(has_console(Id, In, Out, Err)),
    set_stream(In,  alias(user_input)),
    set_stream(Out, alias(user_output)),
    set_stream(Err, alias(user_error)),
    set_stream(In,  alias(current_input)),
    set_stream(Out, alias(current_output)),
    enable_line_editing(In,Out,Err),
    thread_at_exit(detach_console(Id)).
:- endif.
attach_console(Title) :-
    print_message(error, cannot_attach_console(Title)),
    fail.

:- if(current_predicate(open_console/4)).
console_title(Thread, Title) :-         % uses tabbed consoles
    current_prolog_flag(console_menu_version, qt),
    !,
    human_thread_id(Thread, Id),
    format(atom(Title), 'Thread ~w', [Id]).
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

%!  enable_line_editing(+In, +Out, +Err) is det.
%
%   Enable line editing for the console.  This   is  by built-in for the
%   Windows console. We can also provide it   for the X11 xterm(1) based
%   console if we use the BSD libedit based command line editor.

enable_line_editing(_In, _Out, _Err) :-
    current_prolog_flag(readline, editline),
    exists_source(library(editline)),
    use_module(library(editline)),
    !,
    call(el_wrap).
enable_line_editing(_In, _Out, _Err).

disable_line_editing(_In, _Out, _Err) :-
    current_predicate(el_unwrap/1),
    !,
    call(el_unwrap(user_input)).
disable_line_editing(_In, _Out, _Err).


%!  detach_console(+ThreadId) is det.
%
%   Destroy the console for ThreadId.

detach_console(Id) :-
    (   retract(has_console(Id, In, Out, Err))
    ->  disable_line_editing(In, Out, Err),
        close(In, [force(true)]),
        close(Out, [force(true)]),
        close(Err, [force(true)])
    ;   true
    ).

%!  interactor is det.
%!  interactor(?Title) is det.
%
%   Run a Prolog toplevel in another thread   with a new console window.
%   If Title is given, this will be used as the window title.

interactor :-
    interactor(_).

interactor(Title) :-
    current_prolog_flag(epilog, true),
    !,
    (   nonvar(Title)
    ->  Options = [title(Title)]
    ;   Options = []
    ),
    autoload_call(epilog(Options)).
interactor(Title) :-
    can_open_console,
    !,
    thread_self(Me),
    thread_create(thread_run_interactor(Me, Title), _Id,
                  [ detached(true)
                  ]),
    thread_get_message(Msg),
    (   Msg = title(Title0)
    ->  Title = Title0
    ;   Msg = throw(Error)
    ->  throw(Error)
    ;   Msg = false
    ->  fail
    ).
interactor(Title) :-
    print_message(error, cannot_attach_console(Title)),
    fail.

thread_run_interactor(Creator, Title) :-
    set_prolog_flag(query_debug_settings, debug(false, false)),
    Error = error(Formal,_),
    (   catch(attach_console(Title), Error, true)
    ->  (   var(Formal)
        ->  thread_send_message(Creator, title(Title)),
            print_message(banner, thread_welcome),
            prolog
        ;   thread_send_message(Creator, throw(Error))
        )
    ;   thread_send_message(Creator, false)
    ).

%!  thread_run_interactor
%
%   Attach a console and run a Prolog toplevel in the current thread.

thread_run_interactor :-
    set_prolog_flag(query_debug_settings, debug(false, false)),
    attach_console(_Title),
    print_message(banner, thread_welcome),
    prolog.

:- endif.                               % have open_console/4

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
    user:message_hook/3.

user:message_hook(trace_mode(on), _, Lines) :-
    \+ thread_has_console,
    \+ current_prolog_flag(gui_tracer, true),
    catch(attach_console, _, fail),
    print_message_lines(user_error, '% ', Lines).

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
    [ 'Cannot attach a console (requires swipl-win or POSIX pty support)' ].

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

