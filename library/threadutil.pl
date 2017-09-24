/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1999-2017, University of Amsterdam
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

:- module(thread_util,
          [ thread_run_interactor/0,    % interactor main loop
            threads/0,                  % List available threads
            join_threads/0,             % Join all terminated threads
            interactor/0,               % Create a new interactor
            interactor/1,               % ?Title
            thread_has_console/0,       % True if thread has a console
            attach_console/0,           % Create a new console for thread.
            attach_console/1,           % ?Title

            tspy/1,                     % :Spec
            tspy/2,                     % :Spec, +ThreadId
            tdebug/0,
            tdebug/1,                   % +ThreadId
            tnodebug/0,
            tnodebug/1,                 % +ThreadId
            tprofile/1                  % +ThreadId
          ]).
:- use_module(library(apply)).
:- use_module(library(lists)).
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

%!  interactor is det.
%!  interactor(?Title) is det.
%
%   Run a Prolog toplevel in another thread   with a new console window.
%   If Title is given, this will be used as the window title.

interactor :-
    interactor(_).

interactor(Title) :-
    thread_self(Me),
    thread_create(thread_run_interactor(Me, Title), _Id,
                  [ detached(true),
                    debug(false)
                  ]),
    thread_get_message(title(Title)).

thread_run_interactor(Creator, Title) :-
    set_prolog_flag(query_debug_settings, debug(false, false)),
    attach_console(Title),
    thread_send_message(Creator, title(Title)),
    print_message(banner, thread_welcome),
    prolog.

%!  thread_run_interactor
%
%   Attach a console and run a Prolog toplevel in the current thread.

thread_run_interactor :-
    set_prolog_flag(query_debug_settings, debug(false, false)),
    attach_console(_Title),
    print_message(banner, thread_welcome),
    prolog.

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
attach_console(Title) :-
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

%!  open_console(+Title, -In, -Out, -Err) is det.
%
%   Open a new console window and unify In,  Out and Err with the input,
%   output and error streams for the new console.

:- multifile xterm_args/1.
:- dynamic   xterm_args/1.

:- if(current_predicate(win_open_console/5)).

open_console(Title, In, Out, Err) :-
    thread_self(Id),
    regkey(Id, Key),
    win_open_console(Title, In, Out, Err,
                     [ registry_key(Key)
                     ]).

regkey(Key, Key) :-
    atom(Key).
regkey(_, 'Anonymous').

:- else.

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
xterm_args(['-fa', 'monospace;pixelsize=11;regular']).
xterm_args(['-fg', '#000000']).
xterm_args(['-bg', '#ffffdd']).
xterm_args(['-sb', '-sl', 1000, '-rightbar']).

open_console(Title, In, Out, Err) :-
    findall(Arg, xterm_args(Arg), Args),
    append(Args, Argv),
    open_xterm(Title, In, Out, Err, Argv).

:- endif.

%!  enable_line_editing(+In, +Out, +Err) is det.
%
%   Enable line editing for the console.  This   is  by built-in for the
%   Windows console. We can also provide it   for the X11 xterm(1) based
%   console if we use the BSD libedit based command line editor.

:- if((current_prolog_flag(readline, editline),
       exists_source(library(editline)))).
:- use_module(library(editline)).
enable_line_editing(_In, _Out, _Err) :-
    current_prolog_flag(readline, editline),
    !,
    el_wrap.
:- endif.
enable_line_editing(_In, _Out, _Err).

:- if(current_predicate(el_unwrap/1)).
disable_line_editing(_In, _Out, _Err) :-
    el_unwrap(user_input).
:- endif.
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
    forall(debug_target(Id), thread_signal(Id, gdebug)).

tdebug(ThreadID) :-
    thread_signal(ThreadID, gdebug).

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
    get0(_),
    thread_signal(Thread,
                  (   profiler(_, false),
                      show_profile([])
                  )).


%!  init_pce
%
%   Make sure XPCE is running if it is   attached, so we can use the
%   graphical display using in_pce_thread/1.

init_pce :-
    current_prolog_flag(gui, true),
    !,
    call(send(@(display), open)).   % avoid autoloading
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

