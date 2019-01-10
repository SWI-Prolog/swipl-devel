/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1985-2017, University of Amsterdam
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

:- module('$toplevel',
          [ '$initialise'/0,            % start Prolog
            '$toplevel'/0,              % Prolog top-level (re-entrant)
            '$compile'/0,               % `-c' toplevel
            '$config'/0,                % --dump-runtime-variables toplevel
            initialize/0,               % Run program initialization
            version/0,                  % Write initial banner
            version/1,                  % Add message to the banner
            prolog/0,                   % user toplevel predicate
            '$query_loop'/0,            % toplevel predicate
            residual_goals/1,           % +Callable
            (initialization)/1,         % initialization goal (directive)
            '$thread_init'/0,           % initialise thread
            (thread_initialization)/1   % thread initialization goal
            ]).


                 /*******************************
                 *       FILE_SEARCH_PATH       *
                 *******************************/

:- multifile user:file_search_path/2.

user:file_search_path(user_profile, app_preferences('.')).
:- if(current_prolog_flag(windows, true)).
user:file_search_path(app_preferences, app_data('.')).
user:file_search_path(app_data, PrologAppData) :-
    current_prolog_flag(windows, true),
    catch(win_folder(appdata, AppData), _, fail),
    atom_concat(AppData, '/SWI-Prolog', PrologAppData),
    (   exists_directory(PrologAppData)
    ->  true
    ;   catch(make_directory(PrologAppData), _, fail)
    ).
:- else.
user:file_search_path(app_data, UserLibDir) :-
    catch(expand_file_name('~/lib/swipl', [UserLibDir]), _, fail).
:- endif.
user:file_search_path(app_preferences, UserHome) :-
    catch(expand_file_name(~, [UserHome]), _, fail).


                 /*******************************
                 *         VERSION BANNER       *
                 *******************************/

:- dynamic
    prolog:version_msg/1.

%!  version is det.
%
%   Print the Prolog banner message and messages registered using
%   version/1.

version :-
    print_message(banner, welcome).

%!  version(+Message) is det.
%
%   Add message to version/0

:- multifile
    system:term_expansion/2.

system:term_expansion((:- version(Message)),
                      prolog:version_msg(Message)).

version(Message) :-
    (   prolog:version_msg(Message)
    ->  true
    ;   assertz(prolog:version_msg(Message))
    ).


                /********************************
                *         INITIALISATION        *
                *********************************/

%       note: loaded_init_file/2 is used by prolog_load_context/2 to
%       confirm we are loading a script.

:- dynamic
    loaded_init_file/2.             % already loaded init files

'$load_init_file'(none) :- !.
'$load_init_file'(Base) :-
    loaded_init_file(Base, _),
    !.
'$load_init_file'(InitFile) :-
    exists_file(InitFile),
    !,
    ensure_loaded(user:InitFile).
'$load_init_file'(Base) :-
    absolute_file_name(user_profile(Base), InitFile,
                       [ access(read),
                         file_errors(fail)
                       ]),
    asserta(loaded_init_file(Base, InitFile)),
    load_files(user:InitFile,
               [ scope_settings(false)
               ]).
'$load_init_file'(_).

'$load_system_init_file' :-
    loaded_init_file(system, _),
    !.
'$load_system_init_file' :-
    '$cmd_option_val'(system_init_file, Base),
    Base \== none,
    current_prolog_flag(home, Home),
    file_name_extension(Base, rc, Name),
    atomic_list_concat([Home, '/', Name], File),
    absolute_file_name(File, Path,
                       [ file_type(prolog),
                         access(read),
                         file_errors(fail)
                       ]),
    asserta(loaded_init_file(system, Path)),
    load_files(user:Path,
               [ silent(true),
                 scope_settings(false)
               ]),
    !.
'$load_system_init_file'.

'$load_script_file' :-
    loaded_init_file(script, _),
    !.
'$load_script_file' :-
    '$cmd_option_val'(script_file, OsFiles),
    load_script_files(OsFiles).

load_script_files([]).
load_script_files([OsFile|More]) :-
    prolog_to_os_filename(File, OsFile),
    (   absolute_file_name(File, Path,
                           [ file_type(prolog),
                             access(read),
                             file_errors(fail)
                           ])
    ->  asserta(loaded_init_file(script, Path)),
        load_files(user:Path, []),
        load_files(More)
    ;   throw(error(existence_error(script_file, File), _))
    ).


                 /*******************************
                 *       AT_INITIALISATION      *
                 *******************************/

:- meta_predicate
    initialization(0).

:- '$iso'((initialization)/1).

%!  initialization(:Goal)
%
%   Runs Goal after loading the file in which this directive
%   appears as well as after restoring a saved state.
%
%   @see initialization/2

initialization(Goal) :-
    Goal = _:G,
    prolog:initialize_now(G, Use),
    !,
    print_message(warning, initialize_now(G, Use)),
    initialization(Goal, now).
initialization(Goal) :-
    initialization(Goal, after_load).

:- multifile
    prolog:initialize_now/2,
    prolog:message//1.

prolog:initialize_now(load_foreign_library(_),
                      'use :- use_foreign_library/1 instead').
prolog:initialize_now(load_foreign_library(_,_),
                      'use :- use_foreign_library/2 instead').

prolog:message(initialize_now(Goal, Use)) -->
    [ 'Initialization goal ~p will be executed'-[Goal],nl,
      'immediately for backward compatibility reasons', nl,
      '~w'-[Use]
    ].

'$run_initialization' :-
    '$run_initialization'(_, []),
    '$thread_init'.

%!  initialize
%
%   Run goals registered with `:-  initialization(Goal, program).`. Stop
%   with an exception if a goal fails or raises an exception.

initialize :-
    forall('$init_goal'(when(program), Goal, Ctx),
           run_initialize(Goal, Ctx)).

run_initialize(Goal, Ctx) :-
    (   catch(Goal, E, true),
        (   var(E)
        ->  true
        ;   throw(error(initialization_error(E, Goal, Ctx), _))
        )
    ;   throw(error(initialization_error(failed, Goal, Ctx), _))
    ).


                 /*******************************
                 *     THREAD INITIALIZATION    *
                 *******************************/

:- meta_predicate
    thread_initialization(0).
:- dynamic
    '$at_thread_initialization'/1.

%!  thread_initialization(:Goal)
%
%   Run Goal now and everytime a new thread is created.

thread_initialization(Goal) :-
    assert('$at_thread_initialization'(Goal)),
    call(Goal),
    !.

'$thread_init' :-
    (   '$at_thread_initialization'(Goal),
        (   call(Goal)
        ->  fail
        ;   fail
        )
    ;   true
    ).


                 /*******************************
                 *     FILE SEARCH PATH (-p)    *
                 *******************************/

%!  '$set_file_search_paths' is det.
%
%   Process -p PathSpec options.

'$set_file_search_paths' :-
    '$cmd_option_val'(search_paths, Paths),
    (   '$member'(Path, Paths),
        atom_chars(Path, Chars),
        (   phrase('$search_path'(Name, Aliases), Chars)
        ->  '$reverse'(Aliases, Aliases1),
            forall('$member'(Alias, Aliases1),
                   asserta(user:file_search_path(Name, Alias)))
        ;   print_message(error, commandline_arg_type(p, Path))
        ),
        fail ; true
    ).

'$search_path'(Name, Aliases) -->
    '$string'(NameChars),
    [=],
    !,
    {atom_chars(Name, NameChars)},
    '$search_aliases'(Aliases).

'$search_aliases'([Alias|More]) -->
    '$string'(AliasChars),
    path_sep,
    !,
    { '$make_alias'(AliasChars, Alias) },
    '$search_aliases'(More).
'$search_aliases'([Alias]) -->
    '$string'(AliasChars),
    '$eos',
    !,
    { '$make_alias'(AliasChars, Alias) }.

path_sep -->
    { current_prolog_flag(windows, true)
    },
    !,
    [;].
path_sep -->
    [:].

'$string'([]) --> [].
'$string'([H|T]) --> [H], '$string'(T).

'$eos'([], []).

'$make_alias'(Chars, Alias) :-
    catch(term_to_atom(Alias, Chars), _, fail),
    (   atom(Alias)
    ;   functor(Alias, F, 1),
        F \== /
    ),
    !.
'$make_alias'(Chars, Alias) :-
    atom_chars(Alias, Chars).


                 /*******************************
                 *   LOADING ASSIOCIATED FILES  *
                 *******************************/

%!  argv_files(-Files) is det.
%
%   Update the Prolog flag =argv=, extracting  the leading directory and
%   files.

argv_files(Files) :-
    current_prolog_flag(argv, Argv),
    no_option_files(Argv, Argv1, Files),
    (   Argv1 \== Argv
    ->  set_prolog_flag(argv, Argv1)
    ;   true
    ).

no_option_files([--|Argv], Argv, []) :- !.
no_option_files([OsScript|Argv], Argv, [Script]) :-
    prolog_to_os_filename(Script, OsScript),
    access_file(Script, read),
    catch(setup_call_cleanup(
              open(Script, read, In),
              ( get_char(In, '#'),
                get_char(In, '!')
              ),
              close(In)),
          _, fail),
    !.
no_option_files([OsFile|Argv0], Argv, [File|T]) :-
    file_name_extension(_, Ext, OsFile),
    user:prolog_file_type(Ext, prolog),
    !,
    prolog_to_os_filename(File, OsFile),
    no_option_files(Argv0, Argv, T).
no_option_files(Argv, Argv, []).

clean_argv :-
    (   current_prolog_flag(argv, [--|Argv])
    ->  set_prolog_flag(argv, Argv)
    ;   true
    ).

%!  associated_files(-Files)
%
%   If SWI-Prolog is started as <exe> <file>.<ext>, where <ext> is
%   the extension registered for associated files, set the Prolog
%   flag associated_file, switch to the directory holding the file
%   and -if possible- adjust the window title.

associated_files([]) :-
    current_prolog_flag(saved_program_class, runtime),
    !,
    clean_argv.
associated_files(Files) :-
    '$set_prolog_file_extension',
    argv_files(Files),
    (   Files = [File|_]
    ->  absolute_file_name(File, AbsFile),
        set_prolog_flag(associated_file, AbsFile),
        set_working_directory(File),
        set_window_title(Files)
    ;   true
    ).

%!  set_working_directory(+File)
%
%   When opening as a GUI application, e.g.,  by opening a file from
%   the Finder/Explorer/..., we typically  want   to  change working
%   directory to the location of  the   primary  file.  We currently
%   detect that we are a GUI app  by the Prolog flag =console_menu=,
%   which is set by swipl-win[.exe].

set_working_directory(File) :-
    current_prolog_flag(console_menu, true),
    access_file(File, read),
    !,
    file_directory_name(File, Dir),
    working_directory(_, Dir).
set_working_directory(_).

set_window_title([File|More]) :-
    current_predicate(system:window_title/2),
    !,
    (   More == []
    ->  Extra = []
    ;   Extra = ['...']
    ),
    atomic_list_concat(['SWI-Prolog --', File | Extra], ' ', Title),
    system:window_title(_, Title).
set_window_title(_).


%!  start_pldoc
%
%   If the option  =|--pldoc[=port]|=  is   given,  load  the  PlDoc
%   system.

start_pldoc :-
    '$cmd_option_val'(pldoc_server, Server),
    (   Server == ''
    ->  call((doc_server(_), doc_browser))
    ;   catch(atom_number(Server, Port), _, fail)
    ->  call(doc_server(Port))
    ;   print_message(error, option_usage(pldoc)),
        halt(1)
    ).
start_pldoc.


%!  load_associated_files(+Files)
%
%   Load Prolog files specified from the commandline.

load_associated_files(Files) :-
    (   '$member'(File, Files),
        load_files(user:File, [expand(false)]),
        fail
    ;   true
    ).

:- if(current_predicate(system:win_registry_get_value/3)).
hkey('HKEY_CURRENT_USER/Software/SWI/Prolog').
hkey('HKEY_LOCAL_MACHINE/Software/SWI/Prolog').

'$set_prolog_file_extension' :-
    hkey(Key),
    catch(win_registry_get_value(Key, fileExtension, Ext0),
          _, fail),
    !,
    (   atom_concat('.', Ext, Ext0)
    ->  true
    ;   Ext = Ext0
    ),
    (   user:prolog_file_type(Ext, prolog)
    ->  true
    ;   asserta(user:prolog_file_type(Ext, prolog))
    ).
:- endif.
'$set_prolog_file_extension'.


                /********************************
                *        TOPLEVEL GOALS         *
                *********************************/

%!  '$initialise' is semidet.
%
%   Called from PL_initialise()  to  do  the   Prolog  part  of  the
%   initialization. If an exception  occurs,   this  is  printed and
%   '$initialise' fails.

'$initialise' :-
    catch(initialise_prolog, E, initialise_error(E)).

initialise_error('$aborted') :- !.
initialise_error(E) :-
    print_message(error, initialization_exception(E)),
    fail.

initialise_prolog :-
    '$clean_history',
    '$run_initialization',
    '$load_system_init_file',
    set_toplevel,
    associated_files(Files),
    '$set_file_search_paths',
    init_debug_flags,
    start_pldoc,
    attach_packs,
    '$cmd_option_val'(init_file, OsFile),
    prolog_to_os_filename(File, OsFile),
    '$load_init_file'(File),
    catch(setup_colors, E, print_message(warning, E)),
    '$load_script_file',
    load_associated_files(Files),
    '$cmd_option_val'(goals, Goals),
    (   Goals == [],
        \+ '$init_goal'(when(_), _, _)
    ->  version                                 % default interactive run
    ;   run_init_goals(Goals),
        (   load_only
        ->  version
        ;   run_program_init,
            run_main_init
        )
    ).

set_toplevel :-
    '$cmd_option_val'(toplevel, TopLevelAtom),
    catch(term_to_atom(TopLevel, TopLevelAtom), E,
          (print_message(error, E),
           halt(1))),
    create_prolog_flag(toplevel_goal, TopLevel, [type(term)]).

load_only :-
    current_prolog_flag(os_argv, OSArgv),
    memberchk('-l', OSArgv),
    current_prolog_flag(argv, Argv),
    \+ memberchk('-l', Argv).

%!  run_init_goals(+Goals) is det.
%
%   Run registered initialization goals  on  order.   If  a  goal fails,
%   execution is halted.

run_init_goals([]).
run_init_goals([H|T]) :-
    run_init_goal(H),
    run_init_goals(T).

run_init_goal(Text) :-
    catch(term_to_atom(Goal, Text), E,
          (   print_message(error, init_goal_syntax(E, Text)),
              halt(2)
          )),
    run_init_goal(Goal, Text).

%!  run_program_init is det.
%
%   Run goals registered using

run_program_init :-
    forall('$init_goal'(when(program), Goal, Ctx),
           run_init_goal(Goal, @(Goal,Ctx))).

run_main_init :-
    findall(Goal-Ctx, '$init_goal'(when(main), Goal, Ctx), Pairs),
    '$last'(Pairs, Goal-Ctx),
    !,
    (   current_prolog_flag(toplevel_goal, default)
    ->  set_prolog_flag(toplevel_goal, halt)
    ;   true
    ),
    run_init_goal(Goal, @(Goal,Ctx)).
run_main_init.

run_init_goal(Goal, Ctx) :-
    (   catch_with_backtrace(user:Goal, E, true)
    ->  (   var(E)
        ->  true
        ;   print_message(error, init_goal_failed(E, Ctx)),
            halt(2)
        )
    ;   (   current_prolog_flag(verbose, silent)
        ->  Level = silent
        ;   Level = error
        ),
        print_message(Level, init_goal_failed(failed, Ctx)),
        halt(1)
    ).

%!  init_debug_flags is det.
%
%   Initialize the various Prolog flags that   control  the debugger and
%   toplevel.

init_debug_flags :-
    once(print_predicate(_, [print], PrintOptions)),
    create_prolog_flag(answer_write_options, PrintOptions, []),
    create_prolog_flag(prompt_alternatives_on, determinism, []),
    create_prolog_flag(toplevel_extra_white_line, true, []),
    create_prolog_flag(toplevel_print_factorized, false, []),
    create_prolog_flag(print_write_options,
                       [ portray(true), quoted(true), numbervars(true) ],
                       []),
    create_prolog_flag(toplevel_residue_vars, false, []),
    '$set_debugger_write_options'(print).

%!  setup_backtrace
%
%   Initialise printing a backtrace.

setup_backtrace :-
    (   \+ current_prolog_flag(backtrace, false),
        load_setup_file(library(prolog_stack))
    ->  true
    ;   true
    ).

%!  setup_colors is det.
%
%   Setup  interactive  usage  by  enabling    colored   output.

setup_colors :-
    (   \+ current_prolog_flag(color_term, false),
        stream_property(user_input, tty(true)),
        stream_property(user_error, tty(true)),
        stream_property(user_output, tty(true)),
        load_setup_file(user:library(ansi_term))
    ->  true
    ;   true
    ).

%!  setup_history
%
%   Enable per-directory persistent history.

setup_history :-
    (   \+ current_prolog_flag(save_history, false),
        stream_property(user_input, tty(true)),
        \+ current_prolog_flag(readline, false),
        load_setup_file(library(prolog_history))
    ->  prolog_history(enable)
    ;   true
    ),
    set_default_history,
    '$load_history'.

%!  setup_readline
%
%   Setup line editing.

setup_readline :-
    (   current_prolog_flag(readline, swipl_win)
    ->  true
    ;   stream_property(user_input, tty(true)),
        current_prolog_flag(tty_control, true),
        \+ getenv('TERM', dumb),
        (   current_prolog_flag(readline, ReadLine)
        ->  true
        ;   ReadLine = true
        ),
        readline_library(ReadLine, Library),
        load_setup_file(library(Library))
    ->  set_prolog_flag(readline, Library)
    ;   set_prolog_flag(readline, false)
    ).

readline_library(true, Library) :-
    !,
    preferred_readline(Library).
readline_library(false, _) :-
    !,
    fail.
readline_library(Library, Library).

preferred_readline(editline).
preferred_readline(readline).

%!  load_setup_file(+File) is semidet.
%
%   Load a file and fail silently if the file does not exist.

load_setup_file(File) :-
    catch(load_files(File,
                     [ silent(true),
                       if(not_loaded)
                     ]), _, fail).


:- '$hide'('$toplevel'/0).              % avoid in the GUI stacktrace

%!  '$toplevel'
%
%   Called from PL_toplevel()

'$toplevel' :-
    '$runtoplevel',
    print_message(informational, halt).

%!  '$runtoplevel'
%
%   Actually run the toplevel. The values   `default`  and `prolog` both
%   start the interactive toplevel, where `prolog` implies the user gave
%   =|-t prolog|=.
%
%   @see prolog/0 is the default interactive toplevel

'$runtoplevel' :-
    current_prolog_flag(toplevel_goal, TopLevel0),
    toplevel_goal(TopLevel0, TopLevel),
    user:TopLevel.

:- dynamic  setup_done/0.
:- volatile setup_done/0.

toplevel_goal(default, '$query_loop') :-
    !,
    setup_interactive.
toplevel_goal(prolog, '$query_loop') :-
    !,
    setup_interactive.
toplevel_goal(Goal, Goal).

setup_interactive :-
    setup_done,
    !.
setup_interactive :-
    asserta(setup_done),
    catch(setup_backtrace, E, print_message(warning, E)),
    catch(setup_readline,  E, print_message(warning, E)),
    catch(setup_history,   E, print_message(warning, E)).

%!  '$compile'
%
%   Toplevel called when invoked with -c option.

'$compile' :-
    '$load_system_init_file',
    '$set_file_search_paths',
    init_debug_flags,
    '$run_initialization',
    attach_packs,
    catch('$compile_wic', E, (print_message(error, E), halt(1))).

%!  '$config'
%
%   Toplevel when invoked with --dump-runtime-variables

'$config' :-
    '$load_system_init_file',
    '$set_file_search_paths',
    init_debug_flags,
    '$run_initialization',
    load_files(library(prolog_config)),
    (   catch(prolog_dump_runtime_variables, E,
              (print_message(error, E), halt(1)))
    ->  true
    ;   print_message(error, error(goal_failed(prolog_dump_runtime_variables),_))
    ).


                /********************************
                *    USER INTERACTIVE LOOP      *
                *********************************/

%!  prolog
%
%   Run the Prolog toplevel. This is now  the same as break/0, which
%   pretends  to  be  in  a  break-level    if  there  is  a  parent
%   environment.

prolog :-
    break.

:- create_prolog_flag(toplevel_mode, backtracking, []).

%!  '$query_loop'
%
%   Run the normal Prolog query loop.  Note   that  the query is not
%   protected by catch/3. Dealing with  unhandled exceptions is done
%   by the C-function query_loop().  This   ensures  that  unhandled
%   exceptions are really unhandled (in Prolog).

'$query_loop' :-
    current_prolog_flag(toplevel_mode, recursive),
    !,
    break_level(Level),
    read_expanded_query(Level, Query, Bindings),
    (   Query == end_of_file
    ->  print_message(query, query(eof))
    ;   '$call_no_catch'('$execute'(Query, Bindings)),
        (   current_prolog_flag(toplevel_mode, recursive)
        ->  '$query_loop'
        ;   '$switch_toplevel_mode'(backtracking),
            '$query_loop'           % Maybe throw('$switch_toplevel_mode')?
        )
    ).
'$query_loop' :-
    break_level(BreakLev),
    repeat,
        read_expanded_query(BreakLev, Query, Bindings),
        (   Query == end_of_file
        ->  !, print_message(query, query(eof))
        ;   '$execute'(Query, Bindings),
            (   current_prolog_flag(toplevel_mode, recursive)
            ->  !,
                '$switch_toplevel_mode'(recursive),
                '$query_loop'
            ;   fail
            )
        ).

break_level(BreakLev) :-
    (   current_prolog_flag(break_level, BreakLev)
    ->  true
    ;   BreakLev = -1
    ).

read_expanded_query(BreakLev, ExpandedQuery, ExpandedBindings) :-
    '$current_typein_module'(TypeIn),
    (   stream_property(user_input, tty(true))
    ->  '$system_prompt'(TypeIn, BreakLev, Prompt),
        prompt(Old, '|    ')
    ;   Prompt = '',
        prompt(Old, '')
    ),
    trim_stacks,
    repeat,
      read_query(Prompt, Query, Bindings),
      prompt(_, Old),
      catch(call_expand_query(Query, ExpandedQuery,
                              Bindings, ExpandedBindings),
            Error,
            (print_message(error, Error), fail)),
    !.


%!  read_query(+Prompt, -Goal, -Bindings) is det.
%
%   Read the next query. The first  clause   deals  with  the case where
%   !-based history is enabled. The second is   used  if we have command
%   line editing.

read_query(Prompt, Goal, Bindings) :-
    current_prolog_flag(history, N),
    integer(N), N > 0,
    !,
    read_history(h, '!h',
                 [trace, end_of_file],
                 Prompt, Goal, Bindings).
read_query(Prompt, Goal, Bindings) :-
    remove_history_prompt(Prompt, Prompt1),
    repeat,                                 % over syntax errors
    prompt1(Prompt1),
    read_query_line(user_input, Line),
    '$save_history_line'(Line),             % save raw line (edit syntax errors)
    '$current_typein_module'(TypeIn),
    catch(read_term_from_atom(Line, Goal,
                              [ variable_names(Bindings),
                                module(TypeIn)
                              ]), E,
          (   print_message(error, E),
              fail
          )),
    !,
    '$save_history_event'(Line).            % save event (no syntax errors)

%!  read_query_line(+Input, -Line) is det.

read_query_line(Input, Line) :-
    catch(read_term_as_atom(Input, Line), Error, true),
    save_debug_after_read,
    (   var(Error)
    ->  true
    ;   Error = error(syntax_error(_),_)
    ->  print_message(error, Error),
        fail
    ;   print_message(error, Error),
        throw(Error)
    ).

%!  read_term_as_atom(+Input, -Line)
%
%   Read the next term as an  atom  and   skip  to  the newline or a
%   non-space character.

read_term_as_atom(In, Line) :-
    '$raw_read'(In, Line),
    (   Line == end_of_file
    ->  true
    ;   skip_to_nl(In)
    ).

%!  skip_to_nl(+Input) is det.
%
%   Read input after the term. Skips   white  space and %... comment
%   until the end of the line or a non-blank character.

skip_to_nl(In) :-
    repeat,
    peek_char(In, C),
    (   C == '%'
    ->  skip(In, '\n')
    ;   char_type(C, space)
    ->  get_char(In, _),
        C == '\n'
    ;   true
    ),
    !.

remove_history_prompt('', '') :- !.
remove_history_prompt(Prompt0, Prompt) :-
    atom_chars(Prompt0, Chars0),
    clean_history_prompt_chars(Chars0, Chars1),
    delete_leading_blanks(Chars1, Chars),
    atom_chars(Prompt, Chars).

clean_history_prompt_chars([], []).
clean_history_prompt_chars(['~', !|T], T) :- !.
clean_history_prompt_chars([H|T0], [H|T]) :-
    clean_history_prompt_chars(T0, T).

delete_leading_blanks([' '|T0], T) :-
    !,
    delete_leading_blanks(T0, T).
delete_leading_blanks(L, L).


%!  set_default_history
%
%   Enable !-based numbered command history. This  is enabled by default
%   if we are not running under GNU-emacs  and   we  do not have our own
%   line editing.

set_default_history :-
    current_prolog_flag(history, _),
    !.
set_default_history :-
    (   (   \+ current_prolog_flag(readline, false)
        ;   current_prolog_flag(emacs_inferior_process, true)
        )
    ->  create_prolog_flag(history, 0, [])
    ;   create_prolog_flag(history, 25, [])
    ).


                 /*******************************
                 *        TOPLEVEL DEBUG        *
                 *******************************/

%!  save_debug_after_read
%
%   Called right after the toplevel read to save the debug status if
%   it was modified from the GUI thread using e.g.
%
%     ==
%     thread_signal(main, gdebug)
%     ==
%
%   @bug Ideally, the prompt would change if debug mode is enabled.
%        That is hard to realise with all the different console
%        interfaces supported by SWI-Prolog.

save_debug_after_read :-
    current_prolog_flag(debug, true),
    !,
    save_debug.
save_debug_after_read.

save_debug :-
    (   tracing,
        notrace
    ->  Tracing = true
    ;   Tracing = false
    ),
    current_prolog_flag(debug, Debugging),
    set_prolog_flag(debug, false),
    create_prolog_flag(query_debug_settings,
                       debug(Debugging, Tracing), []).

restore_debug :-
    current_prolog_flag(query_debug_settings, debug(Debugging, Tracing)),
    set_prolog_flag(debug, Debugging),
    (   Tracing == true
    ->  trace
    ;   true
    ).

:- initialization
    create_prolog_flag(query_debug_settings, debug(false, false), []).


                /********************************
                *            PROMPTING          *
                ********************************/

'$system_prompt'(Module, BrekLev, Prompt) :-
    current_prolog_flag(toplevel_prompt, PAtom),
    atom_codes(PAtom, P0),
    (    Module \== user
    ->   '$substitute'('~m', [Module, ': '], P0, P1)
    ;    '$substitute'('~m', [], P0, P1)
    ),
    (    BrekLev > 0
    ->   '$substitute'('~l', ['[', BrekLev, '] '], P1, P2)
    ;    '$substitute'('~l', [], P1, P2)
    ),
    current_prolog_flag(query_debug_settings, debug(Debugging, Tracing)),
    (    Tracing == true
    ->   '$substitute'('~d', ['[trace] '], P2, P3)
    ;    Debugging == true
    ->   '$substitute'('~d', ['[debug] '], P2, P3)
    ;    '$substitute'('~d', [], P2, P3)
    ),
    atom_chars(Prompt, P3).

'$substitute'(From, T, Old, New) :-
    atom_codes(From, FromCodes),
    phrase(subst_chars(T), T0),
    '$append'(Pre, S0, Old),
    '$append'(FromCodes, Post, S0) ->
    '$append'(Pre, T0, S1),
    '$append'(S1, Post, New),
    !.
'$substitute'(_, _, Old, Old).

subst_chars([]) -->
    [].
subst_chars([H|T]) -->
    { atomic(H),
      !,
      atom_codes(H, Codes)
    },
    Codes,
    subst_chars(T).
subst_chars([H|T]) -->
    H,
    subst_chars(T).


                /********************************
                *           EXECUTION           *
                ********************************/

%!  '$execute'(Goal, Bindings) is det.
%
%   Execute Goal using Bindings.

'$execute'(Var, _) :-
    var(Var),
    !,
    print_message(informational, var_query(Var)).
'$execute'(Goal, Bindings) :-
    '$current_typein_module'(TypeIn),
    '$dwim_correct_goal'(TypeIn:Goal, Bindings, Corrected),
    !,
    setup_call_cleanup(
        '$set_source_module'(M0, TypeIn),
        expand_goal(Corrected, Expanded),
        '$set_source_module'(M0)),
    print_message(silent, toplevel_goal(Expanded, Bindings)),
    '$execute_goal2'(Expanded, Bindings).
'$execute'(_, _) :-
    notrace,
    print_message(query, query(no)).

'$execute_goal2'(Goal, Bindings) :-
    restore_debug,
    residue_vars(Goal, Vars),
    deterministic(Det),
    (   save_debug
    ;   restore_debug, fail
    ),
    flush_output(user_output),
    call_expand_answer(Bindings, NewBindings),
    (    \+ \+ write_bindings(NewBindings, Vars, Det)
    ->   !
    ).
'$execute_goal2'(_, _) :-
    save_debug,
    print_message(query, query(no)).

residue_vars(Goal, Vars) :-
    current_prolog_flag(toplevel_residue_vars, true),
    !,
    call_residue_vars(Goal, Vars).
residue_vars(Goal, []) :-
    toplevel_call(Goal).

toplevel_call(Goal) :-
    call(Goal),
    no_lco.

no_lco.

%!  write_bindings(+Bindings, +ResidueVars, +Deterministic) is semidet.
%
%   Write   bindings   resulting   from   a     query.    The   flag
%   prompt_alternatives_on determines whether the   user is prompted
%   for alternatives. =groundness= gives   the  classical behaviour,
%   =determinism= is considered more adequate and informative.
%
%   Succeeds if the user accepts the answer and fails otherwise.
%
%   @arg ResidueVars are the residual constraints and provided if
%        the prolog flag `toplevel_residue_vars` is set to
%        `project`.

write_bindings(Bindings, ResidueVars, Det) :-
    '$current_typein_module'(TypeIn),
    translate_bindings(Bindings, Bindings1, ResidueVars, TypeIn:Residuals),
    write_bindings2(Bindings1, Residuals, Det).

write_bindings2([], Residuals, _) :-
    current_prolog_flag(prompt_alternatives_on, groundness),
    !,
    print_message(query, query(yes(Residuals))).
write_bindings2(Bindings, Residuals, true) :-
    current_prolog_flag(prompt_alternatives_on, determinism),
    !,
    print_message(query, query(yes(Bindings, Residuals))).
write_bindings2(Bindings, Residuals, _Det) :-
    repeat,
        print_message(query, query(more(Bindings, Residuals))),
        get_respons(Action),
    (   Action == redo
    ->  !, fail
    ;   Action == show_again
    ->  fail
    ;   !,
        print_message(query, query(done))
    ).

%!  residual_goals(:NonTerminal)
%
%   Directive that registers NonTerminal as a collector for residual
%   goals.

:- multifile
    residual_goal_collector/1.

:- meta_predicate
    residual_goals(2).

residual_goals(NonTerminal) :-
    throw(error(context_error(nodirective, residual_goals(NonTerminal)), _)).

system:term_expansion((:- residual_goals(NonTerminal)),
                      '$toplevel':residual_goal_collector(M2:Head)) :-
    prolog_load_context(module, M),
    strip_module(M:NonTerminal, M2, Head),
    '$must_be'(callable, Head).

%!  prolog:residual_goals// is det.
%
%   DCG that collects residual goals that   are  not associated with
%   the answer through attributed variables.

:- public prolog:residual_goals//0.

prolog:residual_goals -->
    { findall(NT, residual_goal_collector(NT), NTL) },
    collect_residual_goals(NTL).

collect_residual_goals([]) --> [].
collect_residual_goals([H|T]) -->
    ( call(H) -> [] ; [] ),
    collect_residual_goals(T).



%!  prolog:translate_bindings(+Bindings0, -Bindings, +ResidueVars,
%!                            +ResidualGoals, -Residuals) is det.
%
%   Translate the raw variable bindings  resulting from successfully
%   completing a query into a  binding   list  and  list of residual
%   goals suitable for human consumption.
%
%   @arg    Bindings is a list of binding(Vars,Value,Substitutions),
%           where Vars is a list of variable names. E.g.
%           binding(['A','B'],42,[])` means that both the variable
%           A and B have the value 42. Values may contain terms
%           '$VAR'(Name) to indicate sharing with a given variable.
%           Value is always an acyclic term. If cycles appear in the
%           answer, Substitutions contains a list of substitutions
%           that restore the original term.
%
%   @arg    Residuals is a pair of two lists representing residual
%           goals. The first element of the pair are residuals
%           related to the query variables and the second are
%           related that are disconnected from the query.

:- public
    prolog:translate_bindings/5.
:- meta_predicate
    prolog:translate_bindings(+, -, +, +, :).

prolog:translate_bindings(Bindings0, Bindings, ResVars, ResGoals, Residuals) :-
    translate_bindings(Bindings0, Bindings, ResVars, ResGoals, Residuals).

translate_bindings(Bindings0, Bindings, ResidueVars, Residuals) :-
    prolog:residual_goals(ResidueGoals, []),
    translate_bindings(Bindings0, Bindings, ResidueVars, ResidueGoals,
                       Residuals).

translate_bindings(Bindings0, Bindings, [], [], _:[]-[]) :-
    term_attvars(Bindings0, []),
    !,
    join_same_bindings(Bindings0, Bindings1),
    factorize_bindings(Bindings1, Bindings2),
    bind_vars(Bindings2, Bindings3),
    filter_bindings(Bindings3, Bindings).
translate_bindings(Bindings0, Bindings, ResidueVars, ResGoals0,
                   TypeIn:Residuals-HiddenResiduals) :-
    project_constraints(Bindings0, ResidueVars),
    hidden_residuals(ResidueVars, Bindings0, HiddenResiduals0),
    omit_qualifiers(HiddenResiduals0, TypeIn, HiddenResiduals),
    copy_term(Bindings0+ResGoals0, Bindings1+ResGoals1, Residuals0),
    '$append'(ResGoals1, Residuals0, Residuals1),
    omit_qualifiers(Residuals1, TypeIn, Residuals),
    join_same_bindings(Bindings1, Bindings2),
    factorize_bindings(Bindings2, Bindings3),
    bind_vars(Bindings3, Bindings4),
    filter_bindings(Bindings4, Bindings).

hidden_residuals(ResidueVars, Bindings, Goal) :-
    term_attvars(ResidueVars, Remaining),
    term_attvars(Bindings, QueryVars),
    subtract_vars(Remaining, QueryVars, HiddenVars),
    copy_term(HiddenVars, _, Goal).

subtract_vars(All, Subtract, Remaining) :-
    sort(All, AllSorted),
    sort(Subtract, SubtractSorted),
    ord_subtract(AllSorted, SubtractSorted, Remaining).

ord_subtract([], _Not, []).
ord_subtract([H1|T1], L2, Diff) :-
    diff21(L2, H1, T1, Diff).

diff21([], H1, T1, [H1|T1]).
diff21([H2|T2], H1, T1, Diff) :-
    compare(Order, H1, H2),
    diff3(Order, H1, T1, H2, T2, Diff).

diff12([], _H2, _T2, []).
diff12([H1|T1], H2, T2, Diff) :-
    compare(Order, H1, H2),
    diff3(Order, H1, T1, H2, T2, Diff).

diff3(<,  H1, T1,  H2, T2, [H1|Diff]) :-
    diff12(T1, H2, T2, Diff).
diff3(=, _H1, T1, _H2, T2, Diff) :-
    ord_subtract(T1, T2, Diff).
diff3(>,  H1, T1, _H2, T2, Diff) :-
    diff21(T2, H1, T1, Diff).


%!  project_constraints(+Bindings, +ResidueVars) is det.
%
%   Call   <module>:project_attributes/2   if   the    Prolog   flag
%   `toplevel_residue_vars` is set to `project`.

project_constraints(Bindings, ResidueVars) :-
    !,
    term_attvars(Bindings, AttVars),
    phrase(attribute_modules(AttVars), Modules0),
    sort(Modules0, Modules),
    term_variables(Bindings, QueryVars),
    project_attributes(Modules, QueryVars, ResidueVars).
project_constraints(_, _).

project_attributes([], _, _).
project_attributes([M|T], QueryVars, ResidueVars) :-
    (   current_predicate(M:project_attributes/2),
        catch(M:project_attributes(QueryVars, ResidueVars), E,
              print_message(error, E))
    ->  true
    ;   true
    ),
    project_attributes(T, QueryVars, ResidueVars).

attribute_modules([]) --> [].
attribute_modules([H|T]) -->
    { get_attrs(H, Attrs) },
    attrs_modules(Attrs),
    attribute_modules(T).

attrs_modules([]) --> [].
attrs_modules(att(Module, _, More)) -->
    [Module],
    attrs_modules(More).


%!  join_same_bindings(Bindings0, Bindings)
%
%   Join variables that are bound to the   same  value. Note that we
%   return the _last_ value. This is   because the factorization may
%   be different and ultimately the names will   be  printed as V1 =
%   V2, ... VN = Value. Using the  last, Value has the factorization
%   of VN.

join_same_bindings([], []).
join_same_bindings([Name=V0|T0], [[Name|Names]=V|T]) :-
    take_same_bindings(T0, V0, V, Names, T1),
    join_same_bindings(T1, T).

take_same_bindings([], Val, Val, [], []).
take_same_bindings([Name=V1|T0], V0, V, [Name|Names], T) :-
    V0 == V1,
    !,
    take_same_bindings(T0, V1, V, Names, T).
take_same_bindings([Pair|T0], V0, V, Names, [Pair|T]) :-
    take_same_bindings(T0, V0, V, Names, T).


%!  omit_qualifiers(+QGoals, +TypeIn, -Goals) is det.
%
%   Omit unneeded module qualifiers  from   QGoals  relative  to the
%   given module TypeIn.


omit_qualifiers([], _, []).
omit_qualifiers([Goal0|Goals0], TypeIn, [Goal|Goals]) :-
    omit_qualifier(Goal0, TypeIn, Goal),
    omit_qualifiers(Goals0, TypeIn, Goals).

omit_qualifier(M:G0, TypeIn, G) :-
    M == TypeIn,
    !,
    omit_meta_qualifiers(G0, TypeIn, G).
omit_qualifier(M:G0, TypeIn, G) :-
    predicate_property(TypeIn:G0, imported_from(M)),
    \+ predicate_property(G0, transparent),
    !,
    G0 = G.
omit_qualifier(_:G0, _, G) :-
    predicate_property(G0, built_in),
    \+ predicate_property(G0, transparent),
    !,
    G0 = G.
omit_qualifier(M:G0, _, M:G) :-
    atom(M),
    !,
    omit_meta_qualifiers(G0, M, G).
omit_qualifier(G0, TypeIn, G) :-
    omit_meta_qualifiers(G0, TypeIn, G).

omit_meta_qualifiers(V, _, V) :-
    var(V),
    !.
omit_meta_qualifiers((QA,QB), TypeIn, (A,B)) :-
    !,
    omit_qualifier(QA, TypeIn, A),
    omit_qualifier(QB, TypeIn, B).
omit_meta_qualifiers(freeze(V, QGoal), TypeIn, freeze(V, Goal)) :-
    callable(QGoal),
    !,
    omit_qualifier(QGoal, TypeIn, Goal).
omit_meta_qualifiers(when(Cond, QGoal), TypeIn, when(Cond, Goal)) :-
    callable(QGoal),
    !,
    omit_qualifier(QGoal, TypeIn, Goal).
omit_meta_qualifiers(G, _, G).


%!  bind_vars(+BindingsIn, -Bindings)
%
%   Bind variables to '$VAR'(Name), so they are printed by the names
%   used in the query. Note that by   binding  in the reverse order,
%   variables bound to one another come out in the natural order.

bind_vars(Bindings0, Bindings) :-
    bind_query_vars(Bindings0, Bindings, SNames),
    bind_skel_vars(Bindings, Bindings, SNames, 1, _).

bind_query_vars([], [], []).
bind_query_vars([binding(Names,Var,[Var2=Cycle])|T0],
                [binding(Names,Cycle,[])|T], [Name|SNames]) :-
    Var == Var2,                   % also implies var(Var)
    !,
    '$last'(Names, Name),
    Var = '$VAR'(Name),
    bind_query_vars(T0, T, SNames).
bind_query_vars([B|T0], [B|T], AllNames) :-
    B = binding(Names,Var,Skel),
    bind_query_vars(T0, T, SNames),
    (   var(Var), \+ attvar(Var), Skel == []
    ->  AllNames = [Name|SNames],
        '$last'(Names, Name),
        Var = '$VAR'(Name)
    ;   AllNames = SNames
    ).



bind_skel_vars([], _, _, N, N).
bind_skel_vars([binding(_,_,Skel)|T], Bindings, SNames, N0, N) :-
    bind_one_skel_vars(Skel, Bindings, SNames, N0, N1),
    bind_skel_vars(T, Bindings, SNames, N1, N).

%!  bind_one_skel_vars(+Subst, +Bindings, +VarName, +N0, -N)
%
%   Give names to the factorized variables that   do not have a name
%   yet. This introduces names  _S<N>,   avoiding  duplicates.  If a
%   factorized variable shares with another binding, use the name of
%   that variable.
%
%   @tbd    Consider the call below. We could remove either of the
%           A = x(1).  Which is best?
%
%           ==
%           ?- A = x(1), B = a(A,A).
%           A = x(1),
%           B = a(A, A), % where
%               A = x(1).
%           ==

bind_one_skel_vars([], _, _, N, N).
bind_one_skel_vars([Var=Value|T], Bindings, Names, N0, N) :-
    (   var(Var)
    ->  (   '$member'(binding(Names, VVal, []), Bindings),
            same_term(Value, VVal)
        ->  '$last'(Names, VName),
            Var = '$VAR'(VName),
            N2 = N0
        ;   between(N0, infinite, N1),
            atom_concat('_S', N1, Name),
            \+ memberchk(Name, Names),
            !,
            Var = '$VAR'(Name),
            N2 is N1 + 1
        )
    ;   N2 = N0
    ),
    bind_one_skel_vars(T, Bindings, Names, N2, N).


%!  factorize_bindings(+Bindings0, -Factorized)
%
%   Factorize cycles and sharing in the bindings.

factorize_bindings([], []).
factorize_bindings([Name=Value|T0], [binding(Name, Skel, Subst)|T]) :-
    '$factorize_term'(Value, Skel, Subst0),
    (   current_prolog_flag(toplevel_print_factorized, true)
    ->  Subst = Subst0
    ;   only_cycles(Subst0, Subst)
    ),
    factorize_bindings(T0, T).


only_cycles([], []).
only_cycles([B|T0], List) :-
    (   B = (Var=Value),
        Var = Value,
        acyclic_term(Var)
    ->  only_cycles(T0, List)
    ;   List = [B|T],
        only_cycles(T0, T)
    ).


%!  filter_bindings(+Bindings0, -Bindings)
%
%   Remove bindings that must not be printed. There are two of them:
%   Variables whose name start with '_'  and variables that are only
%   bound to themselves (or, unbound).

filter_bindings([], []).
filter_bindings([H0|T0], T) :-
    hide_vars(H0, H),
    (   (   arg(1, H, [])
        ;   self_bounded(H)
        )
    ->  filter_bindings(T0, T)
    ;   T = [H|T1],
        filter_bindings(T0, T1)
    ).

hide_vars(binding(Names0, Skel, Subst), binding(Names, Skel, Subst)) :-
    hide_names(Names0, Skel, Subst, Names).

hide_names([], _, _, []).
hide_names([Name|T0], Skel, Subst, T) :-
    (   sub_atom(Name, 0, _, _, '_'),
        current_prolog_flag(toplevel_print_anon, false),
        sub_atom(Name, 1, 1, _, Next),
        char_type(Next, prolog_var_start)
    ->  true
    ;   Subst == [],
        Skel == '$VAR'(Name)
    ),
    !,
    hide_names(T0, Skel, Subst, T).
hide_names([Name|T0], Skel, Subst, [Name|T]) :-
    hide_names(T0, Skel, Subst, T).

self_bounded(binding([Name], Value, [])) :-
    Value == '$VAR'(Name).

%!  get_respons(-Action)
%
%   Read the continuation entered by the user.

get_respons(Action) :-
    repeat,
        flush_output(user_output),
        get_single_char(Char),
        answer_respons(Char, Action),
        (   Action == again
        ->  print_message(query, query(action)),
            fail
        ;   !
        ).

answer_respons(Char, again) :-
    '$in_reply'(Char, '?h'),
    !,
    print_message(help, query(help)).
answer_respons(Char, redo) :-
    '$in_reply'(Char, ';nrNR \t'),
    !,
    print_message(query, if_tty([ansi(bold, ';', [])])).
answer_respons(Char, redo) :-
    '$in_reply'(Char, 'tT'),
    !,
    trace,
    save_debug,
    print_message(query, if_tty([ansi(bold, '; [trace]', [])])).
answer_respons(Char, continue) :-
    '$in_reply'(Char, 'ca\n\ryY.'),
    !,
    print_message(query, if_tty([ansi(bold, '.', [])])).
answer_respons(0'b, show_again) :-
    !,
    break.
answer_respons(Char, show_again) :-
    print_predicate(Char, Pred, Options),
    !,
    print_message(query, if_tty(['~w'-[Pred]])),
    set_prolog_flag(answer_write_options, Options).
answer_respons(-1, show_again) :-
    !,
    print_message(query, halt('EOF')),
    halt(0).
answer_respons(Char, again) :-
    print_message(query, no_action(Char)).

print_predicate(0'w, [write], [ quoted(true),
                                spacing(next_argument)
                              ]).
print_predicate(0'p, [print], [ quoted(true),
                                portray(true),
                                max_depth(10),
                                spacing(next_argument)
                              ]).


                 /*******************************
                 *          EXPANSION           *
                 *******************************/

:- user:dynamic(expand_query/4).
:- user:multifile(expand_query/4).

call_expand_query(Goal, Expanded, Bindings, ExpandedBindings) :-
    user:expand_query(Goal, Expanded, Bindings, ExpandedBindings),
    !.
call_expand_query(Goal, Expanded, Bindings, ExpandedBindings) :-
    toplevel_variables:expand_query(Goal, Expanded, Bindings, ExpandedBindings),
    !.
call_expand_query(Goal, Goal, Bindings, Bindings).


:- user:dynamic(expand_answer/2).
:- user:multifile(expand_answer/2).

call_expand_answer(Goal, Expanded) :-
    user:expand_answer(Goal, Expanded),
    !.
call_expand_answer(Goal, Expanded) :-
    toplevel_variables:expand_answer(Goal, Expanded),
    !.
call_expand_answer(Goal, Goal).
