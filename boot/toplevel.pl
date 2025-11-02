/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1985-2025, University of Amsterdam
                              VU University Amsterdam
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
            '$execute_query'/3,         % +Query, +Bindings, -Truth
            residual_goals/1,           % +Callable
            (initialization)/1,         % initialization goal (directive)
            '$thread_init'/0,           % initialise thread
            (thread_initialization)/1   % thread initialization goal
            ]).


                 /*******************************
                 *         VERSION BANNER       *
                 *******************************/

:- dynamic prolog:version_msg/1.
:- multifile prolog:version_msg/1.

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

%!  load_init_file(+ScriptMode) is det.
%
%   Load the user customization file. This can  be done using ``swipl -f
%   file`` or simply using ``swipl``. In the   first  case we search the
%   file both directly and over  the   alias  `user_app_config`.  In the
%   latter case we only use the alias.

load_init_file(_) :-
    '$cmd_option_val'(init_file, OsFile),
    !,
    prolog_to_os_filename(File, OsFile),
    load_init_file(File, explicit).
load_init_file(prolog) :-
    !,
    load_init_file('init.pl', implicit).
load_init_file(none) :-
    !,
    load_init_file('init.pl', implicit).
load_init_file(_).

%!  loaded_init_file(?Base, ?AbsFile)
%
%   Used by prolog_load_context/2 to confirm we are loading a script.

:- dynamic
    loaded_init_file/2.             % already loaded init files

load_init_file(none, _) :- !.
load_init_file(Base, _) :-
    loaded_init_file(Base, _),
    !.
load_init_file(InitFile, explicit) :-
    exists_file(InitFile),
    !,
    ensure_loaded(user:InitFile).
load_init_file(Base, _) :-
    absolute_file_name(user_app_config(Base), InitFile,
                       [ access(read),
                         file_errors(fail)
                       ]),
    !,
    asserta(loaded_init_file(Base, InitFile)),
    load_files(user:InitFile,
               [ scope_settings(false)
               ]).
load_init_file('init.pl', implicit) :-
    (   current_prolog_flag(windows, true),
        absolute_file_name(user_profile('swipl.ini'), InitFile,
                           [ access(read),
                             file_errors(fail)
                           ])
    ;   expand_file_name('~/.swiplrc', [InitFile]),
        exists_file(InitFile)
    ),
    !,
    print_message(warning, backcomp(init_file_moved(InitFile))).
load_init_file(_, _).

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
        load_files(user:Path),
        load_files(user:More)
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
    '$set_prolog_file_extension',
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

%!  '$thread_init'
%
%   Called by start_thread() from pl-thread.c before the thread's goal.

'$thread_init' :-
    set_prolog_flag(toplevel_thread, false),
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
    { current_prolog_flag(path_sep, Sep) },
    [Sep].

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

%!  argv_prolog_files(-Files, -ScriptMode) is det.
%
%   Update the Prolog flag `argv`, extracting  the leading script files.
%   This is called after the C based  parser removed Prolog options such
%   as ``-q``, ``-f none``, etc.  These   options  are available through
%   '$cmd_option_val'/2.
%
%   Our task is to update the Prolog flag   `argv`  and return a list of
%   the files to be loaded.   The rules are:
%
%     - If we find ``--`` all remaining options must go to `argv`
%     - If we find *.pl files, these are added to Files and possibly
%       remaining arguments are "script" arguments.
%     - If we find an existing file, this is Files and possibly
%       remaining arguments are "script" arguments.
%     - File we find [search:]name, find search(name) as Prolog file,
%       make this the content of `Files` and pass the remainder as
%       options to `argv`.
%
%   @arg ScriptMode is one of
%
%     - exe
%       Program is a saved state
%     - prolog
%       One or more *.pl files on commandline
%     - script
%       Single existing file on commandline
%     - app
%       [path:]cli-name on commandline
%     - none
%       Normal interactive session

argv_prolog_files([], exe) :-
    current_prolog_flag(saved_program_class, runtime),
    !,
    clean_argv.
argv_prolog_files(Files, ScriptMode) :-
    current_prolog_flag(argv, Argv),
    no_option_files(Argv, Argv1, Files, ScriptMode),
    (   (   nonvar(ScriptMode)
        ;   Argv1 == []
        )
    ->  (   Argv1 \== Argv
        ->  set_prolog_flag(argv, Argv1)
        ;   true
        )
    ;   '$usage',
        halt(1)
    ).

no_option_files([--|Argv], Argv, [], ScriptMode) :-
    !,
    (   ScriptMode = none
    ->  true
    ;   true
    ).
no_option_files([Opt|_], _, _, ScriptMode) :-
    var(ScriptMode),
    sub_atom(Opt, 0, _, _, '-'),
    !,
    '$usage',
    halt(1).
no_option_files([OsFile|Argv0], Argv, [File|T], ScriptMode) :-
    file_name_extension(_, Ext, OsFile),
    user:prolog_file_type(Ext, prolog),
    !,
    ScriptMode = prolog,
    prolog_to_os_filename(File, OsFile),
    no_option_files(Argv0, Argv, T, ScriptMode).
no_option_files([OsScript|Argv], Argv, [Script], ScriptMode) :-
    var(ScriptMode),
    !,
    prolog_to_os_filename(PlScript, OsScript),
    (   exists_file(PlScript)
    ->  Script = PlScript,
        ScriptMode = script
    ;   cli_script(OsScript, Script)
    ->  ScriptMode = app,
        set_prolog_flag(app_name, OsScript)
    ;   '$existence_error'(file, PlScript)
    ).
no_option_files(Argv, Argv, [], ScriptMode) :-
    (   ScriptMode = none
    ->  true
    ;   true
    ).

cli_script(CLI, Script) :-
    (   sub_atom(CLI, Pre, _, Post, ':')
    ->  sub_atom(CLI, 0, Pre, _, SearchPath),
        sub_atom(CLI, _, Post, 0, Base),
        Spec =.. [SearchPath, Base]
    ;   Spec = app(CLI)
    ),
    absolute_file_name(Spec, Script,
                       [ file_type(prolog),
                         access(exist),
                         file_errors(fail)
                       ]).

clean_argv :-
    (   current_prolog_flag(argv, [--|Argv])
    ->  set_prolog_flag(argv, Argv)
    ;   true
    ).

%!  win_associated_files(+Files)
%
%   If SWI-Prolog is started as <exe> <file>.<ext>, where <ext> is
%   the extension registered for associated files, set the Prolog
%   flag associated_file, switch to the directory holding the file
%   and -if possible- adjust the window title.

win_associated_files(Files) :-
    (   Files = [File|_]
    ->  absolute_file_name(File, AbsFile),
        set_prolog_flag(associated_file, AbsFile),
        forall(prolog:set_app_file_config(Files), true)
    ;   true
    ).

:- multifile
    prolog:set_app_file_config/1.               % +Files

%!  start_pldoc
%
%   If the option ``--pldoc[=port]`` is given, load the PlDoc system.

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
    load_files(user:Files).

hkey('HKEY_CURRENT_USER/Software/SWI/Prolog').
hkey('HKEY_LOCAL_MACHINE/Software/SWI/Prolog').

'$set_prolog_file_extension' :-
    current_prolog_flag(windows, true),
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

initialise_error(unwind(abort)) :- !.
initialise_error(unwind(halt(_))) :- !.
initialise_error(E) :-
    print_message(error, initialization_exception(E)),
    fail.

initialise_prolog :-
    apply_defines,
    init_optimise,
    '$run_initialization',
    '$load_system_init_file',                   % -F file
    set_toplevel,                               % set `toplevel_goal` flag from -t
    '$set_file_search_paths',                   % handle -p alias=dir[:dir]*
    init_debug_flags,
    setup_app,
    start_pldoc,                                % handle --pldoc[=port]
    main_thread_init.

%!  main_thread_init
%
%   Deal with the _Epilog_ toplevel. If  the   flag  `epilog` is set and
%   xpce is around, create an epilog window   and complete the user part
%   of the initialization in the epilog thread.

main_thread_init :-
    current_prolog_flag(epilog, true),
    thread_self(main),
    current_prolog_flag(xpce, true),
    exists_source(library(epilog)),
    !,
    setup_theme,
    catch(setup_backtrace, E, print_message(warning, E)),
    use_module(library(epilog)),
    call(epilog([ init(user_thread_init),
                  main(true)
                ])).
main_thread_init :-
    setup_theme,
    user_thread_init.

%!  user_thread_init
%
%   Complete the toplevel startup.  This may run in a separate thread.

user_thread_init :-
    opt_attach_packs,
    argv_prolog_files(Files, ScriptMode),
    load_init_file(ScriptMode),                 % -f file
    catch(setup_colors, E, print_message(warning, E)),
    win_associated_files(Files),                % swipl-win: cd and update title
    '$load_script_file',                        % -s file (may be repeated)
    load_associated_files(Files),
    '$cmd_option_val'(goals, Goals),            % -g goal (may be repeated)
    (   ScriptMode == app
    ->  run_program_init,                       % initialization(Goal, program)
        run_main_init(true)
    ;   Goals == [],
        \+ '$init_goal'(when(_), _, _)          % no -g or -t or initialization(program)
    ->  version                                 % default interactive run
    ;   run_init_goals(Goals),                  % run -g goals
        (   load_only                           % used -l to load
        ->  version
        ;   run_program_init,                   % initialization(Goal, program)
            run_main_init(false)                % initialization(Goal, main)
        )
    ).

%!  setup_theme

:- multifile
    prolog:theme/1.

setup_theme :-
    current_prolog_flag(theme, Theme),
    exists_source(library(theme/Theme)),
    !,
    use_module(library(theme/Theme)).
setup_theme.

%!  apply_defines
%
%   Handle -Dflag[=value] options

apply_defines :-
    '$cmd_option_val'(defines, Defs),
    apply_defines(Defs).

apply_defines([]).
apply_defines([H|T]) :-
    apply_define(H),
    apply_defines(T).

apply_define(Def) :-
    sub_atom(Def, B, _, A, '='),
    !,
    sub_atom(Def, 0, B, _, Flag),
    sub_atom(Def, _, A, 0, Value0),
    (   '$current_prolog_flag'(Flag, Value0, _Scope, Access, Type)
    ->  (   Access \== write
        ->  '$permission_error'(set, prolog_flag, Flag)
        ;   text_flag_value(Type, Value0, Value)
        ),
	set_prolog_flag(Flag, Value)
    ;   (   atom_number(Value0, Value)
	->  true
	;   Value = Value0
	),
	set_defined(Flag, Value)
    ).
apply_define(Def) :-
    atom_concat('no-', Flag, Def),
    !,
    set_user_boolean_flag(Flag, false).
apply_define(Def) :-
    set_user_boolean_flag(Def, true).

set_user_boolean_flag(Flag, Value) :-
    current_prolog_flag(Flag, Old),
    !,
    (   Old == Value
    ->  true
    ;   set_prolog_flag(Flag, Value)
    ).
set_user_boolean_flag(Flag, Value) :-
    set_defined(Flag, Value).

text_flag_value(integer, Text, Int) :-
    atom_number(Text, Int),
    !.
text_flag_value(float, Text, Float) :-
    atom_number(Text, Float),
    !.
text_flag_value(term, Text, Term) :-
    term_string(Term, Text, []),
    !.
text_flag_value(_, Value, Value).

set_defined(Flag, Value) :-
    define_options(Flag, Options), !,
    create_prolog_flag(Flag, Value, Options).

%!  define_options(+Flag, -Options)
%
%   Define the options with which to create   Flag. This can be used for
%   known flags to control -for example- their type.

define_options('SDL_VIDEODRIVER', []).
define_options(_, [warn_not_accessed(true)]).

%!  init_optimise
%
%   Load library(apply_macros) if ``-O`` is effective.

init_optimise :-
    current_prolog_flag(optimise, true),
    !,
    use_module(user:library(apply_macros)).
init_optimise.

opt_attach_packs :-
    current_prolog_flag(packs, true),
    !,
    attach_packs.
opt_attach_packs.

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

run_main_init(_) :-
    findall(Goal-Ctx, '$init_goal'(when(main), Goal, Ctx), Pairs),
    '$last'(Pairs, Goal-Ctx),
    !,
    (   current_prolog_flag(toplevel_goal, default)
    ->  set_prolog_flag(toplevel_goal, halt)
    ;   true
    ),
    run_init_goal(Goal, @(Goal,Ctx)).
run_main_init(true) :-
    '$existence_error'(initialization, main).
run_main_init(_).

run_init_goal(Goal, Ctx) :-
    (   catch_with_backtrace(user:Goal, E, true)
    ->  (   var(E)
        ->  true
        ;   init_goal_failed(E, Ctx)
        )
    ;   (   current_prolog_flag(verbose, silent)
        ->  Level = silent
        ;   Level = error
        ),
        print_message(Level, init_goal_failed(failed, Ctx)),
        halt(1)
    ).

init_goal_failed(E, Ctx) :-
    print_message(error, init_goal_failed(E, Ctx)),
    init_goal_failed(E).

init_goal_failed(_) :-
    thread_self(main),
    !,
    halt(2).
init_goal_failed(_).

%!  init_debug_flags is det.
%
%   Initialize the various Prolog flags that   control  the debugger and
%   toplevel.

init_debug_flags :-
    Keep = [keep(true)],
    create_prolog_flag(answer_write_options,
                       [ quoted(true), portray(true), max_depth(10),
                         spacing(next_argument)], Keep),
    create_prolog_flag(prompt_alternatives_on, determinism, Keep),
    create_prolog_flag(toplevel_extra_white_line, true, Keep),
    create_prolog_flag(toplevel_print_factorized, false, Keep),
    create_prolog_flag(print_write_options,
                       [ portray(true), quoted(true), numbervars(true) ],
                       Keep),
    create_prolog_flag(toplevel_residue_vars, false, Keep),
    create_prolog_flag(toplevel_list_wfs_residual_program, true, Keep),
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
        \+ getenv('TERM', dumb),
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
    ).

%!  setup_readline
%
%   Setup line editing.

setup_readline :-
    (   stream_property(user_input, tty(true)),
        current_prolog_flag(tty_control, true),
        \+ getenv('TERM', dumb),
        (   current_prolog_flag(readline, ReadLine)
        ->  true
        ;   ReadLine = true
        ),
        readline_library(ReadLine, Library),
        (   load_setup_file(library(Library))
        ->  true
        ;   print_message(warning,
                          error(existence_error(library, library(Library)),
                                _)),
            fail
        )
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

%!  load_setup_file(+File) is semidet.
%
%   Load a file and fail silently if the file does not exist.

load_setup_file(File) :-
    catch(load_files(File,
                     [ silent(true),
                       if(not_loaded)
                     ]), error(_,_), fail).


%!  setup_app is det.
%
%   When running as an "app", behave as such. The behaviour depends on
%   the platform.
%
%     - Windows
%       If Prolog is started using --win_app, try to change directory
%       to <My Documents>\Prolog.

:- if(current_prolog_flag(windows,true)).

setup_app :-
    current_prolog_flag(associated_file, _),
    !.
setup_app :-
    '$cmd_option_val'(win_app, true),
    !,
    catch(my_prolog, E, print_message(warning, E)).
setup_app.

my_prolog :-
    win_folder(personal, MyDocs),
    atom_concat(MyDocs, '/Prolog', PrologDir),
    (   ensure_dir(PrologDir)
    ->  working_directory(_, PrologDir)
    ;   working_directory(_, MyDocs)
    ).

ensure_dir(Dir) :-
    exists_directory(Dir),
    !.
ensure_dir(Dir) :-
    catch(make_directory(Dir), E, (print_message(warning, E), fail)).

:- elif(current_prolog_flag(apple, true)).
use_app_settings(true).                        % Indicate we need app settings

setup_app :-
    apple_set_locale,
    current_prolog_flag(associated_file, _),
    !.
setup_app :-
    current_prolog_flag(bundle, true),
    current_prolog_flag(executable, Exe),
    file_base_name(Exe, 'SWI-Prolog'),
    !,
    setup_macos_app.
setup_app.

apple_set_locale :-
    (   getenv('LC_CTYPE', 'UTF-8'),
        apple_current_locale_identifier(LocaleID),
        atom_concat(LocaleID, '.UTF-8', Locale),
        catch(setlocale(ctype, _Old, Locale), _, fail)
    ->  setenv('LANG', Locale),
        unsetenv('LC_CTYPE')
    ;   true
    ).

setup_macos_app :-
    restore_working_directory,
    !.
setup_macos_app :-
    expand_file_name('~/Prolog', [PrologDir]),
    (   exists_directory(PrologDir)
    ->  true
    ;   catch(make_directory(PrologDir), MkDirError,
              print_message(warning, MkDirError))
    ),
    catch(working_directory(_, PrologDir), CdError,
          print_message(warning, CdError)),
    !.
setup_macos_app.

:- elif(current_prolog_flag(emscripten, true)).
setup_app.
:- else.
use_app_settings(true).                        % Indicate we need app settings

% Other (Unix-like) platforms.
setup_app :-
    running_as_app,
    restore_working_directory,
    !.
setup_app.

%!  running_as_app is semidet.
%
%   True if we were started from the dock.

running_as_app :-
%   getenv('FLATPAK_SANDBOX_DIR', _),
    current_prolog_flag(epilog, true),
    stream_property(In, file_no(0)),
    \+ stream_property(In, tty(true)),
    !.

:- endif.


:- if((current_predicate(use_app_settings/1),
       use_app_settings(true))).


                /*******************************
                *    APP WORKING DIRECTORY     *
                *******************************/

save_working_directory :-
    working_directory(WD, WD),
    app_settings(Settings),
    (   Settings.get(working_directory) == WD
    ->  true
    ;   app_save_settings(Settings.put(working_directory, WD))
    ).

restore_working_directory :-
    at_halt(save_working_directory),
    app_settings(Settings),
    WD = Settings.get(working_directory),
    catch(working_directory(_, WD), _, fail),
    !.

                /*******************************
                *           SETTINGS           *
                *******************************/

%!  app_settings(-Settings:dict) is det.
%
%   Get a dict holding the persistent application settings.

app_settings(Settings) :-
    app_settings_file(File),
    access_file(File, read),
    catch(setup_call_cleanup(
              open(File, read, In, [encoding(utf8)]),
              read_term(In, Settings, []),
              close(In)),
          Error,
          (print_message(warning, Error), fail)),
    !.
app_settings(#{}).

%!  app_save_settings(+Settings:dict) is det.
%
%   Save the given application settings dict.

app_save_settings(Settings) :-
    app_settings_file(File),
    catch(setup_call_cleanup(
              open(File, write, Out, [encoding(utf8)]),
              write_term(Out, Settings,
                         [ quoted(true),
                           module(system), % default operators
                           fullstop(true),
                           nl(true)
                         ]),
              close(Out)),
          Error,
          (print_message(warning, Error), fail)).


app_settings_file(File) :-
    absolute_file_name(user_app_config('app_settings.pl'), File,
                       [ access(write),
                         file_errors(fail)
                       ]).
:- endif.% app_settings

                /*******************************
                *           TOPLEVEL           *
                *******************************/

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
    (   catch('$compile_', E, (print_message(error, E), halt(1)))
    ->  true
    ;   print_message(error, error(goal_failed('$compile'), _)),
        halt(1)
    ),
    halt.                               % set exit code

'$compile_' :-
    '$load_system_init_file',
    catch(setup_colors, _, true),
    '$set_file_search_paths',
    init_debug_flags,
    '$run_initialization',
    opt_attach_packs,
    use_module(library(qsave)),
    qsave:qsave_toplevel.

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

%!  prolog:repl_loop_hook(+BeginEnd, +BreakLevel) is nondet.
%
%   Multifile  hook  that  allows  acting    on   starting/stopping  the
%   interactive REPL loop. Called as
%
%       forall(prolog:repl_loop_hook(BeginEnd, BreakLevel), true)
%
%   @arg BeginEnd is one of `begin` or `end`
%   @arg BreakLevel is 0 for the normal toplevel, -1 when
%   non-interactive and >0 for _break environments_.

:- multifile
    prolog:repl_loop_hook/2.

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
    break_level(BreakLev),
    setup_call_cleanup(
        notrace(call_repl_loop_hook(begin, BreakLev, IsToplevel)),
        '$query_loop'(BreakLev),
        notrace(call_repl_loop_hook(end, BreakLev, IsToplevel))).

call_repl_loop_hook(begin, BreakLev, IsToplevel) =>
    (   current_prolog_flag(toplevel_thread, IsToplevel)
    ->  true
    ;   IsToplevel = false
    ),
    set_prolog_flag(toplevel_thread, true),
    call_repl_loop_hook_(begin, BreakLev).
call_repl_loop_hook(end, BreakLev, IsToplevel) =>
    set_prolog_flag(toplevel_thread, IsToplevel),
    call_repl_loop_hook_(end, BreakLev).

call_repl_loop_hook_(BeginEnd, BreakLev) :-
    forall(prolog:repl_loop_hook(BeginEnd, BreakLev), true).


'$query_loop'(BreakLev) :-
    current_prolog_flag(toplevel_mode, recursive),
    !,
    read_expanded_query(BreakLev, Query, Bindings),
    (   Query == end_of_file
    ->  print_message(query, query(eof))
    ;   '$call_no_catch'('$execute_query'(Query, Bindings, _)),
        (   current_prolog_flag(toplevel_mode, recursive)
        ->  '$query_loop'(BreakLev)
        ;   '$switch_toplevel_mode'(backtracking),
            '$query_loop'(BreakLev)     % Maybe throw('$switch_toplevel_mode')?
        )
    ).
'$query_loop'(BreakLev) :-
    repeat,
        read_expanded_query(BreakLev, Query, Bindings),
        (   Query == end_of_file
        ->  !, print_message(query, query(eof))
        ;   '$execute_query'(Query, Bindings, _),
            (   current_prolog_flag(toplevel_mode, recursive)
            ->  !,
                '$switch_toplevel_mode'(recursive),
                '$query_loop'(BreakLev)
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
    trim_heap,
    repeat,
      (   catch(read_query(Prompt, Query, Bindings),
                error(io_error(_,_),_), fail)
      ->  prompt(_, Old),
          catch(call_expand_query(Query, ExpandedQuery,
                                  Bindings, ExpandedBindings),
                Error,
                (print_message(error, Error), fail))
      ;   set_prolog_flag(debug_on_error, false),
          thread_exit(io_error)
      ),
    !.


%!  read_query(+Prompt, -Goal, -Bindings) is det.
%
%   Read the next query. The first  clause   deals  with  the case where
%   !-based history is enabled. The second is   used  if we have command
%   line editing.

:- multifile
    prolog:history/2.

:- if(current_prolog_flag(emscripten, true)).
read_query(_Prompt, Goal, Bindings) :-
    '$can_yield',
    !,
    await(query, GoalString),
    term_string(Goal, GoalString, [variable_names(Bindings)]).
:- endif.
read_query(Prompt, Goal, Bindings) :-
    prolog:history(current_input, enabled),
    !,
    read_term_with_history(
        Goal,
        [ show(h),
          help('!h'),
          no_save([trace]),
          prompt(Prompt),
          variable_names(Bindings)
        ]).
read_query(Prompt, Goal, Bindings) :-
    remove_history_prompt(Prompt, Prompt1),
    repeat,                                 % over syntax errors
    prompt1(Prompt1),
    read_query_line(user_input, Line),
    '$current_typein_module'(TypeIn),
    catch(read_term_from_atom(Line, Goal,
                              [ variable_names(Bindings),
                                module(TypeIn)
                              ]), E,
          (   print_message(error, E),
              fail
          )),
    !.

%!  read_query_line(+Input, -Query:atom) is det.
%
%   Read a query as an atom. If Query is '$silent'(Goal), execute `Goal`
%   in module `user` and read the   next  query. This supports injecting
%   goals in some GNU-Emacs modes.

read_query_line(Input, Line) :-
    stream_property(Input, error(true)),
    !,
    Line = end_of_file.
read_query_line(Input, Line) :-
    catch(read_term_as_atom(Input, Line0), Error, true),
    save_debug_after_read,
    (   var(Error)
    ->  (   catch(term_string(Goal, Line0), error(_,_), fail),
            Goal = '$silent'(SilentGoal)
        ->  Error = error(_,_),
            catch_with_backtrace(ignore(SilentGoal), Error,
                                 print_message(error, Error)),
            read_query_line(Input, Line)
        ;   Line = Line0
        )
    ;   catch(print_message(error, Error), _, true),
        (   Error = error(syntax_error(_),_)
        ->  fail
        ;   throw(Error)
        )
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

%!  '$execute_query'(Goal, Bindings, -Truth) is det.
%
%   Execute Goal using Bindings.

'$execute_query'(Var, _, true) :-
    var(Var),
    !,
    print_message(informational, var_query(Var)).
'$execute_query'(Goal, Bindings, Truth) :-
    '$current_typein_module'(TypeIn),
    '$dwim_correct_goal'(TypeIn:Goal, Bindings, Corrected),
    !,
    setup_call_cleanup(
        '$set_source_module'(M0, TypeIn),
        expand_goal(Corrected, Expanded),
        '$set_source_module'(M0)),
    print_message(silent, toplevel_goal(Expanded, Bindings)),
    '$execute_goal2'(Expanded, Bindings, Truth).
'$execute_query'(_, _, false) :-
    notrace,
    print_message(query, query(no)).

'$execute_goal2'(Goal, Bindings, true) :-
    restore_debug,
    '$current_typein_module'(TypeIn),
    residue_vars(TypeIn:Goal, Vars, TypeIn:Delays, Chp),
    deterministic(Det),
    (   save_debug
    ;   restore_debug, fail
    ),
    flush_output(user_output),
    (   Det == true
    ->  DetOrChp = true
    ;   DetOrChp = Chp
    ),
    call_expand_answer(Goal, Bindings, NewBindings),
    (    \+ \+ write_bindings(NewBindings, Vars, Delays, DetOrChp)
    ->   !
    ).
'$execute_goal2'(_, _, false) :-
    save_debug,
    print_message(query, query(no)).

residue_vars(Goal, Vars, Delays, Chp) :-
    current_prolog_flag(toplevel_residue_vars, true),
    !,
    '$wfs_call'(call_residue_vars(stop_backtrace(Goal, Chp), Vars), Delays).
residue_vars(Goal, [], Delays, Chp) :-
    '$wfs_call'(stop_backtrace(Goal, Chp), Delays).

stop_backtrace(Goal, Chp) :-
    toplevel_call(Goal),
    prolog_current_choice(Chp).

toplevel_call(Goal) :-
    call(Goal),
    no_lco.

no_lco.

%!  write_bindings(+Bindings, +ResidueVars, +Delays, +DetOrChp)
%!	is semidet.
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

write_bindings(Bindings, ResidueVars, Delays, DetOrChp) :-
    '$current_typein_module'(TypeIn),
    translate_bindings(Bindings, Bindings1, ResidueVars, TypeIn:Residuals),
    omit_qualifier(Delays, TypeIn, Delays1),
    write_bindings2(Bindings, Bindings1, Residuals, Delays1, DetOrChp).

write_bindings2(OrgBindings, [], Residuals, Delays, _) :-
    current_prolog_flag(prompt_alternatives_on, groundness),
    !,
    name_vars(OrgBindings, [], t(Residuals, Delays)),
    print_message(query, query(yes(Delays, Residuals))).
write_bindings2(OrgBindings, Bindings, Residuals, Delays, true) :-
    current_prolog_flag(prompt_alternatives_on, determinism),
    !,
    name_vars(OrgBindings, Bindings, t(Residuals, Delays)),
    print_message(query, query(yes(Bindings, Delays, Residuals))).
write_bindings2(OrgBindings, Bindings, Residuals, Delays, Chp) :-
    repeat,
        name_vars(OrgBindings, Bindings, t(Residuals, Delays)),
        print_message(query, query(more(Bindings, Delays, Residuals))),
        get_respons(Action, Chp),
    (   Action == redo
    ->  !, fail
    ;   Action == show_again
    ->  fail
    ;   !,
        print_message(query, query(done))
    ).

%!  name_vars(+OrgBinding, +Bindings, +Term) is det.
%
%   Give a name ``_[A-Z][0-9]*`` to all variables   in Term, that do not
%   have a name due to Bindings. Singleton   variables in Term are named
%   `_`. The behavior depends on these Prolog flags:
%
%     - toplevel_name_variables
%       Only act when `true`, else name_vars/3 is a no-op.
%     - toplevel_print_anon
%
%   Variables are named by unifying them to `'$VAR'(Name)`
%
%   @arg Bindings is a list Name=Value

name_vars(OrgBindings, Bindings, Term) :-
    current_prolog_flag(toplevel_name_variables, true),
    answer_flags_imply_numbervars,
    !,
    '$term_multitons'(t(Bindings,Term), Vars),
    bindings_var_names(OrgBindings, Bindings, VarNames),
    name_vars_(Vars, VarNames, 0),
    term_variables(t(Bindings,Term), SVars),
    anon_vars(SVars).
name_vars(_OrgBindings, _Bindings, _Term).

name_vars_([], _, _).
name_vars_([H|T], Bindings, N) :-
    name_var(Bindings, Name, N, N1),
    H = '$VAR'(Name),
    name_vars_(T, Bindings, N1).

anon_vars([]).
anon_vars(['$VAR'('_')|T]) :-
    anon_vars(T).

%!  name_var(+Reserved, -Name, +N0, -N) is det.
%
%   True when Name is a valid name for   a new variable where the search
%   is guided by the number N0. Name may not appear in Reserved.

name_var(Reserved, Name, N0, N) :-
    between(N0, infinite, N1),
    I is N1//26,
    J is 0'A + N1 mod 26,
    (   I == 0
    ->  format(atom(Name), '_~c', [J])
    ;   format(atom(Name), '_~c~d', [J, I])
    ),
    \+ memberchk(Name, Reserved),
    !,
    N is N1+1.

%!  bindings_var_names(+OrgBindings, +TransBindings, -VarNames) is det.
%
%   Find the joined set of variable names   in the original bindings and
%   translated bindings. When generating new names,  we better also omit
%   names  that  appear  in  the  original  bindings  (but  not  in  the
%   translated bindigns).

bindings_var_names(OrgBindings, TransBindings, VarNames) :-
    phrase(bindings_var_names_(OrgBindings), VarNames0, Tail),
    phrase(bindings_var_names_(TransBindings), Tail, []),
    sort(VarNames0, VarNames).

%!  bindings_var_names_(+Bindings)// is det.
%
%   Produce a list of variable names that appear in Bindings. This deals
%   both with the single and joined representation of bindings.

bindings_var_names_([]) --> [].
bindings_var_names_([H|T]) -->
    binding_var_names(H),
    bindings_var_names_(T).

binding_var_names(binding(Vars,_Value,_Subst)) ==>
    var_names(Vars).
binding_var_names(Name=_Value) ==>
    [Name].

var_names([]) --> [].
var_names([H|T]) --> [H], var_names(T).


%!  answer_flags_imply_numbervars
%
%   True when the answer will be  written recognising '$VAR'(N). If this
%   is not the case we should not try to name the variables.

answer_flags_imply_numbervars :-
    current_prolog_flag(answer_write_options, Options),
    numbervars_option(Opt),
    memberchk(Opt, Options),
    !.

numbervars_option(portray(true)).
numbervars_option(portrayed(true)).
numbervars_option(numbervars(true)).

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
    \+ current_prolog_flag(xref, true),
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
    translate_bindings(Bindings0, Bindings, ResVars, ResGoals, Residuals),
    name_vars(Bindings0, Bindings, t(ResVars, ResGoals, Residuals)).

% should not be required.
prolog:name_vars(Bindings, Term) :- name_vars([], Bindings, Term).
prolog:name_vars(Bindings0, Bindings, Term) :- name_vars(Bindings0, Bindings, Term).

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
omit_meta_qualifiers(tnot(QA), TypeIn, tnot(A)) :-
    !,
    omit_qualifier(QA, TypeIn, A).
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

%!  get_respons(-Action, +Chp)
%
%   Read the continuation entered by the user.

:- if(current_prolog_flag(emscripten, true)).
get_respons(Action, Chp) :-
    '$can_yield',
    !,
    repeat,
        await(more, CommandS),
        atom_string(Command, CommandS),
        more_action(Command, Chp, Action),
        (   Action == again
        ->  print_message(query, query(action)),
            fail
        ;   !
        ).
:- endif.
get_respons(Action, Chp) :-
    repeat,
        flush_output(user_output),
        get_single_char(Code),
        find_more_command(Code, Command, Feedback, Style),
        (   Style \== '-'
        ->  print_message(query, if_tty([ansi(Style, '~w', [Feedback])]))
        ;   true
        ),
        more_action(Command, Chp, Action),
        (   Action == again
        ->  print_message(query, query(action)),
            fail
        ;   !
        ).

find_more_command(-1, end_of_file, 'EOF', warning) :-
    !.
find_more_command(Code, Command, Feedback, Style) :-
    more_command(Command, Atom, Feedback, Style),
    '$in_reply'(Code, Atom),
    !.
find_more_command(Code, again, '', -) :-
    print_message(query, no_action(Code)).

more_command(help,        '?h',        '',          -).
more_command(redo,        ';nrNR \t',  ';',         bold).
more_command(trace,       'tT',        '; [trace]', comment).
more_command(continue,    'ca\n\ryY.', '.',         bold).
more_command(break,       'b',         '',          -).
more_command(choicepoint, '*',         '',          -).
more_command(write,       'w',         '[write]',   comment).
more_command(print,       'p',         '[print]',   comment).
more_command(depth_inc,   '+',         Change,      comment) :-
    (   print_depth(Depth0)
    ->  depth_step(Step),
        NewDepth is Depth0*Step,
        format(atom(Change), '[max_depth(~D)]', [NewDepth])
    ;   Change = 'no max_depth'
    ).
more_command(depth_dec,   '-',         Change,      comment) :-
    (   print_depth(Depth0)
    ->  depth_step(Step),
        NewDepth is max(1, Depth0//Step),
        format(atom(Change), '[max_depth(~D)]', [NewDepth])
    ;   Change = '[max_depth(10)]'
    ).

more_action(help, _, Action) =>
    Action = again,
    print_message(help, query(help)).
more_action(redo, _, Action) =>			% Next
    Action = redo.
more_action(trace, _, Action) =>
    Action = redo,
    trace,
    save_debug.
more_action(continue, _, Action) =>             % Stop
    Action = continue.
more_action(break, _, Action) =>
    Action = show_again,
    break.
more_action(choicepoint, Chp, Action) =>
    Action = show_again,
    print_last_chpoint(Chp).
more_action(end_of_file, _, Action) =>
    Action = show_again,
    halt(0).
more_action(again, _, Action) =>
    Action = again.
more_action(Command, _, Action),
    current_prolog_flag(answer_write_options, Options0),
    print_predicate(Command, Options0, Options) =>
    Action = show_again,
    set_prolog_flag(answer_write_options, Options).

print_depth(Depth) :-
    current_prolog_flag(answer_write_options, Options),
    memberchk(max_depth(Depth), Options),
    !.

%!  print_predicate(+Action, +Options0, -Options) is semidet.
%
%   Modify  the  `answer_write_options`  value  according  to  the  user
%   command.

print_predicate(write, Options0, Options) :-
    edit_options([-portrayed(true),-portray(true)],
                 Options0, Options).
print_predicate(print, Options0, Options) :-
    edit_options([+portrayed(true)],
                 Options0, Options).
print_predicate(depth_inc, Options0, Options) :-
    (   '$select'(max_depth(D0), Options0, Options1)
    ->  depth_step(Step),
        D is D0*Step,
        Options = [max_depth(D)|Options1]
    ;   Options = Options0
    ).
print_predicate(depth_dec, Options0, Options) :-
    (   '$select'(max_depth(D0), Options0, Options1)
    ->  depth_step(Step),
        D is max(1, D0//Step),
        Options = [max_depth(D)|Options1]
    ;   D = 10,
        Options = [max_depth(D)|Options0]
    ).

depth_step(5).

edit_options([], Options, Options).
edit_options([H|T], Options0, Options) :-
    edit_option(H, Options0, Options1),
    edit_options(T, Options1, Options).

edit_option(-Term, Options0, Options) =>
    (   '$select'(Term, Options0, Options)
    ->  true
    ;   Options = Options0
    ).
edit_option(+Term, Options0, Options) =>
    functor(Term, Name, 1),
    functor(Var, Name, 1),
    (   '$select'(Var, Options0, Options1)
    ->  Options = [Term|Options1]
    ;   Options = [Term|Options0]
    ).

%!  print_last_chpoint(+Chp) is det.
%
%   Print the last choicepoint when an answer is nondeterministic.

print_last_chpoint(Chp) :-
    current_predicate(print_last_choice_point/0),
    !,
    print_last_chpoint_(Chp).
print_last_chpoint(Chp) :-
    use_module(library(prolog_stack), [print_last_choicepoint/2]),
    print_last_chpoint_(Chp).

print_last_chpoint_(Chp) :-
    print_last_choicepoint(Chp, [message_level(information)]).


                 /*******************************
                 *          EXPANSION           *
                 *******************************/

:- user:dynamic(expand_query/4).
:- user:multifile(expand_query/4).

call_expand_query(Goal, Expanded, Bindings, ExpandedBindings) :-
    (   '$replace_toplevel_vars'(Goal, Expanded0, Bindings, ExpandedBindings0)
    ->  true
    ;   Expanded0 = Goal, ExpandedBindings0 = Bindings
    ),
    (   user:expand_query(Expanded0, Expanded, ExpandedBindings0, ExpandedBindings)
    ->  true
    ;   Expanded = Expanded0, ExpandedBindings = ExpandedBindings0
    ).


:- dynamic
    user:expand_answer/2,
    prolog:expand_answer/3.
:- multifile
    user:expand_answer/2,
    prolog:expand_answer/3.

call_expand_answer(Goal, BindingsIn, BindingsOut) :-
    (   prolog:expand_answer(Goal, BindingsIn, BindingsOut)
    ->  true
    ;   user:expand_answer(BindingsIn, BindingsOut)
    ->  true
    ;   BindingsOut = BindingsIn
    ),
    '$save_toplevel_vars'(BindingsOut),
    !.
call_expand_answer(_, Bindings, Bindings).
