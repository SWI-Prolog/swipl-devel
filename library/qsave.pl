/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1995-2018, University of Amsterdam
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

:- module(qsave,
          [ qsave_program/1,                    % +File
            qsave_program/2                     % +File, +Options
          ]).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(error)).

/** <module> Save current program as a state or executable

This library provides qsave_program/1  and   qsave_program/2,  which are
also used by the commandline sequence below.

  ==
  swipl -o exe -c file.pl ...
  ==
*/

:- meta_predicate
    qsave_program(+, :).

:- public save_option/3.                        % used by '$compile_wic'/0

save_option(stack_limit, integer,
            "Stack limit (bytes)").
save_option(goal,        callable,
            "Main initialization goal").
save_option(toplevel,    callable,
            "Toplevel goal").
save_option(init_file,   atom,
            "Application init file").
save_option(class,       oneof([runtime,development]),
            "Development state").
save_option(op,          oneof([save,standard]),
            "Save operators").
save_option(autoload,    boolean,
            "Resolve autoloadable predicates").
save_option(map,         atom,
            "File to report content of the state").
save_option(stand_alone, boolean,
            "Add emulator at start").
save_option(emulator,    ground,
            "Emulator to use").
save_option(foreign,     oneof([save,no_save]),
            "Include foreign code in state").
save_option(obfuscate,   boolean,
            "Obfuscate identifiers").
save_option(verbose,     boolean,
            "Be more verbose about the state creation").
save_option(undefined,   oneof([ignore,error]),
            "How to handle undefined predicates").

term_expansion(save_pred_options,
               (:- predicate_options(qsave_program/2, 2, Options))) :-
    findall(O,
            ( save_option(Name, Type, _),
              O =.. [Name,Type]
            ),
            Options).

save_pred_options.

:- set_prolog_flag(generate_debug_info, false).

:- dynamic
    verbose/1,
    saved_resource_file/1.
:- volatile
    verbose/1,                  % contains a stream-handle
    saved_resource_file/1.

%!  qsave_program(+File) is det.
%!  qsave_program(+File, :Options) is det.
%
%   Make a saved state in file `File'.

qsave_program(File) :-
    qsave_program(File, []).

qsave_program(FileBase, Options0) :-
    meta_options(is_meta, Options0, Options),
    check_options(Options),
    exe_file(FileBase, File, Options),
    option(class(SaveClass),    Options, runtime),
    option(init_file(InitFile), Options, DefInit),
    default_init_file(SaveClass, DefInit),
    prepare_entry_points(Options),
    save_autoload(Options),
    open_map(Options),
    prepare_state(Options),
    create_prolog_flag(saved_program, true, []),
    create_prolog_flag(saved_program_class, SaveClass, []),
    (   exists_file(File)
    ->  delete_file(File)
    ;   true
    ),
    open(File, write, StateOut, [type(binary)]),
    make_header(StateOut, SaveClass, Options),
    zip_open_stream(StateOut, RC, []),
    save_options(RC, SaveClass,
                 [ init_file(InitFile)
                 | Options
                 ]),
    save_resources(RC, SaveClass),
    lock_files(SaveClass),
    save_program(RC, SaveClass, Options),
    save_foreign_libraries(RC, Options),
    zip_close(RC, [comment("SWI-Prolog saved state")]),
    '$mark_executable'(File),
    close_map,
    cleanup.

cleanup :-
    retractall(saved_resource_file(_)).

is_meta(goal).
is_meta(toplevel).

exe_file(Base, Exe, Options) :-
    current_prolog_flag(windows, true),
    option(stand_alone(true), Options, true),
    file_name_extension(_, '', Base),
    !,
    file_name_extension(Base, exe, Exe).
exe_file(Exe, Exe, _).

default_init_file(runtime, none) :- !.
default_init_file(_,       InitFile) :-
    '$cmd_option_val'(init_file, InitFile).


                 /*******************************
                 *           HEADER             *
                 *******************************/

%!  make_header(+Out:stream, +SaveClass, +Options) is det.

make_header(Out, _, Options) :-
    option(emulator(OptVal), Options),
    !,
    absolute_file_name(OptVal, [access(read)], Emulator),
    setup_call_cleanup(
        open(Emulator, read, In, [type(binary)]),
        copy_stream_data(In, Out),
        close(In)).
make_header(Out, _, Options) :-
    (   current_prolog_flag(windows, true)
    ->  DefStandAlone = true
    ;   DefStandAlone = false
    ),
    option(stand_alone(true), Options, DefStandAlone),
    !,
    current_prolog_flag(executable, Executable),
    setup_call_cleanup(
        open(Executable, read, In, [type(binary)]),
        copy_stream_data(In, Out),
        close(In)).
make_header(Out, SaveClass, _Options) :-
    current_prolog_flag(unix, true),
    !,
    current_prolog_flag(executable, Executable),
    current_prolog_flag(posix_shell, Shell),
    format(Out, '#!~w~n', [Shell]),
    format(Out, '# SWI-Prolog saved state~n', []),
    (   SaveClass == runtime
    ->  ArgSep = ' -- '
    ;   ArgSep = ' '
    ),
    format(Out, 'exec ${SWIPL-~w} -x "$0"~w"$@"~n~n', [Executable, ArgSep]).
make_header(_, _, _).


                 /*******************************
                 *           OPTIONS            *
                 *******************************/

min_stack(stack_limit, 100_000).

convert_option(Stack, Val, NewVal, "~w") :-     % stack-sizes are in K-bytes
    min_stack(Stack, Min),
    !,
    (   Val == 0
    ->  NewVal = Val
    ;   NewVal is max(Min, Val)
    ).
convert_option(toplevel, Callable, Callable, "~q") :- !.
convert_option(_, Value, Value, "~w").

doption(Name) :- min_stack(Name, _).
doption(init_file).
doption(system_init_file).
doption(class).
doption(home).

%!  save_options(+ArchiveHandle, +SaveClass, +Options)
%
%   Save the options in the '$options'   resource. The home directory is
%   saved for development  states  to  make   it  keep  refering  to the
%   development home.
%
%   The script files (-s script) are not saved   at all. I think this is
%   fine to avoid a save-script loading itself.

save_options(RC, SaveClass, Options) :-
    zipper_open_new_file_in_zip(RC, '$prolog/options.txt', Fd, []),
    (   doption(OptionName),
            '$cmd_option_val'(OptionName, OptionVal0),
            save_option_value(SaveClass, OptionName, OptionVal0, OptionVal1),
            OptTerm =.. [OptionName,OptionVal2],
            (   option(OptTerm, Options)
            ->  convert_option(OptionName, OptionVal2, OptionVal, FmtVal)
            ;   OptionVal = OptionVal1,
                FmtVal = "~w"
            ),
            atomics_to_string(["~w=", FmtVal, "~n"], Fmt),
            format(Fd, Fmt, [OptionName, OptionVal]),
        fail
    ;   true
    ),
    save_init_goals(Fd, Options),
    close(Fd).

%!  save_option_value(+SaveClass, +OptionName, +OptionValue, -FinalValue)

save_option_value(Class,   class, _,     Class) :- !.
save_option_value(runtime, home,  _,     _) :- !, fail.
save_option_value(_,       _,     Value, Value).

%!  save_init_goals(+Stream, +Options)
%
%   Save initialization goals. If there  is   a  goal(Goal)  option, use
%   that, else save the goals from '$cmd_option_val'/2.

save_init_goals(Out, Options) :-
    option(goal(Goal), Options),
    !,
    format(Out, 'goal=~q~n', [Goal]),
    save_toplevel_goal(Out, halt, Options).
save_init_goals(Out, Options) :-
    '$cmd_option_val'(goals, Goals),
    forall(member(Goal, Goals),
           format(Out, 'goal=~w~n', [Goal])),
    (   Goals == []
    ->  DefToplevel = default
    ;   DefToplevel = halt
    ),
    save_toplevel_goal(Out, DefToplevel, Options).

save_toplevel_goal(Out, _Default, Options) :-
    option(toplevel(Goal), Options),
    !,
    unqualify_reserved_goal(Goal, Goal1),
    format(Out, 'toplevel=~q~n', [Goal1]).
save_toplevel_goal(Out, _Default, _Options) :-
    '$cmd_option_val'(toplevel, Toplevel),
    Toplevel \== default,
    !,
    format(Out, 'toplevel=~w~n', [Toplevel]).
save_toplevel_goal(Out, Default, _Options) :-
    format(Out, 'toplevel=~q~n', [Default]).

unqualify_reserved_goal(_:prolog, prolog) :- !.
unqualify_reserved_goal(_:default, default) :- !.
unqualify_reserved_goal(Goal, Goal).


                 /*******************************
                 *           RESOURCES          *
                 *******************************/

save_resources(_RC, development) :- !.
save_resources(RC, _SaveClass) :-
    feedback('~nRESOURCES~n~n', []),
    copy_resources(RC),
    forall(declared_resource(Name, FileSpec, Options),
           save_resource(RC, Name, FileSpec, Options)).

declared_resource(RcName, FileSpec, []) :-
    current_predicate(_, M:resource(_,_)),
    M:resource(Name, FileSpec),
    mkrcname(M, Name, RcName).
declared_resource(RcName, FileSpec, Options) :-
    current_predicate(_, M:resource(_,_,_)),
    M:resource(Name, A2, A3),
    (   is_list(A3)
    ->  FileSpec = A2,
        Options = A3
    ;   FileSpec = A3
    ),
    mkrcname(M, Name, RcName).

%!  mkrcname(+Module, +NameSpec, -Name)
%
%   Turn a resource name term into a resource name atom.

mkrcname(user, Name0, Name) :-
    !,
    path_segments_to_atom(Name0, Name).
mkrcname(M, Name0, RcName) :-
    path_segments_to_atom(Name0, Name),
    atomic_list_concat([M, :, Name], RcName).

path_segments_to_atom(Name0, Name) :-
    phrase(segments_to_atom(Name0), Atoms),
    atomic_list_concat(Atoms, /, Name).

segments_to_atom(Var) -->
    { var(Var), !,
      instantiation_error(Var)
    }.
segments_to_atom(A/B) -->
    !,
    segments_to_atom(A),
    segments_to_atom(B).
segments_to_atom(A) -->
    [A].

%!  save_resource(+Zipper, +Name, +FileSpec, +Options) is det.
%
%   Add the content represented by FileSpec to Zipper under Name.

save_resource(RC, Name, FileSpec, _Options) :-
    absolute_file_name(FileSpec,
                       [ access(read),
                         file_errors(fail)
                       ], File),
    !,
    feedback('~t~8|~w~t~32|~w~n',
             [Name, File]),
    zipper_append_file(RC, Name, File, []).
save_resource(RC, Name, FileSpec, Options) :-
    findall(Dir,
            absolute_file_name(FileSpec, Dir,
                               [ access(read),
                                 file_type(directory),
                                 file_errors(fail),
                                 solutions(all)
                               ]),
            Dirs),
    Dirs \== [],
    !,
    forall(member(Dir, Dirs),
           ( feedback('~t~8|~w~t~32|~w~n',
                      [Name, Dir]),
             zipper_append_directory(RC, Name, Dir, Options))).
save_resource(RC, Name, _, _Options) :-
    '$rc_handle'(SystemRC),
    copy_resource(SystemRC, RC, Name),
    !.
save_resource(_, Name, FileSpec, _Options) :-
    print_message(warning,
                  error(existence_error(resource,
                                        resource(Name, FileSpec)),
                        _)).

copy_resources(ToRC) :-
    '$rc_handle'(FromRC),
    zipper_members(FromRC, List),
    (   member(Name, List),
        \+ declared_resource(Name, _, _),
        \+ reserved_resource(Name),
        copy_resource(FromRC, ToRC, Name),
        fail
    ;   true
    ).

reserved_resource('$prolog/state.qlf').
reserved_resource('$prolog/options.txt').

copy_resource(FromRC, ToRC, Name) :-
    (   zipper_goto(FromRC, file(Name))
    ->  true
    ;   existence_error(resource, Name)
    ),
    zipper_file_info(FromRC, _Name, Attrs),
    get_dict(time, Attrs, Time),
    setup_call_cleanup(
        zipper_open_current(FromRC, FdIn,
                            [ type(binary),
                              time(Time)
                            ]),
        setup_call_cleanup(
            zipper_open_new_file_in_zip(ToRC, Name, FdOut, []),
            ( feedback('~t~8|~w~t~24|~w~n',
                       [Name, '<Copied from running state>']),
              copy_stream_data(FdIn, FdOut)
            ),
            close(FdOut)),
        close(FdIn)).


		 /*******************************
		 *           OBFUSCATE		*
		 *******************************/

%!  create_mapping(+Options) is det.
%
%   Call hook to obfuscate symbols.

:- multifile prolog:obfuscate_identifiers/1.

create_mapping(Options) :-
    option(obfuscate(true), Options),
    !,
    (   predicate_property(prolog:obfuscate_identifiers(_), number_of_clauses(N)),
        N > 0
    ->  true
    ;   use_module(library(obfuscate))
    ),
    (   catch(prolog:obfuscate_identifiers(Options), E,
              print_message(error, E))
    ->  true
    ;   print_message(warning, failed(obfuscate_identifiers))
    ).
create_mapping(_).

%!  lock_files(+SaveClass) is det.
%
%   When saving as `runtime`, lock all files  such that when running the
%   program the system stops checking existence and modification time on
%   the filesystem.
%
%   @tbd `system` is a poor name.  Maybe use `resource`?

lock_files(runtime) :-
    !,
    '$set_source_files'(system).                % implies from_state
lock_files(_) :-
    '$set_source_files'(from_state).

%!  save_program(+Zipper, +SaveClass, +Options) is det.
%
%   Save the program itself as virtual machine code to Zipper.

save_program(RC, SaveClass, Options) :-
    zipper_open_new_file_in_zip(RC, '$prolog/state.qlf', StateFd, []),
    setup_call_cleanup(
        ( current_prolog_flag(access_level, OldLevel),
          set_prolog_flag(access_level, system), % generate system modules
          '$open_wic'(StateFd, Options)
        ),
        ( create_mapping(Options),
          save_modules(SaveClass),
          save_records,
          save_flags,
          save_prompt,
          save_imports,
          save_prolog_flags,
          save_operators(Options),
          save_format_predicates
        ),
        ( '$close_wic',
          set_prolog_flag(access_level, OldLevel)
        )),
    close(StateFd).


                 /*******************************
                 *            MODULES           *
                 *******************************/

save_modules(SaveClass) :-
    forall(special_module(X),
           save_module(X, SaveClass)),
    forall((current_module(X), \+ special_module(X)),
           save_module(X, SaveClass)).

special_module(system).
special_module(user).


%!  prepare_entry_points(+Options)
%
%   Prepare  the  --goal=Goal  and  --toplevel=Goal  options.  Preparing
%   implies autoloading the definition and declaring it _public_ such at
%   it doesn't get obfuscated.

prepare_entry_points(Options) :-
    define_init_goal(Options),
    define_toplevel_goal(Options).

define_init_goal(Options) :-
    option(goal(Goal), Options),
    !,
    entry_point(Goal).
define_init_goal(_).

define_toplevel_goal(Options) :-
    option(toplevel(Goal), Options),
    !,
    entry_point(Goal).
define_toplevel_goal(_).

entry_point(Goal) :-
    define_predicate(Goal),
    (   \+ predicate_property(Goal, built_in),
        \+ predicate_property(Goal, imported_from(_))
    ->  goal_pi(Goal, PI),
        public(PI)
    ;   true
    ).

define_predicate(Head) :-
    '$define_predicate'(Head),
    !.   % autoloader
define_predicate(Head) :-
    strip_module(Head, _, Term),
    functor(Term, Name, Arity),
    throw(error(existence_error(procedure, Name/Arity), _)).

goal_pi(M:G, QPI) :-
    !,
    strip_module(M:G, Module, Goal),
    functor(Goal, Name, Arity),
    QPI = Module:Name/Arity.
goal_pi(Goal, Name/Arity) :-
    functor(Goal, Name, Arity).

%!  prepare_state(+Options) is det.
%
%   Prepare the executable by  running   the  `prepare_state` registered
%   initialization hooks.

prepare_state(_) :-
    forall('$init_goal'(when(prepare_state), Goal, Ctx),
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
                 *            AUTOLOAD          *
                 *******************************/

%!  save_autoload(+Options) is det.
%
%   Resolve all autoload dependencies.
%
%   @error existence_error(procedures, List) if undefined(true) is
%   in Options and there are undefined predicates.

save_autoload(Options) :-
    option(autoload(true),  Options, true),
    !,
    autoload(Options).
save_autoload(_).


                 /*******************************
                 *             MODULES          *
                 *******************************/

%!  save_module(+Module, +SaveClass)
%
%   Saves a module

save_module(M, SaveClass) :-
    '$qlf_start_module'(M),
    feedback('~n~nMODULE ~w~n', [M]),
    save_unknown(M),
    (   P = (M:_H),
        current_predicate(_, P),
        \+ predicate_property(P, imported_from(_)),
        save_predicate(P, SaveClass),
        fail
    ;   '$qlf_end_part',
        feedback('~n', [])
    ).

save_predicate(P, _SaveClass) :-
    predicate_property(P, foreign),
    !,
    P = (M:H),
    functor(H, Name, Arity),
    feedback('~npre-defining foreign ~w/~d ', [Name, Arity]),
    '$add_directive_wic'('$predefine_foreign'(M:Name/Arity)).
save_predicate(P, SaveClass) :-
    P = (M:H),
    functor(H, F, A),
    feedback('~nsaving ~w/~d ', [F, A]),
    (   H = resource(_,_,_),
        SaveClass \== development
    ->  save_attribute(P, (dynamic)),
        (   M == user
        ->  save_attribute(P, (multifile))
        ),
        feedback('(Skipped clauses)', []),
        fail
    ;   true
    ),
    (   no_save(P)
    ->  true
    ;   save_attributes(P),
        \+ predicate_property(P, (volatile)),
        (   nth_clause(P, _, Ref),
            feedback('.', []),
            '$qlf_assert_clause'(Ref, SaveClass),
            fail
        ;   true
        )
    ).

no_save(P) :-
    predicate_property(P, volatile),
    \+ predicate_property(P, dynamic),
    \+ predicate_property(P, multifile).

pred_attrib(meta_predicate(Term), Head, meta_predicate(M:Term)) :-
    !,
    strip_module(Head, M, _).
pred_attrib(Attrib, Head,
            '$set_predicate_attribute'(M:Name/Arity, AttName, Val)) :-
    attrib_name(Attrib, AttName, Val),
    strip_module(Head, M, Term),
    functor(Term, Name, Arity).

attrib_name(dynamic,                dynamic,                true).
attrib_name(volatile,               volatile,               true).
attrib_name(thread_local,           thread_local,           true).
attrib_name(multifile,              multifile,              true).
attrib_name(public,                 public,                 true).
attrib_name(transparent,            transparent,            true).
attrib_name(discontiguous,          discontiguous,          true).
attrib_name(notrace,                trace,                  false).
attrib_name(show_childs,            hide_childs,            false).
attrib_name(built_in,               system,                 true).
attrib_name(nodebug,                hide_childs,            true).
attrib_name(quasi_quotation_syntax, quasi_quotation_syntax, true).
attrib_name(iso,                    iso,                    true).


save_attribute(P, Attribute) :-
    pred_attrib(Attribute, P, D),
    (   Attribute == built_in       % no need if there are clauses
    ->  (   predicate_property(P, number_of_clauses(0))
        ->  true
        ;   predicate_property(P, volatile)
        )
    ;   Attribute == 'dynamic'      % no need if predicate is thread_local
    ->  \+ predicate_property(P, thread_local)
    ;   true
    ),
    '$add_directive_wic'(D),
    feedback('(~w) ', [Attribute]).

save_attributes(P) :-
    (   predicate_property(P, Attribute),
        save_attribute(P, Attribute),
        fail
    ;   true
    ).

%       Save status of the unknown flag

save_unknown(M) :-
    current_prolog_flag(M:unknown, Unknown),
    (   Unknown == error
    ->  true
    ;   '$add_directive_wic'(set_prolog_flag(M:unknown, Unknown))
    ).

                 /*******************************
                 *            RECORDS           *
                 *******************************/

save_records :-
    feedback('~nRECORDS~n', []),
    (   current_key(X),
        X \== '$topvar',                        % do not safe toplevel variables
        feedback('~n~t~8|~w ', [X, V]),
        recorded(X, V, _),
        feedback('.', []),
        '$add_directive_wic'(recordz(X, V, _)),
        fail
    ;   true
    ).


                 /*******************************
                 *            FLAGS             *
                 *******************************/

save_flags :-
    feedback('~nFLAGS~n~n', []),
    (   current_flag(X),
        flag(X, V, V),
        feedback('~t~8|~w = ~w~n', [X, V]),
        '$add_directive_wic'(set_flag(X, V)),
        fail
    ;   true
    ).

save_prompt :-
    feedback('~nPROMPT~n~n', []),
    prompt(Prompt, Prompt),
    '$add_directive_wic'(prompt(_, Prompt)).


                 /*******************************
                 *           IMPORTS            *
                 *******************************/

%!  save_imports
%
%   Save  import  relations.  An  import  relation  is  saved  if  a
%   predicate is imported from a module that is not a default module
%   for the destination module. If  the   predicate  is  dynamic, we
%   always define the explicit import relation to make clear that an
%   assert must assert on the imported predicate.

save_imports :-
    feedback('~nIMPORTS~n~n', []),
    (   predicate_property(M:H, imported_from(I)),
        \+ default_import(M, H, I),
        functor(H, F, A),
        feedback('~t~8|~w:~w/~d <-- ~w~n', [M, F, A, I]),
        '$add_directive_wic'(qsave:restore_import(M, I, F/A)),
        fail
    ;   true
    ).

default_import(To, Head, From) :-
    '$get_predicate_attribute'(To:Head, (dynamic), 1),
    predicate_property(From:Head, exported),
    !,
    fail.
default_import(Into, _, From) :-
    default_module(Into, From).

%!  restore_import(+TargetModule, +SourceModule, +PI) is det.
%
%   Restore import relation. This notably   deals  with imports from
%   the module =user=, avoiding a message  that the predicate is not
%   exported.

restore_import(To, user, PI) :-
    !,
    export(user:PI),
    To:import(user:PI).
restore_import(To, From, PI) :-
    To:import(From:PI).

                 /*******************************
                 *         PROLOG FLAGS         *
                 *******************************/

save_prolog_flags :-
    feedback('~nPROLOG FLAGS~n~n', []),
    '$current_prolog_flag'(Flag, Value, _Scope, write, Type),
    \+ no_save_flag(Flag),
    feedback('~t~8|~w: ~w (type ~q)~n', [Flag, Value, Type]),
    '$add_directive_wic'(qsave:restore_prolog_flag(Flag, Value, Type)),
    fail.
save_prolog_flags.

no_save_flag(argv).
no_save_flag(os_argv).
no_save_flag(access_level).
no_save_flag(tty_control).
no_save_flag(readline).
no_save_flag(associated_file).
no_save_flag(cpu_count).
no_save_flag(hwnd).                     % should be read-only, but comes
                                        % from user-code

%!  restore_prolog_flag(+Name, +Value, +Type)
%
%   Deal  with  possibly   protected    flags   (debug_on_error  and
%   report_error are protected flags for the runtime kernel).

restore_prolog_flag(Flag, Value, _Type) :-
    current_prolog_flag(Flag, Value),
    !.
restore_prolog_flag(Flag, Value, _Type) :-
    current_prolog_flag(Flag, _),
    !,
    catch(set_prolog_flag(Flag, Value), _, true).
restore_prolog_flag(Flag, Value, Type) :-
    create_prolog_flag(Flag, Value, [type(Type)]).


                 /*******************************
                 *           OPERATORS          *
                 *******************************/

%!  save_operators(+Options) is det.
%
%   Save operators for all modules.   Operators for =system= are
%   not saved because these are read-only anyway.

save_operators(Options) :-
    !,
    option(op(save), Options, save),
    feedback('~nOPERATORS~n', []),
    forall(current_module(M), save_module_operators(M)),
    feedback('~n', []).
save_operators(_).

save_module_operators(system) :- !.
save_module_operators(M) :-
    forall('$local_op'(P,T,M:N),
           (   feedback('~n~t~8|~w ', [op(P,T,M:N)]),
               '$add_directive_wic'(op(P,T,M:N))
           )).


                 /*******************************
                 *       FORMAT PREDICATES      *
                 *******************************/

save_format_predicates :-
    feedback('~nFORMAT PREDICATES~n', []),
    current_format_predicate(Code, Head),
    qualify_head(Head, QHead),
    D = format_predicate(Code, QHead),
    feedback('~n~t~8|~w ', [D]),
    '$add_directive_wic'(D),
    fail.
save_format_predicates.

qualify_head(T, T) :-
    functor(T, :, 2),
    !.
qualify_head(T, user:T).


                 /*******************************
                 *       FOREIGN LIBRARIES      *
                 *******************************/

%!  save_foreign_libraries(+Archive, +Options) is det.
%
%   Save current foreign libraries into the archive.

save_foreign_libraries(RC, Options) :-
    option(foreign(save), Options),
    !,
    feedback('~nFOREIGN LIBRARIES~n', []),
    forall(current_foreign_library(FileSpec, _Predicates),
           ( find_foreign_library(FileSpec, File, Time),
             term_to_atom(FileSpec, Name),
             zipper_append_file(RC, Name, File, [time(Time)])
           )).
save_foreign_libraries(_, _).

%!  find_foreign_library(+FileSpec, -File, -Time) is det.
%
%   Find the shared object specified by   FileSpec.  If posible, the
%   shared object is stripped to reduce   its size. This is achieved
%   by calling strip -o <tmp> <shared-object>. Note that the file is
%   a Prolog tmp file and will be deleted on halt.
%
%   @bug    Should perform OS search on failure

find_foreign_library(FileSpec, SharedObject, Time) :-
    absolute_file_name(FileSpec,
                       [ file_type(executable),
                         access(read),
                         file_errors(fail)
                       ], File),
    !,
    time_file(File, Time),
    (   absolute_file_name(path(strip), Strip,
                           [ access(execute),
                             file_errors(fail)
                           ]),
        tmp_file(shared, Stripped),
        format(atom(Cmd), '"~w" -o "~w" "~w"',
               [ Strip, Stripped, File ]),
        shell(Cmd)
    ->  SharedObject = Stripped
    ;   SharedObject = File
    ).


                 /*******************************
                 *             UTIL             *
                 *******************************/

open_map(Options) :-
    option(map(Map), Options),
    !,
    open(Map, write, Fd),
    asserta(verbose(Fd)).
open_map(_) :-
    retractall(verbose(_)).

close_map :-
    retract(verbose(Fd)),
    close(Fd),
    !.
close_map.

feedback(Fmt, Args) :-
    verbose(Fd),
    !,
    format(Fd, Fmt, Args).
feedback(_, _).


check_options([]) :- !.
check_options([Var|_]) :-
    var(Var),
    !,
    throw(error(domain_error(save_options, Var), _)).
check_options([Name=Value|T]) :-
    !,
    (   save_option(Name, Type, _Comment)
    ->  (   must_be(Type, Value)
        ->  check_options(T)
        ;   throw(error(domain_error(Type, Value), _))
        )
    ;   throw(error(domain_error(save_option, Name), _))
    ).
check_options([Term|T]) :-
    Term =.. [Name,Arg],
    !,
    check_options([Name=Arg|T]).
check_options([Var|_]) :-
    throw(error(domain_error(save_options, Var), _)).
check_options(Opt) :-
    throw(error(domain_error(list, Opt), _)).


%!  zipper_append_file(+Zipper, +Name, +File, +Options) is det.
%
%   Append the content of File under Name to the open Zipper.

zipper_append_file(_, Name, _, _) :-
    saved_resource_file(Name),
    !.
zipper_append_file(_, _, File, _) :-
    source_file(File),
    !.
zipper_append_file(Zipper, Name, File, Options) :-
    (   option(time(_), Options)
    ->  Options1 = Options
    ;   time_file(File, Stamp),
        Options1 = [time(Stamp)|Options]
    ),
    setup_call_cleanup(
        open(File, read, In, [type(binary)]),
        setup_call_cleanup(
            zipper_open_new_file_in_zip(Zipper, Name, Out, Options1),
            copy_stream_data(In, Out),
            close(Out)),
        close(In)),
    assertz(saved_resource_file(Name)).

%!  zipper_add_directory(+Zipper, +Name, +Dir, +Options) is det.
%
%   Add a directory entry. Dir  is  only   used  if  there  is no option
%   time(Stamp).

zipper_add_directory(Zipper, Name, Dir, Options) :-
    (   option(time(Stamp), Options)
    ->  true
    ;   time_file(Dir, Stamp)
    ),
    atom_concat(Name, /, DirName),
    (   saved_resource_file(DirName)
    ->  true
    ;   setup_call_cleanup(
            zipper_open_new_file_in_zip(Zipper, DirName, Out,
                                        [ method(store),
                                          time(Stamp)
                                        | Options
                                        ]),
            true,
            close(Out)),
        assertz(saved_resource_file(DirName))
    ).

add_parent_dirs(Zipper, Name, Dir, Options) :-
    (   option(time(Stamp), Options)
    ->  true
    ;   time_file(Dir, Stamp)
    ),
    file_directory_name(Name, Parent),
    (   Parent \== Name
    ->  add_parent_dirs(Zipper, Parent, [time(Stamp)|Options])
    ;   true
    ).

add_parent_dirs(_, '.', _) :-
    !.
add_parent_dirs(Zipper, Name, Options) :-
    zipper_add_directory(Zipper, Name, _, Options),
    file_directory_name(Name, Parent),
    (   Parent \== Name
    ->  add_parent_dirs(Zipper, Parent, Options)
    ;   true
    ).


%!  zipper_append_directory(+Zipper, +Name, +Dir, +Options) is det.
%
%   Append the content of  Dir  below   Name  in  the  resource archive.
%   Options:
%
%     - include(+Patterns)
%     Only add entries that match an element from Patterns using
%     wildcard_match/2.
%     - exclude(+Patterns)
%     Ignore entries that match an element from Patterns using
%     wildcard_match/2.
%
%   @tbd Process .gitignore.  There also seem to exists other
%   standards for this.

zipper_append_directory(Zipper, Name, Dir, Options) :-
    exists_directory(Dir),
    !,
    add_parent_dirs(Zipper, Name, Dir, Options),
    zipper_add_directory(Zipper, Name, Dir, Options),
    directory_files(Dir, Members),
    forall(member(M, Members),
           (   reserved(M)
           ->  true
           ;   ignored(M, Options)
           ->  true
           ;   atomic_list_concat([Dir,M], /, Entry),
               atomic_list_concat([Name,M], /, Store),
               catch(zipper_append_directory(Zipper, Store, Entry, Options),
                     E,
                     print_message(warning, E))
           )).
zipper_append_directory(Zipper, Name, File, Options) :-
    zipper_append_file(Zipper, Name, File, Options).

reserved(.).
reserved(..).

%!  ignored(+File, +Options) is semidet.
%
%   Ignore File if there is an  include(Patterns) option that does *not*
%   match File or an exclude(Patterns) that does match File.

ignored(File, Options) :-
    option(include(Patterns), Options),
    \+ ( (   is_list(Patterns)
         ->  member(Pattern, Patterns)
         ;   Pattern = Patterns
         ),
         wildcard_match(Pattern, File)
       ),
    !.
ignored(File, Options) :-
    option(exclude(Patterns), Options),
    (   is_list(Patterns)
    ->  member(Pattern, Patterns)
    ;   Pattern = Patterns
    ),
    wildcard_match(Pattern, File),
    !.


                 /*******************************
                 *            MESSAGES          *
                 *******************************/

:- multifile prolog:message/3.

prolog:message(no_resource(Name, File)) -->
    [ 'Could not find resource ~w on ~w or system resources'-
      [Name, File] ].
