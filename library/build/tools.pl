/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2021, SWI-Prolog Solutions b.v.
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

:- module(build_tools,
	  [ build_steps/3,              % +Steps, +SrcDir, +Options
	    prolog_install_prefix/1,    % -Prefix
	    run_process/3,              % +Executable, +Argv, +Options
	    has_program/3,              % +Spec, -Path, +Env
	    path_sep/1,                 % -Separator
	    ensure_build_dir/3          % +Dir, +State0, -State
	  ]).
:- autoload(library(lists), [selectchk/3, member/2, append/3, last/2]).
:- autoload(library(option), [option/2, option/3, dict_options/2]).
:- autoload(library(pairs), [pairs_values/2]).
:- autoload(library(process), [process_create/3, process_wait/2]).
:- autoload(library(readutil), [read_stream_to_codes/3]).
:- autoload(library(dcg/basics), [string/3]).
:- autoload(library(apply), [foldl/4, maplist/2]).
:- autoload(library(filesex), [directory_file_path/3, make_directory_path/1]).
:- autoload(library(prolog_config), [apple_bundle_libdir/1]).
:- autoload(library(solution_sequences), [distinct/2]).

% The plugins.  Load them in the order of preference.
:- use_module(conan).
:- use_module(cmake).
:- use_module(make).

:- multifile
    prolog:build_file/2,                % ?File, ?Toolchain
    prolog:build_step/4,                % ?Step, ?Tool, ?SrcDir, ?BuildDir
    prolog:build_environment/2,         % ?Name, ?Value
    prolog_pack:environment/2.          % ?Name, ?Value (backward compatibility)

/** <module> Utilities for building foreign resources

This module implements the build system   that is used by pack_install/1
and pack_rebuild/1. The build system is a plugin based system where each
plugin knows about a specific  build   toolchain.  The plugins recognise
whether they are applicable based on  the   existence  of files that are
unique to the toolchain.   Currently it supports

  - [conan](https://conan.io/) for the installation of dependencies
  - [cmake](https://cmake.org/) for configuration and building
  - [GNU tools](https://www.gnu.org) including `automake` and `autoconf`
    for configuration and building
*/

%!  build_steps(+Steps:list, SrcDir:atom, +Options) is det.
%
%   Run the desired build steps.  Normally,   Steps  is  the list below,
%   optionally prefixed with `distclean` or `clean`. `[test]` may be
%   omited if ``--no-test`` is effective.
%
%       [[dependencies], [configure], build, [test], install]
%
%   Each step finds an applicable toolchain  based on known unique files
%   and calls the matching plugin to perform  the step. A step may fail,
%   which causes the system to try an  alternative. A step that wants to
%   abort the build process must  throw  an   exception.
%
%   @tbd If no tool  is  willing  to   execute  some  step,  the step is
%   skipped. This is ok for some steps such as `dependencies` or `test`.
%   Possibly we should force the `install` step to succeed?

build_steps(Steps, SrcDir, Options) :-
    dict_options(Dict0, Options),
    setup_path,
    build_environment(BuildEnv, Options),
    State0 = Dict0.put(#{ env: BuildEnv,
			  src_dir: SrcDir
			}),
    foldl(build_step, Steps, State0, _State).

build_step(Spec, State0, State) :-
    step_name(Spec, Step),
    prolog:build_file(File, Tool),
    directory_file_path(State0.src_dir, File, Path),
    exists_file(Path),
    prolog:build_step(Step, Tool, State0, State),
    post_step(Step, Tool, State),
    !.
build_step([_], State, State) :-
    !.
build_step(Step, State, State) :-
    print_message(warning, build(step_failed(Step))).

step_name([Step], Step) :-              % options
    !.
step_name(Step, Step).

post_step(configure, _, State) :-
    !,
    save_build_environment(State.bin_dir, State.env).
post_step(_, _, _).


%!  ensure_build_dir(+Dir, +State0, -State) is det.
%
%   Create the build directory. Dir is normally   either '.' to build in
%   the source directory or `build` to create a `build` subdir.

ensure_build_dir(_, State0, State) :-
    _ = State0.get(bin_dir),
    !,
    State = State0.
ensure_build_dir(., State0, State) :-
    !,
    State = State0.put(bin_dir, State0.src_dir).
ensure_build_dir(Dir, State0, State) :-
    directory_file_path(State0.src_dir, Dir, BinDir),
    make_directory_path(BinDir),
    !,
    State = State0.put(bin_dir, BinDir).


		 /*******************************
		 *          ENVIRONMENT		*
		 *******************************/

%!  build_environment(-Env, +Options) is det.
%
%   Assemble a clean  build  environment   for  creating  extensions  to
%   SWI-Prolog. Env is a list of   `Var=Value` pairs. The variable names
%   depend on the `pack_version(Version)`  term   from  `pack.pl`.  When
%   absent or `1`, the old names are used. These names are confusing and
%   conflict with some build environments. Using `2` (or later), the new
%   names are used. The list below  first   names  the new name and than
%   between parenthesis, the old name.  Provided variables are:
%
%     $ ``PATH`` :
%     contains the environment path with the directory
%     holding the currently running SWI-Prolog instance prepended
%     in front of it.  As a result, `swipl` is always present and
%     runs the same SWI-Prolog instance as the current Prolog process.
%     $ ``SWIPL`` :
%     contains the absolute file name of the running executable.
%     $ ``SWIPL_PACK_VERSION`` :
%     Version of the pack system (1 or 2)
%     $ ``SWIPL_VERSION`` (``SWIPLVERSION``) :
%     contains the numeric SWI-Prolog version defined as
%     _|Major*10000+Minor*100+Patch|_.
%     $ ``SWIPL_HOME_DIR`` (``SWIHOME``) :
%     contains the directory holding the SWI-Prolog home.
%     $ ``SWIPL_ARCH`` (``SWIARCH``) :
%     contains the machine architecture identifier.
%     $ ``SWIPL_MODULE_DIR`` (``PACKSODIR``) :
%     constains the destination directory for shared objects/DLLs
%     relative to a Prolog pack, i.e., ``lib/$SWIARCH``.
%     $ ``SWIPL_MODULE_LIB`` (``SWISOLIB``) :
%     The SWI-Prolog library or an empty string when it is not required
%     to link modules against this library (e.g., ELF systems)
%     $ ``SWIPL_LIB`` (``SWILIB``) :
%     The SWI-Prolog library we need to link to for programs that
%     _embed_ SWI-Prolog (normally ``-lswipl``).
%     $ ``SWIPL_INCLUDE_DIRS`` :
%     CMake style variable that contains the directory holding
%     ``SWI-Prolog.h``, ``SWI-Stream.h`` and ``SWI-cpp.h``.
%     $ ``SWIPL_LIBRARIES_DIR`` :
%     CMake style variable that contains the directory holding `libswipl`
%     $ ``SWIPL_CC`` (``CC``) :
%     Prefered C compiler
%     $ ``SWIPL_LD`` (``LD``) :
%     Prefered linker
%     $ ``SWIPL_CFLAGS`` (``CLFLAGS``) :
%     C-Flags for building extensions. Always contains ``-ISWIPL-INCLUDE-DIR``.
%     $ ``SWIPL_MODULE_LDFLAGS`` (``LDSOFLAGS``) :
%     Link flags for linking modules.
%     $ ``SWIPL_MODULE_EXT`` (``SOEXT``) :
%     File name extension for modules (e.g., `so` or `dll`)
%     $ ``SWIPL_PREFIX`` (``PREFIX``) :
%     Install prefix for global binaries, libraries and include files.

build_environment(Env, Options) :-
    findall(Name=Value,
	    distinct(Name, user_environment(Name, Value)),
	    UserEnv),
    findall(Name=Value,
	    ( def_environment(Name, Value, Options),
	      \+ memberchk(Name=_, UserEnv)
	    ),
	    DefEnv),
    append(UserEnv, DefEnv, Env).

user_environment(Name, Value) :-
    prolog:build_environment(Name, Value).
user_environment(Name, Value) :-
    prolog_pack:environment(Name, Value).

%!  prolog:build_environment(-Name, -Value) is nondet.
%
%   Hook  to  define  the  environment   for  building  packs.  This
%   Multifile hook extends the  process   environment  for  building
%   foreign extensions. A value  provided   by  this  hook overrules
%   defaults provided by def_environment/3. In  addition to changing
%   the environment, this may be used   to pass additional values to
%   the environment, as in:
%
%     ==
%     prolog:build_environment('USER', User) :-
%         getenv('USER', User).
%     ==
%
%   @arg Name is an atom denoting a valid variable name
%   @arg Value is either an atom or number representing the
%          value of the variable.


%!  def_environment(-Name, -Value, +Options) is nondet.
%
%   True if Name=Value must appear in   the environment for building
%   foreign extensions.

def_environment('PATH', Value, _) :-
    getenv('PATH', PATH),
    current_prolog_flag(executable, Exe),
    file_directory_name(Exe, ExeDir),
    prolog_to_os_filename(ExeDir, OsExeDir),
    path_sep(Sep),
    atomic_list_concat([OsExeDir, Sep, PATH], Value).
def_environment('SWIPL', Value, _) :-
    current_prolog_flag(executable, Value).
def_environment('SWIPL_PACK_VERSION', Value, Options) :-
    option(pack_version(Value), Options, 1).
def_environment(VAR, Value, Options) :-
    env_name(version, VAR, Options),
    current_prolog_flag(version, Value).
def_environment(VAR, Value, Options) :-
    env_name(home, VAR, Options),
    current_prolog_flag(home, Value).
def_environment(VAR, Value, Options) :-
    env_name(arch, VAR, Options),
    current_prolog_flag(arch, Value).
def_environment(VAR, Value, Options) :-
    env_name(module_dir, VAR, Options),
    current_prolog_flag(arch, Arch),
    atom_concat('lib/', Arch, Value).
def_environment(VAR, Value, Options) :-
    env_name(module_lib, VAR, Options),
    current_prolog_flag(c_libplso, Value).
def_environment(VAR, '-lswipl', Options) :-
    env_name(lib, VAR, Options).
def_environment(VAR, Value, Options) :-
    env_name(cc, VAR, Options),
    (   getenv('CC', Value)
    ->  true
    ;   default_c_compiler(Value)
    ->  true
    ;   current_prolog_flag(c_cc, Value)
    ).
def_environment(VAR, Value, Options) :-
    env_name(ld, VAR, Options),
    (   getenv('LD', Value)
    ->  true
    ;   current_prolog_flag(c_cc, Value)
    ).
def_environment('SWIPL_INCLUDE_DIRS', Value, _) :- % CMake style environment
    current_prolog_flag(home, Home),
    atom_concat(Home, '/include', Value).
def_environment('SWIPL_LIBRARIES_DIR', Value, _) :-
    swipl_libraries_dir(Value).
def_environment(VAR, Value, Options) :-
    env_name(cflags, VAR, Options),
    (   getenv('CFLAGS', SystemFlags)
    ->  Extra = [' ', SystemFlags]
    ;   Extra = []
    ),
    current_prolog_flag(c_cflags, Value0),
    current_prolog_flag(home, Home),
    atomic_list_concat([Value0, ' -I"', Home, '/include"' | Extra], Value).
def_environment(VAR, Value, Options) :-
    env_name(module_ldflags, VAR, Options),
    (   getenv('LDFLAGS', SystemFlags)
    ->  Extra = [SystemFlags|System]
    ;   Extra = System
    ),
    (   current_prolog_flag(windows, true)
    ->  prolog_library_dir(LibDir),
	atomic_list_concat(['-L"', LibDir, '"'], SystemLib),
	System = [SystemLib]
    ;   apple_bundle_libdir(LibDir)
    ->  atomic_list_concat(['-L"', LibDir, '"'], SystemLib),
	System = [SystemLib]
    ;   current_prolog_flag(c_libplso, '')
    ->  System = []                 % ELF systems do not need this
    ;   prolog_library_dir(SystemLibDir),
	atomic_list_concat(['-L"',SystemLibDir,'"'], SystemLib),
	System = [SystemLib]
    ),
    current_prolog_flag(c_ldflags, LDFlags),
    atomic_list_concat([LDFlags, '-shared' | Extra], ' ', Value).
def_environment(VAR, Value, Options) :-
    env_name(module_ext, VAR, Options),
    current_prolog_flag(shared_object_extension, Value).
def_environment('PREFIX', Value, _) :-
    prolog_install_prefix(Value).

swipl_libraries_dir(Dir) :-
    current_prolog_flag(windows, true),
    !,
    current_prolog_flag(home, Home),
    atom_concat(Home, '/bin', Dir).
swipl_libraries_dir(Dir) :-
    apple_bundle_libdir(Dir),
    !.
swipl_libraries_dir(Dir) :-
    prolog_library_dir(Dir).

env_name(Id, Name, Options) :-
    option(pack_version(V), Options, 1),
    must_be(oneof([1,2]), V),
    env_name_v(Id, V, Name).

env_name_v(version,        1, 'SWIPLVERSION').
env_name_v(version,        2, 'SWIPL_VERSION').
env_name_v(home,           1, 'SWIHOME').
env_name_v(home,           2, 'SWIPL_HOME_DIR').
env_name_v(module_dir,     1, 'PACKSODIR').
env_name_v(module_dir,     2, 'SWIPL_MODULE_DIR').
env_name_v(module_lib,     1, 'SWISOLIB').
env_name_v(module_lib,     2, 'SWIPL_MODULE_LIB').
env_name_v(lib,            1, 'SWILIB').
env_name_v(lib,            2, 'SWIPL_LIB').
env_name_v(arch,           1, 'SWIARCH').
env_name_v(arch,           2, 'SWIPL_ARCH').
env_name_v(cc,             1, 'CC').
env_name_v(cc,             2, 'SWIPL_CC').
env_name_v(ld,             1, 'LD').
env_name_v(ld,             2, 'SWIPL_LD').
env_name_v(cflags,         1, 'CFLAGS').
env_name_v(cflags,         2, 'SWIPL_CFLAGS').
env_name_v(module_ldflags, 1, 'LDSOFLAGS').
env_name_v(module_ldflags, 2, 'SWIPL_MODULE_LDFLAGS').
env_name_v(module_ext,     1, 'SOEXT').
env_name_v(module_ext,     2, 'SWIPL_MODULE_EXT').
env_name_v(prefix,         1, 'PREFIX').
env_name_v(prefix,         2, 'SWIPL_PREFIX').

%!  prolog_library_dir(-Dir) is det.
%
%   True when Dir is the directory holding ``libswipl.$SOEXT``

:- multifile
    prolog:runtime_config/2.

prolog_library_dir(Dir) :-
    prolog:runtime_config(c_libdir, Dir),
    !.
prolog_library_dir(Dir) :-
    current_prolog_flag(windows, true),
    current_prolog_flag(home, Home),
    !,
    atomic_list_concat([Home, bin], /, Dir).
prolog_library_dir(Dir) :-
    current_prolog_flag(home, Home),
    (   current_prolog_flag(c_libdir, Rel)
    ->  atomic_list_concat([Home, Rel], /, Dir)
    ;   current_prolog_flag(arch, Arch)
    ->  atomic_list_concat([Home, lib, Arch], /, Dir)
    ).

%!  default_c_compiler(-CC) is semidet.
%
%   Try to find a  suitable  C   compiler  for  compiling  packages with
%   foreign code.
%
%   @tbd Needs proper defaults for Windows.  Find MinGW?  Find MSVC?

default_c_compiler(CC) :-
    preferred_c_compiler(CC),
    has_program(path(CC), _),
    !.

preferred_c_compiler(gcc).
preferred_c_compiler(clang).
preferred_c_compiler(cc).

%!  save_build_environment(+BuildDir, +Env) is det.
%
%   Create  a  shell-script  ``buildenv.sh``  that  contains  the  build
%   environment. This may be _sourced_ in the build directory to run the
%   build steps outside Prolog. It  may   also  be  useful for debugging
%   purposes.

save_build_environment(BuildDir, Env) :-
    directory_file_path(BuildDir, 'buildenv.sh', EnvFile),
    setup_call_cleanup(
	open(EnvFile, write, Out),
	write_env_script(Out, Env),
	close(Out)).

write_env_script(Out, Env) :-
    format(Out,
	   '# This file contains the environment that can be used to\n\c
	    # build the foreign pack outside Prolog.  This file must\n\c
	    # be loaded into a bourne-compatible shell using\n\c
	    #\n\c
	    #   $ source buildenv.sh\n\n',
	   []),
    forall(member(Var=Value, Env),
	   format(Out, '~w=\'~w\'\n', [Var, Value])),
    format(Out, '\nexport ', []),
    forall(member(Var=_, Env),
	   format(Out, ' ~w', [Var])),
    format(Out, '\n', []).

%!  prolog_install_prefix(-Prefix) is semidet.
%
%   Return the directory that can be  passed into `configure` or `cmake`
%   to install executables and other  related   resources  in  a similar
%   location as SWI-Prolog itself.  Tries these rules:
%
%     1. If the Prolog flag `pack_prefix` at a writable directory, use
%        this.
%     2. If the current executable can be found on $PATH and the parent
%        of the directory of the executable is writable, use this.
%     3. If the user has a writable ``~/bin`` directory, use ``~``.

prolog_install_prefix(Prefix) :-
    current_prolog_flag(pack_prefix, Prefix),
    access_file(Prefix, write),
    !.
prolog_install_prefix(Prefix) :-
    current_prolog_flag(os_argv, [Name|_]),
    has_program(path(Name), EXE),
    file_directory_name(EXE, Bin),
    file_directory_name(Bin, Prefix0),
    (   local_prefix(Prefix0, Prefix1)
    ->  Prefix = Prefix1
    ;   Prefix = Prefix0
    ),
    access_file(Prefix, write),
    !.
prolog_install_prefix(Prefix) :-
    expand_file_name(~, [UserHome]),
    directory_file_path(UserHome, bin, BinDir),
    exists_directory(BinDir),
    access_file(BinDir, write),
    !,
    Prefix = UserHome.

local_prefix('/usr', '/usr/local').


		 /*******************************
		 *          RUN PROCESSES       *
		 *******************************/

%!  run_process(+Executable, +Argv, +Options) is det.
%
%   Run Executable.  Defined options:
%
%     - directory(+Dir)
%       Execute in the given directory
%     - output(-Out)
%       Unify Out with a list of codes representing stdout of the
%       command.  Otherwise the output is handed to print_message/2
%       with level =informational=.
%     - error(-Error)
%       As output(Out), but messages are printed at level =error=.
%     - env(+Environment)
%       Environment passed to the new process.
%
%   If Executable is path(Program) and we   have  an environment we make
%   sure to use  the  ``PATH``  from   this  environment  for  searching
%   `Program`.

run_process(path(Exe), Argv, Options) :-
    option(env(BuildEnv), Options),
    !,
    setup_call_cleanup(
	b_setval('$build_tool_env', BuildEnv),
	run_process(pack_build_path(Exe), Argv, Options),
	nb_delete('$build_tool_env')).
run_process(Executable, Argv, Options) :-
    \+ option(output(_), Options),
    \+ option(error(_), Options),
    current_prolog_flag(unix, true),
    current_prolog_flag(threads, true),
    !,
    process_create_options(Options, Extra),
    process_create(Executable, Argv,
		   [ stdout(pipe(Out)),
		     stderr(pipe(Error)),
		     process(PID)
		   | Extra
		   ]),
    thread_create(relay_output([output-Out, error-Error]), Id, []),
    process_wait(PID, Status),
    thread_join(Id, _),
    (   Status == exit(0)
    ->  true
    ;   throw(error(process_error(process(Executable, Argv), Status), _))
    ).
run_process(Executable, Argv, Options) :-
    process_create_options(Options, Extra),
    setup_call_cleanup(
	process_create(Executable, Argv,
		       [ stdout(pipe(Out)),
			 stderr(pipe(Error)),
			 process(PID)
		       | Extra
		       ]),
	(   read_stream_to_codes(Out, OutCodes, []),
	    read_stream_to_codes(Error, ErrorCodes, []),
	    process_wait(PID, Status)
	),
	(   close(Out),
	    close(Error)
	)),
    print_error(ErrorCodes, Options),
    print_output(OutCodes, Options),
    (   Status == exit(0)
    ->  true
    ;   throw(error(process_error(process(Executable, Argv), Status), _))
    ).

process_create_options(Options, Extra) :-
    option(directory(Dir), Options, .),
    (   option(env(Env), Options)
    ->  Extra = [cwd(Dir), environment(Env)]
    ;   Extra = [cwd(Dir)]
    ).

relay_output([]) :- !.
relay_output(Output) :-
    pairs_values(Output, Streams),
    wait_for_input(Streams, Ready, infinite),
    relay(Ready, Output, NewOutputs),
    relay_output(NewOutputs).

relay([], Outputs, Outputs).
relay([H|T], Outputs0, Outputs) :-
    selectchk(Type-H, Outputs0, Outputs1),
    (   at_end_of_stream(H)
    ->  close(H),
	relay(T, Outputs1, Outputs)
    ;   read_pending_codes(H, Codes, []),
	relay(Type, Codes),
	relay(T, Outputs0, Outputs)
    ).

relay(error,  Codes) :-
    set_prolog_flag(message_context, []),
    print_error(Codes, []).
relay(output, Codes) :-
    print_output(Codes, []).

print_output(OutCodes, Options) :-
    option(output(Codes), Options),
    !,
    Codes = OutCodes.
print_output(OutCodes, _) :-
    print_message(informational, build(process_output(OutCodes))).

print_error(OutCodes, Options) :-
    option(error(Codes), Options),
    !,
    Codes = OutCodes.
print_error(OutCodes, _) :-
    phrase(classify_message(Level), OutCodes, _),
    print_message(Level, build(process_output(OutCodes))).

classify_message(error) -->
    string(_), "fatal:",
    !.
classify_message(error) -->
    string(_), "error:",
    !.
classify_message(warning) -->
    string(_), "warning:",
    !.
classify_message(informational) -->
    [].


:- multifile user:file_search_path/2.
user:file_search_path(pack_build_path, Dir) :-
    nb_current('$build_tool_env', Env),
    memberchk('PATH'=Path, Env),
    path_sep(Sep),
    atomic_list_concat(Dirs, Sep, Path),
    member(Dir, Dirs),
    Dir \== ''.

%!  has_program(+Spec) is semidet.
%!  has_program(+Spec, -Path) is semidet.
%!  has_program(+Spec, -Path, +Env:list) is semidet.
%
%   True when the OS has the program  Spec at the absolute file location
%   Path. Normally called as   e.g.  has_program(path(cmake), CMakeExe).
%   The second allows passing in an  environment as Name=Value pairs. If
%   this contains a value for ``PATH``,  this   is  used rather than the
%   current path variable.

has_program(Prog) :-
    has_program(Prog, _).
has_program(Program, Path) :-
    has_program(Program, Path, []).

has_program(path(Program), Path, Env), memberchk('PATH'=_, Env) =>
    setup_call_cleanup(
	b_setval('$build_tool_env', Env),
	has_program(pack_build_path(Program), Path, []),
	nb_delete('$build_tool_env')).
has_program(Name, Path, Env), plain_program_name(Name) =>
    has_program(path(Name), Path, Env).
has_program(Program, Path, _Env) =>
    exe_options(ExeOptions),
    absolute_file_name(Program, Path,
		       [ file_errors(fail)
		       | ExeOptions
		       ]).

plain_program_name(Name) :-
    atom(Name),
    \+ sub_atom(Name, _, _, _, '/').

exe_options(Options) :-
    current_prolog_flag(windows, true),
    !,
    Options = [ extensions(['',exe,com]), access(read) ].
exe_options(Options) :-
    Options = [ access(execute) ].

%!  path_sep(-Sep) is det.
%
%   Path separator for the OS. `;` for Windows, `:` for POSIX.

path_sep(Sep) :-
    (   current_prolog_flag(windows, true)
    ->  Sep = ';'
    ;   Sep = ':'
    ).


		 /*******************************
		 *             OS PATHS		*
		 *******************************/

setup_path :-
    current_prolog_flag(windows, true),
    !,
    setup_path([make, gcc]).
setup_path.

%!  setup_path(+Programs) is det.
%
%   Deals  with  specific  platforms  to  add  specific  directories  to
%   ``$PATH`` such that we can  find   the  tools.  Currently deals with
%   MinGW on Windows to provide `make` and `gcc`.

setup_path(Programs) :-
    maplist(has_program, Programs).
setup_path(_) :-
    current_prolog_flag(windows, true),
    !,
    (   mingw_extend_path
    ->  true
    ;   print_message(error, build(no_mingw))
    ).
setup_path(_).

%!  mingw_extend_path is semidet.
%
%   Check that gcc.exe is on ``%PATH%``  and if not, try to extend the
%   search path.

mingw_extend_path :-
    absolute_file_name(path('gcc.exe'), _,
		       [ access(exist),
			 file_errors(fail)
		       ]),
    !.
mingw_extend_path :-
    mingw_root(MinGW),
    directory_file_path(MinGW, bin, MinGWBinDir),
    atom_concat(MinGW, '/msys/*/bin', Pattern),
    expand_file_name(Pattern, MsysDirs),
    last(MsysDirs, MSysBinDir),
    prolog_to_os_filename(MinGWBinDir, WinDirMinGW),
    prolog_to_os_filename(MSysBinDir, WinDirMSYS),
    getenv('PATH', Path0),
    atomic_list_concat([WinDirMSYS, WinDirMinGW, Path0], ';', Path),
    setenv('PATH', Path),
    print_message(informational,
		  build(mingw_extend_path(WinDirMSYS, WinDirMinGW))).

mingw_root(MinGwRoot) :-
    current_prolog_flag(executable, Exe),
    sub_atom(Exe, 1, _, _, :),
    sub_atom(Exe, 0, 1, _, PlDrive),
    Drives = [PlDrive,c,d],
    member(Drive, Drives),
    format(atom(MinGwRoot), '~a:/MinGW', [Drive]),
    exists_directory(MinGwRoot),
    !.

		 /*******************************
		 *            MESSAGES          *
		 *******************************/

:- multifile prolog:message//1.

prolog:message(build(Msg)) -->
    message(Msg).

message(no_mingw) -->
    [ 'Cannot find MinGW and/or MSYS.'-[] ].
message(process_output(Codes)) -->
    { split_lines(Codes, Lines) },
    process_lines(Lines).
message(step_failed(Step)) -->
    [ 'No build plugin could execute build step ~p'-[Step] ].
message(mingw_extend_path(WinDirMSYS, WinDirMinGW)) -->
    [ 'Extended %PATH% with ~p and ~p'-[WinDirMSYS, WinDirMinGW] ].

split_lines([], []) :- !.
split_lines(All, [Line1|More]) :-
    append(Line1, [0'\n|Rest], All),
    !,
    split_lines(Rest, More).
split_lines(Line, [Line]).

process_lines([]) --> [].
process_lines([H|T]) -->
    [ '~s'-[H] ],
    (   {T==[]}
    ->  []
    ;   [nl], process_lines(T)
    ).
