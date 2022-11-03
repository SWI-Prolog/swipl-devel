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

:- module(build_cmake,
          []).
:- use_module(tools).

/** <module> CMake plugin to deal with build steps

Manage a CMake project. This prefers  the `ninja` generator if available
in ``$PATH``.
*/

:- multifile
    cmake_option/2.                     % +Env, -Option

:- multifile
    prolog:build_file/2,
    prolog:build_step/4.                % Step, Tool, SrcDir, BuildDir

prolog:build_file('CMakeLists.txt', cmake).

prolog:build_step(configure, cmake, State0, State) :-
    ensure_build_dir(build, State0, State),
    findall(Opt, cmake_option(State, Opt), Argv, [..]),
    run_process(path(cmake), Argv,
                [ directory(State.bin_dir),
                  env(State.env)
                ]).
prolog:build_step(build, cmake, State0, State) :-
    ensure_build_dir(build, State0, State),
    run_process(path(cmake), ['--build', '.'],
                [ directory(State.bin_dir),
                  env(State.env)
                ]).
prolog:build_step(test, cmake, State0, State) :-
    ensure_build_dir(build, State0, State),
    (   directory_file_path(State.bin_dir, 'CTestTestfile.cmake', TestFile),
        exists_file(TestFile)
    ->  test_jobs(Jobs),
        run_process(path(ctest), ['-j', Jobs, '--output-on-failure'],
                    [ directory(State.bin_dir),
                      env(State.env)
                    ])
    ;   true
    ).
prolog:build_step(install, cmake, State0, State) :-
    ensure_build_dir(build, State0, State),
    run_process(path(cmake), ['--install', '.'],
                [ directory(State.bin_dir),
                  env(State.env)
                ]).
prolog:build_step(clean, cmake, State0, State) :-
    ensure_build_dir(build, State0, State),
    run_cmake_target(State, clean).
prolog:build_step(distclean, cmake, State, State) :-
    directory_file_path(State.src_dir, build, BinDir),
    (   exists_directory(BinDir)
    ->  delete_directory_and_contents(BinDir)
    ;   true
    ).

%!  cmake_option(+State, -Define) is nondet.

cmake_option(_, CDEF) :-
    current_prolog_flag(executable, Exe),
    format(atom(CDEF), '-DSWIPL=~w', [Exe]).
cmake_option(_, CDEF) :-
    prolog_install_prefix(Prefix),
    format(atom(CDEF), '-DCMAKE_INSTALL_PREFIX=~w', [Prefix]).
cmake_option(State, CDEF) :-
    cmake_build_type(State, Type),
    format(atom(CDEF), '-DCMAKE_BUILD_TYPE=~w', [Type]).
cmake_option(State, Opt) :-
    has_program(path(ninja), _, State.env),
    member(Opt, ['-G', 'Ninja']).

run_cmake_target(State, Target) :-
    cmake_generator_file(Generator, File),
    directory_file_path(State.bin_dir, File, AbsFile),
    exists_file(AbsFile),
    run_process(path(Generator), [Target],
                [ directory(State.bin_dir),
                  env(State.env)
                ]).

cmake_generator_file(ninja, 'build.ninja').
cmake_generator_file(make,  'Makefile').

cmake_build_type(State, Type) :-
    Type = State.get(build_type),
    !.
cmake_build_type(_, Type) :-
    current_prolog_flag(cmake_build_type, PlType),
    project_build_type(PlType, Type),
    !.
cmake_build_type(_, 'Release').

project_build_type('PGO', 'Release').
project_build_type('DEB', 'Release').
project_build_type(Type, Type).


test_jobs(Jobs) :-
    current_prolog_flag(cpu_count, Cores),
    Jobs is max(1, max(min(4,Cores), Cores//2)).
