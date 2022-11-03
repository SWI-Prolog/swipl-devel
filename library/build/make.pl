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

:- module(build_make,
          []).
:- use_module(tools).

:- autoload(library(apply), [maplist/3]).
:- autoload(library(error), [existence_error/2]).
:- autoload(library(filesex), [directory_file_path/3]).
:- autoload(library(lists), [max_list/2]).
:- use_module(library(debug), [debug/3]).

/** <module> Make plugin to deal with build steps.

This build plugin deals with GNU  style   packages.  It  knows about the
following programs:

  - automake to create Makefile.in from Makefile.am
  - autoheader to create `config.h.in` from `configure.in`
  - autoconf to create `configure` from `configure.in`
  - `configure` to create `Makefile`
  - `make` for the make steps.
*/

:- multifile
    prolog:build_file/2,
    prolog:build_step/4.                % Step, Tool, +State0, -State

prolog:build_file('configure',    configure).
prolog:build_file('configure.in', autoconf).
prolog:build_file('configure.ac', autoconf).
prolog:build_file('Makefile',     make).
prolog:build_file('makefile',     make).
prolog:build_file('Makefile.am',  automake).

prolog:build_step(configure, configure, State0, State) :-
    ensure_build_dir(., State0, State),
    automake(State),
    findall(Opt, configure_option(Opt), Opts),
    debug(build(configure), 'Configuring using configure', []),
    run_process(path(bash), [configure|Opts],
                [ env(State.env),
                  directory(State.bin_dir)
                ]).
prolog:build_step(configure, autoconf, State0, State) :-
    ensure_build_dir(., State0, State),
    automake(State),
    ProcessOptions = [directory(State.bin_dir), env(State.env)],
    findall(Opt, configure_option(Opt), ConfigOpts),
    debug(build(configure), 'Running autoheader', []),
    run_process(path(autoheader), [], ProcessOptions),
    debug(build(configure), 'Running autoconf', []),
    run_process(path(autoconf),   [], ProcessOptions),
    debug(build(configure), 'Configuring using configure', []),
    run_process(path(bash),       [configure|ConfigOpts], ProcessOptions).
prolog:build_step(build, make, State0, State) :-
    ensure_build_dir(., State0, State),
    debug(build(build), 'Running make', []),
    run_make(State, []).
prolog:build_step(install, make, State0, State) :-
    ensure_build_dir(., State0, State),
    debug(build(build), 'Running make', []),
    run_make(State, [install]).
prolog:build_step(test, make, State0, State) :-
    ensure_build_dir(., State0, State),
    debug(build(test), 'Running make check', []),
    run_make(State, [check]).
prolog:build_step(clean, make, State0, State) :-
    ensure_build_dir(., State0, State),
    debug(build(clean), 'Running make clean', []),
    run_make(State, [clean]).
prolog:build_step(distclean, make, State0, State) :-
    ensure_build_dir(., State0, State),
    debug(build(distclean), 'Running make distclean', []),
    run_make(State, [distclean]).

automake(State) :-
    needs_build(State.src_dir/'Makefile.in', [State.src_dir/'Makefile.am']),
    !,
    debug(build(configure), 'Running automake', []),
    run_process(path(automake), [],
                [ env(State.env),
                  directory(State.src_dir)
                ]).
automake(_).

run_make(State, Argv) :-
    make_program(State.env, Prog),
    run_process(Prog, Argv,
                [ directory(State.bin_dir),
                  env(State.env)
                ]).

make_program(BuildEnv, Prog) :-
    (   memberchk('MAKE'=Name, BuildEnv)
    ->  true
    ;   getenv('MAKE', Name)
    ),
    has_program(Name, Prog, BuildEnv),
    !.
make_program(BuildEnv, Prog) :-
    make_candidate(Name),
    has_program(Name, Prog, BuildEnv),
    !.
make_program(_, _) :-
    existence_error(program, make).

make_candidate(gmake).
make_candidate(make).

configure_option(Opt) :-
    prolog_install_prefix(Prefix),
    format(atom(Opt), '--prefix=~w', [Prefix]).

needs_build(Target, Sources) :-
    Error = error(existence_error(file, _), _),
    maplist(to_file, Sources, SourceFiles),
    catch(maplist(time_file, SourceFiles, SourceTimes),
          Error,
          fail),
    max_list(SourceTimes, SrcTime),
    maplist(to_file, Target, TargetFile),
    (   catch(to_file(TargetFile, TargetTime),
              Error, fail)
    ->  SrcTime > TargetTime
    ;   true
    ).

to_file(File, Path), atom(File) =>
    Path = File.
to_file(File, Path), string(File) =>
    atom_string(Path, File).
to_file(Dir/File, Path) =>
    directory_file_path(Dir, File, Path).


