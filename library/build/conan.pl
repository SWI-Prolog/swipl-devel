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

:- module(build_conan,
          []).
:- autoload(library(apply), [foldl/4]).
:- autoload(library(filesex), [directory_file_path/3]).
:- autoload(library(lists), [select/4, append/3]).
:- autoload(library(readutil), [read_line_to_string/2]).
:- autoload(library(build/tools), [ensure_build_dir/3, run_process/3]).
:- autoload(library(dcg/basics), [whites/2, remainder/3]).

:- use_module(tools).

/** <module> Support Conan dependency handling

[Conan](https://conan.io)  is  a  cross-platform   package  manager  for
(notably) C and C++. It  can  be   used  to  get  access to dependencies
required by the pack. This notably provides an alternative for libraries
that are either not well maintained by  e.g., the Linux distributions or
where the public versions are often outdated.

A conan step is excuted if a   file `conanfile.txt` or `conanfile.py` is
found. This plugin knows about conan `virtualenv` and if this is enabled
it adds the environment settings  from   the  generated vitualenv to the
build process.
*/

:- multifile
    prolog:build_file/2,
    prolog:build_step/4,                % +Step, +Tool, +SrcDir, +BuildDir
    prolog:build_config/5.              % +Type, +Tool, +SrcDir, +BuildDir, -Config

prolog:build_file('conanfile.txt',  conan).
prolog:build_file('conanfile.py',   conan).

prolog:build_step(dependencies, conan, State0, State) :-
    ensure_build_dir(build, State0, State1),
    run_process(path(conan), ['install', '-b', missing, State1.src_dir],
                [ env(State1.env),
                  directory(State1.bin_dir)
                ]),
    load_conan_virtualenv(State1, State).

load_conan_virtualenv(State0, State) :-
    directory_file_path(State0.bin_dir, 'environment.sh.env', ConanEnvFile),
    (   exists_file(ConanEnvFile)
    ->  import_environment(ConanEnvFile, State0.env, Env),
        State = State0.put(env,Env)
    ;   State = State0
    ).


%!  import_environment(+File, +Env0, -Env) is det.
%
%   Extend the environment using settings from File. This currently aims
%   at conan virtual environments. File is  supposed to contain variable
%   assignments in POSIX shell  compatible   syntax.  Value  assignments
%   deals  with  single  and  double  quotes  as  well  as  interpolated
%   variables.  Variable  substitution  deals    with  plain  variables,
%   ``${VAR-Default}``, ``${VAR:-Default}``, ``${VAR+Alternative}``  and
%   ``${VAR:+Alternative}``

import_environment(File, Env0, Env) :-
    setup_call_cleanup(
        open(File, read, In),
        join_environment(In, Env0, Env),
        close(In)).

join_environment(In, Env0, Env) :-
    read_line_to_string(In, Line),
    (   Line == end_of_file
    ->  Env = Env0
    ;   join_line(Line, Env0, Env1),
        join_environment(In, Env1, Env)
    ).

join_line(Line, Env0, Env) :-
    string_codes(Line, Codes),
    phrase(env_assigments(Assignments, Env0), Codes),
    foldl(env_assign, Assignments, Env0, Env).

env_assign(Var=Value, Env0, Env) :-
    select(Var=_, Env0, Var=Value, Env),
    !.
env_assign(Var=Value, Env, [Var=Value|Env]).

env_assigments([Var=Value|T], Env) -->
    whites,
    vname(Var), "=", !,
    var_value(ValueCodes, Env, -1),
    { atom_codes(Value, ValueCodes) },
    env_assigments(T, Env).
env_assigments([], _) -->
    whites,
    (   "#"
    ->  remainder(_)
    ;   []
    ).

var_value(Codes, Env, EOF) -->
    "\"",
    !,
    dquoted(Codes, Tail, Env),
    var_value(Tail, Env, EOF).
var_value(Codes, Env, EOF) -->
    "\'",
    !,
    squoted(Codes, Tail),
    var_value(Tail, Env, EOF).
var_value(Codes, Env, EOF) -->
    "$",
    !,
    subst_var(Codes, Tail, Env),
    var_value(Tail, Env, EOF).
var_value([H|T], Env0, EOF) -->
    [H],
    { \+ (   H == EOF
         ;   H == -1,
             code_type(H, white)
         )
    },
    !,
    var_value(T, Env0, EOF).
var_value([], _, _) -->
    [].

dquoted(Codes, Tail, _) -->
    "\"",
    !,
    { Tail = Codes }.
dquoted(Codes, Tail, Env) -->
    "$",
    !,
    subst_var(Codes, Tail0, Env),
    dquoted(Tail0, Tail, Env).
dquoted([H|T], Tail, Env) -->
    [H],
    dquoted(T, Tail, Env).

squoted(Codes, Tail) -->
    "'",
    !,
    { Tail = Codes }.
squoted([H|T], Tail) -->
    [H],
    squoted(T, Tail).

subst_var(Codes, Tail, Env) -->
    "{",
    !, vname(Name),
    var_default(Name, Env, Codes, Tail),
    "}".
subst_var(Codes, Tail, Env) -->
    vname(Name),
    {   memberchk(Name=Value, Env)
    ->  string_codes(Value, CodesC),
        append(CodesC, Tail, Codes)
    ;   Tail = Codes
    }.

vname(Name) -->
    [C],
    { code_type(C, csymf) },
    vname_cont(Cs),
    { atom_codes(Name, [C|Cs]) }.

vname_cont([H|T]) -->
    [H],
    { code_type(H, csym) },
    !,
    vname_cont(T).
vname_cont([]) -->
    [].

var_default(Name, Env, Codes, Tail) -->
    var_def_sep(Op, Null),
    var_value(Default, Env, 0'}),
    {   Op == (-)
    ->  (   memberchk(Name=Value, Env),
            \+ isnull(Null, Value)
        ->  string_codes(Value, CodesC),
            append(CodesC, Tail, Codes)
        ;   append(Default, Tail, Codes)
        )
    ;   Op == (+),
        (   memberchk(Name=Value, Env),
            \+ isnull(Null, Value)
        ->  append(Default, Tail, Codes)
        ;   Tail = Codes
        )
    }.

var_def_sep(-, false) --> "-".
var_def_sep(-, true)  --> ":-".
var_def_sep(+, false) --> "+".
var_def_sep(+, true)  --> ":+".

isnull(false, _).
isnull(true, '').
isnull(true, "").
