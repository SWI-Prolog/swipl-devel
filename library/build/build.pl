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

:- module(build_build,
          [ run_process/3,              % +Executable, +Argv, +Options
            path_sep/1                  % -Separator
          ]).
:- autoload(library(lists), [selectchk/3, member/2]).
:- autoload(library(option), [option/2, option/3]).
:- autoload(library(pairs), [pairs_values/2]).
:- autoload(library(process), [process_create/3, process_wait/2]).
:- autoload(library(readutil), [read_stream_to_codes/3]).
:- autoload(library(dcg/basics), [string/3]).

/** <module> Utilities for building foreign resources
*/


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
        b_setval('$prolog_pack_build_env', BuildEnv),
        run_process(pack_build_path(Exe), Argv, Options),
        nb_delete('$prolog_pack_build_env')).
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
    ->  Extra = [cwd(Dir), env(Env)]
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
    print_message(informational, pack(process_output(OutCodes))).

print_error(OutCodes, Options) :-
    option(error(Codes), Options),
    !,
    Codes = OutCodes.
print_error(OutCodes, _) :-
    phrase(classify_message(Level), OutCodes, _),
    print_message(Level, pack(process_output(OutCodes))).

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
    nb_current('$prolog_pack_build_env', Env),
    memberchk('PATH'=Path, Env),
    path_sep(Sep),
    atomic_list_concat(Dirs, Sep, Path),
    member(Dir, Dirs),
    Dir \== ''.

%!  path_sep(-Sep) is det.
%
%   Path separator for the OS. `;` for Windows, `:` for POSIX.

path_sep(Sep) :-
    (   current_prolog_flag(windows, true)
    ->  Sep = ';'
    ;   Sep = ':'
    ).
