/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2013-2025, University of Amsterdam
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

:- module(test_saved_states,
          [ test_saved_states/0
          ]).

has_foreign_lib(Lib) :-
    absolute_file_name(foreign(Lib), _,
                       [ file_type(executable),
                         file_errors(fail),
                         access(read)
                       ]).

:- if(has_foreign_lib(process)).

:- use_module(library(plunit)).
:- use_module(library(process)).
:- use_module(library(filesex)).
:- use_module(library(lists)).
:- use_module(library(readutil)).
:- use_module(library(debug)).
:- if(has_foreign_lib(rlimit)).
:- use_module(library(rlimit)).
:- endif.

% keep debug statements
:- set_prolog_flag(optimise_debug, false).

:- debug(save).

/** <module> Test saved states

This module tests the saved state generation capabilities.
*/

test_saved_states :-
    \+ enough_files,
    !,
    format(user_error,
           'Skipped saved state files because the system does\n\c
        not offer us enough open files~n', []).
test_saved_states :-
    run_tests([ saved_state
              ]).

:- dynamic
    test_dir/1.

:- prolog_load_context(directory, Dir),
   retractall(test_dir(_)),
   asserta(test_dir(Dir)).

%!  enough_files
%
%   This test uses quite a  few   file  descriptors, apparently more
%   than some confined build and test  environments offer. Hence, we
%   test for this and try to enlarge the number.

:- if(current_predicate(rlimit/3)).
enough_files :-
    catch(rlimit(nofile, Limit, Limit), E,
          print_message(warning, E)),
    (   subsumes_term(error(domain_error(resource, nofile), _), E)
    ->  debug(save(max_files), 'Unknown max open files', [])
    ;   Limit > 16,
        debug(save(max_files), 'Reported ~D max open files', [Limit])
    ),
    !.
enough_files :-
    MaxOpen = 16,
    catch(rlimit(nofile, _, MaxOpen), E,
          ( print_message(warning, E),
            fail
          )),
    debug(save(max_files), 'Raised max open files to ~d', [MaxOpen]).
:- else.
enough_files :-
    debug(save(max_files), 'No info on max open files; assuming ok', []).
:- endif.

%!  state_output(+Id, -FileName)
%
%   Name of the file we use for temporary output of the state.

state_output(Id, State) :-
    working_directory(Dir, Dir),
    current_prolog_flag(pid, Pid),
    format(atom(File), 'test_state_~w_~w.exe', [Id, Pid]),
    directory_file_path(Dir, File, State).

:- dynamic
    win_path_set/0.

set_windows_path :-
    \+ win_path_set,
    current_prolog_flag(windows, true),
    !,
    current_prolog_flag(executable, WinExeOS),
    prolog_to_os_filename(WinExe, WinExeOS),
    file_directory_name(WinExe, PlWinDir),
    prolog_to_os_filename(PlWinDir, WinDir),
    getenv('PATH', Path0),
    atomic_list_concat([WinDir, Path0], ';', Path),
    setenv('PATH', Path),
    asserta(win_path_set).
set_windows_path.

create_state(File, Output, Args) :-
    test_dir(TestDir),
    append(Args, [ '-o', Output, '-c', File, '-f', none ], AllArgs),
    debug(save, 'Creating state in ~q using prolog(swipl) ~q', [TestDir, AllArgs]),
    process_create(prolog(swipl), AllArgs,
                   [ cwd(TestDir),
                     stderr(pipe(Err))
                   ]),
    read_stream_to_codes(Err, ErrOutput),
    close(Err),
    debug(save, 'Saved state', []),
    assertion(no_error(ErrOutput)).

:- multifile prolog:prolog_tool/4.
:- dynamic   prolog:prolog_tool/4.

%!  run_state(+State, +Args, -Result:list) is det.
%
%   Run the created saved State. There  are two scenarios. Normally, the
%   State should be an  executable,  so  we   can  directly  run  it. If
%   SWI-Prolog is embedded though, this may not   work.  In that case we
%   must run the equivalent of  ``swipl   -x  State  <args>``. This code
%   assumes    that    if    prolog:prolog_tool/4      succeeds.     See
%   prolog:prolog_tool/4 for details.

run_state(State, Args, Result) :-
    prolog:prolog_tool(swipl, _Prog, Args, _Args1),
    !,
    run_state1(prolog(swipl), ['-x',State|Args], Result).
run_state(Exe, Args, Result) :-
    run_state1(Exe, Args, Result).

run_state1(Exe, Args, Result) :-
    debug(save, 'Running state ~q ~q', [Exe, Args]),
    set_windows_path,
    current_prolog_flag(home, HOME), % needed for MSYS2
    process_create(Exe, Args,
                   [ stdout(pipe(Out)),
                     stderr(pipe(Err)),
                     environment(['SWI_HOME_DIR'=HOME])
                   ]),
    call_cleanup(
        ( read_terms(Out, Result),
          read_stream_to_codes(Err, ErrOutput)
        ),
        ( close(Out),
          close(Err)
        )),
    assertion(no_error(ErrOutput)).

remove_state(State) :-
    debugging(save(keep)),
    !,
    print_message(information, format('Created state in "~w"', [State])).
remove_state(State) :-
    catch(delete_file(State), _, true).

%!  read_terms(+In:stream, -Data:list)
%
%   True when Data are the Prolog  terms  on   In.  We  use a 60 seconds
%   timeout to avoid this test  from   hanging  indefinitely in case the
%   saved state does not terminate.

read_terms(In, List) :-
    (   current_prolog_flag(windows, true)
    ->  true
    ;   set_stream(In, timeout(60))
    ),
    read_term(In, T0, []),
    read_terms(T0, In, List).
read_terms(end_of_file, _, []) :- !.
read_terms(H, In, [H|T]) :-
    read_term(In, H2, []),
    read_terms(H2, In, T).


no_error(Codes) :-
    \+ phrase((..., "ERROR:"), Codes),
    \+ phrase((..., "error:"), Codes),
    \+ phrase((..., "Warning:"), Codes).

... --> [] | [_], ... .


                 /*******************************
                 *             TESTS            *
                 *******************************/

:- begin_tests(saved_state).

test(true, Result == [true]) :-
    state_output(1, Exe),
    call_cleanup(
        ( create_state('input/plain.pl', Exe, ['-g', echo_true]),
          run_state(Exe, [], Result)
        ),
        remove_state(Exe)).
test(argv, Result == [[aap,noot,mies]]) :-
    state_output(2, Exe),
    call_cleanup(
        ( create_state('input/plain.pl', Exe, ['-g', echo_argv]),
          run_state(Exe, [aap, noot, mies], Result)
        ),
        remove_state(Exe)).
test(true, Result == [true]) :-
    state_output(3, Exe),
    call_cleanup(
        ( create_state('input/data.pl', Exe, ['-g', test]),
          run_state(Exe, [], Result)
        ),
        remove_state(Exe)).

:- end_tests(saved_state).

:- else.                                % No library(process) found

test_saved_states :-
    format(user_error, 'Skipped saved state tests; requires clib~n', []).

:- endif.
