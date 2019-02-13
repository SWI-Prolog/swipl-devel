/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2013-2015, University of Amsterdam
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
	  [ test_saved_states/0,
	    build_test_shlibs/0
	  ]).

:- prolog_load_context(directory, Here),
   atom_concat(Here, '../../../../packages/clib', ClibDir0),
   absolute_file_name(ClibDir0, ClibDir),
   asserta(user:file_search_path(library, ClibDir)),
   asserta(user:file_search_path(foreign, ClibDir)).

has_foreign_lib(Lib) :-
    absolute_file_name(foreign(Lib), _,
		       [ file_type(executable),
			 file_errors(fail),
			 access(read)
		       ]).

:- if(has_foreign_lib(process)).

:- use_module(library(plunit)).
:- use_module(library(lists)).
:- use_module(library(process)).
:- use_module(library(filesex)).
:- use_module(library(readutil)).
:- use_module(library(debug)).
:- if(has_foreign_lib(rlimit)).
:- use_module(library(rlimit)).
:- endif.

% keep debug statements
:- set_prolog_flag(optimise, false).

%:- debug(save).

/** <module> Test saved states

This moduele tests the saved state generation capabilities.
*/

test_saved_states :-
	\+ enough_files, !,
	format(user_error,
	       'Skipped saved state files because the system does\n\c
	        not offer us enough open files~n', []).
test_saved_states :-
	run_tests([ saved_state,
	            multi_arch
		  ]).

:- dynamic
	test_dir/1.

:- prolog_load_context(directory, Dir),
   retractall(test_dir(_)),
   asserta(test_dir(Dir)).

%%	enough_files
%
%	This test uses quite a  few   file  descriptors, apparently more
%	than some confined build and test  environments offer. Hence, we
%	test for this and try to enlarge the number.

:- if(current_predicate(rlimit/3)).
enough_files :-
	catch(rlimit(nofile, Limit, Limit), E,
	      print_message(warning, E)),
	(   subsumes_term(error(domain_error(resource, nofile), _), E)
	->  debug(save(max_files), 'Unknown max open files', [])
	;   Limit > 16,
	    debug(save(max_files), 'Reported ~D max open files', [Limit])
	), !.
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

%%	state_output(+Id, -FileName)
%
%	Name of the file we use for temporary output of the state.

state_output(Id, State) :-
	working_directory(Dir, Dir),
	current_prolog_flag(pid, Pid),
	format(atom(File), 'test_state_~w_~w.exe', [Id, Pid]),
	directory_file_path(Dir, File, State).

me(Exe) :-
	current_prolog_flag(executable, WinExeOS),
	prolog_to_os_filename(WinExe, WinExeOS),
	file_base_name(WinExe, WinFile),
	downcase_atom(WinFile, 'swipl-win.exe'), !,
	file_directory_name(WinExe, WinDir),
	atomic_list_concat([WinDir, 'swipl.exe'], /, PlExe),
	prolog_to_os_filename(PlExe, Exe).
me(MeSH) :-
	absolute_file_name('swipl.sh', MeSH,
			   [ access(execute),
			     file_errors(fail)
			   ]), !.
me(Exe) :-
	current_prolog_flag(executable, Exe).

:- dynamic
	win_path_set/0.

set_windows_path :-
	\+ win_path_set,
	current_prolog_flag(windows, true), !,
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
	create_state(File, Output, Args, ErrOutput),
	assertion(no_error(ErrOutput)).

create_state(File, Output, Args, ErrOutput) :-
	me(Me),
	append(Args, ['-o', Output, '-c', File, '-f', none], AllArgs),
	test_dir(TestDir),
	debug(save, 'Creating state in ~q using ~q ~q', [TestDir, Me, AllArgs]),
	process_create(Me, AllArgs,
		       [ cwd(TestDir),
			 stderr(pipe(Err)),
			 process(Pid)
		       ]),
	read_stream_to_codes(Err, ErrOutput),
	close(Err),
	process_wait(Pid,_Status), % to allow error status
	debug(save, 'Saved state', []).

run_state(Exe, Args, Result) :-
	debug(save, 'Running state ~q ~q', [Exe, Args]),
	set_windows_path,
	process_create(Exe, Args,
		       [ stdout(pipe(Out)),
			 stderr(pipe(Err))
		       ]),
	call_cleanup(
	    ( read_terms(Out, Result),
	      read_stream_to_codes(Err, ErrOutput)
	    ),
	    ( close(Out),
	      close(Err)
	    )),
	assertion(no_error(ErrOutput)).

remove_state(_State) :-
	debugging(save(keep)), !.
remove_state(State) :-
	catch(delete_file(State), _, true).

swipl_ld(SwiplLd) :-
	current_prolog_flag(executable,SwiplExe),
	file_directory_name(SwiplExe, Dir),
	format(atom(SwiplLd0), "~w/swipl-ld",[Dir]),
	prolog_to_os_filename(SwiplLd0,SwiplLd).

build_shlib(Name) :-
	swipl_ld(SwiplLd),
	format(atom(Cmd),
	       "~w -shared -fPIC -o input/~w.so input/~w.c",
	       [SwiplLd,Name, Name]),
	shell(Cmd).

build_shlib(Name, Dep, DepDir) :-
	swipl_ld(SwiplLd),
	format(atom(Cmd),
	       "~w -shared -fPIC -L~w -l~w -o input/~w.so input/~w.c ",
	       [SwiplLd, DepDir, Dep, Name, Name]),
	shell(Cmd).

build_test_shlibs :-
	build_shlib("shlib_no_deps"),
	build_shlib("libdep"),
	build_shlib("shlib_with_dep", "dep", "input").

%%	read_terms(+In:stream, -Data:list)
%
%	True when Data are the Prolog terms on In.

read_terms(In, List) :-
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
		 *	       TESTS		*
		 *******************************/

:- begin_tests(saved_state, [sto(rational_trees)]).

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


:- begin_tests(multi_arch).
test(load_shlib_no_deps, Result == [three_three_three]) :-
	state_output(4, Exe),
	call_cleanup(
	    ( create_state('input/multi_arch_simple.pl', Exe, ['-g', shlib_test]),
	      run_state(Exe, [], Result)
	    ),
	    remove_state(Exe)).
test(load_wrong_arch, ErrOut == "ERROR: architecture_shlib(x86_64-strange1) `'./input/shlib_no_deps'' does not exist\n") :-
	state_output(5, Exe),
	call_cleanup(
	    ( create_state('input/multi_arch_simple.pl', Exe,
	                 ['--foreign=x86_64-strange1', '-g', shlib_test],
	                 ErrOut0),
	      string_codes(ErrOut, ErrOut0)
	    ),
	    remove_state(Exe)).
test(compatible_arch, Result == [three_three_three]) :-
	state_output(6, Exe),
	call_cleanup(
	    ( create_state('input/multi_arch_compat.pl', Exe,
	                   [ '--foreign=x86_64-myarch',
	                     '--no-autoload',
	                     '-g', shlib_test
	                   ]),
	      run_state(Exe, [], Result)
	    ),
	    remove_state(Exe)).
test(retry_missing_shlib, Result == [three_three_three]) :-
	state_output(7, Exe),
	setup_call_cleanup(
	    rename_file('input/shlib_no_deps.so',
	                'input/shlib_no_deps.so.bak'),
	    ( create_state('input/missing_shlib.pl', Exe,
	                   [ '--no-autoload',
	                     '-g', shlib_test
	                   ]),
	      run_state(Exe, [], Result)
	    ),
	    ( catch(rename_file('input/shlib_no_deps.so.bak',
	                        'input/shlib_no_deps.so'),
	                   _,true),
	      remove_state(Exe)
	    )).
test(shlib_with_dep, Result == [three_three_three]) :-
	state_output(8, Exe),
	setup_call_cleanup(
	    copy_file('input/libdep.so','libdep.so'), % for linker to find it
	    ( create_state('input/shlib_with_dep.pl', Exe,
	                   [ '--no-autoload',
	                     '--foreign=save',
	                     '-g', shlib_test
	                   ]),
	      delete_file('libdep.so'), % to load it from state
	      run_state(Exe, [], Result)
	    ),
	    ( catch(delete_file('libdep.so'), _, true),
	      remove_state(Exe)
	    )).
:- end_tests(multi_arch).

:- else.				% No library(process) found

test_saved_states :-
	format(user_error, 'Skipped saved state tests; requires clib~n', []).

:- endif.


