/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2013, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(test_saved_states,
	  [ test_saved_states/0
	  ]).

:- prolog_load_context(directory, Here),
   atom_concat(Here, '../../../../packages/clib', ClibDir0),
   absolute_file_name(ClibDir0, ClibDir),
   asserta(user:file_search_path(library, ClibDir)),
   asserta(user:file_search_path(foreign, ClibDir)).

:- if(absolute_file_name(foreign(process), _,
			 [ file_type(executable),
			   file_errors(fail),
			   access(read)
			 ])).

:- use_module(library(plunit)).
:- use_module(library(lists)).
:- use_module(library(process)).
:- use_module(library(filesex)).
:- use_module(library(readutil)).
:- use_module(library(debug)).
:- if(exists_source(library(rlimit))).
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
	        not offer us enough file open files~n', []).
test_saved_states :-
	run_tests([ saved_state
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

%%	state_output(-FileName)
%
%	Name of the file we use for temporary output of the state.

state_output(State) :-
	working_directory(Dir, Dir),
	directory_file_path(Dir, 'test_state.exe', State).

me(MeSH) :-
	absolute_file_name('swipl.sh', MeSH,
			   [ access(execute),
			     file_errors(fail)
			   ]), !.
me(Exe) :-
	current_prolog_flag(executable, Exe).

create_state(File, Output, Args) :-
	me(Me),
	append(Args, ['-o', Output, '-c', File], AllArgs),
	test_dir(TestDir),
	debug(save, 'Creating state in ~q using ~q ~q', [TestDir, Me, AllArgs]),
	process_create(Me, AllArgs,
		       [ cwd(TestDir),
			 stderr(pipe(Err))
		       ]),
	read_stream_to_codes(Err, ErrOutput),
	close(Err),
	debug(save, 'Saved state', []),
	assertion(no_error(ErrOutput)).

run_state(Exe, Args, Result) :-
	debug(save, 'Running state ~q ~q', [Exe, Args]),
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

remove_state(State) :-
	catch(delete_file(State), _, true).

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
	state_output(Exe),
	call_cleanup(
	    ( create_state('input/plain.pl', Exe, ['-g', echo_true]),
	      run_state(Exe, [], Result)
	    ),
	    remove_state(Exe)).
test(argv, Result == [[aap,noot,mies]]) :-
	state_output(Exe),
	call_cleanup(
	    ( create_state('input/plain.pl', Exe, ['-g', echo_argv]),
	      run_state(Exe, [aap, noot, mies], Result)
	    ),
	    remove_state(Exe)).

:- end_tests(saved_state).

:- else.				% No library(process) found

test_saved_states :-
	format(user_error, 'Skipped saved state tests; requires clib~n', []).

:- endif.


