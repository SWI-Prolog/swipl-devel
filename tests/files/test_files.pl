/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2009-2022, University of Amsterdam
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

:- module(test_files, [test_files/0]).
:- use_module(library(plunit)).
:- use_module(library(debug)).
:- use_module(library(apply)).
:- if(exists_source(library(process))).
:- use_module(library(process), [process_create/3, process_wait/2]).
:- endif.

/** <module> Test file-handling

This module is a Unit test for Prolog file-handling primitives.

@author	Jan Wielemaker
*/

test_files :-
	run_tests([ files
		  ]).

:- begin_tests(files).

% this test verifies that atoms associated with temporary files
% are properly deleted.

test(tmp_cleanup) :-
	tmp_atoms(Atoms0),
	(   between(1, 10, _),
	    tmp_file(magic_sjwefrbas, Tmp),
	    open(Tmp, write, Out),
	    close(Out),
	    delete_file(Tmp),
	    fail
	;   tmp_atoms(Atoms1)
	),
        subset(Atoms0, Atoms1).

test(tmp_directory, [true(Dir1 \== Dir2), cleanup(rm_dirs([Dir1,Dir2]))]) :-
	tmp_directory(test, Dir1),
	tmp_directory(test, Dir2),
	assertion(exists_directory(Dir1)),
	assertion(exists_directory(Dir2)),
	current_prolog_flag(tmp_dir, Tmp),
	file_directory_name(Dir1, Parent),
	assertion(same_file(Parent, Tmp)).
test(tmp_directory_clash,		% the next name exists: skip it
     [ condition(\+ current_prolog_flag(windows, true)),
       true(Dir \== Next),
       cleanup(rm_dirs([Next,Dir]))
     ]) :-
	tmp_file(test, Name),
	atomic_list_concat(Parts, '_', Name),
	once(append(Prefix, [N0], Parts)),
	atom_number(N0, N),
	N1 is N+1,
	append(Prefix, [N1], NextParts),
	atomic_list_concat(NextParts, '_', Next),
	make_directory(Next),
	tmp_directory(test, Dir),
	assertion(exists_directory(Dir)).

test(tmp_directory_halt,		% halt removes files before their dir
     [ condition(exists_source(library(process))),
       Left == []
     ]) :-
	tmp_directory(halt, Parent),
	format(string(Goal),		% 10 nested dirs, each with a file
	       "set_prolog_flag(tmp_dir, ~q),\c
		forall(between(1, 10, _),\c
		       ( tmp_directory(nested, D),\c
			 set_prolog_flag(tmp_dir, D),\c
			 tmp_file_stream(text, _, S), close(S)\c
		       ))", [Parent]),
	current_prolog_flag(executable, Exe),
	process_create(Exe, ['-f', none, '-g', Goal, '-t', halt],
		       [process(PID)]),
	process_wait(PID, exit(0)),
	directory_files(Parent, Entries),
	subtract(Entries, ['.', '..'], Left),
	delete_directory_and_contents(Parent).

rm_dirs(Dirs) :-
	forall(member(Dir, Dirs),
	       catch(delete_directory(Dir), error(_,_), true)).

tmp_atoms(List) :-
	flush_unregistering,
	agc,
	findall(X, tmp_atom(X), Xs),
	sort(Xs, List).

tmp_atom(X) :-
	current_atom(X),
	sub_atom(X, _, _, _, magic_sjwefrbas).

:- dynamic a/1.

%%	flush_unregistering
%
%	Register/unregister    an    arbitrary    atom      to     flush
%	LD->atoms.unregistering.

flush_unregistering :-
	assertz(a(a)),
	retractall(a(_)).

%%	agc
%
%	If   other   threads   are   active,   it   is   possible   that
%	garbage_collect_atoms/0 succeeds without doing anything: the AGC
%	request is scheduled, but  executed  at   a  time  that no other
%	threads execute slow calls that block AGC.
%
%	This predicate loops until AGC has really been performed.

agc :-
	statistics(agc, AGC0),
	repeat,
	    garbage_collect_atoms,
	    statistics(agc, AGC1),
	(   AGC1 > AGC0
	->  !
	;   sleep(0.01),
	    fail
	).

:- meta_predicate
	with_input_stream(+, +, ?, 0).

with_input_stream(Type, Content, In, Action) :-
	setup_call_cleanup(
	    setup_call_cleanup(
		tmp_file_stream(Type, File, Out),
		write(Out, Content),
		close(Out)),
	    setup_call_cleanup(
		open(File, read, In),
		call(Action),
		close(In)),
	    delete_file(File)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% more tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

magic_pred_123.

test(directory_files, error(existence_error(_,_))) :-
	directory_files(magic_sgf7222389y91, _).
test(directory_files, true) :-
	once(predicate_property(_:magic_pred_123, file(File))),
	file_directory_name(File, Dir),
	file_base_name(File, Plain),
	directory_files(Dir, Files),
	memberchk(Plain, Files).
test(max_path_len, error(representation_error(max_path_length))) :-
	long_segment(40000, F),
	absolute_file_name(F, _, [access(read)]).
test(max_path_len, error(representation_error(max_path_length))) :-
	long_segment(40000, F),
	file_base_name(F, _).
test(max_path_len, error(representation_error(max_path_length))) :-
	long_segment(40000, D),
	long_segment(2000, F),
	atomic_list_concat([D,F], /, Path),
	file_directory_name(Path, _).
test(at_end_of_stream) :-
	with_input_stream(
	    text, '', In,
	    at_end_of_stream(In)).
test(at_end_of_stream) :-
	with_input_stream(
	    text, 'a', In,
	    ( get_char(In, C),
	      assertion(C == 'a'),
	      at_end_of_stream(In))).
test(at_end_of_stream) :-
	with_input_stream(
	    text, 'a', In,
	    \+ at_end_of_stream(In)).
test(file_directory_name, D == '/a') :-
	file_directory_name('/a/b/', D).
test(file_directory_name, D == '.') :-
	file_directory_name('abc', D).
test(file_directory_name, D == '.') :-
	file_directory_name('abc/', D).
test(file_base_name, D == 'b') :-
	file_base_name('/a/b/', D).
test(file_base_name, D == '') :-
	file_base_name('', D).
test(file_base_name, D == 'aaa') :-
	file_base_name('aaa/', D).
test(file_base_name, D == '/') :-
	file_base_name('///', D).
test(nul, [condition(current_prolog_flag(windows,true))]) :-
	setup_call_cleanup(
	    open(nul, write, Out),
	    format(Out, 'Sending to NUL', []),
	    close(Out)).

long_segment(N, Seg) :-
	length(L, N),
	maplist(=('a'), L),
	atom_chars(Seg, L).

:- end_tests(files).
