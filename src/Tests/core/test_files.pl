/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2009-2015, University of Amsterdam
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

:- module(test_files, [test_files/0]).
:- use_module(library(plunit)).
:- use_module(library(debug)).
:- use_module(library(apply)).

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

%%	agc/0
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
	length(L, 10000),
	maplist(=('a'), L),
	atom_chars(F, L),
	absolute_file_name(F, _, [access(read)]).
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

:- end_tests(files).
