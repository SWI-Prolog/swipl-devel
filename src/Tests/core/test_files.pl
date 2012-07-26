/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemak@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2008, University of Amsterdam

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

:- module(test_files, [test_files/0]).
:- use_module(library(plunit)).

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

test(tmp_cleanup, Atoms1 == Atoms0) :-
	tmp_atoms(Atoms0),
	(   between(1, 10, _),
	    tmp_file(magic_sjwefrbas, Tmp),
	    open(Tmp, write, Out),
	    close(Out),
	    delete_file(Tmp),
	    fail
	;   tmp_atoms(Atoms1)
	).

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

:- end_tests(files).
