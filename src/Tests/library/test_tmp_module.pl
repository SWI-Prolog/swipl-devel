/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           www.swi-prolog.org
    Copyright (c)  2014, University of Amsterdam
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

:- module(test_tmp_module,
	  [ test_tmp_module/0,
	    test_tmp_module/1
	  ]).
:- use_module(library(modules)).
:- use_module(library(thread)).

%%	test_tmp_module
%
%	Test  concurrent  loading  of  nqueens   in  multiple  temporary
%	modules. This test was first of all  designed to test for memory
%	leaks.

test_tmp_module :-
	test_tmp_module(100).

test_tmp_module(N) :-
	length(L, N),
	concurrent_maplist(tmp_queen_list, L),
	garbage_collect_atoms.

queens_file(File) :-
	source_file(test_tmp_module, Here),
	file_directory_name(Here, Dir),
	atomic_list_concat([Dir, /, 'data/queens.pl'], File).

tmp_queens(S) :-
	N is random(1<<62),		% was uuid(UUID), but that is a package
	format(atom(UUID), 'tmp-~d', [N]),
	queens_file(Queens),
	in_temporary_module(
	    UUID,
	    setup_call_cleanup(
		open(Queens, read, In),
		load_files(UUID,
			   [ module(UUID),
			     stream(In),
			     silent(true)
			   ]),
		close(In)),
	    call(queens(8, S))).

tmp_queen_list(L) :-
	findnsols(10, S, tmp_queens(S), L), !.

