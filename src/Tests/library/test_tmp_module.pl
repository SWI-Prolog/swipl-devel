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

