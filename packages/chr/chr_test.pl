/*  $Id$

    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1996 University of Amsterdam. All rights reserved.
*/

:- asserta(user:file_search_path(chr, '.')).
:- asserta(user:file_search_path(library, '.')).
:- use_module(chr).			% == library(chr)

:- set_prolog_flag(optimise, true).
%:- set_prolog_flag(trace_gc, true).

:- format('CHR test suite.  To run all tests run ?- test.~n~n', []).

% Required to get this always running regardless of user LANG setting.
% Without this the tests won't run on machines with -for example- LANG=ja
% according to NIDE Naoyuki, nide@ics.nara-wu.ac.jp.  Thanks!

:- getenv('LANG', _) -> setenv('LANG', 'C'); true.
	

		 /*******************************
		 *	      SCRIPTS		*
		 *******************************/


:- dynamic
	script_dir/1.

set_script_dir :-
	script_dir(_), !.
set_script_dir :-
	find_script_dir(Dir),
	assert(script_dir(Dir)).

find_script_dir(Dir) :-
	prolog_load_context(file, File),
	follow_links(File, RealFile),
	file_directory_name(RealFile, Dir).

follow_links(File, RealFile) :-
	read_link(File, _, RealFile), !.
follow_links(File, File).


:- set_script_dir.

run_test_script(Script) :-
	file_base_name(Script, Base),
	file_name_extension(Pred, _, Base),
	load_files(Script, []), %[silent(true)]),
	Pred.

run_test_scripts(Directory) :-
	(   script_dir(ScriptDir),
	    concat_atom([ScriptDir, /, Directory], Dir),
	    exists_directory(Dir)
	->  true
	;   Dir = Directory
	),
	atom_concat(Dir, '/*.chr', Pattern),
	expand_file_name(Pattern, Files),
	file_base_name(Dir, BaseDir),
	format('Running scripts from ~w ', [BaseDir]), flush,
	run_scripts(Files),
	format(' done~n').

run_scripts([]).
run_scripts([H|T]) :-
	(   catch(run_test_script(H), Except, true)
	->  (   var(Except)
	    ->  put(.), flush
	    ;   Except = blocked(Reason)
	    ->  assert(blocked(H, Reason)),
		put(!), flush
	    ;   script_failed(H, Except)
	    )
	;   script_failed(H, fail)
	),
	run_scripts(T).

script_failed(File, fail) :-
	format('~NScript ~w failed~n', [File]),
	assert(failed(script(File))).
script_failed(File, Except) :-
	message_to_string(Except, Error),
	format('~NScript ~w failed: ~w~n', [File, Error]),
	assert(failed(script(File))).


		 /*******************************
		 *        TEST MAIN-LOOP	*
		 *******************************/

testdir('Tests').

:- dynamic
	failed/1,
	blocked/2.

test :-
	retractall(failed(_)),
	retractall(blocked(_,_)),
	scripts,
	report_blocked,
	report_failed.

scripts :-
	forall(testdir(Dir), run_test_scripts(Dir)).


report_blocked :-
	findall(Head-Reason, blocked(Head, Reason), L),
	(   L \== []
        ->  format('~nThe following tests are blocked:~n', []),
	    (	member(Head-Reason, L),
		format('    ~p~t~40|~w~n', [Head, Reason]),
		fail
	    ;	true
	    )
        ;   true
	).
report_failed :-
	findall(X, failed(X), L),
	length(L, Len),
	(   Len > 0
        ->  format('~n*** ~w tests failed ***~n', [Len]),
	    fail
        ;   format('~nAll tests passed~n', [])
	).

test_failed(R, Except) :-
	clause(Head, _, R),
	functor(Head, Name, 1),
	arg(1, Head, TestName),
	clause_property(R, line_count(Line)),
	clause_property(R, file(File)),
	(   Except == fail
	->  format('~N~w:~d: Test ~w(~w) failed~n',
		   [File, Line, Name, TestName])
	;   message_to_string(Except, Error),
	    format('~N~w:~d: Test ~w(~w):~n~t~8|ERROR: ~w~n',
		   [File, Line, Name, TestName, Error])
	),
	assert(failed(Head)).

blocked(Reason) :-
	throw(blocked(Reason)).

