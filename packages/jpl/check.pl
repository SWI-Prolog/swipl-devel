/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2004, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- asserta(file_search_path(foreign, '.')).
:- asserta(file_search_path(jar, '.')).

:- use_module(jpl).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
RDF-DB test file.  A test is a clause of the form:

	<TestSet>(<Name>-<Number>) :- Body.

If the body fails, an appropriate  error   message  is  printed. So, all
goals are supposed to  succeed.  The   predicate  testset/1  defines the
available test sets. The public goals are:

	?- runtest(+TestSet).
	?- test.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- print_message(informational,
		 format('JPL test suite.  To run all tests run ?- test.~n~n',
			[])).

% Required to get this always running regardless of user LANG setting.
% Without this the tests won't run on machines with -for example- LANG=ja
% according to NIDE Naoyuki, nide@ics.nara-wu.ac.jp.  Thanks!

:- getenv('LANG', _) -> setenv('LANG', 'C'); true.


		 /*******************************
		 *	      RESOURCE		*
		 *******************************/

simple(1) :-
	jpl_new('TestJPL', [hello], Obj),
	jpl_get(Obj, name, Name),
	Name == hello.


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
	load_files(Script, [silent(true)]),
	Pred.

run_test_scripts(Directory) :-
	(   script_dir(ScriptDir),
	    concat_atom([ScriptDir, /, Directory], Dir),
	    exists_directory(Dir)
	->  true
	;   Dir = Directory
	),
	atom_concat(Dir, '/*.pl', Pattern),
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

testset(simple).

%	testdir(Dir)
%	
%	Enumerate directories holding tests.

testdir('Tests').

:- dynamic
	failed/1,
	blocked/2.

test :-
	retractall(failed(_)),
	retractall(blocked(_,_)),
	forall(testset(Set), runtest(Set)),
	scripts,
%	statistics,
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

runtest(Name) :-
	format('Running test set "~w" ', [Name]),
	flush,
	functor(Head, Name, 1),
	nth_clause(Head, _N, R),
	clause(Head, _, R),
	(   catch(Head, Except, true)
	->  (   var(Except)
	    ->  put(.), flush
	    ;   Except = blocked(Reason)
	    ->  assert(blocked(Head, Reason)),
		put(!), flush
	    ;   test_failed(R, Except)
	    )
	;   test_failed(R, fail)
	),
	fail.
runtest(_) :-
	format(' done.~n').
	
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

