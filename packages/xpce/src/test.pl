/*  $Id$

    Part of XPCE/SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2005, University of Amsterdam

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
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
XPCE/SWI-Prolog test file.  A test is a clause of the form:

	<TestSet>(<Name>-<Number>) :- Body.

If the body fails, an appropriate  error   message  is  printed. So, all
goals are supposed to  succeed.  The   predicate  testset/1  defines the
available test sets. The public goals are:

	?- runtest(+TestSet).
	?- test.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- format('XPCE/SWI-Prolog test suite.  To run all tests run ?- test.~n~n', []).


		 /*******************************
		 *	      NAMES		*
		 *******************************/

name(name-1) :-
	Atom = foobar,
	get(chain(Atom), head, Copy),
	Copy == Atom.


		 /*******************************
		 *	       FORMAT		*
		 *******************************/

fmt(fmt-1) :-
	get(string('hello %s', world), value, X),
	X == 'hello world'.
fmt(fmt-2) :-
	get(string('hello %d', 42), value, X),
	X == 'hello 42'.
fmt(fmt-3) :-
	get(string('%4dxx', 42), value, X),
	X == '  42xx'.
fmt(fmt-4) :-
	get(string('%-4dxx', 42), value, X),
	X == '42  xx'.
fmt(fmt-5) :-
	get(string('|%8s|', hello), value, X),
	X == '|   hello|'.
fmt(fmt-5) :-
	get(string('|%-8s|', hello), value, X),
	X == '|hello   |'.




		 /*******************************
		 *	    WIDE NAMES		*
		 *******************************/

watom(Atom) :-
	numlist(1050, 1080, L),
	atom_codes(Atom, L).

wname(wname-1) :-			% Create wide character name
	numlist(1050, 1080, L),
	atom_codes(Atom, L),
	forall(between(0, 30, I),
	       ( get(Atom, character, I, C),
		 C =:= I+1050)).
wname(wname-2) :-			% get it back as an atom
	watom(Atom),
	get(chain(Atom), head, Copy),
	Copy == Atom.


		 /*******************************
		 *	   WIDE STRINGS		*
		 *******************************/

wstring(create-1) :-
	numlist(1050, 1080, L),
	new(S, string(L)),
	forall(between(0, 30, I),
	       ( get(S, character, I, C),
		 C =:= I+1050)).
wstring(cvt-1) :-
	numlist(1050, 1080, L),
	new(S, string(L)),
	get(S, value, Atom),
	atom_codes(Atom, L).
wstring(fmt-1) :-
	watom(A),
	get(string('hello %s', A), value, A2),
	atom_concat('hello ', A, A2).
wstring(append-1) :-
	new(S, string([97,98])),
	send(S, append, string([1060,1061])),
	get(S, value, V),
	atom_codes(V, [97,98,1060,1061]).
wstring(char-1) :-
	new(S, string(x)),
	send(S, character, 0, 1000),
	get(S, size, 1),
	get(S, character, 0, X),
	X == 1000.
wstring(tr-1) :-
	new(S, string(abc)),
	send(S, translate, b, 1060),
	get(S, value, N),
	atom_codes(N, [97,1060,99]).
wstring(sub-1) :-
	new(S, string([60,61,1060])),
	get(S, sub, 0, 2, S2),
	send(S2, equal, string([60,61])).
wstring(split-1) :-
	List = ["aap", [1060,1061], "noot"],
	maplist(atom_codes, Atoms, List),
	concat_atom(Atoms, -, Text),
	get(Text, split, -, Chain),
	chain_list(Chain, Splitted),
	Splitted == Atoms.


		 /*******************************
		 *	     SOURCE SINK	*
		 *******************************/

srcsink(contents-1) :-
	numlist(32, 1000, L),
	tmp_file(test, Tmp),
	open(Tmp, write, Out, [encoding(utf8)]),
	maplist(put_code(Out), L),
	close(Out),
	new(File, file(Tmp, utf8)),
	get(File, contents, String),
	delete_file(Tmp),
	get(String, value, Atom),
	atom_codes(Atom, L).
srcsink(read-1) :-
	numlist(32, 1000, L),
	tmp_file(test, Tmp),
	open(Tmp, write, Out, [encoding(utf8)]),
	maplist(put_code(Out), L),
	close(Out),
	new(File, file(Tmp, utf8)),
	send(File, open, read),
	get(File, read, String),
	send(File, close),
	delete_file(Tmp),
	get(String, value, Atom),
	atom_codes(Atom, L).


		 /*******************************
		 *	    TEXT-BUFFER		*
		 *******************************/

textbuffer(promote-1) :-
	new(TB, text_buffer),
	send(TB, append, hello),
	watom(Wide),
	send(TB, append, Wide),
	get(TB, contents, string(New)),
	atom_concat(hello, Wide, New).
textbuffer(file-1) :-
	new(TB, text_buffer),
	numlist(32, 1000, L),
	atom_codes(WAtom, L),
	send(TB, append, WAtom),
	tmp_file(test, Tmp),
	send(TB, save, file(Tmp, utf8)),
	get(file(Tmp, utf8), contents, string(Copy)),
	delete_file(Tmp),
	Copy == WAtom.


		 /*******************************
		 *	  OBJECT-AS-FILE	*
		 *******************************/

asfile(tb-1) :-
	Term = hello('World'),
	new(TB, text_buffer),
	pce_open(TB, write, Out),
	format(Out, '~q', [Term]),
	close(Out),
	get(TB?contents, value, Atom),
	term_to_atom(Term, Atom).
asfile(tb-2) :-
	watom(Wide),
	Term = hello(Wide),
	new(TB, text_buffer),
	pce_open(TB, write, Out),
	format(Out, '~q', [Term]),
	close(Out),
	get(TB?contents, value, Atom),
	term_to_atom(Term, Atom).
asfile(tb-3) :-
	watom(Wide),
	Terms = [ aap, hello(Wide), hello(42) ],
	new(TB, text_buffer),
	pce_open(TB, write, Out),
	forall(member(T, Terms),
	       format(Out, '~q.~n', [T])),
	close(Out),
	pce_open(TB, read, In),
	read_stream_to_terms(In, Read),
	close(In),
	Read =@= Terms.

read_stream_to_terms(In, Terms) :-
	read(In, T0),
	read_stream_to_terms(T0, In, Terms).

read_stream_to_terms(end_of_file, _, []) :- !.
read_stream_to_terms(H, In, [H|T]) :-
	read(In, T0),
	read_stream_to_terms(T0, In, T).


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

testset(name).				% XPCE names
testset(wname).				% Names holding wide characters
testset(wstring).			% Strings holding wide characters
testset(fmt).				% Formatting actions
testset(srcsink).			% Source/Sink operations
testset(textbuffer).
testset(asfile).			% test pce_open and friends

%	testdir(Dir)
%	
%	Enumerate directories holding tests.

:- multifile
	testdir/1.

%testdir('Tests/attvar').


:- dynamic
	failed/1,
	blocked/2.

test :-
	retractall(failed(_)),
	retractall(blocked(_,_)),
	forall(testset(Set),		% force XOCE incremental GC
	       send(@prolog, runtest, Set)),
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

