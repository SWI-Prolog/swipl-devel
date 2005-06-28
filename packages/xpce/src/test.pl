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

:- get(@pce, version, V),
   format('XPCE/SWI-Prolog test suite. (XPCE version ~w)~n\
          To run all tests run ?- test.~n~n', [V]).



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
fmt(fmt-6) :-
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
wname(append-1) :-
	atom_codes(X, [1100]),
	get('', append, X, Y),
	Y == X.


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
wstring(store-1) :-
	numlist(32, 1000, L),
	atom_codes(Watom, L),
	tmp_file(tb, Tmp),
	new(S, string('%s', Watom)),
	send(S, save_in_file, Tmp),
	get(file(Tmp), object, S2),
	get(S2, value, Value),
	Value == Watom.


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
		 *	       FILE		*
		 *******************************/

foreign(Name) :-
	atom_codes(Name, [1087, 1083, 1072, 1090, 1085, 1072, 1103]).

file(env-1) :-
	new(F, file('$PCEHOME/Defaults')),
	send(F, exists),
	absolute_file_name(pce('Defaults'), PlName),
	get(F, name, PceName),
	same_file(PceName, PlName),
	send(F, same, PlName).
file(home-1) :-
	new(F, file('~/foobar')),
	get(F, name, PceName),
	expand_file_name('~/foobar', [PlName]),
	same_file(PceName, PlName).
file(abs-1) :-
	new(F, file(foobar)),
	get(F, absolute_path, Abs),
	is_absolute_file_name(Abs),
	file_directory_name(Abs, Here),
	working_directory(H,H),
	same_file(Here, H).
file(exists-1) :-
	expand_file_name(*, Files),
	forall(member(F, Files),
	       (   exists_file(F)
	       ->  send(file(F), exists)
	       ;   \+ send(file(F), exists)
	       )).
file(utf8-1) :-
	foreign(Name),
	new(F, file(Name)),
	send(F, open, write),
	send(F, append, 'Hello world\n'),
	send(F, close),
	new(F2, file(Name)),
	exists_file(Name),
	send(F2, exists),
	get(F2, contents, string('Hello world\n')),
	send(F2, remove).
file(backup-1) :-
	foreign(Name),
	get(file(Name), backup_file_name, Backup),
	atom_concat(Name, ~, Backup).


		 /*******************************
		 *	      DIRECTORY		*
		 *******************************/

dir(cwd-1) :-
	new(D, directory(.)),
	get(D, path, Path),
	same_file(Path, '.').
dir(parent-1) :-
	new(D, directory(.)),
	get(D, parent, PD),
	get(PD, path, Parent),
	working_directory(CWD, CWD),
	file_directory_name(CWD, PlParent),
	same_file(Parent, PlParent).
dir(members-1) :-
	new(D, directory(.)),
	get_chain(D, files, Files),
	forall(member(F, Files), send(file(F), exists)).
dir(members-2) :-
	new(D, directory(.)),
	get_chain(D, directories, Dirs),
	forall(member(F,Dirs), send(directory(F), exists)).
dir(members-3) :-
	new(D, directory(.)),
	get_chain(D, files, Files),
	get_chain(D, directories, Dirs),
	append(Files, Dirs, All),
	sort(All, S0),
	expand_file_name(*, PlAll),
	sort(PlAll, S1),
	S0 == S1.
dir(foreign-1) :-
	foreign(Name),
	new(D, directory(Name)),
	send(D, make),
	send(D, exists),
	delete_directory(Name).
dir(foreign-2) :-
	foreign(Name),
	new(D, directory(Name)),
	send(D, make),
	send(D, exists),
	new(D2, directory(.)),
	get_chain(D2, directories, Dirs),
	member(Name, Dirs),
	delete_directory(Name).
dir(foreign-3) :-
	foreign(Name),
	new(D, directory(Name)),
	send(D, make),
	send(D, exists),
	send(D, cd),
	new(D2, directory(.)),
	get(D2, path, PD2),
	file_directory_name(PD2, Old),
	working_directory(_, ..),
	same_file(Old, '.'),
	delete_directory(Name).


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
textbuffer(store-1) :-
	tmp_file(tb, Tmp),
	new(TB, text_buffer('Hello world')),
	send(TB, save_in_file, Tmp),
	get(file(Tmp), object, TB2),
	get(TB2?contents, value, Value),
	Value == 'Hello world'.
textbuffer(store-2) :-
	numlist(32, 1000, L),
	atom_codes(WAtom, L),
	tmp_file(tb, Tmp),
	new(TB, text_buffer(WAtom)),
	send(TB, save_in_file, Tmp),
	get(file(Tmp), object, TB2),
	get(TB2?contents, value, Value),
	Value == WAtom.


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
asfile(tb-4) :-
	numlist(0, 20000, L),
	atom_codes(Atom, L),
	new(TB, text_buffer),
	send(TB, contents, Atom),
	pce_open(TB, read, In),
	get_code(In, 0),
	seek(In, 0, current, P1), P1 == 1,
	forall(member(Pos, [20, 5000, 10240]),
	       (   seek(In, Pos, bof, P), P == Pos,
		   get_code(In, Code), Code == Pos
	       )),
	seek(In, 0, eof, EndPos), atom_length(Atom, EndPos),
	close(In),
	free(TB).

read_stream_to_terms(In, Terms) :-
	read(In, T0),
	read_stream_to_terms(T0, In, Terms).

read_stream_to_terms(end_of_file, _, []) :- !.
read_stream_to_terms(H, In, [H|T]) :-
	read(In, T0),
	read_stream_to_terms(T0, In, T).


		 /*******************************
		 *	       BOM		*
		 *******************************/

bom(bom-1) :-
	tmp_file(bom, File),
	open(File, write, Out,
	     [ encoding(unicode_le),
	       bom(true)
	     ]),
	forall(between(32, 1100, C), put_code(Out, C)),
	close(Out),
	new(V, view),
	send(V, load, file(File)),
	get(V?file, encoding, unicode_le),
	send(V, append, '\nHello world'),
	send(V, save),
	new(F, file(File)),
	send(F, open, read),
	get(F, encoding, unicode_le),
	get(F, bom, @on),
	send(F, close),
	delete_file(File).


		 /*******************************
		 *	     SELECTION		*
		 *******************************/

selection(cutpaste-1) :-
	Atom = 'hello world',
	send(@display, selection, primary, string(Atom)),
	get(@display, selection, primary, utf8_string, string(X)),
	X == Atom.
selection(cutpaste-2) :-
	watom(Atom),
	send(@display, selection, primary, string(Atom)),
	get(@display, selection, primary, utf8_string, string(X)),
	X == Atom.


		 /*******************************
		 *	       IMAGE		*
		 *******************************/

image(bitmap-1) :-
	new(I, image(@nil, 100, 100, bitmap)),
	get(I, pixel, 10, 10, @off),
	send(I, pixel, 10, 10, @on),
	get(I, pixel, 10, 10, @on),
	free(I).


		 /*******************************
		 *	       REGEX		*
		 *******************************/

re_tb(TB, From, GapAt) :-
	new(TB, text_buffer(From)),
	send(TB, insert, GapAt, x),
	send(TB, delete, GapAt, 1).

re_frag(Frag, From) :-
	new(TB, text_buffer),
	send(TB, append, 'leader '),
	get(TB, size, Start),
	send(TB, append, From),
	send(TB, append, ' trailer'),
	atom_length(From, Len),
	new(Frag, fragment(TB, Start, Len)).

re_target(In, atom, In).
re_target(In, text_buffer, TB) :-
	re_tb(TB, In, 0).
re_target(In, fragment, Frag) :-
	re_frag(Frag, In).

re_free(Atom) :-
	atom(Atom), !.
re_free(Frag) :-
	send(Frag, instance_of, fragment), !,
	get(Frag, text_buffer, TB),
	free(TB).
re_free(Obj) :-
	free(Obj).

re_match(regex(foo), @default, @default, 'aap foo bar', 4-7).
re_match(regex(foo), 4, @default, 'aap foo bar', 4-7).
re_match(regex(foo), 11, 0, 'aap foo bar', 4-7).
re_match(regex(foo, @off), @default, @default, 'aap FOO bar', 4-7).
re_match(regex(foo), @default, @default, String, 4-7) :-
	atom_codes(W, [1080]),
	atom_concat('aap foo bar', W, String).

regex(regex-1) :-
	State = state(ok),
	(   re_match(Term, Start, End, In, From-To),
	    new(Re, Term),
	    re_target(In, How, Target),
	    (   send(Re, search, Target, Start, End),
		get(Re, register_start, From),
		get(Re, register_end, To)
	    ->  re_free(Target),
		put(.), flush
	    ;   format('~NRegex: failed ~w on ~w (~w)~n', [Term, In, How]),
		nb_setarg(1, State, error)
	    ),
	    fail
	;   arg(1, State, ok)
	).
regex(regex-2) :-			% backref test
	send(regex('(.)x\\1'), match, 'axa').
regex(regex-3) :-			% backward search
	get(regex('[({[]|:'), search, '{ ', 2, 0, X),
	X == 0.
regex(regex-4) :-			% BOSONLY on branches
	\+ send(regex('foo|bar'), match, 'xx bar').
regex(regex-5) :-			% end-of-line in partial-matches
	get(regex('^.'), search, 'hello', 5, 0, X),
	X == 0.


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
testset(file).				% file (-name) handling
testset(dir).				% directory (-name) handling
testset(bom).				% Byte Order Mark hanling
testset(textbuffer).
testset(asfile).			% test pce_open and friends
testset(selection).			% X11 selection
testset(image).				% Simple image manipulation
testset(regex).				% Regular expression matches

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

