/*  $Id$

    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1996 University of Amsterdam. All rights reserved.
*/

:- asserta(file_search_path(foreign, '.')).
:- use_module(rdf_db).
:- use_module(rdfs).
:- use_module(library(xsdp_types)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
RDF-DB test file.  A test is a clause of the form:

	<TestSet>(<Name>-<Number>) :- Body.

If the body fails, an appropriate  error   message  is  printed. So, all
goals are supposed to  succeed.  The   predicate  testset/1  defines the
available test sets. The public goals are:

	?- runtest(+TestSet).
	?- test.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- format('RDF-DB test suite.  To run all tests run ?- test.~n~n', []).

% Required to get this always running regardless of user LANG setting.
% Without this the tests won't run on machines with -for example- LANG=ja
% according to NIDE Naoyuki, nide@ics.nara-wu.ac.jp.  Thanks!

:- getenv('LANG', _) -> setenv('LANG', 'C'); true.


		 /*******************************
		 *	     TEST DATA		*
		 *******************************/

data(string, '').
data(string, 'This is a nice string').

data(int, 0).
data(int, -67).
data(int, 327848).

data(float, 0.0).
data(float, 48.25).

data(term, [let, us, test, a, list]).
data(term, [let, us, test, another, list]).


		 /*******************************
		 *	      LOAD/SAVE		*
		 *******************************/

save_reload_db :-
	tmp_file(rdf, File),
	rdf_save_db(File),
	rdf_reset_db,
	rdf_load_db(File),
	delete_file(File).


save_reload :-
	tmp_file(rdf, File),
	rdf_save(File),
	rdf_reset_db,
	rdf_load(File,
		 [ base_uri([]),	% do not qualify
		   convert_typed_literal(convert_typed)
		 ]),
	delete_file(File).

%	convert_typed(+Type, +Content, -Object)
%	
%	Convert to type(Type, PrologValue), providing the inverse of
%	the default RDF as produced by rdf_db.pl

convert_typed(Type, Content, type(Type, Value)) :-
	xsdp_convert(Type, Content, Value).


		 /*******************************
		 *	      RESOURCE		*
		 *******************************/

resource(1) :-
	rdf_assert(x, a, aap),
	rdf_assert(x, a, noot),
	findall(X, rdf(x, a, X), L),
	L == [aap, noot].


		 /*******************************
		 *	    SIMPLE LITERAL	*
		 *******************************/

literal(1) :-
	findall(V, data(_, V), Vs),
	forall(member(Value, Vs),
	       rdf_assert(x, a, literal(Value))),
	findall(V, (rdf(x, a, X), X = literal(V)), V2),
	V2 == Vs.


		 /*******************************
		 *	   UNIFYING ARGS	*
		 *******************************/

same(1) :-
	rdf_assert(a,b,c),
	rdf_assert(x,x,x),
	rdf(X,X,X),
	X == x.

		 /*******************************
		 *	   TYPED LITERALS	*
		 *******************************/

typed(1) :-
	findall(type(T,V), data(T, V), TVs),
	forall(member(Value, TVs),
	       rdf_assert(x, a, literal(Value))),
	findall(V, (rdf(x, a, X), X = literal(V)), V2),
	V2 == TVs.
typed(2) :-
	findall(type(T,V), data(T, V), TVs),
	forall(member(Value, TVs),
	       rdf_assert(x, a, literal(Value))),
	findall(V, rdf(x, a, literal(V)), V2),
	V2 == TVs.
typed(3) :-
	findall(type(T,V), data(T, V), TVs),
	forall(member(Value, TVs),
	       rdf_assert(x, a, literal(Value))),
	X = type(T,V),
	findall(X, rdf(x, a, literal(X)), TV2),
	TV2 == TVs.
typed(save_db) :-
	findall(type(T,V), data(T, V), TVs),
	forall(member(Value, TVs),
	       rdf_assert(x, a, literal(Value))),
	save_reload_db,
	X = type(T,V),
	findall(X, rdf(x, a, literal(X)), TV2),
	TV2 == TVs.
typed(save) :-
	findall(type(T,V),
		( data(T, V),
		  T \== term,
		  V \== ''
		), TVs),
	forall(member(Value, TVs),
	       rdf_assert(x, a, literal(Value))),
	save_reload,
	findall(X, rdf(x, a, literal(X)), TV2),
	(   same_set(TV2, TVs)
	->  true
	;   format('TV2 = ~q~n', [TV2]),
	    fail
	).


		 /*******************************
		 *	 XML:LANG HANDLING	*
		 *******************************/

lang_data :-
	lang_data(x, a).

lang_data(S, A) :-
	rdf_assert(S, A, literal(lang(nl, 'Jan'))),
	rdf_assert(S, A, literal(lang(en, 'John'))),
	rdf_assert(S, A, literal('Johannes')).

same_set(S1, S2) :-
	sort(S1, Sorted1),
	sort(S2, Sorted2),
	Sorted1 =@= Sorted2.

lang(1) :-
	lang_data,
	findall(X, rdf(x, a, literal(X)), Xs),
	Xs == [ lang(nl, 'Jan'),
		lang(en, 'John'),
		'Johannes'
	      ]. 
lang(2) :-
	lang_data,
	findall(X, rdf(x, a, literal(lang(nl, X))), Xs),
	Xs == [ 'Jan' ].
lang(3) :-
	lang_data,
	X = lang(_,_),
	findall(X, rdf(x, a, literal(X)), Xs),
	Xs =@= [ lang(nl, 'Jan'),
		 lang(en, 'John'),
		 lang(_,  'Johannes')
	       ].
lang(4) :-
	lang_data,
	rdf(S, P, literal('Jan')), S == x, P == a,
	rdf(S, P, literal('Johannes')), S == x, P == a.
lang(save_db) :-
	lang_data,
	save_reload_db,
	X = lang(_,_),
	findall(X, rdf(x, a, literal(X)), Xs),
	(   Xs =@= [ lang(nl, 'Jan'),  lang(en, 'John'), lang(_, 'Johannes') ]
	->  true
	;   format(user_error, 'Xs = ~w~n', [Xs]),
	    fail
	).
lang(save) :-
	lang_data,
	save_reload,
	findall(X, rdf(x, a, literal(X)), Xs),
	(   same_set(Xs,
		     [ lang(nl, 'Jan'),  lang(en, 'John'), 'Johannes' ])
	->  true
	;   format(user_error, 'Xs = ~q~n', [Xs]),
	    fail
	).

		 /*******************************
		 *	       UPDATE		*
		 *******************************/

update(subject) :-
	rdf_assert(x, a, v),
	rdf_update(x, a, v, subject(y)),
	rdf(y, a, v).
update(predicate) :-
	rdf_assert(x, a, v),
	rdf_update(x, a, v, predicate(b)),
	rdf(x, b, v).
update(object-1) :-
	rdf_assert(x, a, v),
	rdf_update(x, a, v, object(w)),
	rdf(x, a, w).
update(object-2) :-
	rdf_assert(x, a, v),
	rdf_update(x, a, v, object(literal(hello))),
	rdf(x, a, literal(hello)).
update(object-3) :-
	rdf_assert(x, a, v),
	rdf_update(x, a, v, object(literal(lang(nl, hallo)))),
	rdf(x, a, literal(lang(nl, hallo))).
update(object-4) :-			% add lang
	rdf_assert(x, a, literal(hallo)),
	rdf_update(x, a, literal(hallo),
		   object(literal(lang(nl, hallo)))),
	rdf(x, a, literal(lang(nl, hallo))).
update(object-5) :-			% only change lang
	rdf_assert(x, a, literal(lang(en, hallo))),
	rdf_update(x, a, literal(lang(en, hallo)),
		   object(literal(lang(nl, hallo)))),
	rdf(x, a, literal(lang(nl, hallo))).
update(object-6) :-			% drop lang
	rdf_assert(x, a, literal(lang(en, hallo))),
	rdf_update(x, a, literal(lang(en, hallo)),
		   object(literal(hallo))),
	rdf(x, a, literal(hallo)).


		 /*******************************
		 *	    TRANSACTIONS	*
		 *******************************/

transaction(empty-1) :-
	rdf_transaction(true),
	findall(rdf(S,P,O), rdf(S,P,O), L),
	L == [].
transaction(assert-1) :-
	rdf_transaction(rdf_assert(x, a, v)),
	findall(rdf(S,P,O), rdf(S,P,O), L),
	L == [ rdf(x, a, v)
	     ].
transaction(assert-2) :-
	\+ rdf_transaction((rdf_assert(x, a, v), fail)),
	findall(rdf(S,P,O), rdf(S,P,O), L),
	L == [].
transaction(nest-1) :-
	rdf_transaction( ( rdf_assert(x, a, v),
			   rdf_transaction(rdf_assert(x, a, v2)))),
	findall(rdf(S,P,O), rdf(S,P,O), L),
	L == [ rdf(x, a, v),
	       rdf(x, a, v2)
	     ].
transaction(nest-2) :-
	rdf_transaction( ( rdf_assert(x, a, v),
			   \+ rdf_transaction((rdf_assert(x, a, v2),fail)))),
	findall(rdf(S,P,O), rdf(S,P,O), L),
	L == [ rdf(x, a, v)
	     ].
transaction(deadlock-1) :-
	rdf_assert(x,y,z,g),
	rdf_assert(x,y,z,g),
	rdf_transaction(rdf(_S, _P, _O, _G)).
transaction(deadlock-2) :-
	tmp_file(rdf, F1),
	tmp_file(rdf, F2),
	rdf_assert(a, b, c, f1),
	rdf_assert(x, y, z, f2),
	rdf_save_db(F1, f1),
	rdf_save_db(F2, f2),
	rdf_reset_db,

	rdf_assert(l, f, F1),
	rdf_assert(l, f, F2),
	rdf_transaction(forall(rdf(l, f, F),
			       rdf_load_db(F))),
	findall(rdf(S,P,O), rdf(S,P,O), L),
	L == [ rdf(l,f,F1),
	       rdf(l,f,F2),
	       rdf(a,b,c),
	       rdf(x,y,z)
	     ],
	delete_file(F1),
	delete_file(F2).


		 /*******************************
		 *	       LABELS		*
		 *******************************/

label(1) :-
	rdf_global_id(rdfs:label, Label),
	lang_data(x, Label),
	findall(L, rdfs_label(x, L), Ls), Ls = ['Jan', 'John', 'Johannes'].
label(2) :-
	rdf_global_id(rdfs:label, Label),
	lang_data(x, Label),
	findall(L, rdfs_label(x, en, L), Ls), Ls = ['John'].


		 /*******************************
		 *	       MATCH		*
		 *******************************/

match(1) :-
	rdf_assert(a,b,literal('hello there world!')),
	rdf(a,b,literal(substring('llo'), _)).
match(2) :-
	rdf_assert(a,b,literal('hello there world!')),
	rdf(a,b,literal(word('there'), _)).
match(3) :-
	rdf_assert(a,b,literal('hello there world!')),
	rdf(a,b,literal(word('hello'), _)).
match(4) :-
	rdf_assert(a,b,literal('hello there world!')),
	rdf(a,b,literal(word('world'), _)).
match(5) :-
	rdf_assert(a,b,literal('hello there world!')),
	rdf(a,b,literal(like('*there*'), _)).
match(6) :-
	rdf_assert(a,b,literal('hello there world!')),
	rdf(a,b,literal(like('*world!*'), _)).
match(7) :-				% test backtracking
	rdf_assert(a,b,literal('hello there world there universe!')),
	rdf(a,b,literal(like('*th*uni*'), _)).


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

testset(resource).
testset(literal).
testset(same).
testset(typed).
testset(lang).
testset(update).
testset(transaction).
testset(label).
testset(match).

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
	statistics,
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
	rdf_reset_db,			% reset before each script
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

