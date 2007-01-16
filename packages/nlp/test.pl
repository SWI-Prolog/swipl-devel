:- module(test_nlp,
	  [ run_tests/0,
            run_tests/1
	  ]).

:- asserta(user:file_search_path(foreign, '.')).
:- asserta(user:file_search_path(library, '.')).
:- asserta(user:file_search_path(library, '../plunit')).

:- use_module(library(plunit)).
:- use_module(library(porter_stem)).
:- use_module(library(double_metaphone)).

:- begin_tests(stem).

test(stem, [true(X==walk)]) :-
	porter_stem(walks, X).
test(stem, [true(X==walk)]) :-
	porter_stem(walk, X).
test(tokens, [true(X==[hello, world, !])]) :-
	tokenize_atom('hello world!', X).
test(stem_list, [true(X==[hello, world])]) :-
	atom_to_stem_list('hello worlds!', X).

:- end_tests(stem).

:- begin_tests(metaphone).

test(metaphone, [true(X=='ARLT')]) :-
     double_metaphone(world, X).

:- end_tests(metaphone).

