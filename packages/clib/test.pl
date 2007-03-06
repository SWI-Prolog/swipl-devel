:- module(test_nlp,
	  [ run_tests/0,
            run_tests/1
	  ]).

:- asserta(user:file_search_path(foreign, '.')).
:- asserta(user:file_search_path(library, '.')).
:- asserta(user:file_search_path(library, '../plunit')).

:- use_module(library(plunit)).
:- use_module(library(crypt)).
:- use_module(library(sha)).

:- begin_tests(crypt).

test(default, []) :-
	Passwd = "My password",
	crypt(Passwd, E),
	ground(E),
	crypt(Passwd, E).
test(md5, []) :-
	Passwd = "My password",
	append("$1$", _, E),
	crypt(Passwd, E),
	ground(E),
	crypt(Passwd, E).

:- end_tests(crypt).


:- begin_tests(sha).

test(sha1, true(Hash=[136, 67, 215, 249, 36, 22, 33, 29,
		      233, 235, 185, 99, 255, 76, 226,
		      129, 37, 147, 40, 120])) :-
	sha_hash(foobar, Hash, []).
test(sha1, true(Hash=[136, 67, 215, 249, 36, 22, 33, 29,
		      233, 235, 185, 99, 255, 76, 226, 
		      129, 37, 147, 40, 120])) :-
	sha_hash(foobar, Hash, [algorithm(sha1)]).
test(sha256, true(Hash=[195, 171, 143, 241, 55, 32, 232, 173,
			144, 71, 221, 57, 70, 107, 60, 137, 
			116, 229, 146, 194, 250, 56, 61, 74,
			57, 96, 113, 76, 174, 240, 196, 242])) :-
	sha_hash(foobar, Hash, [algorithm(sha256)]).

:- end_tests(sha).

