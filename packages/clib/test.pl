:- module(test_nlp,
	  [ run_tests/0,
            run_tests/1
	  ]).

:- asserta(user:file_search_path(foreign, '.')).
:- asserta(user:file_search_path(library, '.')).
:- asserta(user:file_search_path(library, '../plunit')).

:- use_module(library(plunit)).
:- use_module(library(crypt)).

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


