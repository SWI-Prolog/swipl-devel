:- module(ztest,
	  [
	  ]).
:- asserta(user:file_search_path(foreign, '.')).
:- asserta(user:file_search_path(library, '.')).
:- asserta(user:file_search_path(library, '../plunit')).

:- use_module(user:library(zlib)).
:- use_module(user:library(plunit)).

:- begin_tests(zlib).

:- end_tests(zlib).
