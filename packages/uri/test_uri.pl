:- module(test_uri,
	  [ run_tests/0,
            run_tests/1
	  ]).

:- asserta(user:file_search_path(foreign, '.')).
:- asserta(user:file_search_path(library, '.')).
:- asserta(user:file_search_path(library, '../plunit')).

:- use_module(library(plunit)).
:- use_module(library(uri)).

:- begin_tests(uri).

test(encode, X == 'a%20b') :-
	encode_uri('a b', X).
test(encode_uri_component,
     A=='~!%40%23%24%25%5E%26*()%7B%7D%5B%5D%3D%3A%2F%2C%3B%3F%2B\'%22%5C') :-
	encode_uri_component('~!@#$%^&*(){}[]=:/,;?+\'"\\', A).
test(decode_uri_component, A=='~!@#$%^&*(){}[]=:/,;?+\'"\\') :-
        decode_uri_component('~!%40%23%24%25%5E%26*()%7B%7D%5B%5D%3D%3A%2F%2C%3B%3F%2B\'%22%5C', A).

:- end_tests(uri).
