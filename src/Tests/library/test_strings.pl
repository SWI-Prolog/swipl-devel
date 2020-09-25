:- module(test_strings,
          [ test_strings/0
          ]).
:- use_module(library(plunit)).
:- use_module(strings).

test_strings :-
    run_tests([strings]).

:- begin_tests(strings).

test(plain, A == "hello world") :-
    A = {|string(To)||hello world|}.
test(interpolate, A == "hello world") :-
    To = world,
    A = {|string(To)||hello {To}|}.
test(interpolate, error(existence_error(template_var, 'ToX'))) :-
    To = world,
    _ = {|string(To)||hello {ToX}|}.
test(interpolate, A == "hello world") :-
    To = no_world,
    A = {|string(To)||hello {ToX,world}|}.
test(dedent, A == "hello\n  world\n") :-
    A = {|string||
	 | hello
         |   world
         |}.
test(dedent, A == "hello\n  world\n") :-
    A = {|string||
	 hello
           world
         |}.
test(dedent, A == "hello\n  world") :-
    A = {|string||
	 | hello
         |   world|}.

:- end_tests(strings).
