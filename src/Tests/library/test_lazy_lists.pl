:- module(test_lazy_lists,
	  [ test_lazy_lists/0
	  ]).
:- use_module(lazy_lists).
:- use_module(library(plunit)).
:- use_module(library(dcg/basics)).

test_lazy_lists :-
	run_tests([ lazy_lists
		  ]).

:- begin_tests(lazy_lists).

test(list, Rest == `world`) :-
	setup_call_cleanup(
	    ( open_string("hello world", In),
	      lazy_list(lazy_get_codes(In, 1), List)
	    ),
	    phrase(("hello", " ", string(Rest)), List),
	    close(In)),
	!.

:- end_tests(lazy_lists).
