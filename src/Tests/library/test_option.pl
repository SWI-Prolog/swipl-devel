:- module(test_option,
	  [ test_option/0
	  ]).
:- use_module(library(plunit)).

/** <module> Test set for library(option)


@tbd	A lot more tests.  In particular test and precise the relation
	between options and modules.
*/

test_option :-
	run_tests([ option
		  ]).

:- begin_tests(option).
:- use_module(library(option)).

test(merge_empty, X == [a(b), c(a, b)]) :-
	merge_options([a=b, c(a,b)], [], X).
test(merge_a2, X == [a(b), c(a, b)]) :-
	merge_options([a=b, c(a,b)], [a(d)], X).
test(merge_a2, X == [a(b), c(a, b)]) :-
	merge_options([a=b, c(a,b)], [a(d), c(x,y)], X).
test(merge_a2, X == [a(b), c(x, y)]) :-
	merge_options([a=b], [a(d), c(x,y)], X).

:- end_tests(option).
