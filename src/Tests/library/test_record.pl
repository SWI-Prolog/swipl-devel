:- module(test_record,
	  [ test_record/0
	  ]).
:- use_module(library(plunit)).

/** <module> Test set for library(record)


@tbd	A lot more tests.  In particular test and precise the relation
	between records and modules.
*/

test_record :-
	run_tests([ record
		  ]).

:- begin_tests(record).
:- use_module(library(record)).

:- record foo(x:integer).
:- record bar(x:foo).
:- record r(x:list(integer)).

test(record_type, true) :-
	make_foo([x(10)], Foo),
	make_bar([x(Foo)], Bar),
	is_bar(Bar).
test(list_type, true) :-
	make_r([x([1,2,3])], _).
test(list_type, error(type_error(_,_))) :-
	make_r([x([1,2,a])], _).

:- end_tests(record).
