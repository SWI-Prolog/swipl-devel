:- module(ex_simple, []).
:- use_module(library(plunit)).

:- begin_tests(lists).

test(true) :-
	true.

test(fail) :-
	\+ fail.

test(fail, [fail]) :-
	fail.

test(member) :-
	member(a, [a]), !.

test(member, [nondet]) :-
	member(_, [a]).

test(member, [true(X == a)]) :-
	member(X, [a]), !.

test(member, [all(V == [a,b,c])]) :-
	member(V, [a,b,c]).

test(append) :-
	append("aap", "noot", X),
	X == "aapnoot".

:- end_tests(lists).
