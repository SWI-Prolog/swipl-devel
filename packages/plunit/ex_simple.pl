:- module(ex_simple, []).
:- use_module(plunit).

:- begin_tests(lists).

test(true) :-
	true.

test(fail) :-
	\+ fail.

test(member) :-
	member(a, [a]).

test(member, [all(V == [a,b,c])]) :-
	member(V, [a,b,c]).

test(append) :-
	append("aap", "noot", X),
	X == "aapnoot".

:- end_tests(lists).
