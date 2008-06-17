:- module(test_pio, [test_pio/0]).
:- use_module(library(plunit)).
test_pio :-
	run_tests.

:- begin_tests(phrase_from_file, []).
:- use_module(library(pio)).

... --> [] | [_], ... .

seq([]) --> [].
seq([E|Es]) -->
	[E],
	seq(Es).

cfc(Content,Tmp) :-
	tmp_file(plunit_pio,Tmp),
	open(Tmp,write,Out),
	format(Out,'~s',[Content]),
	close(Out).

df(Tmp) :-
	delete_file(Tmp).

test(null, [setup(cfc("",Null)),cleanup(df(Null)) ]) :-
	phrase_from_file([],Null).
test(null, [setup(cfc("",Null)),cleanup(df(Null)), fail]) :-
	phrase_from_file("a",Null).
test(null, [setup(cfc("",Null)),cleanup(df(Null)), nondet]) :-
	phrase_from_file(([]|"a"),Null).
test(null, [setup(cfc("",Null)),cleanup(df(Null)), nondet]) :-
	phrase_from_file(("a"|[]),Null).
test(null, [setup(cfc("",Null)),cleanup(df(Null)), nondet]) :-
	phrase_from_file(...,Null).
test(null, [setup(cfc("",Null)),cleanup(df(Null))]) :-
	phrase_from_file(([],[],{true}),Null).


test(aba, [setup(cfc("aba",ABA)),cleanup(df(ABA)) ]) :-
	phrase_from_file("aba",ABA).
test(aba, [setup(cfc("aba",ABA)),cleanup(df(ABA)) ]) :-
	phrase_from_file(("aca"|"aba"),ABA).
test(aba, [setup(cfc("aba",ABA)),cleanup(df(ABA)), nondet]) :-
	phrase_from_file(("abx"|"aba"|"ada"),ABA).
test(aba, [setup(cfc("aba",ABA)),cleanup(df(ABA)) ]) :-
	phrase_from_file([A,_,A], ABA).
test(aba, [setup(cfc("aba",ABA)),cleanup(df(ABA)), nondet ]) :-
	phrase_from_file(([A],...,[A]), ABA).
test(aba, [setup(cfc("aba",ABA)),cleanup(df(ABA)), fail ]) :-
	phrase_from_file((...,[A,A],...), ABA).
test(aba, [setup(cfc("aba",ABA)),cleanup(df(ABA)), fail ]) :-
	phrase_from_file((...,"c",...), ABA).
test(aba, [setup(cfc("aba",ABA)),cleanup(df(ABA)), nondet ]) :-
	phrase_from_file((seq(Seq),...,seq(Seq)), ABA),
	Seq = [_|_].


:- end_tests(phrase_from_file).
