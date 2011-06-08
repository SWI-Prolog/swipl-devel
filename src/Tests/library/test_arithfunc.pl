:- module(test_arithfunc,
	  [ test_arithfunc/0
	  ]).
:- use_module(library(plunit)).
:- if(exists_source(library(arithfunc))).
:- use_module(library(arithfunc)).

%%	test_arithfunc
%
%	Test  emulation  of  arithmetic_function/1    for  compatibility
%	reasons.

test_arithfunc :-
	run_tests([ arithmetic_function
		  ]).

:- begin_tests(arithmetic_function).

:- arithmetic_function(ten/0).
:- arithmetic_function(twice/1).
:- arithmetic_function(mean/2).
:- arithmetic_function(euler/0).
:- arithmetic_function(fail/0).
:- arithmetic_function(except/0).
:- arithmetic_function(fac/1).

ten(10).
twice(X, R) :-
	R is X * 2.
mean(X1, X2, R) :-
	R is (X1 + X2)/2.

euler(2.71828).

fail(_) :- fail.

except(_) :-
	throw(error(foobar)).

fac(1,1) :- !.
fac(X,N) :-
	X > 1,
	X2 is X - 1,
	fac(X2, N0),
	N is N0 * X.

test(func, A == 10) :-
	A is ten.
test(func, A == 10) :-
	A is twice(5).
test(func, A == 10) :-
	A is mean(0, 20).
test(euler, EE =:= 6*euler*7*1) :-
        EE is 6*euler*7*1.
test(fail, [blocked(partial_emulation)]) :-
	catch(_ is fail, E, true),
	compound(E), E = error(E2, _),
	compound(E2), E2 = failure_error(_).
test(except, throw(error(foobar))) :-
	_ is except.
test(depth_limit, throw(depth_limit_exceeded)) :-
	_A is fac(10).
test(flag, NV == 50) :-
	flag(f, Old, 100),
	flag(f, V, mean(V, 0)),
	flag(f, NV, Old).

:- if(current_prolog_flag(bounded, false)). % GMP implies rational

:- arithmetic_function(idiv/2).

idiv(Dd,Dr,Iq):-
        Q is Dd/Dr,
        rational(Q,Qt,Qn),
        Iq is Qt//Qn.

test(idiv, Qi == 3) :-
	Qi is idiv(3 rdiv 2,2 rdiv 5).

:- endif.

:- end_tests(arithmetic_function).

:- else.

test_arithfunc.

:- endif.
