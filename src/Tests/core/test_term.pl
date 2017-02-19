/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2008-2016, University of Amsterdam
                              VU University Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(test_term, [test_term/0]).
:- use_module(library(plunit)).
:- use_module(library(debug)).

/** <module> Test Prolog core term manipulation primitives

This module is a Unit test for  Prolog built-ins that process terms,
such as numbervars, univ, etc.

@author	Jan Wielemaker
*/

test_term :-
	run_tests([ numbervars,
		    variant,
		    compound,
		    zero_arity_compound
		  ]).

:- begin_tests(numbervars).

test(single, End == 1) :-
	numbervars(_, 0, End).
test(single_s, End == 0) :-
	numbervars(_, 0, End, [singletons(true)]).
test(single_offset, End == 1001) :-
	numbervars(f(A,_B,A), 1000, End, [singletons(true)]),
	assertion(arg(1, A, 1000)).
test(neg, End == -6) :-
	numbervars(f(X), -7, End),
	assertion(X == '$VAR'(-7)).
test(shared, End == 1) :-
	X = d(_),
	Y = t(X,X),
	numbervars(Y, 0, End, []).
test(shared_s, End == 1) :-
	X = d(_),
	Y = t(X,X),
	numbervars(Y, 0, End, [singletons(true)]).
test(cyclic, [sto(rational_trees), End == 1]) :-
	X = d(X, _),
	numbervars(X, 0, End, []).
% currently singletons(true) is ignored for cyclic terms
test(cyclic_s, [sto(rational_trees), End == 1]) :-
	X = d(X, _),
	numbervars(X, 0, End, [singletons(true)]).
test(twice_singleton, X == '$VAR'('_')) :-
	T = a(X),
	numbervars(T, 0, _E1, [singletons(true)]),
	numbervars(T, 0, _E2, [singletons(true)]).
test(shift, true) :-
	between(1, 20, X),
	trim_stacks,
	Len is 1<<X,
	length(List, Len),
	statistics(global_shifts, S0),
	numbervars(List, 0, _),
	statistics(global_shifts, S1),
	S1 > S0, !.

:- end_tests(numbervars).


:- begin_tests(variant).

test(simple) :-
	v(A), v(B),
	a(A) =@= a(B).
test(shared) :-
	a(A) =@= a(A).
test(shared) :-
	a(A, A) =@= a(A, A).
test(shared) :-
	a(A, B) =@= a(A, B).
test(shared, fail) :-
	v(B),
	a(A, B) =@= a(A, A).
test(shared, fail) :-
	v(A),
	a(A, B) =@= a(B, B).
test(dubious, true) :-
	v(X), v(Z),
	a(X, Y) =@= a(Y, Z).
test(common, true) :-			% Bug #464
	A=x(_),
	s(A, A) =@= s(x(B), x(B)).
test(common, fail) :-
	X = x(A), v(B),
	a(X,A) =@= a(X,B).
test(common, fail) :-
	X = x(A), v(B),
	a(A,X) =@= a(B,X).
test(cyclic, [sto(rational_trees)]) :-
	A = f(A),
	A =@= f(A).
test(cyclic, [fail, sto(rational_trees)]) :-
	S = s(S),
	S =@= s(s(s(s(1)))).
test(cyclic, [sto(rational_trees)]) :-
	S = s(s(S)), X = s(s(s(X))),
	S =@= X.
test(cyclic, [sto(rational_trees)]) :-
	S = s(x(S)), X = s(x(s(x(X)))),
	S =@= X.
test(shared, fail) :-
	v(A), v(B),
	X = x(A), Y = x(B),
	s(X,Y,X) =@= s(X,Y,Y).
test(cycle, [sto(rational_trees), fail]) :- % Ulrich
	A=[_V1,_V2|A], D=[_V3|A],
	A =@= D.
test(symmetry, [fail]) :-		    % Ulrich
	A=[B|C], D=[C|B],
	B=[X|_Y], C=[_Z|X],
	A =@= D.
test(symmetry, [fail]) :-		    % Ulrich
	X=s(_Xi), Y=s(_Yi), Z=s(_Zi),
	A=v(X,Y,X), D=v(Z,X,Y),
	A =@= D.
test(ground, [sto(rational_trees), fail]) :- % Ulrich
	A=[A|B], B=[A], D=[[A|B]|A],
	A = [_,_], D = [_,_,_],
	A =@= D.
test(sharing_cycles, [sto(rational_trees), fail]) :-
	A=[A|B], C=[A|D], B=[A|E], F=[A|F], D=[F|E],
	A =@= C.
test(cycle_with_prefix, [sto(rational_trees), fail]) :-
	A = [A|_],
	X = [A|Y],
	B = [X|Y],
	A =@= B.

v(_).

:- end_tests(variant).


:- begin_tests(compound).

test(functor, error(domain_error(_,_))) :-
	functor(a(), _, _).
test(=.., error(domain_error(_,_))) :-
	a() =.. _.
test(=.., error(type_error(atomic,f(a)))) :-
	_Var =.. [f(a)].
test(compound_name_arity, A == 0) :-
	compound_name_arity(a(), _N, A).
test(compound_name_arity, T == a()) :-
	compound_name_arity(T, a, 0).
test(compound_name_arity, T =@= a(_)) :-
	compound_name_arity(T, a, 1).
test(compound_name_arity, error(type_error(compound,a))) :-
	compound_name_arity(a, _, _).
test(compound_name_arguments, T == a()) :-
	compound_name_arguments(T, a, []).
test(compound_name_arguments, Args == []) :-
	compound_name_arguments(a(), _N, Args).

:- end_tests(compound).

:- begin_tests(zero_arity_compound).

:- '$clausable'((t1/1,t2/1,t3/1,t4/1,t5/1)).

t1(A) :- t(A,a()).
t2(A) :- t(a(),A).
t3(a()).
t4(a(1, a())).
t5(a(a(),1)).

t(X,X).

test(clause, B == t(A,a())) :-
	clause(t1(A), B).
test(clause, B == t(a(),A)) :-
	clause(t2(A), B).
test(clause, A == a()) :-
	clause(t3(A), true).
test(clause, A == a(1,a())) :-
	clause(t4(A), true).
test(clause, A == a(a(),1)) :-
	clause(t5(A), true).

:- end_tests(zero_arity_compound).
