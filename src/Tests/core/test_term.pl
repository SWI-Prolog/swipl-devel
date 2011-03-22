/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2007, University of Amsterdam

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

:- module(test_term, [test_term/0]).
:- use_module(library(plunit)).

/** <module> Test Prolog core term manipulation primitives

This module is a Unit test for  Prolog built-ins that process terms,
suchj as numbervars, univ, etc.

@author	Jan Wielemaker
*/

test_term :-
	run_tests([ numbervars,
		    variant
		  ]).

:- begin_tests(numbervars).

test(single, End == 1) :-
	numbervars(_, 0, End).
test(single_s, End == 0) :-
	numbervars(_, 0, End, [singletons(true)]).
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
test(sharing_cycles, [sto(rational_trees), fail, fixme(sharing_cycles)]) :-
	A=[A|B], C=[A|D], B=[A|E], F=[A|F], D=[F|E],
	A =@= C.

v(_).

:- end_tests(variant).
