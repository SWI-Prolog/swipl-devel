/*  Part of SWI-Prolog

    Author:        Paulo Moura and Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2015, Kyndi Inc
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

:- module(test_yall, [test_yall/0, bench_yall/0]).
:- use_module(library(plunit)).

/** <module> Test lambda expressions

This module tests the implementation of Logtalk's lambda expressions syntax.
The development of this module was sponsored by Kyndi, Inc.

Several tests are adapted from Ulrich Neumerkel's lambda proposal examples.
Some tests and some becnhmarks were posted in public Prolog discussion
forums. Other tests result from Logtalk development work.

@author	Paulo Moura and Jan Wielemaker
*/

:- use_module(library(yall)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(statistics)).


test_yall :-
	run_tests(yall_tests).


bench_yall :-
	bench1,
	bench2.

bench1 :-
	numlist(1, 100000, List),
	writeln('Using maplist/2 with a closure for testing less(0, X) \c
	         with X in [1..100000]: '),
	time(maplist(less(0), List)),
	writeln('Using maplist/2 with a lambda for testing less(0, X) \c
	         with X in [1..100000]:  '),
	time(maplist([X]>>less(0,X), List)),
	nl.

% the second benchmark is based on code posted by Jan Wielemaker in the
% SWI-Prolog mailing list:

bench2 :-
	numlist(1, 100000, List),
	writeln('Adding 1 to every integer in the list [1..100000] \c
	         using a local add1/2 predicate:'),
	time(add1(List, _)),
	writeln('Adding 1 to every integer in the list [1..100000] \c
	       using maplist/3 with the plus/3 built-in predicate:'),
	time(maplist(plus(1), List, _)),
	writeln('Adding 1 to every integer in the list [1..100000] \c
	       using maplist/3 with a lambda argument with a is/2 goal:'),
	time(maplist([X,Y]>>(Y is X+1), List, _)),
	nl.

% auxiliary predicates for the benchmarks and  unit tests

less(X, Y) :-
	X < Y.

add1([], []).
add1([H0| T0], [H| T]) :-
	H is H0 + 1,
	add1(T0, T).

f(x, y).

p(1,a).
p(2,b).

% country(Country, Capital, Population, Currency)
country(portugal, lisbon, 10, euro).
country(spain, madrid, 46, euro).
country(england, london, 51, pound_sterling).
country(malaysia, kuala_lumpur, 28, ringgit).
country(iraq, baghdad, 31, dinar).
country(tunisia, tunis, 10, dinar).

% foldl/4 in disguise
:- meta_predicate(sum(2, *, *, *)).
:- meta_predicate(sum(2, *, *, *, *)).

sum(Closure, Inf, Sup, Result) :-
	sum(Closure, Inf, Sup, 0, Result).

sum(Closure, Inf, Sup, Acc, Result) :-
	(	Inf =< Sup ->
		call(Closure, Inf, Sum),
		Acc2 is Acc + Sum,
		Next is Inf + 1,
		sum(Closure, Next, Sup, Acc2, Result)
	;	Result = Acc
	).


:- begin_tests(yall_tests).

test(call, R == 25) :-
	call([X,Y]>>(Y is X*X), 5, R).

test(call, T == 625) :-
	call([Z]>>(call([X,Y]>>(Y is X*X), 5, R), Z is R*R), T).

test(call, true) :-
	call(f, X, Y),
	call([X]>>f(X), X, Y),
	call([X,Y]>>f(X,Y), X, Y),
	call([X]>>({X}/[Y]>>f(X,Y)), X, Y),
	call(call(f, X), Y),
	call(f(X), Y),
	f(X, Y).

test(call, true) :-
	call([X,Y,Z]>>plus(X,Y,Z), 1, 2, 3),
	call([X,Y]>>plus(X,Y), 1, 2, 3),
	call([X]>>plus(X), 1, 2, 3),
	call([]>>plus, 1, 2, 3),
	call(plus, 1, 2, 3).

test(disjunction, all(A-B=[1-1,1-2,2-1,2-2])) :-
	maplist([X]>>(X=1;X=2), [A,B]).

test(free, all(X=[1,2])) :-
	{X}/p(X,_).

test(maplist, true) :-
	numlist(1, 100, List),
	maplist([X]>>less(0,X),List).

test(maplist, true) :-
	maplist([X]>>(X>3),[4,5,9]).

test(maplist, Zs == [a-1,b-2,c-3]) :-
	maplist([X,Y]>>(X=A-B,Y=B-A), [1-a,2-b,3-c], Zs).

test(maplist, Zs == [a-1,b-2,c-3]) :-
	maplist([X,B-A]>>(X=A-B), [1-a,2-b,3-c], Zs).

test(maplist, Zs == [a-1,b-2,c-3]) :-
	maplist([A-B,B-A]>>true, [1-a,2-b,3-c], Zs).

test(maplist, Zs == [a-1,b-2,c-3]) :-
	maplist([A-B]>>([B-A]>>true), [1-a,2-b,3-c], Zs).

test(maplist, true) :-
	Xsss = [[[1,2,3],[4]],[[5]]],
	maplist(maplist(maplist([X,Y]>>(Y is X+3))), Xsss,  Ysss),
	Xsss == [[[1,2,3],[4]],[[5]]],
	Ysss == [[[4,5,6],[7]],[[8]]].

test(maplist, Ys == [[1],[2]]) :-
	maplist([X,[X]]>>true,[1,2],Ys).

test(maplist, Xs == [1,2]) :-
	maplist([X,[X]]>>true,Xs,[[1],[2]]).

test(maplist, R == [1000,124]) :-
	maplist([N,M]>>(length(L, N), length([_|L], M)), [999,123],R).

test(maplist, R == [1000,124]) :-
	maplist([N]>>([M]>>(length(L, N), length([_|L], M))), [999,123],R).

% the following two tests were contributed by Boris Vassilev:

test(include, R == [a(1)]) :-
	include([X]>>(X=a(_)), [a(1), b(2)], R).

test(include, R == [a(1)]) :-
	include([a(_)]>>true, [a(1), b(2)], R).

test(include, R == [a(1)]) :-
	include([a(_)]>>true, [b(2), a(1)], R).

test(include, R == []) :-
	include([a(_)]>>true, [b(2), b(1)], R).

% some sample queries using lambda expressions as goals (not closures):

test(trivial, true) :-
	call([]>>true).

test(trivial, true) :-
	call({}/true).

test(trivial, true) :-
	call({}/[]>>true).

test(trivial, true) :-
	call({_}/true).

test(fold_left_in_disguise, R == 45) :-
	sum([X,Y]>>(Y is X), 0, 9, R).

test(fold_left_in_disguise, R == 285) :-
	sum([X,Y]>>(Y is X*X), 0, 9, R).

test(fold_left_in_disguise, R == 330) :-
	sum([X,Y]>>(sum([W,Z]>>(Z is W), X, 9, Y)), 0, 9, R).

test(dif, true) :-
	Xs = [_A,_B], maplist({X}/[Y]>>dif(X,Y), Xs).

test(dif, true) :-
	Xs = [_A,_B], maplist({X}/dif(X), Xs).

test(carlo_capelli_example, R == 9) :-
	foldl([A,B,C]>>(A mod 2 =:= 1 -> C is B+A ; C=B), [1,2,3,4,5], 0, R).

% adapted from an Ulrich Neumerkel example:

test(setof, Currencies == [dinar, euro, pound_sterling, ringgit]) :-
	setof(Currency, {Currency}/country(_, _, _, Currency), Currencies).

% adapted from a Richard O'Keefe example:

%test(common_prefix, true) :-
%	maplist({Front}/append(Front), Xs, Ys).

test(distances, true) :-
	Points = [(1,4),(2,5),(8,3)],
	maplist([(X,Y),Z]>>(Z is sqrt(X*X + Y*Y)), Points, Distances),
	Distances = [D1, D2, D3],
	abs(D1 - 4.123105625617661) < 0.0001,
	abs(D2 - 5.385164807134504) < 0.0001,
	abs(D3 - 8.54400374531753) < 0.0001.

test(distances, true) :-
	Points = [(1,4),(2,5),(8,3)],
	maplist([(X,Y)]>>([Z]>>(Z is sqrt(X*X + Y*Y))), Points, Distances),
	Distances = [D1, D2, D3],
	abs(D1 - 4.123105625617661) < 0.0001,
	abs(D2 - 5.385164807134504) < 0.0001,
	abs(D3 - 8.54400374531753) < 0.0001.

test(sum_squares, Result == [5, 25, 61]) :-
	maplist([[X,Y],Z]>>(Z is X*X + Y*Y), [[1,2],[3,4],[5,6]], Result).

test(sum_squares, Result == [5, 25, 61]) :-
	maplist([[X,Y]]>>([Z]>>(Z is X*X + Y*Y)), [[1,2],[3,4],[5,6]], Result).

% test access to the grammar rule implicit list of tokens using the call//1 built-in
% non-terminal and lambda expressions:

test(dcgs, true) :-
	phrase(call([[], []]>>true), [], []).

test(dcgs, [Input,Rest] == [[],[]]) :-
	phrase(call([[], []]>>true), Input, Rest).

test(dcgs, true) :-
	phrase(call([Input, Rest]>>(subtract(Input, Rest, [1]))), [1,2,3], [2,3]).

% three nasty examples of getting a grammar rule difference list arguments
% as they require using variables as both lambda free and lambda parameters:

test(dcgs, [Input,Rest] == [[1,2,3],[2,3]]) :-
	phrase(call({Input,Rest}/[Input,Rest]>>true), [1,2,3], [2,3]).

test(dcgs, Rest == [2,3]) :-
	phrase(call({Rest}/[_,Rest]>>true), [1,2,3], [2,3]).

test(dcgs, Element == 1) :-
	phrase(call({Element}/[[Element|_],_]>>true), [1,2,3], [2,3]).

:- end_tests(yall_tests).
