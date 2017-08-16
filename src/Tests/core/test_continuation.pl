/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           www.swi-prolog.org
    Copyright (c)  2017, University of Amsterdam
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

:- module(test_continuation,
	  [ test_continuation/0
	  ]).
user:file_search_path(library, '../packages/plunit').
:- use_module(library(plunit)).
:- use_module(library(debug)).

test_continuation :-
	run_tests([ continuation
		  ]).


:- meta_predicate
	init_iterator(0, -),
	with_list(+, 0),
	with_read(0).

fromList([]).
fromList([X|Xs]) :-
	yield(X),
	fromList(Xs).

enumFromTo(L,U) :-
	(   L =< U
	->  yield(L),
	    NL is L + 1,
	    enumFromTo(NL,U)
	;   true
	).

enumFrom(L) :-
	yield(L),
	NL is L + 1,
	enumFrom(NL).

yield(Term) :-
	shift(yield(Term)).

init_iterator(Goal,Iterator) :-
	reset(Goal,YE,Cont),
	(   Cont \== 0,
	    YE = yield(Element)
	->  Iterator = next(Element,Cont)
	;   Iterator = done
	).

next(next(Element,Cont),Element,Iterator) :-
	init_iterator(Cont,Iterator).

sum(Iterator,Acc,Sum) :-
	(   next(Iterator,X,NIterator)
	->  debug(sum, 'Next = ~q', [X]),
	    NAcc is Acc + X,
	    sum(NIterator,NAcc,Sum)
	;   Acc = Sum
	).

sum(Sum) :-
	ask(X),
	ask(Y),
	Sum is X + Y.

ask(X) :-
	shift(ask(X)).

with_read(Goal) :-
	reset(Goal,Term,Cont),
	(   Cont \== 0,
	    Term = ask(X)
	->  read(X),
	    with_read(Cont)
	;   true
	).

with_list(L, Goal) :-
	reset(Goal,Term,Cont),
	(   Cont \== 0,
	    Term = ask(X)
	->  L = [X|T],
	    with_list(T,Cont)
	;   true
	).

play(G1,G2) :-
	reset(G1, Term1, Cont1),
	(   Cont1 == 0
	->  true
	;   reset(G2,Term2,Cont2),
	    sync(Term1,Term2),
	    play(Cont1,Cont2)
	).

sync(ask(X),yield(X)).
sync(yield(X),ask(X)).

mapL([],[]).
mapL([X|Xs],[Y|Ys]) :-
	yield(X),
	ask(Y),
	mapL(Xs,Ys).

scanSum(Acc) :-
	ask(X),
	NAcc is Acc + X,
	yield(NAcc),
	scanSum(NAcc).

transduce(IG,TG) :-
	reset(TG,TermT,ContT),
	transduce_(TermT,ContT,IG).

transduce_(0,_,_).
transduce_(yield(NValue), ContT, IG) :-
	yield(NValue),
	transduce(IG, ContT).
transduce_(ask(Value), ContT, IG) :-
	reset(IG, TermI, ContI),
	(   ContI == 0
	->  true
	;   TermI = yield(Value),
	    transduce(ContI, ContT)
	).

doubler :-
	ask(Value),
	NValue is Value * 2,
	yield(NValue),
	doubler.

%%	reset_in_cond(-R)
%
%	Test a shift in the condition of if-then-else, \+, etc.

reset_in_cond(R) :-
	reset(shift_in_cond(R), hello(X), Cont),
	X = world,
	call(Cont).

shift_in_cond(R) :-
	(   shift(hello(X))		% keep choice point
	->  format(atom(R), 'Hello ~w', [X])
	;   true
	).

:- begin_tests(continuation).

test(basic, [Ball,After] == [a,after]) :-
	reset(shift(a), Ball, Continuation),
	assertion(callable(Continuation)),
	After = after.
test(sum, Sum == 12) :-
	init_iterator(fromList([7,2,3]), It),
	sum(It, 0, Sum).
test(sum, Sum == 15) :-
	init_iterator(enumFromTo(1,5), It),
	sum(It, 0, Sum).
test(sum, Sum == 3) :-
	with_list([1,2], sum(Sum)).
test(play, L == [1,3,6,10]) :-
	play(mapL([1,2,3,4],L), scanSum(0)).
test(transducer, Sum == 6) :-
	play(sum(Sum),transduce(fromList([1,2]), doubler)).
test(ifthen, R == 'Hello world') :-
	reset_in_cond(R).
test(mcall2, Ball == a) :-
	reset(call(shift, a), Ball, _Continuation).

:- end_tests(continuation).
