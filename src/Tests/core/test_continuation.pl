/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           www.swi-prolog.org
    Copyright (c)  2017-2024, University of Amsterdam
                              VU University Amsterdam
			      SWI-Prolog Solutions b.v.
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
:- use_module(library(plunit)).
:- use_module(library(debug)).
:- use_module(library(prolog_stack)).

test_continuation :-
	run_tests([ continuation,
		    fast_heap
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

%!	test_cref is det.
%
%	The clause reference cannot be the  one   from  the  blob in the
%	continuation as this may be garbage   collected.  The test calls
%	garbage_collect/0   to   GC   the     continuation    and   then
%	garbage_collect_atoms/0 to force GC of the clause references. It
%	then uses get_prolog_backtrace/2 as  the   stack  trace requires
%	valid clause references.
%
%	Tests issue#706

test_cref :-
   reset(p(5), _, Cont),
   call(Cont),
   !.

p(0) :-
   !,
   shift(hello),
   garbage_collect,
   garbage_collect_atoms,
   get_prolog_backtrace(10, _),
   no_lco.
p(N) :-
   N1 is N - 1,
   p(N1),
   no_lco.

no_lco.

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
test(test_cref) :-
	test_cref.

:- dynamic test_cont_context_d/1.
:- meta_predicate tassert(:).

tassert(_:X) :-
   shift(ball),
   assertz(test_cont_context_d(X)),
   no_lco.

test(context, cleanup(retractall(test_cont_context_d(_)))) :-
   reset(tassert(1), _, Cont),
   call(Cont),
   test_cont_context_d(1).

:- end_tests(continuation).

:- begin_tests(fast_heap).

test(fast_heap, forall(data(Term))) :-
     trip(Term).

trip(Data) :-
	'$fast_record'(Data, Ref),
	'$fast_recorded'(Ref, DataCp),
	Data =@= DataCp.

data(Term) :-
	data_(Term).
data(f(Term)) :-
	data_(Term).
data(f(Term,Term)) :-
	data_(Term).

:- if(current_prolog_flag(bounded, false)).
data_(Big) :- Big is random(1<<200).
data_(Big) :- Big is -random(1<<200).
:- endif.
data_(Med) :- Med is random(1<<55).
data_(Med) :- Med is -random(1<<55).
data_(363).
data_(-363).
data_(0).
data_(1r3).
data_(3.14).
data_("This is a nice string").
data_(_Var).
data_(f(x)).
data_(f(X,X)).
data_([nice, list(of(terms))]).

:- end_tests(fast_heap).
