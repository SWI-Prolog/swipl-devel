:- module(test_continuation,
	  [ test_continuation/0
	  ]).
user:file_search_path(library, '../packages/plunit').
:- use_module(library(plunit)).

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
	reset(Goal,Cont,YE),
	(   YE = yield(Element)
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
	reset(Goal,Cont,Term),
	(   Term = ask(X)
	->  read(X),
	    with_read(Cont)
	;   true
	).

with_list(L, Goal) :-
	reset(Goal,Cont,Term),
	(   Term = ask(X)
	->  L = [X|T],
	    with_list(T,Cont)
	;   true
	).

play(G1,G2) :-
	reset(G1, Cont1, Term1),
	(   Cont1 == 0
	->  true
	;   reset(G2,Cont2,Term2),
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
	reset(TG,ContT,TermT),
	transduce_(TermT,ContT,IG).

transduce_(0,_,_).
transduce_(yield(NValue), ContT, IG) :-
	yield(NValue),
	transduce(IG, ContT).
transduce_(ask(Value), ContT, IG) :-
	reset(IG, ContI, TermI),
	(   TermI == 0
	->  true
	;   TermI = yield(Value),
	    transduce(ContI, ContT)
	).

doubler :-
	ask(Value),
	NValue is Value * 2,
	yield(NValue),
	doubler.

:- begin_tests(continuation).

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

:- end_tests(continuation).
