/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           www.swi-prolog.org
    Copyright (c)  2007-2009, University of Amsterdam
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

:- module(test_rbtrees, [test_rbtrees/0]).

test_rbtrees :-
	run_tests(rbtrees).

:- use_module(library(plunit)).
:- use_module(library(debug)).
:- use_module(library(rbtrees)).

:- begin_tests(rbtrees).

test(keys, true(Keys=[1,2,3,4])) :-
	build_ptree(4, Tree),
	rb_keys(Tree, Keys).

test(clone3, true(NPS =@= [1-_, 2-_, 3-_, 4-_])) :-
	build_ptree(4, Tree),
	rb_clone(Tree, _, NPS).

test(pos) :-
	test_pos(100).
test(neg) :-
	test_neg(100).

count(I,_,I).
count(I,M,L) :-
	I < M, I1 is I+1, count(I1,M,L).

test_pos(N) :-
	time(build, build_ptree(N,T)),
	crunch(1, N, T).

test_neg(Size) :-
	rb_new(T0),
	build_ntree(1,Size,T0,T),
	MSize is -Size,
	crunch(MSize, -1, T).

crunch(Min, Max, T) :-
	time(validate, is_rbtree(T)),
	time(clean_up, clean_tree(Min,Max,T,_)),
	time(clean_down, bclean_tree(Max,Min,T,_)),
	time(delete_one,
	     (	 count(Min,Max,X), ( rb_delete(T,X,TF) -> true ; abort ),
		 is_rbtree(TF),
		 debug(rbtrees, 'done ~d', [X]),
		 fail
	     ;	 true
	     )).

build_ptree(Size, Tree) :-
	rb_new(T0),
	build_ptree(1, Size, T0, Tree).

build_ptree(X,X,T0,TF) :- !,
	rb_insert(T0,X,X,TF).
build_ptree(X1,X,T0,TF) :-
	rb_insert(T0,X1,X1,TI),
	X2 is X1+1,
	build_ptree(X2,X,TI,TF).


clean_tree(X,X,T0,TF) :- !,
	rb_delete(T0,X,TF),
	( is_rbtree(TF) -> true ; abort).
clean_tree(X1,X,T0,TF) :-
	rb_delete(T0,X1,TI),
	X2 is X1+1,
	( is_rbtree(TI) -> true ; abort),
	clean_tree(X2,X,TI,TF).

bclean_tree(X,X,T0,TF) :- !,
	debug(rbtrees, 'cleaning ~d', [X]),
	rb_delete(T0,X,TF),
	( is_rbtree(TF) -> true ; abort).
bclean_tree(X1,X,T0,TF) :-
	debug(rbtrees, 'cleaning ~d', [X1]),
	rb_delete(T0,X1,TI),
	X2 is X1-1,
	( is_rbtree(TI) -> true ; abort),
	bclean_tree(X2,X,TI,TF).


build_ntree(X,X,T0,TF) :- !,
	X1 is -X,
	rb_insert(T0,X1,X1,TF).
build_ntree(X1,X,T0,TF) :-
	NX1 is -X1,
	rb_insert(T0,NX1,NX1,TI),
	X2 is X1+1,
	build_ntree(X2,X,TI,TF).

%
% simplified processor
%
%
pretty_print(T) :-
	pretty_print(T,6).

pretty_print(black([],[],[],[]),_) :- !.
pretty_print(red(L,K,_,R),D) :-
	DN is D+6,
	pretty_print(L,DN),
	format('~t~a:~d~*|~n',[r,K,D]),
	pretty_print(R,DN).
pretty_print(black(L,K,_,R),D) :-
	DN is D+6,
	pretty_print(L,DN),
	format('~t~a:~d~*|~n',[b,K,D]),
	pretty_print(R,DN).


:- end_tests(rbtrees).

:- meta_predicate time(+, :).

time(What, Goal) :-
	debugging(time), !,
	statistics(cputime, OldTime),
	statistics(inferences, OldInferences),
	Goal,
	statistics(inferences, NewInferences),
	statistics(cputime, NewTime),
	UsedTime is NewTime - OldTime,
	UsedInf  is NewInferences - OldInferences - 3,
	format('~t~w~15|: ~D inferences in ~2f secs~n',
	       [What, UsedInf, UsedTime]).
time(_, Goal) :-
	Goal.
