/*  Part of SWI-Prolog

    Author:        Jan Wielemaker, Peter Ludemann
    E-mail:        J.Wielemaker@vu.nl, peter.ludemann@gmail.com
    WWW:           www.swi-prolog.org
    Copyright (c)  2007-2021, University of Amsterdam
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

:- module(test_rbtrees, [test_rbtrees/0]).

test_rbtrees :-
	run_tests(rbtrees).

:- use_module(library(plunit)).
:- use_module(library(debug)).
:- use_module(library(rbtrees)).
:- use_module(library(apply), [foldl/4]).

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


% TODO: Improve the following tests. They're quite basic, and mainly
%       exist to ensure that any edits to library(rbtrees) don't break
%       anything obvious.
% TODO: Make similar tests for library(assoc).

% Predicate for testing rb_apply/4, rb_map/3, rb_partial_map/4:
plus1(X, X1) :- X1 is X + 1.

% Predicate for testing rb_map/3:
plus_or_minus(X, X1) :- X1 is X + 1.
plus_or_minus(X, X1) :- X1 is X - 1.

% Predicate for testing rb_map/2:
even(X) :- 0 is X mod 2.

% Predicate for testing rb_fold/4:
sum(K-V, Keys0-Sum0, Keys-Sum) :-
    append(Keys0, [K], Keys),  % TODO: this is O(N) - use diff list?
    Sum is Sum0 + V.

% Predicate used by tests that do foldl to apply a sequence of operations:
:- det(apply_op/3).
apply_op(+Key-Val, Tree0, Tree) =>
    rb_insert(Tree0, Key, Val, Tree).
apply_op(-Key, Tree0, Tree) =>
    rb_delete(Tree0, Key, Tree).

test(empty1, EmptyList == []) :-
    rb_empty(Empty),
    rb_visit(Empty, EmptyList).
test(empty2, EmptyList == []) :-
    list_to_rbtree([], Empty),
    rb_visit(Empty, EmptyList).

test(lookup1, Result == [1,2,3,[a-1,b-2,c-3]]) :-
    list_to_rbtree([a-1,b-2,c-3], Tree),
    rb_visit(Tree, TreeList),
    rb_lookup(a, ValA, Tree),
    rb_lookup(b, ValB, Tree),
    rb_lookup(c, ValC, Tree),
    Result = [ValA,ValB,ValC,TreeList].
test(lookup2, fail) :-
    list_to_rbtree([a-1,b-2,c-3], Tree),
    rb_lookup(d, _, Tree).

test(update1, Tree2List == [a-1,b-222,c-3]) :-
    list_to_rbtree([a-1,b-2,c-3], Tree),
    rb_update(Tree, b, 222, Tree2),
    rb_visit(Tree2, Tree2List).
test(update2, Result == [3, [a-1,b-2,c-333]]) :-
    list_to_rbtree([a-1,b-2,c-3], Tree),
    rb_update(Tree, c, OldVal, 333, Tree2),
    rb_visit(Tree2, Tree2List),
    Result = [OldVal, Tree2List].
test(update3, fail) :-
    list_to_rbtree([a-1], Tree),
    rb_update(Tree, c, _, _).

test(apply1, Tree2List == [a-2, b-2]) :-
    list_to_rbtree([a-1,b-2], Tree),
    rb_apply(Tree, a, plus1, Tree2),
    rb_visit(Tree2, Tree2List).
test(apply2, fail) :-
    list_to_rbtree([a-1,b-2], Tree),
    rb_apply(Tree, c, plus1, _).

test(insert1, Tree3List == [a-111,b-2,c-3]) :-
    list_to_rbtree([a-1,c-3], Tree1),
    rb_insert(Tree1, b, 2, Tree2),
    rb_insert(Tree2, a, 111, Tree3),
    rb_visit(Tree3, Tree3List).
test(insert2, Tree3List == [a-1, x-666]) :-
    rb_empty(Tree1),
    rb_insert_new(Tree1, x, 666, Tree2),
    rb_insert_new(Tree2, a, 1, Tree3),
    rb_visit(Tree3, Tree3List).
test(insert3, fail) :-
    list_to_rbtree([x-1,y-2,z-3], Tree1),
    rb_insert_new(Tree1, x, 666, _).

test(delete1, Tree2List == [b-2,c-3]) :-
    list_to_rbtree([a-1,b-2,c-3], Tree1),
    rb_delete(Tree1, a, Tree2),
    rb_visit(Tree2, Tree2List).
test(delete2, Result == [3, [a-1,b-2]]) :-
    list_to_rbtree([a-1,b-2,c-3], Tree1),
    rb_delete(Tree1, c, Val, Tree2),
    rb_visit(Tree2, Tree2List),
    Result = [Val, Tree2List].
test(delete3, fail) :-
    list_to_rbtree([a-1,b-2,c-3], Tree1),
    rb_delete(Tree1, x, _).

% TODO: create sequences that exercise all the fixup cases in rb_delete/4.
test(sequence, Tree2List == [a-1, b-2, c-555]) :-
    list_to_rbtree([], Tree0),
    foldl(apply_op,
          [+a-1, +b-2, +c-xxx, +d-4, -c, +c-555, -d],
          Tree0, Tree2),
    rb_visit(Tree2, Tree2List).

test(keys1, Keys == [a,b,c,d]) :-
    list_to_rbtree([a-1,b-2,c-3,d-4], Tree),
    rb_keys(Tree, Keys).
test(keys2, Keys == []) :-
    rb_empty(Tree),
    rb_keys(Tree, Keys).

test(map1) :-
    list_to_rbtree([a-2,b-4,c-6,d-8], Tree),
    rb_map(Tree, even).
test(map1a, fail) :-
    list_to_rbtree([a-2,b-4,c-7,d-8], Tree),
    rb_map(Tree, even).
test(map2, Tree2List == [a-2,b-3,c-4]) :-
    list_to_rbtree([a-1,b-2,c-3], Tree1),
    rb_map(Tree1, plus1, Tree2),
    rb_visit(Tree2, Tree2List).
test(map3, Tree2List == [a-2,b-666,c-4]) :-
    list_to_rbtree([a-1,b-666,c-3], Tree1),
    rb_partial_map(Tree1, [a,c], plus1, Tree2),
    rb_visit(Tree2, Tree2List).
test(map3b, fail) :-
    list_to_rbtree([a-1,b-666,c-3], Tree1),
    rb_partial_map(Tree1, [c,a], plus1, _). % [c,a] not ordered
test(map4, Tree2List == [a-1,b-2,c-3]) :-
    list_to_rbtree([a-1,b-2,c-3], Tree1),
    rb_partial_map(Tree1, [], plus1, Tree2),
    rb_visit(Tree2, Tree2List).
test(map5, All == [[a-2,b-3],[a-2,b-1],[a-0,b-3],[a-0,b-1]]) :-
    list_to_rbtree([a-1,b-2], Tree1),
    % TODO: setof to not care about order of traversal
    bagof(Tree2List, Tree2^( rb_map(Tree1, plus_or_minus, Tree2),
                             rb_visit(Tree2, Tree2List)
                           ),
          All).

test(fold, Sum == [a,b,c]-6) :-
    list_to_rbtree([a-1, b-2, c-3], Tree),
    rb_fold(sum, Tree, []-0, Sum).

test(clone) :-
    list_to_rbtree([a-1,b-2,c-3], Tree0),
    rb_clone(Tree0, Tree2, Pairs),
    rb_visit(Tree2, Tree2List),
    assertion(Pairs = [a-_, b-_, c-_]),
    assertion(Tree2List == Pairs).

test(size_min_max, Result == [0,3,4,3,b-2,x-666,[b-2,c-3,x-666]]) :-
    rb_empty(Tree1),
    rb_size(Tree1, Size1),
    list_to_rbtree([a-1,b-2,c-3], Tree2),
    rb_size(Tree2, Size2),
    rb_insert(Tree2, x, 666, Tree3),
    rb_size(Tree3, Size3),
    rb_delete(Tree3, a, Tree4),
    rb_size(Tree4, Size4),
    rb_visit(Tree4, Tree4List),
    rb_min(Tree4, Min, MinVal),
    rb_max(Tree4, Max, MaxVal),
    Result = [Size1,Size2,Size3,Size4,Min-MinVal,Max-MaxVal,Tree4List].

test(del_min1, Result == [a,1,[b-2,c-3]]) :-
    list_to_rbtree([a-1,b-2,c-3], Tree0),
    rb_del_min(Tree0, Key, Val, Tree2),
    rb_visit(Tree2, Tree2List),
    Result = [Key,Val,Tree2List].
test(del_min2, Result == [a,1,[]]) :-
    list_to_rbtree([a-1], Tree0),
    rb_del_min(Tree0, Key, Val, Tree2),
    rb_visit(Tree2, Tree2List),
    Result = [Key,Val,Tree2List].
test(del_min3, fail) :-
    rb_empty(Tree0),
    rb_del_min(Tree0, _Key, _Val, _).

test(del_max1, Result == [c,3,[a-1,b-2]]) :-
    list_to_rbtree([a-1,b-2,c-3], Tree0),
    rb_del_max(Tree0, Key, Val, Tree2),
    rb_visit(Tree2, Tree2List),
    Result = [Key,Val,Tree2List].
test(del_max2, Result == [a,1,[]]) :-
    list_to_rbtree([a-1], Tree0),
    rb_del_max(Tree0, Key, Val, Tree2),
    rb_visit(Tree2, Tree2List),
    Result = [Key,Val,Tree2List].
test(del_max3, fail) :-
    rb_empty(Tree0),
    rb_del_max(Tree0, _Key, _Val, _).

test(rb_min1, fail) :-
    rb_empty(Tree),
    rb_min(Tree, _, _).

test(rb_max1, fail) :-
    rb_empty(Tree),
    rb_max(Tree, _, _).

test(next1, Result == [a-1,b-2,c-3]) :-
    list_to_rbtree([a-1,b-2,c-3], Tree),
    rb_min(Tree, K0, V0),
    rb_next(Tree, K0, K1, V1),
    rb_next(Tree, K1, K2, V2),
    Result = [K0-V0,K1-V1,K2-V2].
test(next2, fail) :-
    list_to_rbtree([a-1,b-2,c-3], Tree),
    (   rb_next(Tree, c, _K, _V)
    ;   rb_next(Tree, bb, _K, _V)
    ).

test(previous1, Result == [c-3,b-2,a-1]) :-
    list_to_rbtree([a-1,b-2,c-3], Tree),
    rb_max(Tree, K0, V0),
    rb_previous(Tree, K0, K1, V1),
    rb_previous(Tree, K1, K2, V2),
    Result = [K0-V0,K1-V1,K2-V2].
test(previous2, fail) :-
    list_to_rbtree([a-1,b-2,c-3], Tree),
    (   rb_previous(Tree, a, _K, _V)
    ;   rb_previous(Tree, bb, _K, _V)
    ).

test(in1, [nondet, A==1]) :-
    list_to_rbtree([a-1,b-2], Tree),
    rb_in(a, A, Tree).
test(in2, [nondet]) :-
    list_to_rbtree([a-1,b-2], Tree),
    rb_in(a, 1, Tree),
    rb_in(b, 2, Tree).
test(in3, fail) :-
    list_to_rbtree([b-2,c-3,d-4], Tree),
    (   rb_in(x, _, Tree)
    ;   rb_in(a, _, Tree)
    ;   rb_in(b2, _, Tree)
    ).
test(in4, KVs == [b-2,c-3,d-4]) :-
    list_to_rbtree([b-2,c-3,d-4], Tree),
    bagof(K-V, rb_in(K, V, Tree), KVs).
test(in5a, All =@= [a-[a-yes,b-3,c-yes,d-_],
                    c-[a-yes,b-3,c-yes,d-_],
                    d-[a-yes,b-3,c-yes,d-yes]]) :-
    list_to_rbtree([a-yes,b-3,c-yes,d-_Yes], Tree),
    findall(K-TreeList, ( rb_in(K, yes, Tree),
                          rb_visit(Tree, TreeList)
                        ),
            All).
test(in5b) :-
    list_to_rbtree([a-yes,b-3,c-yes,d-Yes], Tree),
    findall(Yes-K-TreeList,
            ( rb_in(K, yes, Tree),
              rb_visit(Tree, TreeList)
            ),
            All),
    assertion(All =@= [Yes1-a-[a-yes, b-3, c-yes, d-Yes1],
                       Yes2-c-[a-yes, b-3, c-yes, d-Yes2],
                       yes-d-[a-yes, b-3, c-yes, d-yes]]).

% TODO: similar tests for library(assoc).
test(steadfast_rb_visit) :-
    list_to_rbtree([a-1,b-2], Tree),
    rb_visit(Tree, [a-1,b-2]).
test(steadfast_empty_1a) :-
    rb_empty(Empty),
    rb_empty(Empty). % make sure rb_empty/1 allows an existing tree
test(steadfast_empty_1b) :-
    list_to_rbtree([], Empty),
    rb_empty(Empty).
test(steadfast_new_1a) :-
    rb_new(New),
    rb_new(New). % make sure rb_new/1 allows an existing tree
test(steadfast_new_1b) :-
    list_to_rbtree([], New),
    rb_new(New).
test(steadfast_lookup_3) :-
    list_to_rbtree([a-1,b-2,c-3,d-4], Tree),
    rb_lookup(b, 2, Tree).
test(steadfast_update_4a) :-
    % This test won't succeed for arbitrary rbtrees because there
    % can be more than one form of a tree with a given contents.
    list_to_rbtree([a-1,b-2,c-3,d-4], Tree),
    list_to_rbtree([a-1,b-666,c-3,d-4], Tree2),
    rb_update(Tree, b, 666, Tree2).
test(steadfast_update_5a) :-
    % This test won't succeed for arbitrary rbtrees because there
    % can be more than one form of a tree with a given contents.
    list_to_rbtree([a-1,b-2,c-3,d-4], Tree),
    list_to_rbtree([a-1,b-2,c-666,d-4], Tree2),
    rb_update(Tree, c, 3, 666, Tree2).
test(steadfast_apply_4) :-
    list_to_rbtree([a-1,b-2], Tree),
    list_to_rbtree([a-2,b-2], Tree2),
    rb_apply(Tree, a, plus1, Tree2).
test(steadfast_insert_4) :-
    % This test won't succeed for arbitrary rbtrees because there
    % can be more than one form of a tree with a given contents.
    list_to_rbtree([a-1,c-3], Tree1),
    list_to_rbtree([a-1,b-2,c-3], Tree2),
    list_to_rbtree([a-111,b-2,c-3], Tree3),
    rb_insert(Tree1, b, 2, Tree2),
    rb_insert(Tree2, a, 111, Tree3).
test(steadfast_insert_new_4) :-
    rb_empty(Tree1),
    rb_insert_new(Tree1, x, 666, Tree2),
    rb_insert_new(Tree2, a, 1, Tree3),
    % Can't use list_to_rbtree/2 because the tree has a different shape;
    % repeat the test to check steadfastness:
    rb_empty(Tree1),
    rb_insert_new(Tree1, x, 666, Tree2),
    rb_insert_new(Tree2, a, 1, Tree3).
test(steadfast_delete_3) :-
    list_to_rbtree([a-1,b-2,c-3], Tree1),
    rb_delete(Tree1, a, Tree2),
    % repeat the test to check steadfastness:
    rb_delete(Tree1, a, Tree2).
test(steadfast_delete_4) :-
    list_to_rbtree([a-1,b-2,c-3], Tree1),
    rb_delete(Tree1, c, Val, Tree2),
    % repeat the test to check steadfastness:
    rb_delete(Tree1, c, Val, Tree2).
test(steadfast_keys_2) :-
    list_to_rbtree([a-1,b-2,c-3], Tree),
    rb_keys(Tree, [a,b,c]).
test(steadfast_map_3) :-
    list_to_rbtree([a-1,b-2,c-3], Tree1),
    list_to_rbtree([a-2,b-3,c-4], Tree2),
    rb_map(Tree1, plus1, Tree2).
test(steadfast_min) :-
    list_to_rbtree([a-1,b-2,c-3], Tree),
    rb_min(Tree, a, 1).
test(steadfast_max) :-
    list_to_rbtree([a-1,b-2,c-3], Tree),
    rb_max(Tree, c, 3).
test(steadfast_next) :-
    list_to_rbtree([a-1,b-2,c-3], Tree),
    rb_min(Tree, a, 1),
    rb_next(Tree, a, b, 2),
    rb_next(Tree, b, c, 3).
test(steadfast_previous) :-
    list_to_rbtree([a-1,b-2,c-3], Tree),
    rb_max(Tree, c, 3),
    rb_previous(Tree, c, b, 2),
    rb_previous(Tree, b, a, 1).
test(steadfast_list_to_rbtree) :-
    list_to_rbtree([a-1,b-2,c-3], Tree),
    list_to_rbtree([a-1,b-2,c-3], Tree).
test(steadfast_size) :-
    list_to_rbtree([a-1,b-2,c-3], Tree),
    rb_size(Tree, 3).
test(steadfast_in, [nondet]) :-
    list_to_rbtree([a-1,b-2,c-3], Tree),
    rb_in(b, 2, Tree).

% TODO: similar tests for library(assoc).
test(instantiation_visit_2, [error(existence_error(matching_rule,
                                                   rbtrees:rb_visit(_,_)),
                                   context(rbtrees:rb_visit/2,_))]) :-
    rb_visit(_Tree, _Pairs).
% TODO: add full error + context to the following tests:
test(instantiation_lookup_3a, [error(existence_error(matching_rule,_),_)]) :-
    rb_lookup(_Key,_Value,_Tree).
test(instantiation_lookup_3b, [error(existence_error(matching_rule,_),_)]) :-
    rb_lookup(k,_Value,_Tree).
test(instantiation_lookup_3c, [fail]) :- % TODO: [error(_)]) :- % existence_error or type_error(compound,'')
    list_to_rbtree([a-1,b-2], Tree),
    rb_lookup(_Key,_Value,Tree).
test(instantiation_lookup_3d, V==1) :-
    list_to_rbtree([A-1,b-2], Tree),
    rb_lookup(A,V,Tree).
test(instantiation_update_4a, [error(existence_error(matching_rule,_),_)]) :-
    rb_update(_Tree,_Key,_NewVal,_NewTree).
test(instantiation_update_4b, [error(existence_error(matching_rule,_),_)]) :-
    rb_update(_Tree,k,_NewVal,_NewTree).
test(instantiation_update_4c, [fail]) :- % TODO: [error(existence_error(matching_rule,_),_)]) :-
    list_to_rbtree([a-1,b-2], Tree),
    rb_update(Tree,_Key,_NewVal,_NewTree).
test(instantiation_update_5a, [error(existence_error(matching_rule,_),_)]) :-
    rb_update(_Tree,_Key,_NewVal,_NewTree).
test(instantiation_update_5b, [error(existence_error(matching_rule,_),_)]) :-
    rb_update(_Tree,k,_OldVal,_NewVal,_NewTree).
test(instantiation_update_5c, [error(existence_error(matching_rule,_),_)]) :-
    rb_update(_Tree,k,_OldVal,v,_NewTree).
test(instantiation_update_5d, [fail]) :- % TODO: [error(existence_error(matching_rule,_),_)]) :-
    list_to_rbtree([a-1,b-2], Tree),
    rb_update(Tree,_Key,_OldVal,_NewVal,_NewTree).
test(instantiation_apply_4a, [error(existence_error(matching_rule,_),_)]) :-
    rb_apply(_Tree,_Key,_Goal,_NewTree).
test(instantiation_apply_4b, [error(existence_error(matching_rule,_),_)]) :-
    rb_apply(_Tree,k,_Goal,_NewTree).
test(instantiation_apply_4c, [error(existence_error(matching_rule,_),_)]) :-
    rb_apply(_Tree,k,goal,_NewTree).
test(instantiation_insert_4a, [error(existence_error(matching_rule,_),_)]) :-
    rb_insert(_Tree,_Key,_Value,_NewTree).
test(instantiation_insert_4b, [error(existence_error(matching_rule,_),_)]) :-
    rb_insert(_Tree,k,_Value,_NewTree).
test(instantiation_insert_new_4a, [error(existence_error(matching_rule,_),_)]) :-
    rb_insert_new(_Tree,_Key,_Value,_NewTree).
test(instantiation_insert_new_4a, [error(existence_error(matching_rule,_),_)]) :-
    rb_insert_new(_Tree,k,_Value,_NewTree).
test(instantiation_delete_3a, [error(existence_error(matching_rule,_),_)]) :-
    rb_delete(_Tree,_Key,_NewTree).
test(instantiation_delete_3b, [error(existence_error(matching_rule,_),_)]) :-
    rb_delete(_Tree,k,_NewTree).
test(instantiation_delete_3c, [fail]) :- % TODO: [error(existence_error(matching_rule,_),_)]) :-
    list_to_rbtree([a-1,b-2], Tree),
    rb_delete(Tree,_Key,_NewTree).
test(instantiation_delete_4a, [error(existence_error(matching_rule,_),_)]) :-
    rb_delete(_Tree,_Key,_Val,_NewTree).
test(instantiation_delete_4b, [error(existence_error(matching_rule,_),_)]) :-
    rb_delete(_Tree,k,_Val,_NewTree).
test(instantiation_keys_2, [error(existence_error(matching_rule,_),_)]) :-
    rb_keys(_Tree,_Keys).
test(instantiation_map_2a, [error(existence_error(matching_rule,_),_)]) :-
    rb_map(_Tree,_Goal).
test(instantiation_map_2b, [error(instantiation_error,_)]) :-
    list_to_rbtree([a-1,b-2], Tree),
    rb_map(Tree,_Goal).
test(instantiation_map_2c) :-
    rb_empty(Tree),
    rb_map(Tree,_Goal).
test(instantiation_map_3a, [error(existence_error(matching_rule,_),_)]) :-
    rb_map(_Tree,_Goal,_NewTree).
test(instantiation_map_3b, [error(instantiation_error,_)]) :-
    list_to_rbtree([a-1,b-2], Tree),
    rb_map(Tree,_Goal,_NewTree).
test(instantiation_map_3c) :-
    rb_empty(Tree),
    rb_map(Tree,_Goal,_NewTree).
test(instantiation_partial_map_4, [error(existence_error(matching_rule,_),_)]) :-
    rb_partial_map(_,_,_,_).
test(instantiation_fold_4, [error(existence_error(matching_rule,_),_)]) :-
    rb_fold(_,_,_,_).
test(instantiation_clone_3, [error(existence_error(matching_rule,_),_)]) :-
    rb_clone(_,_,_).
test(instantiation_min_3, [error(existence_error(matching_rule,_),_)]) :-
    rb_min(_,_,_).
test(instantiation_max_3, [error(existence_error(matching_rule,_),_)]) :-
    rb_max(_,_,_).
test(instantiation_del_min_4, [error(existence_error(matching_rule,_),_)]) :-
    rb_del_min(_,_,_,_).
test(instantiation_del_max_4, [error(existence_error(matching_rule,_),_)]) :-
    rb_del_max(_,_,_,_).
test(instantiation_min_3, [error(existence_error(matching_rule,_),_)]) :-
    rb_min(_,_,_).
test(instantiation_next_4, [error(existence_error(matching_rule,_),_)]) :-
    rb_next(_,_,_,_).
test(instantiation_previous_4, [error(existence_error(matching_rule,_),_)]) :-
    rb_previous(_,_,_,_).
test(instantiation_list_to_rbtree_2, [error(instantiation_error,_)]) :- % error from system:sort/2
    list_to_rbtree(_, _).
test(instantiation_ord_list_to_rbtree_2, [error(instantiation_error,_)]) :- % error from system:sort/2
    ord_list_to_rbtree(_, _).
test(instantiation_is_rbtree_1, fail) :-
    is_rbtree(_).

test(not_tree_visit_2, [error(existence_error(matching_rule,_),_)]) :-
    rb_visit(foo, _).
test(not_tree_lookup_3, [error(existence_error(matching_rule,_),_)]) :-
    rb_lookup(_,_,foo).
test(not_tree_update_4, [error(existence_error(matching_rule,_),_)]) :-
    rb_update(foo,_,_,_).
test(not_tree_update_5, [error(existence_error(matching_rule,_),_)]) :-
    rb_update(foo,_,_,_,_).
test(not_tree_apply_4, [error(existence_error(matching_rule,_),_)]) :-
    rb_apply(_,_,_,_).
test(not_tree_insert_4, [error(existence_error(matching_rule,_),_)]) :-
    rb_insert(_,_,_,_).
test(not_tree_insert_new_4, [error(existence_error(matching_rule,_),_)]) :-
    rb_insert_new(_,_,_,_).
test(not_tree_delete_3, [error(existence_error(matching_rule,_),_)]) :-
    rb_delete(_,_,_).
test(not_tree_delete_4, [error(existence_error(matching_rule,_),_)]) :-
    rb_delete(_,_,_,_).
test(not_tree_keys_2, [error(existence_error(matching_rule,_),_)]) :-
    rb_keys(_,_).
test(not_tree_map_2, [error(existence_error(matching_rule,_),_)]) :-
    rb_map(_,_).
test(not_tree_map_3, [error(existence_error(matching_rule,_),_)]) :-
    rb_map(_,_,_).
test(not_tree_partial_map_4, [error(existence_error(matching_rule,_),_)]) :-
    rb_partial_map(_,_,_,_).
test(not_tree_fold_4, [error(existence_error(matching_rule,_),_)]) :-
    rb_fold(_,_,_,_).
test(not_tree_clone_3, [error(existence_error(matching_rule,_),_)]) :-
    rb_clone(_,_,_).
test(not_tree_min_3, [error(existence_error(matching_rule,_),_)]) :-
    rb_min(_,_,_).
test(not_tree_max_3, [error(existence_error(matching_rule,_),_)]) :-
    rb_max(_,_,_).
test(not_tree_del_min_4, [error(existence_error(matching_rule,_),_)]) :-
    rb_del_min(_,_,_,_).
test(not_tree_del_max_4, [error(existence_error(matching_rule,_),_)]) :-
    rb_del_max(_,_,_,_).
test(not_tree_min_3, [error(existence_error(matching_rule,_),_)]) :-
    rb_min(_,_,_).
test(not_tree_next_4, [error(existence_error(matching_rule,_),_)]) :-
    rb_next(_,_,_,_).
test(not_tree_previous_4, [error(existence_error(matching_rule,_),_)]) :-
    rb_previous(_,_,_,_).
test(not_tree_is_rbtree_1, fail) :-
    is_rbtree(foo).

test(is_rbtree_1a) :-
    rb_empty(Tree),
    is_rbtree(Tree).
test(is_rbtree_1b) :-
    list_to_rbtree([a-1,b-2,c-3], Tree),
    is_rbtree(Tree).
test(is_rbtree_1c) :-
    list_to_rbtree([A-1,m-2], Tree),
    A = a,
    rb_visit(Tree, [a-1,m-2]), % in order
    assertion(is_rbtree(Tree)).
test(is_rbtree_1c) :-
    list_to_rbtree([A-1,m-2], Tree),
    A = z,
    rb_visit(Tree, [z-1,m-2]), % out of order
    assertion(\+ is_rbtree(Tree)).


% TODO: The following 2 instantiation tests should fail, but they
%       currently succeed.

% test(instantiation_is_rbtree_1b, fail) :-
%     list_to_rbtree([_-1,b-2], Tree),
%     is_rbtree(Tree).
% test(instantiation_is_rbtree_1c, fail) :-
%     list_to_rbtree([_,b-2], Tree),
%     is_rbtree(Tree).

test(instantiation_size_2, [error(existence_error(matching_rule,_),_)]) :-
    rb_size(_, _).
test(instantiation_in_3, [error(existence_error(matching_rule,_),_)]) :-
    rb_in(_, _, _).

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
