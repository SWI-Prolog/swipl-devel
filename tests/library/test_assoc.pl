/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           www.swi-prolog.org
    Copyright (c)  2010-2015, University of Amsterdam
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

:- module(test_assoc,
	  [ test_assoc/0
	  ]).
:- use_module(library(plunit)).
:- use_module(library(assoc)).
:- use_module(library(sandbox)).

% Test new predicates of assoc library
% Author: Glenn Burgess May 2010
% Modified for using PlUnit by Jan Wielemaker

test_assoc :-
	run_tests([ assoc,
		    assoc_put,
		    assoc_get
		  ]).

:- begin_tests(assoc).

% Test is_assoc first as all other tests rely on this

test(wirth_85, true) :-			% Test tree from Wirth 85
	is_assoc(t(5, 1, -, t(3, 1, <, t(2, 1, <, t(1, 1, -, t, t), t), t(4, 1, -, t, t)), t(8, 1, -, t(7, 1, <, t(6, 1, -, t, t), t), t(10, 1, -, t(9, 1, -, t, t), t(11, 1, -, t, t))))).

test(unbalanced_1, fail) :-
	is_assoc(t(5, 1, <, t(3, 1, <, t(2, 1, <, t(1, 1, -, t, t), t), t(4, 1, -, t, t)), t(8, 1, -, t(7, 1, <, t(6, 1, -, t, t), t), t(10, 1, -, t(9, 1, -, t, t), t(11, 1, -, t, t))))).

test(unbalanced_2, fail) :-
	is_assoc(t(5, 1, -, t(3, 1, -, t(2, 1, <, t(1, 1, -, t, t), t), t(4, 1, -, t, t)), t(8, 1, -, t(7, 1, <, t(6, 1, -, t, t), t), t(10, 1, -, t(9, 1, -, t, t), t(11, 1, -, t, t))))).

test(unbalanced_3, fail) :-
	is_assoc(t(5, 1, -, t(3, 1, <, t(2, 1, <, t(1, 1, -, t, t), t), t(4, 1, -, t, t)), t(8, 1, -, t(7, 1, <, t(6, 1, -, t, t), t), t(10, 1, -, t(9, 1, -, t, t), t(11, 1, >, t, t))))).

test(unordered_1, fail) :-
	is_assoc(t(5, 1, -, t(3, 1, <, t(2, 1, >, t, t(1, 1, -, t, t)), t(4, 1, -, t, t)), t(8, 1, -, t(7, 1, <, t(6, 1, -, t, t), t), t(10, 1, -, t(9, 1, -, t, t), t(11, 1, -, t, t))))).

test(unordered_2, fail) :-
	is_assoc(t(5, 1, -, t(4, 1, <, t(2, 1, <, t(1, 1, -, t, t), t), t(4, 1, -, t, t)), t(8, 1, -, t(7, 1, <, t(6, 1, -, t, t), t), t(10, 1, -, t(9, 1, -, t, t), t(11, 1, -, t, t))))).

test(unordered_3, fail) :-
	is_assoc(t(5, 1, -, t(3, 1, <, t(2, 1, <, t(1, 1, -, t, t), t), t(6, 1, -, t, t)), t(8, 1, -, t(7, 1, <, t(6, 1, -, t, t), t), t(10, 1, -, t(9, 1, -, t, t), t(11, 1, -, t, t))))).

test(bad_structure_1, fail) :-
	is_assoc(t(5, 1, =, t(3, 1, <, t(2, 1, <, t(1, 1, -, t, t), t), t(4, 1, -, t, t)), t(8, 1, -, t(7, 1, <, t(6, 1, -, t, t), t), t(10, 1, -, t(9, 1, -, t, t), t(11, 1, -, t, t))))).

test(bad_structure_2, fail) :-
	is_assoc(t(5, 1, -, t(3, 1, <, t(2, 1, <, t(1, 1, -, t, t), t), t(4, 1, -, t, t)), t(8, 1, -, t(7, 1, <, t(6, 1, -, t, t), t), t(10, 1, -, t(9, 1, -, t, t), t(11, 1, -, f, t))))).

test(bad_structure_keys_not_ground, fail) :-
	is_assoc(t(5, 1, -, t(3-_X, 1, <, t(2, 1, <, t(1, 1, -, t, t), t), t(4, 1, -, t, t)), t(8, 1, -, t(7, 1, <, t(6, 1, -, t, t), t), t(10, 1, -, t(9, 1, -, t, t), t(11, 1, -, t, t))))).

% Test del_min_assoc.
% Recursively extract all elements (first to last)
% At each step check that returned association list is still valid (balanced etc.)
del_min_assoc_to_list( A, L ) :-
	del_min_assoc_to_list( A, [], L ).

del_min_assoc_to_list( t, L, L) :- !.

del_min_assoc_to_list( A, Rest, [Key-Val | Result]) :-
	del_min_assoc( A, Key, Val, A1 ) ->
	is_assoc(A1),
	del_min_assoc_to_list(A1, Rest, Result).

% Test del_max_assoc.
% Recursively extract all elements (last to first)
% At each step check that returned association list is still valid (balanced etc.)
del_max_assoc_to_list( A, L ) :-
	del_max_assoc_to_list( A, [], L ).

del_max_assoc_to_list( t, L, L) :- !.

del_max_assoc_to_list( A, Rest, Result) :-
	del_max_assoc( A, Key, Val, A1 ) ->
	is_assoc(A1),
	del_max_assoc_to_list(A1, [Key-Val | Rest], Result).

test(del_max_min, true) :-
	Wirth = t(5, 1, -, t(3, 1, <, t(2, 3, <, t(1, 4, -, t, t), t), t(4, 5, -, t, t)), t(8, 6, -, t(7, 7, <, t(6, 8, -, t, t), t), t(10, 9, -, t(9, 10, -, t, t), t(11, 11, -, t, t)))),
	del_max_assoc_to_list(Wirth,L),
	assoc_to_list(Wirth,L),
	del_min_assoc_to_list(Wirth,L).

% Test del_assoc
% Extract elements one at a time, comparing results to expected result from Wirth 85.
test_del_assoc( K, Win, V, Wout ) :-
	del_assoc(K, Win, V1, W1) ->
	V = V1,
	Wout = W1,
	is_assoc(W1).

test(wirth_del, true) :-
	Wirtha = t(5, 1, -, t(3, 1, <, t(2, 3, <, t(1, 4, -, t, t), t), t(4, 5, -, t, t)), t(8, 6, -, t(7, 7, <, t(6, 8, -, t, t), t), t(10, 9, -, t(9, 10, -, t, t), t(11, 11, -, t, t)))) ->
	test_del_assoc(4, Wirtha, 5, Wb),
	Wb = t(5, 1, >, t(2, 3, -, t(1, 4, -, t, t), t(3, 1, -, t, t)), t(8, 6, -, t(7, 7, <, t(6, 8, -, t, t), t), t(10, 9, -, t(9, 10, -, t, t), t(11, 11, -, t, t)))),
	test_del_assoc(8, Wb, 6, Wc),
	Wc = t(5, 1, >, t(2, 3, -, t(1, 4, -, t, t), t(3, 1, -, t, t)), t(7, 7, >, t(6, 8, -, t, t), t(10, 9, -, t(9, 10, -, t, t), t(11, 11, -, t, t)))),
	test_del_assoc(6, Wc, 8, Wd),
	Wd = t(5, 1, >, t(2, 3, -, t(1, 4, -, t, t), t(3, 1, -, t, t)), t(10, 9, <, t(7, 7, >, t, t(9, 10, -, t, t)), t(11, 11, -, t, t))),
	test_del_assoc(5, Wd, 1, We1),
	We1 = t(7, 7, -, t(2, 3, -, t(1, 4, -, t, t), t(3, 1, -, t, t)), t(10, 9, -, t(9, 10, -, t, t), t(11, 11, -, t, t))),
   % Wirth's balance algorithm yields a different result for the above. Ours is 'more balanced' in this case.
   % but for the rest of the tests a closer structure is achieved by deleting node 3 (and changing 5 to 3
   % which I haven't done)
	test_del_assoc(3, Wd, 1, We),
	We = t(5, 1, >, t(2, 3, <, t(1, 4, -, t, t), t), t(10, 9, <, t(7, 7, >, t, t(9, 10, -, t, t)), t(11, 11, -, t, t))),
	test_del_assoc(2, We, 3, Wf),   % double rotation to rebalance
	Wf = t(7, 7, -, t(5, 1, <, t(1, 4, -, t, t), t), t(10, 9, -, t(9, 10, -, t, t), t(11, 11, -, t, t))),
	test_del_assoc(1, Wf, 4, Wg),
	Wg = t(7, 7, >, t(5, 1, -, t, t), t(10, 9, -, t(9, 10, -, t, t), t(11, 11, -, t, t))),
	test_del_assoc(7, Wg, 7, Wh),
   % Again, Wirth's algorithm balances the tree differently, although both are correct.
	Wh = t(9, 10, >, t(5, 1, -, t, t), t(10, 9, >, t, t(11, 11, -, t, t))).

% Ah, but the above doesn't test enough cases, possibly because our algorithm is slightly different from Wirth's. So try this:

rand_list( N, L ) :-
	rand_list( N, [], L0 ),
	sort(L0, L).

rand_list( 0, L, L ) :- !.
rand_list( N, Lin, Lout ) :-
	N1 is N - 1,
	R is random(100),
	rand_list(N1, [R-1 | Lin], Lout).

% Exercise (in deletion) avl_geq clauses 1,2 and 5
test(random_min, true) :-
	rand_list(100,L),
	list_to_assoc(L,A),
	del_min_assoc_to_list(A,_L1).

% Exercise (in deletion) avl_geq clauses 3,4 and 6
test(random_max, true) :-
	rand_list(100,L),
	list_to_assoc(L,A),
	del_max_assoc_to_list(A,_L1).

test(del_empty, true) :-
	empty_assoc(A),
	\+ del_min_assoc(A, _, _, _).

test(del_no_member, true) :-
	list_to_assoc([1-a,2-b,4-c], A),
	\+ del_min_assoc(3, A, _, _).

:- end_tests(assoc).

:- begin_tests(assoc_put).

% put_assoc/4 uses C code where it can.  It must do exactly what its
% Prolog definition does.

pl_put(K, A0, V, A) :-
	assoc:insert(A0, K, V, A, _).

put_all([], A, A).
put_all([K-V|T], A0, A) :-
	put_assoc(K, A0, V, A1),
	pl_put(K, A0, V, A2),
	A1 == A2,
	put_all(T, A1, A).

% True if put_assoc/4 and its Prolog definition do the same to a copy
% of the arguments, with the occurs_check flag set to Flag.
same(Flag, K, A0, V, A) :-
	outcome(Flag, put_assoc, put(K,A0,V,A), O1),
	outcome(Flag, pl_put, put(K,A0,V,A), O2),
	O1 =@= O2.

outcome(Flag, Put, Args, Result-Copy) :-
	copy_term(Args, Copy),
	Copy = put(K,A0,V,A),
	current_prolog_flag(occurs_check, Old),
	setup_call_cleanup(
	    set_prolog_flag(occurs_check, Flag),
	    (   catch(call(Put, K, A0, V, A), error(Result, _), true)
	    ->  true
	    ;   Result = failed
	    ),
	    set_prolog_flag(occurs_check, Old)).

chain(I, N, t) :-
	I > N,
	!.
chain(I, N, t(I,I,-,t,R)) :-
	I1 is I+1,
	chain(I1, N, R).

test(random) :-
	findall(K-V, (between(1, 3000, V), K is random(1000)), Pairs),
	put_all(Pairs, t, A),
	is_assoc(A).
test(ascending) :-
	findall(K-K, between(1, 2000, K), Pairs),
	put_all(Pairs, t, A),
	is_assoc(A),
	assoc_to_list(A, Pairs).
test(descending) :-
	findall(K-K, (between(1, 2000, I), K is 2001-I), Pairs),
	put_all(Pairs, t, A),
	is_assoc(A).
test(update_keeps_key) :-
	K1 = f(1),
	duplicate_term(K1, K2),
	put_assoc(K1, t, old, A0),
	put_assoc(K2, A0, new, t(K,new,-,t,t)),
	same_term(K, K1).
test(variable_value) :-
	put_assoc(a, t, V, A),
	get_assoc(a, A, X),
	X == V.
test(bound_output) :-
	list_to_assoc([1-a,2-b,3-c], A0),
	pl_put(4, A0, d, A),
	put_assoc(4, A0, d, A),
	\+ put_assoc(4, A0, e, A),
	\+ put_assoc(4, A0, d, t).
test(not_an_assoc,
     [ forall(member(Bad, [ foo, _, f(x), t(a,1,-,t),
			    t(a,1,-,t,x), t(m,1,-,_,t)
			  ]))
     ]) :-
	same(false, 0, Bad, v, _),
	same(false, z, Bad, v, _).
test(odd_balance,
     [ forall(member(Bad, [ t(a,1,>,t,t), t(a,1,<,t,t),
			    t(a,1,nobal,t,t), t(a,1,_,t,t),
			    t(a,1,-,t,t(b,2,>,t,t))
			  ]))
     ]) :-
	forall(member(K, [0, a, b, c]),
	       same(false, K, Bad, v, _)).
test(deep) :-				% deeper than any AVL tree
	chain(1, 200, Chain),
	same(false, 0, Chain, v, _),
	same(false, 100, Chain, v, _),
	same(false, 201, Chain, v, _).
test(occurs_check, [forall(member(Flag, [false, true, error]))]) :-
	same(Flag, k, t, O1, O1),
	same(Flag, O2, t, v, O2),
	same(Flag, k, t, f(O3), O3),
	same(Flag, z, t(a,O4,-,t,t), v, O4),
	same(Flag, z, t(a,1,-,t,t), v, _),
	same(Flag, a, t(a,1,-,t,t), v, _).
test(sandbox) :-
	safe_goal(put_assoc(a, t, 1, _)).

% Backtracking must undo all bindings put_assoc/4 made.  Variables in a
% term created before the choicepoint are older than the choicepoint,
% so binding them must be trailed.  Variables in the frame are on the
% local stack.

test(backtrack_local, true(all_var([K,V,A]))) :-
	(   put_assoc(K, t, V, A),
	    fail
	;   true
	).
test(backtrack_global, true(all_var([K,V,A]))) :-
	T = f(K,V,A),
	list_to_assoc([1-a,3-c], A0),
	(   arg(1, T, K1), arg(2, T, V1), arg(3, T, A1),
	    put_assoc(K1, A0, V1, A1),
	    fail
	;   true
	).
test(backtrack_bound_later, true(all_var([V,K]))) :-
	list_to_assoc([1-a,3-c], A0),
	(   put_assoc(2, A0, V, A),
	    put_assoc(K, A, x, _),
	    V = 1, K = 0,
	    fail
	;   true
	).
test(backtrack_output_in_tree, true(var(X))) :-
	A0 = t(a,X,-,t,t),
	(   put_assoc(z, A0, v, X),
	    fail
	;   true
	).
test(backtrack_keeps_tree, A0 =@= Copy) :-
	var_assoc(100, A0),
	duplicate_term(A0, Copy),
	forall(between(0, 101, K),
	       (   put_assoc(K, A0, new, A),
		   assoc_to_values(A, Vs),
		   maplist(=(x), Vs),
		   fail
	       ;   true
	       )),
	assoc_to_values(A0, Vs0),
	maplist(var, Vs0).
test(backtrack_gc, A0 =@= Copy) :-
	var_assoc(1000, A0),
	duplicate_term(A0, Copy),
	forall(between(0, 1001, K),
	       (   put_assoc(K, A0, V, A),
		   garbage_collect,
		   get_assoc(K, A, V1),
		   V1 == V,
		   V = K,
		   fail
	       ;   true
	       )).

all_var(List) :-
	maplist(var, List).

%   var_assoc(+N, -Assoc): Assoc maps 1..N to fresh variables

var_assoc(N, Assoc) :-
	findall(K-_, between(1, N, K), Pairs),
	list_to_assoc(Pairs, Assoc).

:- end_tests(assoc_put).

:- begin_tests(assoc_get).

% get_assoc/3 is implemented in C.  It must do what its Prolog
% definition does.

pl_get(Key, t(K,V,_,L,R), Val) :-
	compare(Order, Key, K),
	pl_get(Order, Key, V, L, R, Val).

pl_get(=, _, V, _, _, V).
pl_get(<, Key, _, L, _, V) :-
	pl_get(Key, L, V).
pl_get(>, Key, _, _, R, V) :-
	pl_get(Key, R, V).

% True if get_assoc/3 and its Prolog definition do the same to a copy
% of the arguments, with the occurs_check flag set to Flag.
same(Flag, K, A, V) :-
	outcome(Flag, get_assoc, get(K,A,V), O1),
	outcome(Flag, pl_get, get(K,A,V), O2),
	O1 =@= O2.

outcome(Flag, Get, Args, Result-Copy) :-
	copy_term(Args, Copy),
	Copy = get(K,A,V),
	current_prolog_flag(occurs_check, Old),
	setup_call_cleanup(
	    set_prolog_flag(occurs_check, Flag),
	    (   catch(call(Get, K, A, V), error(Result, _), true)
	    ->  true
	    ;   Result = failed
	    ),
	    set_prolog_flag(occurs_check, Old)).

key(int, I, I).
key(negative, I, K) :- K is -I.
key(big, I, K) :- K is I + (1<<70).
key(tagged_limit, I, K) :-
	current_prolog_flag(max_tagged_integer, Max),
	K is Max - 5 + I.
key(float, I, K) :- K is I*1.0.
key(atom, I, K) :- atom_concat(k, I, K).
key(string, I, K) :- format(string(K), "k~d", [I]).
key(compound, I, k(J,I)) :- J is I mod 7.
key(mixed, I, K) :-
	J is I mod 5,
	nth0(J, [int, float, atom, string, compound], Type),
	key(Type, I, K).

%   assoc(+Type, +N, -Assoc): Assoc holds the even numbers of 1..N as
%   keys of Type.

assoc(Type, N, Assoc) :-
	findall(K-v(K), (between(1, N, I), I mod 2 =:= 0, key(Type, I, K)),
		Pairs),
	list_to_assoc(Pairs, Assoc).

chain(I, N, t) :-
	I > N,
	!.
chain(I, N, t(I,I,-,t,R)) :-
	I1 is I+1,
	chain(I1, N, R).

test(empty, fail) :-
	get_assoc(a, t, _).
test(keys,
     [ forall(( member(Type, [ int, negative, big, tagged_limit, float,
			       atom, string, compound, mixed
			     ]),
		member(N, [1, 2, 3, 10, 100])
	      ))
     ]) :-
	assoc(Type, N, A),
	Max is N+1,
	forall(( between(0, Max, I),
		 key(Type, I, K)
	       ),
	       ( same(false, K, A, _),
		 same(false, K, A, v(K)),
		 same(false, K, A, other)
	       )).
test(large,
     [ forall(member(Type, [int, atom, mixed]))
     ]) :-
	assoc(Type, 20000, A),
	forall(( between(0, 20001, I),
		 key(Type, I, K)
	       ),
	       (   pl_get(K, A, V0)
	       ->  get_assoc(K, A, V),
		   V == V0
	       ;   \+ get_assoc(K, A, _)
	       )).
test(other_type,
     [ forall(member(Type, [int, float, atom, mixed]))
     ]) :-
	assoc(Type, 100, A),
	forall(( member(Type2, [int, big, float, atom, string]),
		 between(0, 101, I),
		 key(Type2, I, K)
	       ),
	       same(false, K, A, _)).
test(int_float, V1-V2 == int-float) :-
	list_to_assoc([1-int, 1.0-float, 2-two], A),
	get_assoc(1, A, V1),
	get_assoc(1.0, A, V2).
test(variable_key) :-
	put_assoc(K, t, v, A0),
	put_assoc(f(X), A0, w, A),
	same(false, K, A, _),
	same(false, f(X), A, _),
	same(false, _, A, _),
	same(false, f(_), A, _),
	same(false, a, A, _).
test(variable_value, X == V) :-
	list_to_assoc([a-1, b-V, c-3], A),
	get_assoc(b, A, X).
test(attributed_value, V-X == 2-woken) :-
	list_to_assoc([a-1, b-2, c-3], A),
	freeze(V, X = woken),
	get_assoc(b, A, V).
test(attributed_value_fails, fail) :-
	list_to_assoc([a-1, b-2, c-3], A),
	freeze(V, fail),
	get_assoc(b, A, V).
test(deterministic) :-
	assoc(int, 100, A),
	forall(between(0, 101, K),
	       (   call_cleanup(get_assoc(K, A, _), Det = true)
	       ->  Det == true
	       ;   true
	       )).
test(backtrack, true(var(V))) :-
	list_to_assoc([a-1, b-2, c-3], A),
	(   get_assoc(b, A, V),
	    fail
	;   true
	).
test(gen_assoc, all(V == [2])) :-
	list_to_assoc([a-1, b-2, c-3], A),
	gen_assoc(b, A, V).
test(not_an_assoc,
     [ forall(member(Bad, [ foo, 42, "t", f(x), t(a,1,-,t) ])),
       error(type_error(btree, _))
     ]) :-
	get_assoc(a, Bad, _).
test(unbound_assoc, error(instantiation_error)) :-
	get_assoc(a, _, _).
test(not_a_node,
     [ forall(member(Bad, [ t(m,1,-,x,y), t(m,1,-,_,_), t(m,1,-,1,2),
			    t(m,1,-,f(a,1,-,t,t),f(z,1,-,t,t)),
			    t(m,1,-,t(a,1),t(z,1))
			  ])),
       fail
     ]) :-
	(   get_assoc(a, Bad, _)
	;   get_assoc(z, Bad, _)
	).
test(deep, V == 300) :-			% deeper than any AVL tree
	chain(1, 300, Chain),
	get_assoc(300, Chain, V).
test(occurs_check, [forall(member(Flag, [false, true, error]))]) :-
	A = t(a,X,-,t,t),
	same(Flag, a, A, f(X)),
	same(Flag, a, A, _),
	same(Flag, a, t(a,f(Y),-,t,t), Y),
	same(Flag, a, t(a,1,-,t,t), _).
test(gc, L == Vs) :-
	numlist(1, 500, L),
	findall(K-K, member(K, L), Pairs),
	list_to_assoc(Pairs, A),
	findall(V,
		( member(K, L),
		  garbage_collect,
		  get_assoc(K, A, V)
		),
		Vs).
test(sandbox) :-
	safe_goal(get_assoc(a, t, _)).

:- end_tests(assoc_get).
