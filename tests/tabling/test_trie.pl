/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2017-2025, University of Amsterdam
                              VU University Amsterdam
			      CWI, Amsterdam
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

:- module(test_trie,
	  [ test_trie/0
	  ]).
:- use_module(library(plunit)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(debug)).
:- use_module(library(pairs)).

test_trie :-
	run_tests([ trie
		  ]).

:- begin_tests(trie).

test(insert_atom, N == noot) :-
	trie_new(T),
	trie_insert(T, aap, noot),
	trie_lookup(T, aap, N).
test(insert_value_term, N == noot(1)) :-
	trie_new(T),
	trie_insert(T, aap, noot(1)),
	trie_lookup(T, aap, N).
test(insert_two, N1-N2 == noot1-noot2) :-
	trie_new(T),
	trie_insert(T, aap1, noot1),
	trie_insert(T, aap2, noot2),
	trie_lookup(T, aap1, N1),
	trie_lookup(T, aap2, N2).
test(insert_compound, N == noot) :-
	trie_new(T),
	trie_insert(T, aap(1), noot),
	trie_lookup(T, aap(1), N).
test(insert_nested, set(Copy == [v(a(x(a)),b(y),c(z))])) :-
	trie_new(T),
	trie_insert(T, v(a(x(a)),b(y),c(z)), true),
	trie_gen(T, Copy, _).
test(insert_nested2, V == 42) :-
	trie_new(T),
	K = user:t([1,2,3,4], [1,2,3,4]),
	trie_insert(T, K, 42),
	trie_gen(T, K, V).
test(insert_nested_get, Copy == v(a(x(a)),b(y),c(z))) :-
	trie_new(T),
	trie_insert(T, v(a(x(a)),b(y),c(z)), true, Node),
	trie_term(Node, Copy).
test(insert_vars, N == noot) :-
	trie_new(T),
	trie_insert(T, aap(_), noot),
	trie_lookup(T, aap(_), N).
test(insert_variant, N == noot) :-
	trie_new(T),
	trie_insert(T, aap(_,_), noot),
	trie_lookup(T, aap(_,_), N),
	assertion(\+ trie_lookup(T, aap(A,A), N)).
test(insert_vars, STKeys =@= Keys) :-
	Keys = [a(1,a,1), a(_,b,2), a(_,_,3), a(A,A,4)],
	trie_new(T),
	forall(member(K, Keys), trie_insert(T, K, true)),
	findall(K, trie_gen(T, K, _), TKeys),
	sort(3, @<, TKeys, STKeys).
test(rescale_cars, STKeys =@= Keys) :-
	maplist(shared_list, [1,10,100,1000,10000], Keys),
	trie_new(T),
	forall(member(K, Keys), trie_insert(T, K, true)),
	findall(K, trie_gen(T, K, _), TKeys),
	sort(2, @<, TKeys, STKeys).
test(insert_gsize, STKeys =@= KPairs) :-
	Keys = [a(1,a), a(1.0,b), a("hello world",c)],
	sort(Keys, KPairs),
	trie_new(T),
	forall(member(K, Keys), trie_insert(T, K, true)),
	findall(K, trie_gen(T, K, _V), TKeys),
	sort(TKeys, STKeys).
test(insert_cycle, [sto(rational_trees)]) :-
	trie_new(T),
	X = f(X),
	catch(trie_insert(T, X, noot), E, true),
	assertion(E = error(type_error(acyclic_term, X),_)),
	\+ trie_gen(T, _, _).
test(insert_attvar,
     [ error(type_error(free_of_attvar,f(_))),
       condition(\+has_trie_attvar_support)
     ]) :-
	trie_new(T),
	freeze(X, true),
	trie_insert(T, f(X), noot).
test(delete, Keys == [aap,mies]) :-
	trie_new(T),
	trie_insert(T, aap, a),
	trie_insert(T, noot, n),
	trie_insert(T, mies, m),
	trie_delete(T, noot, N),
	assertion(N==n),
	findall(K, trie_gen(T, K, _), Keys0),
	sort(Keys0, Keys).
test(gen_indirect, true) :-
	trie_new(T),
	trie_insert(T, 0.25, true),
	trie_gen(T, 0.25).
test(var1, set(Y == [1,2,3])) :-
        test_var(_, Y).
test(var2, set(Y == [1,2])) :-
        test_var(a, Y).
test(var3, set(Y == [1])) :-
        test_var(c, Y).
test(issue_1277, X == [foo]) :-
	trie_new(T),
	trie_insert(T, _, [foo]),
	trie_gen_compiled(T, _, X).
test(gen_compiled2_free, GData =@= Data) :-
	setof(D, data(D), Data),
	trie_new(T),
	maplist(trie_insert(T), Data),
	setof(GD, trie_gen_compiled(T, GD), GData).
test(gen_compiled2_instantiated, GData =@= Data) :-
	setof(D, (data(D), nonvar(D)), Data),
	trie_new(T),
	maplist(trie_insert(T), Data),
	setof(GD, (member(GD, Data), trie_gen_compiled(T, GD)), GData).
test(gen_compiled3_free, GData =@= KVData) :-
	setof(D, data(D), Data),
	copy_term(Data, Copy),
	pairs_keys_values(KVData, Data, Copy),
	trie_new(T),
	maplist(trie_insert(T), Data, Copy),
	setof(GK-GV, trie_gen_compiled(T, GK, GV), GData).
test(gen_compiled3_cycle, error(type_error(acyclic_term,_))) :-
	trie_new(T),
	X = f(X),
	trie_insert(T, x, X),
	forall(trie_gen_compiled(T, K, V), writeln(K-V)).

:- if(current_prolog_flag(bounded, false)).
data(Big) :- Big is random(1<<200).
data(Big) :- Big is -random(1<<200).
:- endif.
data(Med) :- Med is random(1<<55).
data(Med) :- Med is -random(1<<55).
data(363).
data(-363).
data(0).
data(1r3).
data(3.14).
data("This is a nice string").
data(_Var).
data(f(x)).
data(f(X,X)).
data([nice, list(of(terms))]).


shared_list(N, t(List,N)) :-
	length(List, N),
	reverse(List, R),
	R = List.

test_var(X, Y) :-
	trie_new(T),
	trie_insert(T, f(_, 1)),
	trie_insert(T, f(a, 2)),
	trie_insert(T, f(b, 3)),
	trie_gen(T, f(X, Y)).

:- end_tests(trie).

has_trie_attvar_support :-
    trie_new(T),
    put_attr(X, x, t),
    catch(trie_insert(T, f(X)),
          error(type_error(free_of_attvar, _),_),
          fail).
