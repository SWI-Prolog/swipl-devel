/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2016, University of Amsterdam
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

:- module(test_trie,
	  [ test_trie/0
	  ]).
:- use_module(library(plunit)).
:- use_module(library(debug)).

test_trie :-
	run_tests([ trie
		  ]).

:- begin_tests(trie).

test(insert_atom, N == noot) :-
	trie_new(T),
	trie_insert(T, aap, noot),
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
test(insert_vars, N == noot) :-
	trie_new(T),
	trie_insert(T, aap(_), noot),
	trie_lookup(T, aap(_), N).
test(insert_variant, N == noot) :-
	trie_new(T),
	trie_insert(T, aap(_,_), noot),
	trie_lookup(T, aap(_,_), N),
	assertion(\+ trie_lookup(T, aap(A,A), N)).
test(insert_cycle, [sto(rational_trees)]) :-
	trie_new(T),
	X = f(X),
	catch(trie_insert(T, X, noot), E, true),
	assertion(E = error(type_error(acyclic_term, X),_)),
	\+ trie_gen(T, _, _).
test(insert_attvar, error(type_error(free_of_attvar,f(_)))) :-
	trie_new(T),
	freeze(X, true),
	trie_insert(T, f(X), noot).

:- end_tests(trie).
