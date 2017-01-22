/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           www.swi-prolog.org
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

:- module(test_acyclic, [test_acyclic/0]).
:- use_module(library(plunit)).



test_acyclic :-
	run_tests( [ acyclic_terms,
	             cyclic_terms
	           ]).



build_term(Depth, Arity, Term) :-
	N1 is Arity + 1,
	length(L, N1),
	L = [f|Args],
	(   Depth == 1
	->  true
	;   Depth1 is Depth - 1,
	    findall(SubTerm,
	            ( between(1, Arity, _),
	              build_term(Depth1, Arity, SubTerm)
	            ),
	            SubTerms),
	    Args = SubTerms
	),
	Term =.. L.


leftmost_leaf(Term, Leaf) :-
	compound(Term), !,
	arg(1, Term, SubTerm),
	leftmost_leaf(SubTerm, Leaf).
leftmost_leaf(Leaf, Leaf).

rightmost_leaf(Term, Leaf) :-
	compound(Term), !,
	functor(Term, _, Arity),
	arg(Arity, Term, SubTerm),
	rightmost_leaf(SubTerm, Leaf).
rightmost_leaf(Leaf, Leaf).



:- begin_tests(acyclic_terms).

test(var_is_acyclic) :-
	acyclic_term(_).

test(atom_is_acyclic) :-
	acyclic_term(a).

test(term_is_acyclic) :-
	Term = f(_),
	acyclic_term(Term).

test(sub_term_is_acyclic) :-
	Term = f(f(_)),
	acyclic_term(Term).

test(sibling_leaves_is_acyclic) :-
	Term = f(_,_),
	acyclic_term(Term).

test(sub_term_and_leaf_is_acyclic) :-
	Term = f(f(_),_),
	acyclic_term(Term).

test(leaf_and_subterm_is_acyclic) :-
	Term = f(_,f(_)),
	acyclic_term(Term).

test(sibling_sub_terms_is_acyclic) :-
	Term = f(f(_),f(_)),
	acyclic_term(Term).

test(multiple_sibling_sub_terms_is_acyclic) :-
	Term = f(f(_),f(_),f(_)),
	acyclic_term(Term).

test(multiple_sub_terms_is_acyclic) :-
	Term = f(f(_),f(f(_),f(_)),f(_)),
	acyclic_term(Term).

test(shared_term_is_acyclic_1) :-
	SubTerm = f(_),
	Term = f(SubTerm, SubTerm),
	acyclic_term(Term).

test(shared_term_is_acyclic_2) :-
	SubTerm = f(_),
	Term = f(SubTerm, f(SubTerm)),
	acyclic_term(Term).

test(shared_term_is_acyclic_3) :-
	SubTerm = f(_),
	Term = f(f(SubTerm), SubTerm),
	acyclic_term(Term).

test(shared_term_is_acyclic_4) :-
	SubTerm = f(_),
	Term = f(f(SubTerm), f(SubTerm)),
	acyclic_term(Term).

test(zero_arity_term_is_acyclic, [sto(rational_trees)]) :-
	Term = f(),
	acyclic_term(Term).

test(zero_arity_sub_term_is_acyclic) :-
	Term = f(f()),
	acyclic_term(Term).

test(zero_arity_shared_term_is_acyclic) :-
	SubTerm = f(),
	Term = f(SubTerm, SubTerm),
	acyclic_term(Term).

test(deeply_nested_term_is_acyclic) :-
	Term = f(f(f(f(f(f(f(f(f(f(_)))))))))),
	acyclic_term(Term).

test(deeply_nested_left_term_is_acyclic) :-
	Term = f(f(f(f(f(f(f(f(f(f(_, _), _), _), _), _), _), _), _), _), _),
	acyclic_term(Term).

test(deeply_nested_right_term_is_acyclic) :-
	Term = f(_, f(_, f(_, f(_, f(_, f(_, f(_, f(_, f(_, f(_, _)))))))))),
	acyclic_term(Term).

test(deeply_nested_term_is_acyclic) :-
	build_term(10, 2, Term),
	acyclic_term(Term).

test(widely_nested_term_is_acyclic) :-
	build_term(5, 10, Term),
	acyclic_term(Term).

test(list_is_acyclic) :-
	List = [a, b, c],
	acyclic_term(List).

test(empty_list_is_acyclic) :-
	List = [],
	acyclic_term(List).

test(long_list_is_acyclic_1) :-
	length(List, 100000),
	maplist(=('x'), List),
	acyclic_term(List).

test(long_list_is_acyclic_2) :-
	length(List, 100000),
	maplist(=(f(_)), List),
	acyclic_term(List).

test(long_list_is_acyclic_3) :-
	length(List, 100000),
	maplist(=(f(_,_,_,_,_,_,_,_,_,_)), List),
	acyclic_term(List).

:- end_tests(acyclic_terms).



:- begin_tests(cyclic_terms, [sto(rational_trees)]).

test(term_is_cyclic) :-
	Term = f(Term),
	cyclic_term(Term).

test(sub_term_is_acyclic) :-
	SubTerm = f(SubTerm),
	Term = f(SubTerm, SubTerm),
	cyclic_term(Term).

test(sub_term_and_leaf_is_cyclic) :-
	Term = f(f(Term),_),
	cyclic_term(Term).

test(leaf_and_subterm_is_cyclic) :-
	Term = f(_,f(Term)),
	cyclic_term(Term).

test(multiple_sibling_sub_terms_is_cyclic) :-
	Term = f(f(_),f(Term),f(_)),
	cyclic_term(Term).

test(multiple_sub_terms_is_cyclic) :-
	Term = f(f(_),f(f(Term),f(_)),f(_)),
	cyclic_term(Term).

test(deeply_nested_term_is_cyclic) :-
	Term = f(f(f(f(f(f(f(f(f(f(Term)))))))))),
	cyclic_term(Term).

test(deeply_nested_left_term_is_cyclic) :-
	Term = f(f(f(f(f(f(f(f(f(f(Term, _), _), _), _), _), _), _), _), _), _),
	cyclic_term(Term).

test(deeply_nested_right_term_is_cyclic) :-
	Term = f(_, f(_, f(_, f(_, f(_, f(_, f(_, f(_, f(_, f(_, Term)))))))))),
	cyclic_term(Term).

test(deeply_nested_term_is_cyclic_1) :-
	build_term(10, 2, Term),
	leftmost_leaf(Term, Leaf),
	Leaf = Term,
	cyclic_term(Term).

test(deeply_nested_term_is_cyclic_2) :-
	build_term(10, 2, Term),
	rightmost_leaf(Term, Leaf),
	Leaf = Term,
	cyclic_term(Term).

test(widely_nested_term_is_cyclic_1) :-
	build_term(5, 10, Term),
	leftmost_leaf(Term, Leaf),
	Leaf = Term,
	cyclic_term(Term).

test(widely_nested_term_is_cyclic_2) :-
	build_term(5, 10, Term),
	rightmost_leaf(Term, Leaf),
	Leaf = Term,
	cyclic_term(Term).

test(list_is_cyclic) :-
	List = [a, b, c|List],
	cyclic_term(List).

test(long_list_is_cyclic) :-
	List = [_|X],
	length(List0, 100000),
	append(List0, List, X),
	cyclic_term(List).

:- end_tests(cyclic_terms).
