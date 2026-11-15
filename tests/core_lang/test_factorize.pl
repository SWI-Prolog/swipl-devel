/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           www.swi-prolog.org
    Copyright (c)  2010-2011, University of Amsterdam
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

:- module(test_factorize,
	  [ test_factorize/0
	  ]).
:- use_module(library(plunit)).
:- use_module(library(terms)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(nb_set)).

test_factorize :-
	run_tests([ factorize,
		    factorize_contract
		  ]).

:- meta_predicate ok(+, 0).

factorize_term(Term, Skeleton, Substitution) :-
	'$factorize_term'(Term, Skeleton, Substitution).

test_factor(Term) :-
	copy_term(Term, Save),
	term_factorized(Term, FOK, _BOK),
	(   factorize_term(Term, FT, BT),
	    ok(skeleton, variant(FOK, FT)),
	    maplist(call, BT),
	    ok(rebind, variant(FT, Save)),
	    garbage_collect,
	    fail
	;   ok(backtrack, variant(Term, Save))
	).

ok(Id, G) :-
	(   G
	->  true
	;   throw(failed(Id, G))
	).

fumo(0,fumo) :- !.
fumo(N,[F|F]) :-
        N1 is N-1,
        fumo(N1,F).

:- begin_tests(factorize, [sto(rational_trees)]).

test(simple, true) :-
	X = a, test_factor(X).
test(simple, true) :-
	X = a(1), test_factor(X).
test(cyclic, true) :-
	X = a(X), test_factor(X).
test(double, true) :-
	X = a(X,X), test_factor(X).
test(double_cyclic, true) :-
	A = a(X), X = a(A), test_factor(X).
test(double_cyclic, true) :-
	A = a(A), X = x(A,A), test_factor(X).
test(double_cyclic, true) :-
	X = x(A,A), A = a(A), test_factor(X).
test(double_cyclic, true) :-
	A = a(A), X = x(b(A),A), test_factor(X).
test(fumo, true) :-
	fumo(20, X), test_factor(X).

%	The standard order is not antisymmetric on rational trees: for
%	these, compare/3 answers (>) in both directions.  Keying an rbtree
%	on it, as the previous implementation did, could lose a key that was
%	present and then recurse forever.
%
%	@see https://github.com/SWI-Prolog/swipl-devel/issues/1162

test(incoherent_standard_order, true) :-
	A = s(A,0), B = s(C,1), C = s(B,0),
	test_factor(t(A,B)),
	test_factor(t(B,A)).

:- end_tests(factorize).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The tests above compare term_factorized/3 against '$factorize_term'/3.  That
only pins the two against each other, and only where they happen to agree:
'$factorize_term'/3 shares what is *physically* shared, while
term_factorized/3 keys an rbtree on the standard order and therefore shares
everything that is ==, which on rational trees means everything denoting the
same infinite tree.  The two answers coincide for the terms above because
each was built by unification, so every repeated subterm is also shared; they
differ on, say, f(g(a),g(a)) with two separately built g(a) cells.

The tests below pin the properties term_factorized/3 must have whatever it is
implemented on top of, so that the implementation can be replaced.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%!	factorize_case(-Name, -Term) is nondet.
%
%	Corpus for the implementation independent properties.

factorize_case(atom,		a).
factorize_case(compound,	a(1)).
factorize_case(zero_arity,	g(f(),f())).
factorize_case(list,		[a,b]).
factorize_case(list_shared,	p(L,L))		:- L = [a,b].
factorize_case(list_unshared,	p([a,b],[a,b])).
factorize_case(string,		g("abc","abc")).
factorize_case(equal_unshared,	f(g(a),g(a))).
factorize_case(var_shared,	f(X,X)).
factorize_case(attvar,		g(V,V))		:- freeze(V, true).
factorize_case(dollar_var,	g('$VAR'(1),'$VAR'(1))).
factorize_case(cyclic,		T)		:- T = a(T).
factorize_case(cyclic_double,	T)		:- T = a(T,T).
factorize_case(cyclic_mutual,	T)		:- A = a(T), T = a(A).
factorize_case(cyclic_shared,	x(A,A))		:- A = a(A).
factorize_case(cyclic_nested,	x(b(A),A))	:- A = a(A).
factorize_case(bisimilar,	p(A,B))		:- A = f(A), B = f(f(B)).
factorize_case(multi_root,	[A,A])		:- A = f(A).
factorize_case(hydra,		T)		:- hydra(6, T).
factorize_case(fumo,		T)		:- fumo(20, T).

%	hydra(+N, -T) is a cyclic term of N distinct cells, every one of
%	which denotes the same infinite tree h(h(h(...))).  It is the case
%	that separates the cost of the algorithm from the size of the
%	unfolding: the tree has 2**K nodes at depth K.

hydra(N, T) :-
	length(Vs, N),
	Vs = [T|_],
	append(Vs, [T], Chain),
	Chain = [_|Tail],
	maplist(hydra_link, Vs, Tail).

hydra_link(h(X,X), X).

%	forall/1 asserts the bindings of its goal and a cyclic term cannot be
%	asserted, so the tests iterate over names and rebuild the term.

factorize_case_name(Name) :-
	factorize_case(Name, _).

subst_values([], []).
subst_values([_=V|T0], [V|T]) :-
	subst_values(T0, T).

:- begin_tests(factorize_contract, [sto(rational_trees)]).

%	Binding the substitutions must reproduce the term.

test(roundtrip, [forall(factorize_case_name(Name))]) :-
	factorize_case(Name, Term),
	\+ \+ ( term_factorized(Term, Skeleton, Subst),
		maplist(call, Subst),
		Term == Skeleton ).

%	No two substitutions may have equivalent values; if they did the
%	result would not be minimal.

test(minimal, [forall(factorize_case_name(Name))]) :-
	factorize_case(Name, Term),
	term_factorized(Term, _, Subst),
	subst_values(Subst, Values),
	\+ ( append(_, [V|Rest], Values),
	     member(W, Rest),
	     V == W
	   ).

%	Terms denoting the same rational tree must factorize to the same
%	thing, however many cells the input used.  This is what makes the
%	result a canonical form.

test(quotient_hydra, true(N == 1)) :-
	hydra(7, T),
	term_factorized(T, _, Subst),
	length(Subst, N).

test(quotient_bisimilar, true(S == f(V,V))) :-
	A = f(A), B = f(f(B)),
	term_factorized(f(A,B), S, [V=_]).

%	Two variant terms must give variant results.  solution_sequences:
%	trieable/2 and nb_set:key_hash/2 both use Skeleton+Substitution as a
%	key for =@=, so without this they see variants as distinct.
%
%	The substitutions come out in canonical order, which is what makes
%	this hold.  Ordering them by the standard order of the values does
%	not: comparing two variables compares their addresses, so two
%	variants whose variables were created in the opposite order came
%	out in the opposite order.

test(canonical) :-
	canonical_pair(T1, T2),
	term_factorized(T1, S1, E1),
	term_factorized(T2, S2, E2),
	S1-E1 =@= S2-E2.

%	The user visible consequence: add_nb_set/3 promises to reject a
%	variant of a key already in the set.  For a cyclic key nb_set falls
%	back on term_factorized/3, so this used to fail.

test(nb_set_variant) :-
	canonical_pair(T1, T2),
	empty_nb_set(Set),
	add_nb_set(T1, Set, _),
	add_nb_set(T2, Set, New),
	New == false.

%	A cyclic term (so nb_set must go through term_factorized/3) holding
%	two shared subterms that differ only in which variable they carry.

canonical_pair(T1, T2) :-
	mk_pair_a(T1),
	mk_pair_b(T2),
	assertion(T1 =@= T2).

mk_pair_a(c(S,X,X,Y,Y)) :- S = s(S), X = g(_), Y = g(_).
mk_pair_b(c(S,X,X,Y,Y)) :- S = s(S), Y = g(_), X = g(_).

:- end_tests(factorize_contract).
