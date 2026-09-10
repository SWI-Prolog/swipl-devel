/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org
    Copyright (c)  2026, SWI-Prolog Solutions b.v.
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
:- module(test_bisim,
	  [ test_bisim/0,
	    oracle_classes/2,			% +Term, -Classes
	    oracle_names/2			% +Term, -Names
	  ]).
:- use_module(library(plunit)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(yall)).

test_bisim :-
	run_tests([ bisim,
		    automaton
		  ]).

/** <module> Test term_minimal/2

term_minimal/2 collapses a term graph under bisimulation: two cells are the
same node exactly when they denote the same (possibly infinite) tree, which
is the equivalence ==/2 already decides.

oracle_classes/2 below computes the same partition in the most obvious way
that works, so that the real implementation has something independent to be
checked against.  It is quadratic in the number of cells and cubic overall,
and that is fine: it is only ever run on the small terms in this file.
*/

		 /*******************************
		 *	     THE ORACLE		*
		 *******************************/

%!	oracle_classes(+Term, -Classes) is det.
%
%	Classes is the number of equivalence classes of the term graph of
%	Term under bisimulation.  Colour every cell by its functor, then
%	split any class whose members send some argument to cells in
%	different classes, until nothing splits any more.  Starting from
%	the coarsest partition the colours allow and only ever splitting
%	when forced makes the result the coarsest bisimulation.

oracle_classes(Term, Classes) :-
	oracle_partition(Term, _, _, Classes).

%!	oracle_partition(+Term, -Cells, -ClassOf, -Classes) is det.
%
%	Cells is the term graph, ClassOf gives the class of each cell in
%	the same order, and Classes is how many classes there are.

oracle_partition(Term, Cells, ClassOf, Classes) :-
	cells(Term, Cells),
	maplist(colour(Cells), Cells, Colours),
	distinct_count(Colours, N0),
	refine(Cells, Colours, N0, ClassOf, Classes).

%!	oracle_names(+Term, -Names) is det.
%
%	How many classes are used more than once, the root counting as a
%	use.  That is how many equations term_factorized/3 must produce:
%	a class used once is written where it is used, one used twice has
%	to be given a name, and naming them is also what keeps the
%	skeleton finite.
%
%	Uses are counted on the quotient, not on the cells.  The answer
%	holds one cell per class, so however many cells a class had, it
%	refers to each of its arguments once.

oracle_names(Term, Names) :-
	oracle_partition(Term, Cells, ClassOf, Classes),
	numlist(1, Classes, Ids),
	maplist(class_rep(Cells, ClassOf), Ids, Reps),
	nth1_same(Cells, Term, 1, R),
	nth1(R, ClassOf, RootClass),
	findall(C,
		( member(Rep, Reps),
		  compound_name_arity(Rep, _, Arity),
		  between(1, Arity, A),
		  arg(A, Rep, Arg),
		  compound(Arg),
		  nth1_same(Cells, Arg, 1, J),
		  nth1(J, ClassOf, C)
		),
		Used),
	msort([RootClass|Used], Sorted),
	shared_count(Sorted, Names).

class_rep(Cells, ClassOf, Class, Rep) :-
	nth1(I, ClassOf, Class),
	!,
	nth1(I, Cells, Rep).

shared_count([], 0).
shared_count([H|T], N) :-
	span(H, T, Rest, Count),
	shared_count(Rest, N0),
	(   Count > 1
	->  N is N0+1
	;   N = N0
	).

span(H, [H1|T], Rest, N) :-
	H == H1,
	!,
	span(H, T, Rest, N0),
	N is N0+1.
span(_, Rest, Rest, 1).

refine(Cells, Classes0, N0, ClassOf, Classes) :-
	maplist(signature(Cells, Classes0), Cells, Signatures),
	renumber(Signatures, Classes1),
	distinct_count(Classes1, N1),
	(   N1 =:= N0			% the partition did not change: a
	->  ClassOf = Classes1,		% finer partition with as many
	    Classes = N0		% classes is the same partition.
	;   refine(Cells, Classes1, N1, ClassOf, Classes)	% Classes1 is
	).				% used, as it is numbered 1..Classes

%	The signature carries the cell's current class, so each round can
%	only refine the previous partition, never cut across it.

signature(Cells, Classes, Cell, Class-Args) :-
	nth1_same(Cells, Cell, 1, I),
	nth1(I, Classes, Class),
	compound_name_arity(Cell, _, Arity),
	findall(D,
		( between(1, Arity, A),
		  arg(A, Cell, Arg),
		  describe(Cells, Classes, Arg, D)
		),
		Args).

%	A compound argument is described by the class it is in; anything
%	else by itself, so two cells are equivalent only when their
%	arguments are pairwise equivalent.

describe(Cells, Classes, Arg, class(Class)) :-
	compound(Arg),
	!,
	nth1_same(Cells, Arg, 1, I),
	nth1(I, Classes, Class).
describe(_, _, Arg, leaf(Arg)).

colour(Cells, Cell, Colour) :-
	compound_name_arity(Cell, Name, Arity),
	nth1_functor(Cells, Name, Arity, 1, Colour).

renumber(Signatures, Classes) :-
	sort(0, @<, Signatures, Distinct),
	maplist(nth1_eq(Distinct), Signatures, Classes).

distinct_count(List, Count) :-
	sort(0, @<, List, Distinct),
	length(Distinct, Count).

%!	cells(+Term, -Cells) is det.
%
%	Cells is the list of physically distinct compound cells of Term,
%	i.e. its term graph.  Identity is same_term/2, which is what makes
%	this terminate on a cyclic term.

cells(Term, Cells) :-
	cells(Term, [], Cells).

cells(Term, Cells, Cells) :-
	\+ compound(Term),
	!.
cells(Term, Cells0, Cells) :-
	nth1_same(Cells0, Term, 1, _),
	!,
	Cells = Cells0.
cells(Term, Cells0, Cells) :-
	compound_name_arity(Term, _, Arity),
	cell_args(1, Arity, Term, [Term|Cells0], Cells).

cell_args(I, Arity, _, Cells, Cells) :-
	I > Arity,
	!.
cell_args(I, Arity, Term, Cells0, Cells) :-
	arg(I, Term, Arg),
	cells(Arg, Cells0, Cells1),
	I2 is I+1,
	cell_args(I2, Arity, Term, Cells1, Cells).

nth1_same([X|_], Term, I, I) :-
	same_term(X, Term),
	!.
nth1_same([_|T], Term, I0, I) :-
	I1 is I0+1,
	nth1_same(T, Term, I1, I).

nth1_eq(Distinct, Signature, I) :-
	nth1_eq_(Distinct, Signature, 1, I).

nth1_eq_([X|_], Signature, I, I) :-
	X == Signature,
	!.
nth1_eq_([_|T], Signature, I0, I) :-
	I1 is I0+1,
	nth1_eq_(T, Signature, I1, I).

nth1_functor([C|_], Name, Arity, I, I) :-
	compound_name_arity(C, Name, Arity),
	!.
nth1_functor([_|T], Name, Arity, I0, I) :-
	I1 is I0+1,
	nth1_functor(T, Name, Arity, I1, I).

%!	bisim_case(-Name, -Term) is nondet.
%
%	Corpus checked against the oracle.  forall/1 asserts the bindings of
%	its goal and a cyclic term cannot be asserted, so the test iterates
%	over names and rebuilds the term.

bisim_case(flat,	   f(a,b)).
bisim_case(nested,	   f(g(a))).
bisim_case(equal_unshared, f(g(a),g(a))).
bisim_case(shared,	   f(X,X))	   :- X = g(a).
bisim_case(list,	   [a,b]).
bisim_case(list_repeat,	   [a,a]).
bisim_case(list_shared,	   p(L,L))	   :- L = [a,b].
bisim_case(list_unshared,  p([a,b],[a,b])).
bisim_case(zero_arity,	   g(f(),f())).
bisim_case(string,	   g("abc","abc")).
bisim_case(vars_distinct,  f(_,_)).
bisim_case(vars_same,	   f(X,X)).
bisim_case(attvar,	   g(V,V))	   :- freeze(V, true).
bisim_case(cycle,	   T)		   :- T = f(T).
bisim_case(cycle_double,   T)		   :- T = f(T,T).
bisim_case(cycle_mutual,   T)		   :- A = f(T), T = f(A).
bisim_case(bisimilar,	   p(X,Y))	   :- X = f(X), Y = f(f(Y)).
bisim_case(not_bisimilar,  p(X,Y))	   :- X = f(X), Y = g(Y).
bisim_case(cyclic_list,	   T)		   :- T = [a|T].
bisim_case(cyclic_list2,   T)		   :- T = [a,a|T].
bisim_case(hydra,	   T)		   :- hydra(6, T).
bisim_case(deep,	   T)		   :- chain(30, T).

bisim_case_name(Name) :-
	bisim_case(Name, _).

chain(0, x) :- !.
chain(N, h(T)) :-
	M is N-1,
	chain(M, T).

hydra(N, T) :-
	length(Vs, N),
	Vs = [T|_],
	append(Vs, [T], Chain),
	Chain = [_|Tail],
	maplist(hydra_link, Vs, Tail).

hydra_link(h(X,X), X).

%!	automaton_shape(+Automaton, -Nodes, -Sinks) is det.
%
%	How many states of Automaton have successors and how many are
%	sinks.  A state is a sink exactly when its colour is not a
%	compound.

automaton_shape(A, Nodes, Sinks) :-
	compound_name_arguments(A, _, States),
	partition([S]>>compound(S), States, Ns, Ss),
	length(Ns, Nodes),
	length(Ss, Sinks).

%!	term_graph_size(+Term, -Cells) is det.
%
%	Cells is the number of physically distinct compound cells of Term,
%	i.e. the size of its term graph rather than of its unfolding.  A
%	term of N cells may denote a tree of 2**N nodes, or an infinite
%	one.  term_automaton/2 is faithful, giving one state per cell and
%	one per distinct leaf, so the cells are the states that are not
%	sinks.

term_graph_size(Term, Cells) :-
	term_automaton(Term, A),
	automaton_shape(A, Cells, _).

%!	term_graph_classes(+Term, -Classes) is det.
%
%	Classes is the number of cells the graph of Term has after
%	collapsing the ones that denote the same tree.  It is the number
%	of cells term_minimal/2 produces.  Every cell of a term is
%	reachable from its root, so automaton_minimal/2 drops none of
%	them here and only the collapsing is left.

term_graph_classes(Term, Classes) :-
	term_automaton(Term, A),
	automaton_minimal(A, M),
	automaton_shape(M, Classes, _).

:- begin_tests(bisim, [sto(rational_trees)]).

%	First check the oracle itself against cases whose answer can be
%	counted by hand.

test(oracle_flat,	 true(N == 1)) :- oracle_classes(f(a,b), N).
test(oracle_nested,	 true(N == 2)) :- oracle_classes(f(g(a)), N).
test(oracle_unshared,	 true(N == 2)) :- oracle_classes(f(g(a),g(a)), N).
test(oracle_list,	 true(N == 2)) :- oracle_classes([a,b], N).
test(oracle_cycle,	 true(N == 1)) :- X = f(X), oracle_classes(X, N).
test(oracle_bisimilar,	 true(N == 2)) :- X = f(X), Y = f(f(Y)),
					  oracle_classes(p(X,Y), N).
test(oracle_hydra,	 true(N == 1)) :- hydra(6, T), oracle_classes(T, N).
test(oracle_distinct,	 true(N == 3)) :- X = f(X), Y = g(Y),
					  oracle_classes(p(X,Y), N).

%	Then check the implementation against it.

test(quotient, [forall(bisim_case_name(Name))]) :-
	bisim_case(Name, Term),
	term_graph_classes(Term, Classes),
	oracle_classes(Term, Classes).

%	The cells of a term are its graph, not its unfolding: a hydra of N
%	cells denotes a tree with 2**K nodes at depth K, and collapses to
%	one cell because every one of them denotes the same tree.

test(hydra_size,    true(N == 20)) :- hydra(20, T), term_graph_size(T, N).
test(hydra_quotient, true(N == 1)) :- hydra(20, T), term_graph_classes(T, N).

%	The example the variant_sha1/2 documentation gives for what a
%	canonical cycle has to do: [a|A] and [a,a|B] are the same tree.

test(canonical_cycle, true(N1-N2 == 1-1)) :-
	A = [a|A],
	B = [a,a|B],
	term_graph_classes(A, N1),
	term_graph_classes(B, N2).

test(walk_leaves_term_intact, true(T == Copy)) :-
	hydra(8, T),
	copy_term(T, Copy),
	term_graph_classes(T, _),
	garbage_collect.

		 /*******************************
		 *	   term_minimal/2	*
		 *******************************/

%	The contract: the answer is ==, and it uses one cell per class.

test(minimal_equal, [forall(bisim_case_name(Name))]) :-
	bisim_case(Name, Term),
	term_minimal(Term, Minimal),
	Minimal == Term.

test(minimal_size, [forall(bisim_case_name(Name))]) :-
	bisim_case(Name, Term),
	term_graph_classes(Term, Classes),
	term_minimal(Term, Minimal),
	term_graph_size(Minimal, Classes).

%	And it is a fixpoint: nothing is left to share.

test(minimal_idempotent, [forall(bisim_case_name(Name))]) :-
	bisim_case(Name, Term),
	term_minimal(Term, M1),
	term_minimal(M1, M2),
	term_graph_size(M1, Size),
	term_graph_size(M2, Size).

test(minimal_hydra, true(Size-Eq == 1-true)) :-
	hydra(20, T),
	term_minimal(T, M),
	term_graph_size(M, Size),
	( M == T -> Eq = true ; Eq = false ).

test(minimal_survives_gc, true(M == T)) :-
	hydra(12, T),
	term_minimal(T, M),
	garbage_collect.

test(minimal_atomic, true(M == a)) :-
	term_minimal(a, M).

		 /*******************************
		 *	  CANONICAL FORM	*
		 *******************************/

%	The quotient is unique, its numbering is not.  The form must depend
%	on nothing but the shape of the quotient, which is what makes it a
%	key for =@=.

test(canonical_variant, [forall(bisim_case_name(Name))]) :-
	bisim_case(Name, Term),
	copy_term(Term, Copy),
	'$term_canonical_form'(Term, Form),
	'$term_canonical_form'(Copy, Form2),
	Form == Form2.

test(canonical_ground, [forall(bisim_case_name(Name))]) :-
	bisim_case(Name, Term),
	'$term_canonical_form'(Term, Form),
	ground(Form),
	acyclic_term(Form).

%	Terms denoting the same tree get the same form however many cells
%	they were written with.  This is the example variant_sha1/2 gives
%	for what it cannot do.

test(canonical_cycle_same, true(F1 == F2)) :-
	A = [a|A],
	B = [a,a|B],
	'$term_canonical_form'(A, F1),
	'$term_canonical_form'(B, F2).

test(canonical_hydra, true(F == [h(1,1)])) :-
	hydra(9, T),
	'$term_canonical_form'(T, F).

test(canonical_shared, true(F == [f(2,2),g(a)])) :-
	'$term_canonical_form'(f(g(a),g(a)), F).

%	Distinct variables stay distinct and are numbered by where they are
%	first reached, not by when they were created.  term_factorized/3
%	orders its substitutions by the standard order of the values, and
%	comparing two variables compares their addresses, which is exactly
%	what leaks there.

test(canonical_vars_distinct, true(F1 \== F2)) :-
	'$term_canonical_form'(f(X,X), F1),
	'$term_canonical_form'(f(_,_), F2),
	X = X.

test(canonical_variable_age, true(F1 == F2)) :-
	var_age_a(T1),
	var_age_b(T2),
	'$term_canonical_form'(T1, F1),
	'$term_canonical_form'(T2, F2).

var_age_a(c(S,X,X,Y,Y)) :- S = s(S), X = g(_), Y = g(_).
var_age_b(c(S,X,X,Y,Y)) :- S = s(S), Y = g(_), X = g(_).

		 /*******************************
		 *	  term_factorized/3	*
		 *******************************/

%	One equation per class that is used more than once, counting the
%	root as a use, and nothing else.

test(factorized_names, [forall(bisim_case_name(Name))]) :-
	bisim_case(Name, Term),
	oracle_names(Term, Names),
	term_factorized(Term, _, Subst),
	length(Subst, Names).

test(factorized_roundtrip, [forall(bisim_case_name(Name))]) :-
	bisim_case(Name, Term),
	\+ \+ ( term_factorized(Term, Skeleton, Subst),
		maplist(call, Subst),
		Term == Skeleton ).

%	The skeleton has to be finite, which is what naming every class
%	used more than once buys: a cycle reachable from the root either
%	contains the root or is entered from outside, and either way its
%	entry class has a second use.

test(factorized_acyclic, [forall(bisim_case_name(Name))]) :-
	bisim_case(Name, Term),
	term_factorized(Term, Skeleton, Subst),
	acyclic_term(Skeleton),
	maplist([_=V]>>acyclic_term(V), Subst).

		 /*******************************
		 *	  term_factorized/4	*
		 *******************************/

%	minimal(false) shares only what is physically shared, which is what
%	'$factorize_term'/3 does.  The two agree wherever the skeleton is
%	comparable; they differ in the order of the substitutions, as this
%	one is canonical.

test(minimal_false_unshared, true(S-E == f(g(a),g(a))-[])) :-
	term_factorized(f(g(a),g(a)), S, E, [minimal(false)]).
test(minimal_true_unshared, true(N == 1)) :-
	term_factorized(f(g(a),g(a)), _, E, [minimal(true)]),
	length(E, N).
test(minimal_false_shared, true(N == 1)) :-
	X = g(a),
	term_factorized(f(X,X), _, E, [minimal(false)]),
	length(E, N).

%	Two graphs denoting the same tree: the quotient makes them one,
%	physical sharing keeps them apart.

test(minimal_bisimilar, true(N1-N2 == 1-2)) :-
	A = f(A), B = f(f(B)),
	term_factorized(p(A,B), _, E1, [minimal(true)]),
	term_factorized(p(A,B), _, E2, [minimal(false)]),
	length(E1, N1),
	length(E2, N2).

%	Either way, binding the substitutions has to give the term back.

test(minimal_false_roundtrip, [forall(bisim_case_name(Name))]) :-
	bisim_case(Name, Term),
	\+ \+ ( term_factorized(Term, Skeleton, Subst, [minimal(false)]),
		maplist(call, Subst),
		Term == Skeleton ).

%	'$VAR'/1 stands for a variable, so it is left alone unless asked.

test(dollar_var_default, true(E == [])) :-
	A = '$VAR'(1), B = '$VAR'(1),
	term_factorized(g(A,B), _, E).
test(dollar_var_false, true(N == 1)) :-
	A = '$VAR'(1), B = '$VAR'(1),
	term_factorized(g(A,B), _, E, [dollar_var(false)]),
	length(E, N).

test(bad_option, [error(type_error(bool, maybe))]) :-
	term_factorized(a, _, _, [minimal(maybe)]).

:- end_tests(bisim).


		 /*******************************
		 *	     THE AUTOMATON	*
		 *******************************/

%!	oracle_leaves(+Term, -Leaves) is det.
%
%	How many distinct leaves the term graph of Term has.  A leaf is an
%	argument of a cell that is not a compound, and two of them are the
%	same when they are ==, which for the standard order is what @< calls
%	equal.  That is how many sink states term_automaton/2 must produce.

oracle_leaves(Term, Leaves) :-
	cells(Term, Cells),
	maplist(cell_leaves, Cells, Lss),
	append(Lss, Ls),
	distinct_count(Ls, Leaves).

cell_leaves(Cell, Leaves) :-
	compound_name_arguments(Cell, _, Args),
	include([A]>>(\+ compound(A)), Args, Leaves).

%!	state_term(+Automaton, +State, -Term) is det.
%
%	The term State denotes, read back through the API itself: prepend a
%	state that does nothing but point at State, and ask for the term of
%	the automaton that starts there.  It gives a way to check the class
%	map that does not depend on how the states are numbered.

state_term(A, I, Term) :-
	compound_name_arguments(A, _, States),
	maplist(shift_state, States, Shifted),
	I1 is I+1,
	compound_name_arguments(A1, automaton, [s(I1)|Shifted]),
	term_automaton(T, A1),
	T = s(Term).

shift_state(S, S1) :-
	(   compound(S)
	->  compound_name_arguments(S, F, Args),
	    maplist(succ, Args, Args1),
	    compound_name_arguments(S1, F, Args1)
	;   S1 = S
	).

:- begin_tests(automaton, [sto(rational_trees)]).

%	term_automaton/2 is faithful in both directions: one state per
%	physically distinct cell, one per distinct leaf, nothing collapsed.

test(roundtrip, [forall(bisim_case_name(Name))]) :-
	bisim_case(Name, Term),
	term_automaton(Term, A),
	term_automaton(Term2, A),
	assertion(Term2 == Term).

test(shape, [forall(bisim_case_name(Name))]) :-
	bisim_case(Name, Term),
	term_automaton(Term, A),
	automaton_shape(A, Nodes, Sinks),
	cells(Term, Cells),
	length(Cells, NCells),
	oracle_leaves(Term, NLeaves),
	assertion(Nodes == NCells),
	assertion(Sinks == NLeaves).

test(leaf_term, true(A == automaton(a))) :-
	term_automaton(a, A).
test(leaf_back, true(T == a)) :-
	term_automaton(T, automaton(a)).
test(var_term, [true(A == automaton(V))]) :-
	term_automaton(V, A).
test(var_in_term, [true(T == f(V,V))]) :-
	term_automaton(f(V,V), A),
	term_automaton(T, A).

%	Minimising the automaton gives the same partition the oracle does,
%	and the same term term_minimal/2 does.  Every cell of a term is
%	reachable from its root, so nothing is dropped here.

test(classes, [forall(bisim_case_name(Name))]) :-
	bisim_case(Name, Term),
	term_automaton(Term, A),
	automaton_minimal(A, M),
	automaton_shape(M, Nodes, _),
	oracle_classes(Term, Classes),
	assertion(Nodes == Classes).

test(minimal_term, [forall(bisim_case_name(Name))]) :-
	bisim_case(Name, Term),
	term_automaton(Term, A),
	automaton_minimal(A, M),
	term_automaton(Term2, M),
	assertion(Term2 == Term),
	term_graph_classes(Term, Classes),
	term_graph_size(Term2, Cells),
	assertion(Cells == Classes).

test(idempotent, [forall(bisim_case_name(Name))]) :-
	bisim_case(Name, Term),
	term_automaton(Term, A),
	automaton_minimal(A, M),
	automaton_minimal(M, M2),
	assertion(M2 == M).

%	Two states collapse into one exactly when the terms they denote are
%	the same, which state_term/3 reads back without knowing anything
%	about the numbering.

test(map, [forall(bisim_case_name(Name))]) :-
	bisim_case(Name, Term),
	term_automaton(Term, A),
	automaton_minimal(A, _, Map),
	functor(A, _, N),
	forall(( between(1, N, I), between(1, N, J) ),
	       ( arg(I, Map, MI),
		 arg(J, Map, MJ),
		 state_term(A, I, TI),
		 state_term(A, J, TJ),
		 assertion(( MI == MJ
			   ->  TI == TJ
			   ;   TI \== TJ
			   ))
	       )).

%	The numbering falls out of a walk from the start state, so two
%	automata that denote the same tree minimise to the same automaton.

test(canonical) :-
	X = f(X),
	Y = f(f(Y)),
	term_automaton(X, AX),
	term_automaton(Y, AY),
	automaton_minimal(AX, MX),
	automaton_minimal(AY, MY),
	assertion(MX == MY).

test(canonical_variant) :-
	term_automaton(f(g(A),g(B),A,B), A1),
	term_automaton(f(g(C),g(D),C,D), A2),
	automaton_minimal(A1, M1),
	automaton_minimal(A2, M2),
	assertion(M1 =@= M2).

%	Automata that no term can produce.

test(unreachable, true(M == automaton(f(2,2),g(1)))) :-
	automaton_minimal(automaton(f(2,3),g(1),g(1),a,h(4)), M).
test(unreachable_map, true(Map == map(1,2,2,0,0))) :-
	automaton_minimal(automaton(f(2,3),g(1),g(1),a,h(4)), _, Map).
test(cycles_of_different_length, true(M == automaton(f(1)))) :-
	automaton_minimal(automaton(f(2),f(3),f(1)), M).
test(not_bisimilar, true(M == automaton(f(2,3),g(1),h(1)))) :-
	automaton_minimal(automaton(f(2,3),g(1),h(1)), M).
test(sink_only, true(M-Map == automaton(a)-map(1,0,1))) :-
	automaton_minimal(automaton(a,b,a), M, Map).
test(sink_start_term, true(T == a)) :-
	term_automaton(T, automaton(a,f(1))).
test(disconnected, true(M == automaton(f(1)))) :-
	automaton_minimal(automaton(f(1),g(2)), M).
test(out_of_order, true(M == automaton(f(2,2),g(3),a))) :-
	automaton_minimal(automaton(f(3,3),a,g(2)), M).
test(zero_arity, true(M == automaton(g(2,2),f()))) :-
	automaton_minimal(automaton(g(2,3),f(),f()), M).

%	Sinks are states rather than arguments, so there can be far more
%	leaves than edges.  That is what the graph has to be sized for.

test(many_sinks, true(M == automaton(f(2),1))) :-
	numlist(1, 1000, L),
	A =.. [automaton, f(2)|L],
	automaton_minimal(A, M).

test(deep, true(Term2 == Term)) :-
	chain(5000, Term),
	term_automaton(Term, A),
	automaton_minimal(A, M),
	term_automaton(Term2, M).

test(survives_gc, true(Term2 == Term)) :-
	bisim_case(hydra, Term),
	term_automaton(Term, A),
	garbage_collect,
	automaton_minimal(A, M),
	garbage_collect,
	term_automaton(Term2, M).

%	Errors.  A successor names a state, so a number outside the
%	automaton is a state that does not exist rather than a bad type.

test(no_state, [error(existence_error(state, 2))]) :-
	automaton_minimal(automaton(f(2)), _).
test(state_zero, [error(existence_error(state, 0))]) :-
	automaton_minimal(automaton(f(0)), _).
test(state_not_integer, [error(type_error(integer, a))]) :-
	automaton_minimal(automaton(f(a)), _).
test(state_unbound, [error(instantiation_error)]) :-
	automaton_minimal(automaton(f(_)), _).
test(not_compound, [error(type_error(compound, foo))]) :-
	automaton_minimal(foo, _).
test(no_automaton, [error(instantiation_error)]) :-
	automaton_minimal(_, _).
test(bad_state_in_term_automaton, [error(existence_error(state, 9))]) :-
	term_automaton(_, automaton(f(9))).

:- end_tests(automaton).

