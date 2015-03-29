/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2015, VU University Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(solution_sequences,
	  [ distinct/1,			% :Goal
	    distinct/2,			% ?Witness, :Goal
	    limit/2,			% +Limit, :Goal
	    offset/2,			% +Offset, :Goal
	    order_by/2,			% +Spec, :Goal
	    group_by/4			% +By, +Template, :Goal, -Bag
	  ]).
:- use_module(library(nb_set)).
:- use_module(library(error)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).

/** <module> Modify solution sequences

The meta predicates of this library modify  the sequence of solutions of
a goal. The modifications and  the  predicate   names  are  based on the
classical database operations DISTINCT,  LIMIT,   OFFSET,  ORDER  BY and
GROUP BY.

These   predicates   were   introduced   in     the   context   of   the
[SWISH](http://swish.swi-prolog.org) Prolog browser-based   shell, which
can represent the solutions to a predicate  as a table. Notably wrapping
a goal in distinct/1 avoids duplicates  in   the  result table and using
order_by/2 produces a nicely ordered table.

However, the predicates from this  library  can   also  be  used to stay
longer within the clean paradigm  where non-deterministic predicates are
composed  from  simpler  non-deterministic  predicates    by   means  of
conjunction and disjunction. While evaluating   a  conjunction, we might
want to eliminate duplicates of the first part of the conjunction. Below
we give both the classical  solution   for  solving variations of (a(X),
b(X)) and the ones using this library side-by-side.

  $ Avoid duplicates of earlier steps :

    ==
      setof(X, a(X), Xs),		distinct(a(X)),
      member(X, Xs),			b(X)
      b(X).
    ==

    Note that the distinct/1 based solution returns the first result
    of distinct(a(X)) immediately after a/1 produces a result, while
    the setof/3 based solution will first compute all results of a/1.

  $ Only try b(X) only for the top-10 a(X) :

    ==
      setof(X, a(X), Xs),		limit(10, order_by([desc(X)], a(X))),
      reverse(Xs, Desc),		b(X)
      first_max_n(10, Desc, Limit),
      member(X, Limit),
      b(X)
    ==

    Here we see power of composing primitives from this library and
    staying within the paradigm of pure non-deterministic relational
    predicates.

@see all solution predicates findall/3, bagof/3 and setof/3.
@see library(aggregate)
*/

:- meta_predicate
	distinct(0),
	distinct(?, 0),
	limit(+, 0),
	offset(+, 0),
	order_by(+, 0),
	group_by(?, ?, 0, -).

%%	distinct(:Goal).
%%	distinct(?Witness, :Goal).
%
%	True if Goal is true and  no   previous  solution  of Goal bound
%	Witness to the same value. The  variant distinct/1 is equivalent
%	to distinct(Goal,Goal). Semantically, distinct/1 is  the same as
%	the code below, but answers are returned  as soon as they become
%	available rather than first computing the complete answer set.
%
%	  ==
%	  distinct(Goal) :-
%	      findall(Goal, Goal, List),
%	      list_to_set(List, Set),
%	      member(Goal, Set).
%	  ==

distinct(Goal) :-
	distinct(Goal, Goal).
distinct(Witness, Goal) :-
	term_variables(Witness, Vars),
	Witness1 =.. [v|Vars],
	empty_nb_set(Set),
	call(Goal),
	add_nb_set(Witness1, Set, true).

%%	limit(+Count, :Goal)
%
%	Limit the number of solutions. True   if Goal is true, returning
%	at most Count solutions. Solutions are  returned as soon as they
%	become  available.

limit(Count, Goal) :-
	Count > 0,
	State = count(0),
	call(Goal),
	arg(1, State, N0),
	N is N0+1,
	(   N =:= Count
	->  !
	;   nb_setarg(1, State, N)
	).

%%	offset(+Count, :Goal)
%
%	Ignore the first Count  solutions.  True   if  Goal  is true and
%	produces more than Count solutions.  This predicate computes and
%	ignores the first Count solutions.

offset(Count, Goal) :-
	Count > 0, !,
	State = count(0),
	call(Goal),
	arg(1, State, N0),
	(   N0 >= Count
	->  true
	;   N is N0+1,
	    nb_setarg(1, State, N),
	    fail
	).
offset(Count, Goal) :-
	Count =:= 0, !,
	call(Goal).
offset(Count, _) :-
	domain_error(not_less_than_zero, Count).

%%	order_by(Spec, Goal)
%
%	Order solutions according to Spec.  Spec   is  a  list of terms,
%	where each element is one of. The  ordering of solutions of Goal
%	that only differ in variables that are _not_ shared with Spec is
%	not changed.
%
%	  - asc(Term)
%	  Order solution according to ascending Term
%	  - desc(Term)
%	  Order solution according to descending Term

order_by(Spec, Goal) :-
	must_be(list, Spec),
	non_empty_list(Spec),
	maplist(order_witness, Spec, Witnesses0),
	join_orders(Witnesses0, Witnesses),
	non_witness_template(Goal, Witnesses, Others),
	reverse(Witnesses, RevWitnesses),
	maplist(x_vars, RevWitnesses, WitnessVars),
	Template =.. [v,Others|WitnessVars],
	findall(Template, Goal, Results),
	order(RevWitnesses, 2, Results, OrderedResults),
	member(Template, OrderedResults).

order([], _, Results, Results).
order([H|T], N, Results0, Results) :-
	order1(H, N, Results0, Results1),
	N2 is N + 1,
	order(T, N2, Results1, Results).

order1(asc(_), N, Results0, Results) :-
	sort(N, @=<, Results0, Results).
order1(desc(_), N, Results0, Results) :-
	sort(N, @>=, Results0, Results).

non_empty_list([]) :- !,
	domain_error(non_empty_list, []).
non_empty_list(_).

order_witness(Var, _) :-
	var(Var), !,
	instantiation_error(Var).
order_witness(asc(Term), asc(Witness)) :- !,
	witness(Term, Witness).
order_witness(desc(Term), desc(Witness)) :- !,
	witness(Term, Witness).
order_witness(Term, _) :-
	domain_error(order_specifier, Term).

x_vars(asc(Vars), Vars).
x_vars(desc(Vars), Vars).

witness(Term, Witness) :-
	term_variables(Term, Vars),
	Witness =.. [v|Vars].

%%	join_orders(+SpecIn, -SpecOut) is det.
%
%	Merge  subsequent  asc  and   desc    sequences.   For  example,
%	[asc(v(A)), asc(v(B))] becomes [asc(v(A,B))].

join_orders([], []).
join_orders([asc(O1)|T0], [asc(O)|T]) :- !,
	ascs(T0, OL, T1),
	join_witnesses(O1, OL, O),
	join_orders(T1, T).
join_orders([desc(O1)|T0], [desc(O)|T]) :- !,
	descs(T0, OL, T1),
	join_witnesses(O1, OL, O),
	join_orders(T1, T).

ascs([asc(A)|T0], [A|AL], T) :- !,
	ascs(T0, AL, T).
ascs(L, [], L).

descs([desc(A)|T0], [A|AL], T) :- !,
	descs(T0, AL, T).
descs(L, [], L).

join_witnesses(O, [], O) :- !.
join_witnesses(O, OL, R) :-
	term_variables([O|OL], VL),
	R =.. [v|VL].

%%	non_witness_template(+Goal, +Witness, -Template) is det.
%
%	Create a template for the bindings  that   are  not  part of the
%	witness variables.

non_witness_template(Goal, Witness, Template) :-
	ordered_term_variables(Goal, AllVars),
	ordered_term_variables(Witness, WitnessVars),
	ord_subtract(AllVars, WitnessVars, TemplateVars),
	Template =.. [t|TemplateVars].

ordered_term_variables(Term, Vars) :-
	term_variables(Term, Vars0),
	sort(Vars0, Vars).

%%	group_by(+By, +Template, :Goal, -Bag) is nondet.
%
%	Group bindings of Template that have the same value for By. This
%	predicate  is  almost  the  same  as  bagof/3,  but  instead  of
%	specifying  the  existential  variables  we   specify  the  free
%	variables. It is provided for  consistency and complete coverage
%	of the common database vocabulary.

group_by(By, Template, Goal, Bag) :-
	ordered_term_variables(Goal, GVars),
	ordered_term_variables(By+Template, UVars),
	ord_subtract(GVars, UVars, ExVars),
	bagof(Template, ExVars^Goal, Bag).
