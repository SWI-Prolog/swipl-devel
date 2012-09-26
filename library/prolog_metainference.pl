/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 *
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 *
 * Authors: Eva Stöwe, Günter Kniesel and Jan Wielemaker
 *
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 ****************************************************************************/

:- module(prolog_metainference,
	  [ infer_meta_predicate/2,		% :Head, -MetaSpec
	    inferred_meta_predicate/2		% :Head, ?MetaSpec
	  ]).
:- use_module(library(lists)).

:- meta_predicate
	inferred_meta_predicate(:, ?),
	infer_meta_predicate(:, -).

:- dynamic
	inferred_meta_pred/3.			% Head, Module, Meta

/** <module> Infer meta-predicate properties

This module infers meta-predicate properties   by inspecting the clauses
of predicates that call other predicates.   This is extremely useful for
program analysis and refactoring because  many   programs  `in the wild'
have incomplete or incorrect meta-predicate information.

@see	This library is used by prolog_walk_code/1 to improve the
	accuracy of this analysis.
*/


%%	inferred_meta_predicate(:Head, ?MetaSpec) is nodet.
%
%	True when MetaSpec is an   inferred meta-predicate specification
%	for Head.

inferred_meta_predicate(M:Head, MetaSpec) :-
	inferred_meta_pred(Head, M, MetaSpec).
inferred_meta_predicate(M:Head, MetaSpec) :-
	predicate_property(M:Head, imported_from(From)),
	inferred_meta_pred(From, M, MetaSpec).


%%	infer_meta_predicate(:Head, -MetaSpec) is semidet
%
%	True  when  MetaSpec  is  a  meta-predicate  specifier  for  the
%	predicate Head. Derived meta-predicates are   collected and made
%	available through inferred_meta_predicate/2.
%
%	@tbd	Currently only infers ? and 0.

infer_meta_predicate(Head, MetaSpec) :-
	inferred_meta_predicate(Head, MetaSpec), !.
infer_meta_predicate(M:Head, MetaSpec) :-
	predicate_property(M:Head, imported_from(From)), !,
	do_infer_meta_predicate(From:Head, MetaSpec),
	assertz(inferred_meta_pred(Head, From, MetaSpec)).
infer_meta_predicate(M:Head, MetaSpec) :-
	do_infer_meta_predicate(M:Head, MetaSpec),
	assertz(inferred_meta_pred(Head, M, MetaSpec)).

:- meta_predicate
	do_infer_meta_predicate(:, -).

do_infer_meta_predicate(Module:AHead, MetaSpec):-
	functor(AHead, Functor, Arity),
	functor(Head, Functor, Arity),	% Generalise the head
	findall(MetaArgs,
		find_meta_pred_args_in_clause(Module, Head, MetaArgs),
		AllMetaArgs),
	(   AllMetaArgs = []
	->  fail
	;   combine_meta_args(AllMetaArgs,CombinedArgs),
	    MetaSpec =.. [Functor|CombinedArgs]
	).


find_meta_pred_args_in_clause(Module, Head, MetaArgs):-
	clause(Module:Head, Body), !,	% JW: Seems wrong?
	find_meta_vars_in_body(Body, Module, [],  MetaVars),
	find_meta_vars_in_head(Head, MetaVars, MetaArgs).


/**
 * find_meta_vars_in_body(+Term, +Context, +MetaVars, -MetaVars) is det
 *
 * Analyses the code of Term for calls to known meta_predicates (in the
 * module Context).
 * If such a meta-call is found, all terms that appear
 *  - as arguments of those meta-calls,
 *  - are unified / aliased to them,
 *  - are part of those terms,
 *  - or are connected to them via term-manupilation
 * previously in the code of Arg1, are stored in Arg4.
 * Arg3 helps as an accumulator of previously found arguments / terms.
 */

find_meta_vars_in_body(A, _, MetaVars, MetaVars) :-
	(   atomic(A)
	;   var(A)
	), !.
find_meta_vars_in_body(Module:Term, _, KnownMetaVars, MetaVars) :- !,
	find_meta_vars_in_body(Term, Module, KnownMetaVars, MetaVars).
find_meta_vars_in_body((Cond->Then;Else), Context, KnownMetaVars, MetaVars) :- !,
	find_meta_vars_in_body(Then, Context, KnownMetaVars, MetaVarsA),
	(   KnownMetaVars \= MetaVarsA
	->  find_meta_vars_in_body(Cond, Context, MetaVarsA, MetaVars)
	;   find_meta_vars_in_body(Else, Context, KnownMetaVars, MetaVars)
	).
find_meta_vars_in_body((TermA, TermB), Context, KnownMetaVars, MetaVars) :- !,
	find_meta_vars_in_body(TermB, Context, KnownMetaVars, MetaVarsB),
	find_meta_vars_in_body(TermA, Context, MetaVarsB, MetaVars).
find_meta_vars_in_body((TermA; TermB), Context, KnownMetaVars, MetaVars) :- !,
	find_meta_vars_in_body(TermB, Context, KnownMetaVars, MetaVarsB),
	find_meta_vars_in_body(TermA, Context, MetaVarsB, MetaVars).
find_meta_vars_in_body((TermA = TermB), _Context, KnownMetaVars, MetaVars) :- !,
	(   occurs_in(TermA, KnownMetaVars)
	->  add_var_to_set(TermB, KnownMetaVars, OwnMetaVars2)
	;   OwnMetaVars2 = KnownMetaVars
	),
	(   occurs_in(TermB, OwnMetaVars2)
	->  add_var_to_set(TermA, OwnMetaVars2, MetaVars3)
	;   MetaVars3 = OwnMetaVars2
	),
	check_inner_vars(TermA, TermB, MetaVars3, MetaVars).
find_meta_vars_in_body(functor(Term,Functor,_),
		       _Context, KnownMetaVars, MetaVars) :- !,
	(   occurs_in(Term,KnownMetaVars)
	->  add_var_to_set(Functor, KnownMetaVars, MetaVars)
	;   (   occurs_in(Functor,KnownMetaVars)
	    ->  add_var_to_set(Term, KnownMetaVars, MetaVars)
	    ;   MetaVars = KnownMetaVars
	    )
	).
find_meta_vars_in_body(atom_concat(A,B,C), _Context, KnownMetaVars, AllMeta) :- !,
	free_vars_of([A,B,C],Candidates),
	add_meta_vars(Candidates,KnownMetaVars,AllMeta).
find_meta_vars_in_body((Term =.. List), _Context, KnownMetaVars, MetaVars) :- !,
	(   occurs_in(Term,KnownMetaVars)
	->  add_var_to_set(List, KnownMetaVars, MetaVars1),
	    (	nonvar(List),
		List = [Functor|_]
	    ->	add_var_to_set(Functor, MetaVars1, MetaVars)
	    ;	MetaVars = MetaVars1
	    )
	;   occurs_in(List,KnownMetaVars)
	->  add_var_to_set(Term, KnownMetaVars, MetaVars)
	;   nonvar(List),
	    List = [Functor|_],
	    occurs_in(Functor, KnownMetaVars)
	->  add_var_to_set(Term, KnownMetaVars, MetaVars)
	;   MetaVars = KnownMetaVars
	).
find_meta_vars_in_body(arg(_,Term,Arg), _Context, KnownMetaVars, MetaVars) :- !,
	(   occurs_in(Term,KnownMetaVars)
	->  add_var_to_set(Arg, KnownMetaVars, MetaVars)
	;   occurs_in(Arg,KnownMetaVars)
	->  add_var_to_set(Term, KnownMetaVars, MetaVars)
	;   MetaVars = KnownMetaVars
	).


find_meta_vars_in_body(Term, Context, KnownMetaVars, MetaVars):-
	is_metaterm(Context, Term, MetaCombos), !,
	extract_vars(MetaCombos, MetaArgs),
	handel_meta_args(MetaArgs, Context, KnownMetaVars, MetaVars).
find_meta_vars_in_body(_Term, _Context, MetaVars, MetaVars).


/**
 * find_meta_vars_in_head(+Head, +MetaVars, ?MetaArgs) is det
 *
 * Succeeds if Arg1 is the head of a meta-predicate-clause and Arg2 all
 * possible bindings for meta-arguments used in the body in the clause.
 * In this case, Arg3 is bound to a list that represents the
 * meta-argument-binding of the arguments of the Clause.
 *
 * (Currently only working with ? and 0, but should work for each
 *  number and +, - in the futuroe.)
 */

find_meta_vars_in_head(Head, MetaVars, MetaArgs) :-
	Head =.. [_Functor|Args],
	find_args_in_list(Args, MetaVars, MetaArgs, IsMeta),
	IsMeta = true.

find_args_in_list([], _, [], false).
find_args_in_list([Arg|Rest], MetaVars, MetaArgs, IsMeta) :-
	find_args_in_list(Rest,MetaVars,RestMetaArgs, MetaFound),
	(   occurs_in(Arg,MetaVars)
	->  MetaArgs=[0|RestMetaArgs],
	    IsMeta = true
	;   MetaArgs=[?|RestMetaArgs],
	    IsMeta = MetaFound
	).


extract_vars([],[]).
extract_vars([(_,Var)|RestCombo], [Var|RestVars]) :-
	extract_vars(RestCombo, RestVars).

handel_meta_args([], _, Known, Known).
handel_meta_args([A|Rest], Context, Known, MetaVars) :-
	var(A), !,
	add_var_to_set(A, Known, OwnMetaVars),
	handel_meta_args(Rest, Context, OwnMetaVars, MetaVars).
handel_meta_args([A|Rest], Context, Known, MetaVars) :-
	handel_meta_args(Rest, Context, Known, AllOthers),
	find_meta_vars_in_body(A, Context, AllOthers, MetaVars).


%%	check_inner_vars(+TermA, +TermB, +OldMetaVars, -NewMetaVars) is det
%
%	@tbd This solution only works for variables not in OldMetaVars

check_inner_vars(TermA,TermB,OldMetaVars,NewMetaVars):-
	unifiable(TermA, TermB, Unifiers), !,
	check_unifier_list(Unifiers,OldMetaVars,NewMetaVars).
check_inner_vars(_, _, MetaVars, MetaVars).


%%	check_unifier_list(+Unifiers, +MetasIn, -MetasOut)
%
%	@tbd	p(A):- term(A,B)= term(C,C), call(B)

check_unifier_list([], Metas, Metas).
check_unifier_list([A=B|Rest], OldMetas, Metas) :-
	(   occurs_in(A, OldMetas)
	->  add_var_to_set(B, OldMetas, Metas1)
	;   Metas1 = OldMetas
	),
	(   occurs_in(B, OldMetas)
	->  add_var_to_set(A, Metas1, Metas2)
	;   Metas2 = Metas1
	),
	check_unifier_list(Rest, Metas2, Metas).


free_vars_of(List,Free) :-
	include(var, List, Free).


%%	add_meta_vars(+Candidates, +KnownMeta, ?AllMeta)
%
%	Candidates and KnownMeta are lists of   free  variables. If some
%	variable from Candidates is in AllMeta  all the other candidates
%	that do not occur  in  KnownMeta   are  prepended  to  KnownMeta
%	yielding AllMeta.

add_meta_vars(Candidates,KnownMeta,AllMeta) :-
	select(Var,Candidates,OtherCandidates),
	occurs_in(Var,KnownMeta),
	combine_sets_nonbinding(OtherCandidates, KnownMeta, AllMeta), !.

combine_sets_nonbinding([],Set,Set).
combine_sets_nonbinding([E|Rest],OldSet,NewSet) :-
	add_var_to_set(E,OldSet,Set),
	combine_sets_nonbinding(Rest,Set,NewSet).


%%	add_var_to_set(?Var, +Set, ?NewSet) is det.
%
%	True when NewSet is the union of Set and {Var}.

add_var_to_set(Var, Set, NewSet) :-
	(   occurs_in(Var, Set)
	->  NewSet = Set
	;   NewSet = [Var|Set]
	).


%%	occurs_in(?Var, +Set) is semidet.
%
%	True if Var is a member of Set.

occurs_in(Var, Set) :-
	member(OldVar, Set),
	OldVar == Var, !.

combine_meta_args([],[]) :- !.
combine_meta_args([List],List) :- !.
combine_meta_args([MetaArgs|RestMetaArgs],CombinedArgs) :-
	combine_meta_args(RestMetaArgs,RestCombinedArgs),
	combine_two_arg_lists(MetaArgs, RestCombinedArgs, CombinedArgs).

combine_two_arg_lists([], [], []) :- !.
combine_two_arg_lists([ArgA|ArgsA], [ArgB|ArgsB], [CombinedArg|CombinedRest]) :-
	combine_two_arg_lists(ArgsA,ArgsB,CombinedRest),
	(   integer(ArgA)
	->  (	integer(ArgB)
	    ->	CombinedArg is max(ArgA, ArgB)
	    ;	CombinedArg = ArgA
	    )
	;   (	integer(ArgB)
	    ->	CombinedArg = ArgB
	    ;	CombinedArg = ?
	    )
	).


/* *
 * is_metaterm(?Module, -Literal, ?MetaArguments ) is non_det
 * is_metaterm(?Module, +Literal, ?MetaArguments ) is det
 *  Arg1 is a literal representing a metacall and
 *  Arg2 is the list of its meta-arguments each of the form:
 *      (Original_argument_position, Argument).
 */
is_metaterm(Module, Literal, MetaArguments) :-
	must_be(callable, Literal),
	is_meta_pred(Module, Literal, MetaTerm),
	Literal  =.. [Functor|Args],
	MetaTerm =.. [Functor|MetaArgs],
	collect_meta_args(Args,MetaArgs, MetaArguments ).

is_meta_pred(Module, Literal, MetaTerm) :-	%TODO: auf built_in einschr�nken!
	predicate_property(Module:Literal,meta_predicate(MetaTerm)).
is_meta_pred(Module, Literal, MetaTerm) :-
	(   inferred_meta_pred(Literal, Module, MetaTerm)
	;   predicate_property(Module:Literal, imported_from(From)),
	    inferred_meta_pred(Literal, From, MetaTerm)
	).

/* *
* collect_meta_args(+Args,+MetaArgs,?MetaArguments) is det
*
* MetaArguments is unified to a list of all elements of Args that are defined
* as meta-arguments via the corresponding elements of MetaArgs.
* (extract_meta_args/3 is used to select the corresponding elements and to
* build the entries of MetaArguments.)
* Fails if no MetaArguments can be found.
*/

collect_meta_args(Args, MetaArgs, MetaArguments ) :-
	bagof(Meta, extract_meta_argument(Args, MetaArgs, Meta), MetaArguments).

extract_meta_argument(Args, MetaArgs, (N,NewArg)) :-
	nth1(N, MetaArgs, MArg),
	nth1(N, Args, Arg),
	additonal_parameters(MArg, Arg, NewArg).

%%	additonal_parameters(+MetaSpec, +ArgIn, -ArgOut) is semidet.
%
%	If the meta-argument is not a   variable, add as many parameters
%	to it as indicated by the   meta-argument  specifier (0-9). Fail
%	for (skip) parameters marked as ':'  (= module-aware) but not as
%	meta:

additonal_parameters(0, Arg, Arg) :- !.
additonal_parameters(N, Arg, Arg) :-
	integer(N),
	var(Arg), !.
additonal_parameters(N, Arg, NewArg) :-
	integer(N),
	Arg =.. [Functor|Params],
	length(N_Elems, N),
	append(Params, N_Elems, NewParams),
	NewArg =.. [Functor | NewParams].


