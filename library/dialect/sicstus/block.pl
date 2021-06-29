/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2010-2012, University of Amsterdam
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

:- module(block_directive,
	  [ (block)/1,			% +Heads
	    op(1150, fx, (block))
	  ]).

/** <module> Block: declare suspending predicates

This module provides SICStus Prolog-compatible
=|:- block BlockSpec, ...|= declarations for delaying predicate calls if
certain arguments are unbound.

@see	https://sicstus.sics.se/sicstus/docs/3.12.11/html/sicstus/Block-Declarations.html
*/

:- op(1150, fx, user:(block)).

:- multifile
	user:term_expansion/2,
	block_declaration/2.		% Head, Module

head(Var, _) :-
	var(Var), !, fail.
head((H:-_B), Head) :- !,
	head(H, Head).
head(H, Head) :-
	(   H = _:_
	->  Head = H
	;   prolog_load_context(module, M),
	    Head = M:H
	).


%%	block(+Heads).
%
%	Declare predicates to suspend on certain modes. The argument is,
%	like  meta_predicate/1,  a   comma-separated    list   of  modes
%	(_BlockSpecs_). Calls to the predicate is  suspended if at least
%	one of the conditions  implies  by   a  blockspec  evaluated  to
%	=true=. A blockspec  evaluated  to   =true=  iff  all  arguments
%	specified as `-' are unbound.
%
%	Multiple BlockSpecs for a single predicate  can appear in one or
%	more :- block declarations. The   predicate  is suspended untill
%	all mode patterns that apply to it are satisfied.
%
%	The implementation is realised by creating a wrapper that checks
%	the block conditions and either calls the original predicate
%	immediately (if none of the block conditions were true) or uses
%	attributed variables to delay re-evaluating the block condition
%	until any of the arguments in question are bound.
%
%	@compat SICStus Prolog
%	@bug	It is not possible to block on a dynamic predicate
%		because we cannot wrap assert/1.  Likewise, we cannot
%		block foreign predicates, although it would be easier
%		to support this.

block(Spec) :-
	throw(error(context_error(nodirective, block(Spec)), _)).

expand_block_declaration(Spec, Clauses) :-
	prolog_load_context(module, Module),
	phrase(expand_specs(Spec, Module), Clauses).

expand_specs(Var, _) -->
	{ var(Var), !,
	  instantiation_error(Var)
	}.
expand_specs(M:Spec, _) --> !,
	expand_specs(Spec, M).
expand_specs((A,B), Module) --> !,
	expand_specs(A, Module),
	expand_specs(B, Module).
expand_specs(Head, Module) -->
	{ valid_head(Head),
	  check_dynamic(Module:Head),
	  functor(Head, Name, Arity),
	  functor(GenHead, Name, Arity),
	  Clause = '$block_pred'(Head)
	},
	(   { current_predicate(Module:'$block_pred'/1) }
	->  []
	;   [ (:- discontiguous('$block_pred'/1)),
	      (:- public('$block_pred'/1))
	    ]
	),
	(   { prolog_load_context(module, Module) }
	->  [ Clause ]
	;   [ Module:Clause ]
	),
	[ block_directive:block_declaration(GenHead, Module) ].

valid_head(Head) :-
	callable(Head),
	forall(arg(_, Head, A), block_arg(A)).

check_dynamic(Head) :-
	(   predicate_property(Head, dynamic)
	;   predicate_property(Head, foreign)
	),
	permission_error(block, predicate, Head).
check_dynamic(_).

block_arg(A) :-
	var(A), !,
	instantiation_error(A).
block_arg(-) :- !.
block_arg(+) :- !.
block_arg(?) :- !.
block_arg(A) :-
	domain_error(block_argument, A).

%%	wrap_block(+Head, +Term, -Clauses) is det.
%
%	Create a wrapper. The first clause deal   with the case where we
%	already created the wrapper. The second  creates the wrapper and
%	the first clause.

wrap_block(Pred, Term, Clause) :-
	current_predicate(_, Pred), !,
	rename_clause(Term, 'block ', Clause).
wrap_block(Pred, Term, Clauses) :-
	Pred = Module:Head,
	functor(Head, Name, Arity),
	findall(Wrapper, block_wrapper_clause(Module, Name, Arity, Wrapper), Wrappers),
	rename_clause(Term, 'block ', FirstClause),
	append(Wrappers, [FirstClause], Clauses).

%%	block_wrapper_clause(+Module, +Name, +Arity, -Clause) is nondet.
%
%	Generate the clauses for the wrapper. Each blockspec translates
%	to a clause in the wrapper that checks if the block condition is
%	true, i. e. all of the arguments marked as `-` are unbound.
%	If so, attributes are added to all of these unbound arguments,
%	so that once any of them are bound, the wrapper is called again
%	and the block conditions are re-evaluated.
%
%	Finally, a fallthrough clause is always generated, which calls
%	the wrapped predicate immediately if none of the previous
%	clauses (block conditions) was true.

block_wrapper_clause(Module, Name, Arity, (GenHead :- GenBody)) :-
	length(GenArgs, Arity),
	GenHead =.. [Name|GenArgs],
	functor(BlockHead, Name, Arity),
	Module:'$block_pred'(BlockHead),
	BlockHead =.. [_|BlockArgs],
	find_args_to_block_on(BlockArgs, GenArgs, ToBlockOn),
	args_to_var_conditions(ToBlockOn, GenBody, GenBody1),
	GenBody1 = (!, GenBody2),
	args_to_suspend_calls(ToBlockOn, _IsAlreadyUnblocked, Module:GenHead, GenBody2, true).
block_wrapper_clause(_Module, Name, Arity, (GenHead :- WrappedHead)) :-
	length(GenArgs, Arity),
	GenHead =.. [Name|GenArgs],
	atom_concat('block ', Name, WrappedName),
	WrappedHead =.. [WrappedName|GenArgs].

%%	find_args_to_block_on(+BlockArgs, +HeadArgs, -ArgsToBlockOn) is semidet.
%
%	Collect into ArgsToBlockOn all arguments from HeadArgs for which
%	the corresponding argument in BlockArgs is `-`, indicating that
%	the argument is part of the block condition.

find_args_to_block_on([], [], []) :- !.
find_args_to_block_on([-|MoreBlockArgs], [Arg|MoreHeadArgs], [Arg|MoreToBlockOn]) :-
	!,
	find_args_to_block_on(MoreBlockArgs, MoreHeadArgs, MoreToBlockOn).
find_args_to_block_on([_|MoreBlockArgs], [_|MoreHeadArgs], ToBlockOn) :-
	find_args_to_block_on(MoreBlockArgs, MoreHeadArgs, ToBlockOn).

%%	args_to_var_conditions(+ArgsToBlockOn, -Conditions, ?ConditionsTail) is semidet.
%
%	Convert a list of arguments into a conjunction of var/1 checks
%	that succeeds if all arguments are unbound variables.
%
%	This effectively generates an unrolled version of
%	`maplist(var, ArgsToBlockOn), ConditionsTail`.

args_to_var_conditions([], Tail, Tail) :- !.
args_to_var_conditions([Arg|MoreArgs], Conditions, Tail) :-
	Conditions = (var(Arg), MoreConditions),
	args_to_var_conditions(MoreArgs, MoreConditions, Tail).

%%	args_to_suspend_calls(+ArgsToBlockOn, -IsAlreadyUnblocked, +BlockedGoal, -SuspendCalls, +Tail) is semidet.
%
%	Build a sequence of calls that delays BlockedGoal until any
%	variable in ArgsToBlockOn is bound. IsAlreadyUnblocked should be
%	an unbound fresh variable - it is passed directly to unblock/2,
%	which will bind the variable so that the same blocked goal is
%	not called again by another unblock/2 call from the same group.

args_to_suspend_calls([], _, _, Tail, Tail) :- !.
args_to_suspend_calls([Arg|MoreArgs], IsAlreadyUnblocked, BlockedGoal, SuspendCalls, Tail) :-
	SuspendCalls = ('$suspend'(Arg, block_directive, block_directive:unblock(IsAlreadyUnblocked, BlockedGoal)), MoreSuspendCalls),
	args_to_suspend_calls(MoreArgs, IsAlreadyUnblocked, BlockedGoal, MoreSuspendCalls, Tail).


attr_unify_hook(call(ThisGoals), NewVar) :-
	var(NewVar),
	!,
	(   get_attr(NewVar, block_directive, call(OtherGoals))
	->  put_attr(NewVar, block_directive, call((ThisGoals, OtherGoals)))
	;   put_attr(NewVar, block_directive, call(ThisGoals))
	).
attr_unify_hook(call(Goals), _) :- Goals.

:- public unblock/2.
unblock(IsAlreadyUnblocked, _) :- IsAlreadyUnblocked == (-), !.
unblock(-, BlockedGoal) :- BlockedGoal.

attribute_goals(Var) -->
	{get_attr(Var, block_directive, call(Goals))},
	!,
	render_block_goals(Goals).

render_block_goals((Left, Right)) -->
	render_block_goals(Left),
	render_block_goals(Right).
render_block_goals(block_directive:unblock(IsAlreadyUnblocked, BlockedGoal)) -->
	(   {IsAlreadyUnblocked == (-)}
	->  []
	;   [BlockedGoal]
	).


%%	rename_clause(+Clause, +Prefix, -Renamed) is det.
%
%	Rename a clause by prefixing its old name wit h Prefix.

rename_clause((Head :- Body), Prefix, (NewHead :- Body)) :- !,
        rename_clause(Head, Prefix, NewHead).
rename_clause(M:Head, Prefix, M:NewHead) :-
	rename_clause(Head, Prefix, NewHead).
rename_clause(Head, Prefix, NewHead) :-
        Head =.. [Name|Args],
        atom_concat(Prefix, Name, WrapName),
        NewHead =.. [WrapName|Args].


		 /*******************************
		 *	  EXPANSION HOOKS	*
		 *******************************/

system:term_expansion((:- block(Spec)), Clauses) :-
	expand_block_declaration(Spec, Clauses).
system:term_expansion(Term, Wrapper) :-
	head(Term, Module:Head),
	block_declaration(Head, Module),
	wrap_block(Module:Head, Term, Wrapper).

