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

This  module  realises  SICStus  Prolog   =|:-  block  BlockSpec,  ...|=
declarations  using  a   wrapper   predicate    that   calls   the  real
implementation through a coroutining primitive   (typically  when/2, but
freeze/2 for simple cases).

@tbd	This emulation is barely tested.
*/

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
%	The implementation is realised by creating   a wrapper that uses
%	when/2 to realize suspension of the renamed predicate.
%
%	@compat SICStus Prolog
%	@compat If the predicate is blocked on multiple conditions, it
%		will not unblock before _all_ conditions are satisfied.
%		SICStus unblocks when one arbitrary condition is
%		satisfied.
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
wrap_block(Pred, Term, [Wrapper,FirstClause]) :-
	block_declarations(Pred, Modes),
	Pred = _:Head,
	functor(Head, Name, Arity),
	length(Args, Arity),
	GenHead =.. [Name|Args],
	atom_concat('block ', Name, WrappedName),
	WrappedHead =.. [WrappedName|Args],
	when_cond(Modes, Args, Cond),
	simplify_coroute(when(Cond, WrappedHead), Coroute),
	Wrapper = (GenHead :- Coroute),
	rename_clause(Term, 'block ', FirstClause).

block_wrapper((_Head :- Coroute)) :-
	simplify_coroute(when(_,Wrapped), Coroute),
	compound(Wrapped),
	functor(Wrapped, Name, _),
	sub_atom(Name, 0, _, _, 'block ').

block_declarations(M:P, Modes) :-
	functor(P, Name, Arity),
	functor(H, Name, Arity),
	findall(H, M:'$block_pred'(H), Modes).

when_cond([Head], Args, Cond) :- !,
	one_cond(Args, Head, Cond).
when_cond([H|T], Args, (C1,C2)) :-
	one_cond(Args, H, C1),
	when_cond(T, Args, C2).

one_cond(Vars, Spec, Cond) :-
	cond_vars(Vars, 1, Spec, CondVars),
	nonvar_or(CondVars, Cond).

cond_vars([], _, _, []).
cond_vars([H|T0], I, Spec, L) :-
	(   arg(I, Spec, -)
	->  L = [H|T]
	;   L = T
	),
	I2 is I + 1,
	cond_vars(T0, I2, Spec, T).

nonvar_or([V], nonvar(V)).
nonvar_or([V|T], (nonvar(V);C)) :-
	nonvar_or(T, C).

simplify_coroute(when(nonvar(X), C), freeze(X, C)).
simplify_coroute(Coroute, Coroute).


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
	\+ block_wrapper(Term),		% avoid recursion
	wrap_block(Module:Head, Term, Wrapper).

