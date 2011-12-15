/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2010, VU University Amsterdam

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

:- module(coinduction,
	  [ (coinductive)/1,
	    op(1150, fx, (coinductive))
	  ]).
:- use_module(library(error)).

/** <module> Co-Logic Programming

This simple module implements the   directive coinductive/1 as described
in "Co-Logic Programming: Extending Logic  Programming with Coinduction"
by Luke Simon et al. The idea behind coinduction is that a goal succeeds
if it unifies to a parent goal.  This enables some interesting programs,
notably on infinite trees (cyclic terms).

    ==
    :- use_module(library(coinduction)).

    :- coinductive p/1.

    p([1|T]) :- p(T).
    ==

This predicate is  true  for  any   cyclic  list  containing  only  1-s,
regardless of the cycle-length.

@bug	Programs mixing normal predicates and coinductive predicates must
	be _stratified_.  The theory does not apply to normal Prolog calling
	coinductive predicates, calling normal Prolog predicates, etc.

	Stratification is not checked or enforced in any other way and thus
	left as a responsibility to the user.
@see	"Co-Logic Programming: Extending Logic  Programming with Coinduction"
	by Luke Simon et al.
*/

:- multifile
	system:term_expansion/2,
	coinductive_declaration/2.	% Head, Module

%%	head(+Term, -QHead) is semidet.
%
%	Must be first to allow reloading!

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

%%	coinductive(:Spec)
%
%	The  declaration  :-   coinductive    name/arity,   ...  defines
%	predicates as _coinductive_. The predicate definition is wrapped
%	such that goals unify with their  ancestors. This directive must
%	preceed all clauses of the predicate.

coinductive(Spec) :-
	throw(error(context_error(nodirective, coinductive(Spec)), _)).

expand_coinductive_declaration(Spec, Clauses) :-
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
	{ valid_pi(Head, Name, Arity),
	  functor(GenHead, Name, Arity)
	},
	[ coinduction:coinductive_declaration(GenHead, Module) ].


valid_pi(Name/Arity, Name, Arity) :-
	must_be(atom, Name),
	must_be(integer, Arity).


%%	wrap_coinductive(+Head, +Term, -Clauses) is det.
%
%	Create a wrapper. The first clause deal   with the case where we
%	already created the wrapper. The second  creates the wrapper and
%	the first clause.

wrap_coinductive(Pred, Term, Clause) :-
	current_predicate(_, Pred), !,
	rename_clause(Term, 'coinductive ', Clause).
wrap_coinductive(Pred, Term, [Wrapper_1,Wrapper_2,FirstClause]) :-
	Pred = M:Head,
	functor(Head, Name, Arity),
	length(Args, Arity),
	GenHead =.. [Name|Args],
	atom_concat('coinductive ', Name, WrappedName),
	WrappedHead =.. [WrappedName|Args],
	Wrapper_1 = (GenHead :-
			prolog_current_frame(F),
		        prolog_frame_attribute(F, parent, FP),
		        prolog_frame_attribute(FP, parent_goal, M:GenHead)),
	Wrapper_2 = (GenHead :- WrappedHead, coinduction:no_lco),
	rename_clause(Term, 'coinductive ', FirstClause).

:- public no_lco/0.

no_lco.					% true, but do not optimize away

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

system:term_expansion((:- coinductive(Spec)), Clauses) :-
	expand_coinductive_declaration(Spec, Clauses).
system:term_expansion(Term, Wrapper) :-
	head(Term, Module:Head),
	coinductive_declaration(Head, Module),
	wrap_coinductive(Module:Head, Term, Wrapper).


