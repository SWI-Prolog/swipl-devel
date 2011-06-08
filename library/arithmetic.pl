/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2011, VU University Amsterdam

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
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(arithmetic,
	  [ arithmetic_function/1,
	    eval_arithmetic_expression/2  % -Result, :Expression
	  ]).

/** <module> Extensible arithmetic

This module provides a  portable   partial  replacement  of SWI-Prolog's
user-defined  arithmetic  (evaluable)   functions.    It   defines   the
compatibility  directive  arithmetic_function/1  and  support  for  both
runtime and compile-time evaluation of expressions   that  are a mixture
between Prolog predicates  used  as   functions  and  built-in evaluable
terms.
*/

:- meta_predicate
	arithmetic_function(:),
	arithmetic_function(-, :).
:- multifile
	evaluable/2.				% Term, Module

%%	arithmetic_function(:NameArity) is det.
%
%	Declare a predicate as an arithmetic function.
%
%	@deprecated This function provides  a   partial  work around for
%	pure Prolog user-defined arithmetic  functions   that  has  been
%	dropped in SWI-Prolog  5.11.23.  Notably,   it  only  deals with
%	expression know at runtime.

arithmetic_function(Term) :-
	throw(error(context_error(nodirective, arithmetic_function(Term)), _)).

arith_decl_clauses(Term,
		   arithmetic:evaluable(Term, Q)) :-
	prolog_load_context(module, M),
	strip_module(M:Term, Q, Spec),
	(   Spec = Name/Arity
	->  functor(Term, Name, Arity)
	;   type_error(predicate_indicator, Term)
	).

%%	eval_clause(+Term, -Clause) is det.
%
%	Clause is a clause  for   evaluating  the  arithmetic expression
%	Term.

eval_clause(Term, (eval(Gen, M, Result) :- Body)) :-
	functor(Term, Name, Arity),
	functor(Gen, Name, Arity),
	Gen =.. [_|Args],
	eval_args(Args, PlainArgs, M, Goals, [Result is NewTerm]),
	NewTerm =.. [Name|PlainArgs],
	list_conj(Goals, Body).

eval_args([], [], _, Goals, Goals).
eval_args([E0|T0], [A0|T], M, [eval(E0, M, A0)|GT], RT) :-
	eval_args(T0, T, M, GT, RT).

list_conj([One], One) :- !.
list_conj([H|T0], (H,T)) :-
	list_conj(T0, T).

eval_clause(Clause) :-
	current_arithmetic_function(Term),
	eval_clause(Term, Clause).

term_expansion(eval('$builtin', _, _), Clauses) :-
	findall(Clause, eval_clause(Clause), Clauses).


%%	eval_arithmetic_expression(-Result, :Expression) is det.
%
%	Result is the arithmetic result of evaluating Expression.

eval_arithmetic_expression(Result, M:Expression) :-
	eval(Expression, M, Result).

eval(Number, _, Result) :-
	number(Number), !,
	Result = Number.
eval(Term, M, Result) :-
	evaluable(Term, M2),
	visible(M, M2), !,
	call(M2:Term, Result).
eval('$builtin', _, _).


visible(M, M) :- !.
visible(M, Super) :-
	import_module(M, Parent),
	visible(Parent, Super).


		 /*******************************
		 *	  EXPANSION HOOK	*
		 *******************************/

system:term_expansion((:- arithmetic_function(Term)), Clauses) :-
	arith_decl_clauses(Term, Clauses).
