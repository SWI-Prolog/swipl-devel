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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(arithmetic,
	  [ arithmetic_function/1,		% +Name/Arity
	    arithmetic_expression_value/2	% :Expression, -Value
	  ]).
:- use_module(library(error)).
:- use_module(library(lists)).

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
	arithmetic_expression_value(:, -).
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

arith_decl_clauses(NameArity,
		   [(:- public(Name/ImplArity)),
		    arithmetic:evaluable(Term, Q)
		   ]) :-
	prolog_load_context(module, M),
	strip_module(M:NameArity, Q, Spec),
	(   Spec = Name/Arity
	->  functor(Term, Name, Arity),
	    ImplArity is Arity+1
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


%%	arithmetic_expression_value(:Expression, -Result) is det.
%
%	True  when  Result  unifies  with    the  arithmetic  result  of
%	evaluating Expression.

arithmetic_expression_value(M:Expression, Result) :-
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
		 *	   COMPILE-TIME		*
		 *******************************/

math_goal_expansion(A is Expr, Goal) :-
	expand_function(Expr, Native, Pre),
	tidy((Pre, A is Native), Goal).
math_goal_expansion(ExprA =:= ExprB, Goal) :-
	expand_function(ExprA, NativeA, PreA),
	expand_function(ExprB, NativeB, PreB),
	tidy((PreA, PreB, NativeA =:= NativeB), Goal).
math_goal_expansion(ExprA =\= ExprB, Goal) :-
	expand_function(ExprA, NativeA, PreA),
	expand_function(ExprB, NativeB, PreB),
	tidy((PreA, PreB, NativeA =\= NativeB), Goal).
math_goal_expansion(ExprA > ExprB, Goal) :-
	expand_function(ExprA, NativeA, PreA),
	expand_function(ExprB, NativeB, PreB),
	tidy((PreA, PreB, NativeA > NativeB), Goal).
math_goal_expansion(ExprA < ExprB, Goal) :-
	expand_function(ExprA, NativeA, PreA),
	expand_function(ExprB, NativeB, PreB),
	tidy((PreA, PreB, NativeA < NativeB), Goal).
math_goal_expansion(ExprA >= ExprB, Goal) :-
	expand_function(ExprA, NativeA, PreA),
	expand_function(ExprB, NativeB, PreB),
	tidy((PreA, PreB, NativeA >= NativeB), Goal).
math_goal_expansion(ExprA =< ExprB, Goal) :-
	expand_function(ExprA, NativeA, PreA),
	expand_function(ExprB, NativeB, PreB),
	tidy((PreA, PreB, NativeA =< NativeB), Goal).

expand_function(Expression, NativeExpression, Goal) :-
	do_expand_function(Expression, NativeExpression, Goal0),
	tidy(Goal0, Goal).

do_expand_function(X, X, true) :-
	evaluable(X), !.
do_expand_function(Function, Result, ArgCode) :-
	current_arithmetic_function(Function), !,
	Function =.. [Name|Args],
	expand_function_arguments(Args, ArgResults, ArgCode),
	Result =.. [Name|ArgResults].
do_expand_function(Function, Result, (ArgCode, Pred)) :-
	prolog_load_context(module, M),
	evaluable(Function, M2),
	visible(M, M2), !,
	Function =.. [Name|Args],
	expand_predicate_arguments(Args, ArgResults, ArgCode),
	append(ArgResults, [Result], PredArgs),
	Pred =.. [Name|PredArgs].
do_expand_function(Function, _, _) :-
	existence_error(evaluable, Function).


expand_function_arguments([], [], true).
expand_function_arguments([H0|T0], [H|T], (A,B)) :-
	do_expand_function(H0, H, A),
	expand_function_arguments(T0, T, B).

expand_predicate_arguments([], [], true).
expand_predicate_arguments([H0|T0], [H|T], (A,B)) :-
	do_expand_function(H0, H1, A0),
	(   callable(H1),
	    current_arithmetic_function(H1)
	->  A = (A0, H is H1)
	;   A = A0,
	    H = H1
	),
	expand_predicate_arguments(T0, T, B).

%%	evaluable(F) is semidet.
%
%	True if F and all its subterms are evaluable terms or variables.

evaluable(F) :-
	var(F), !.
evaluable(F) :-
	number(F), !.
evaluable([_Code]) :- !.
evaluable(F) :-
	current_arithmetic_function(F),
	forall(arg(_,F,A), evaluable(A)).

%%	tidy(+GoalIn, -GoalOut)
%
%	Cleanup the output from expand_function/3.

tidy(A, A) :-
	var(A), !.
tidy(((A,B),C), R) :- !,
     tidy((A,B,C), R).
tidy((true,A), R) :- !,
	tidy(A, R).
tidy((A,true), R) :- !,
	tidy(A, R).
tidy((A, X is Y), R) :-
	var(X), var(Y), !,
	tidy(A, R),
	X = Y.
tidy((A,B), (TA,TB)) :- !,
	tidy(A, TA),
	tidy(B, TB).
tidy(A, A).


		 /*******************************
		 *	  EXPANSION HOOK	*
		 *******************************/

:- multifile
	system:term_expansion/2,
	system:goal_expansion/2.

system:term_expansion((:- arithmetic_function(Term)), Clauses) :-
	arith_decl_clauses(Term, Clauses).

system:goal_expansion(Math, MathGoal) :-
	math_goal_expansion(Math, MathGoal).
