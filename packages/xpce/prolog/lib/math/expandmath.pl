/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

:- module(expand_math, [expand_function/3]).
:- use_module(expandgoal).
:- require([ append/3
	   , genarg/3
	   ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module cooperates with  library(expandgoal), expanding mathematical
expressions on systems that have limited  math functions and that cannot
define new math funtions.

For each function f/n, it expects a   predicate f/n+1, where the first n
arguments are in the same order as the   function,  and the last is used
for the return value. In ensures  all   arguments  to  the predicate are
evaluated if they contain expressions.

There are two configuration predicates:

    built_in(?Function)
	Says Function is a built-in mathematical function.  It will not
	be converted, but its arguments will if they contain non-builtin
	functions.  Numbers and variables are not converted either.

    math_alias(+Function, -Alias)
	Alows for alias definition.  Some of these aliases are needed
	to call the right function (** --> pow for example).  Some are
	used to expand mathematical constants (pi, e).

Bugs:
	Only experssions appearing in the source-code as an argument to
	one of the math predicates is/2, =:=/2, =\=/2, >/2, </2, >=/2,
	=</2 are translated, and only if these are determined as a goal,
	by the goal expansion package.
- - - - - - - - - - - - - - - -
 - - - - - - - - - - - - - - - - - - - - - */

:- multifile
	user:goal_expansion/2.
:- dynamic
	user:goal_expansion/2.

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


expand_function(_, _, _) :-		% testing under SWI-Prolog
	prolog_load_context(module, quintus), !,
	fail.
expand_function(Expression, NativeExpression, Goal) :-
	do_expand_function(Expression, NativeExpression, Goal0),
	tidy(Goal0, Goal).

do_expand_function(X, X, true) :-
	all_builtin(X), !.
do_expand_function(A0, X, Code) :-
	math_alias(A0, A), !,
	do_expand_function(A, X, Code).
do_expand_function(Function, Result, ArgCode) :-
	built_in(Function), !,
	Function =.. [Name|Args],
	expand_function_arguments(Args, ArgResults, ArgCode),
	Result =.. [Name|ArgResults].
do_expand_function(Function, Result, (ArgCode, Pred)) :-
	Function =.. [Name|Args],
	expand_predicate_arguments(Args, ArgResults, ArgCode),
	append(ArgResults, [Result], PredArgs),
	Pred =.. [Name|PredArgs].

expand_function_arguments([], [], true).
expand_function_arguments([H0|T0], [H|T], (A,B)) :-
	do_expand_function(H0, H, A),
	expand_function_arguments(T0, T, B).

expand_predicate_arguments([], [], true).
expand_predicate_arguments([H0|T0], [H|T], (A,B)) :-
	do_expand_function(H0, H1, A0),
	(   nonvar(H1),
	    built_in(H1)
	->  A = (A0, H is H1)
	;   A = A0,
	    H = H1
	),
	expand_predicate_arguments(T0, T, B).

built_in(_+_).
built_in(_-_).
built_in(_*_).
built_in(-_).
built_in(+_).
built_in(abs(_)).
built_in(_/_).
built_in(_//_).
built_in(div(_,_)).
built_in(mod(_,_)).
built_in(\(_)).
built_in(\(_,_)).
built_in([_]).				% "a" ...
built_in(_/\_).
built_in(_\/_).
built_in(_>>_).
built_in(_<<_).
built_in(integer(_)).
built_in(float(_)).
built_in(max(_,_)).
built_in(min(_,_)).

all_builtin(F) :-
	var(F), !.
all_builtin(F) :-
	number(F), !.
all_builtin(F) :-
	built_in(F),
	\+ (genarg(_,F,A), \+ all_builtin(A)).

math_alias(pi,		3.14159265358979323846).
math_alias(e,		2.7182818284590452354).
math_alias(atan(Y,X),   atan2(Y,X)).
math_alias(**(X,Y),	pow(X,Y)).
math_alias(X^Y,		pow(X,Y)).
math_alias(ceil(X),	ceiling(X)).
math_alias(random(M),	random(0, M)).

		 /*******************************
		 *	       TIDY		*
		 *******************************/

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
tidy((A,B), (TA, TB)) :- !,
	tidy(A, TA),
	tidy(B, TB).
tidy(A, A).

		 /*******************************
		 *	       THE HOOK		*
		 *******************************/

user:goal_expansion(A,B) :-
	expand_math:math_goal_expansion(A,B).


