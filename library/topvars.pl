/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/

:- module(toplevel_variables,
	  [ print_toplevel_variables/0
	  , verbose_expansion/1
	  ]).

:- dynamic
	toplevel_var/2,
	verbose/0.

:- op(100, fx, $).

expand_query(Query, Expanded, Bindings, ExpandedBindings) :-
	expand_vars(Bindings, Query, Expanded),
	delete_bound_vars(Bindings, ExpandedBindings),
	(   verbose,
	    Query \== Expanded
	->  print_query(Expanded, ExpandedBindings)
	;   true
	).

expand_vars(_, Var, Var) :-
	var(Var), !.
expand_vars(Bindings, $(Var), Value) :-
	bind_var(Var, Bindings),
	(   toplevel_var(Var, Value)
	->  !
	;   $warning('~w: No such variable')
	).
expand_vars(Bindings, Term, Expanded) :-
	functor(Term, Name, Arity), !,
	functor(Expanded, Name, Arity),
	End is Arity + 1,
	expand_args(1, End, Bindings, Term, Expanded).
expand_vars(_, Term, Term).


expand_args(End, End, _, _, _) :- !.
expand_args(Arg0, End, Bindings, T0, T) :-
	arg(Arg0, T0, V0),
	expand_vars(Bindings, V0, V1),
	arg(Arg0, T, V1),
	Arg1 is Arg0 + 1,
	expand_args(Arg1, End, Bindings, T0, T).

bind_var(Var, [VarName = TheVar|_]) :-
	Var == TheVar, !,
	Var = VarName.
bind_var(Var, [_|T]) :-
	bind_var(Var, T).


delete_bound_vars([], []).
delete_bound_vars([H|T0], [H|T]) :-
	H = (_Name = Value),
	var(Value), !,
	delete_bound_vars(T0, T).
delete_bound_vars([_|T0], T1) :-
	delete_bound_vars(T0, T1).


expand_answer(Bindings, Bindings) :-
	assert_bindings(Bindings).

assert_bindings([]).
assert_bindings([Var = Value|Tail]) :-
	retractall(toplevel_var(Var, _)),
	assert(toplevel_var(Var, Value)),
	assert_bindings(Tail).
	  

print_toplevel_variables :-
	toplevel_var(Name, Value),
	format('$~w =~t~12|~p~n', [Name, Value]),
	fail.
print_toplevel_variables :-
	toplevel_var(_, _), !.
print_toplevel_variables :-
	format('No defined toplevel variables~n').


verbose_expansion(on) :- !,
	retractall(verbose),
	asserta(verbose).
verbose_expansion(off) :-
	retractall(verbose).


:- user:assert((expand_query(A, B, C, D) :-
	       toplevel_variables:(expand_query(A, B, C, D)))).
:- user:assert((expand_answer(A, B) :-
	       toplevel_variables:(expand_answer(A, B)))).

