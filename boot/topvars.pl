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
	verbose/0.

:- initialization op(1, fx, user:($)).

expand_query(Query, Expanded, Bindings, ExpandedBindings) :-
	expand_vars(Bindings, Query, Expanded),
	free_variables(Expanded, Free),
	delete_bound_vars(Bindings, Free, ExpandedBindings),
	(   verbose,
	    Query \=@= Expanded
	->  print_query(Expanded, ExpandedBindings)
	;   true
	).


print_query(Query, Bindings) :-
	checklist(call, Bindings),
	writeq(Query), write('.'), nl,
	fail.
print_query(_, _).


expand_vars(_, Var, Var) :-
	var(Var), !.
expand_vars(_, Atomic, Atomic) :-
	atomic(Atomic), !.
expand_vars(Bindings, $(Var), Value) :-
	name_var(Var, Bindings, Name),
	(   toplevel_var(Name, Value)
	->  !
	;   $warning('$~w: No such variable', Name)
	).
expand_vars(Bindings, Term, Expanded) :-
	functor(Term, Name, Arity), !,
	functor(Expanded, Name, Arity),
	End is Arity + 1,
	expand_args(1, End, Bindings, Term, Expanded).

expand_args(End, End, _, _, _) :- !.
expand_args(Arg0, End, Bindings, T0, T) :-
	arg(Arg0, T0, V0),
	expand_vars(Bindings, V0, V1),
	arg(Arg0, T, V1),
	Arg1 is Arg0 + 1,
	expand_args(Arg1, End, Bindings, T0, T).

name_var(Var, [VarName = TheVar|_], VarName) :-
	Var == TheVar, !.
name_var(Var, [_|T], Name) :-
	name_var(Var, T, Name).


delete_bound_vars([], _, []).
delete_bound_vars([H|T0], Free, [H|T1]) :-
	H = (_Name = Value),
	v_member(Value, Free), !,
	delete_bound_vars(T0, Free, T1).
delete_bound_vars([_|T0], Free, T1) :-
	delete_bound_vars(T0, Free, T1).

v_member(V, [H|T]) :-
	(   V == H
	;   v_member(V, T)
	).

expand_answer(Bindings, Bindings) :-
	assert_bindings(Bindings).

assert_bindings([]).
assert_bindings([Binding|Tail]) :-
	Binding = (Var = Value),
	forall(recorded('$topvar', Var = _, Ref), erase(Ref)),
	(   (   current_prolog_flag(toplevel_var_size, Count)
	    ->  '$term_complexity'(Value, Count, _)
	    ;   true
	    )
	->  recorda('$topvar', Binding, _)
	;   true
	),
	assert_bindings(Tail).
	  
toplevel_var(Var, Binding) :-
	recorded('$topvar', Var=Binding).

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

:- multifile
	user:expand_query/4,
	user:expand_answer/2.

user:expand_query(A, B, C, D) :-
	toplevel_variables:expand_query(A, B, C, D).
user:expand_answer(A, B) :-
	toplevel_variables:expand_answer(A, B).

