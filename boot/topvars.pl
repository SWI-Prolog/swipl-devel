/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2008, University of Amsterdam

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

:- module(toplevel_variables,
	  [ print_toplevel_variables/0
	  , verbose_expansion/1
	  ]).

:- dynamic
	verbose/0.

:- initialization op(1, fx, user:($)).

expand_query(Query, Expanded, Bindings, ExpandedBindings) :-
	expand_vars(Bindings, Query, Expanded),
	term_variables(Expanded, Free),
	delete_bound_vars(Bindings, Free, ExpandedBindings),
	(   verbose,
	    Query \=@= Expanded
	->  print_query(Expanded, ExpandedBindings)
	;   true
	).


print_query(Query, Bindings) :-
	bind_vars(Bindings),
	writeq(Query), write('.'), nl,
	fail.				% undo bind_vars/2.
print_query(_, _).

bind_vars([]).
bind_vars([Name=Value|Rest]) :-
	Name = Value,
	bind_vars(Rest).


expand_vars(_, Var, Var) :-
	var(Var), !.
expand_vars(_, Atomic, Atomic) :-
	atomic(Atomic), !.
expand_vars(Bindings, $(Var), Value) :-
	name_var(Var, Bindings, Name),
	(   toplevel_var(Name, Value)
	->  !
	;   throw(error(existence_error(variable, Name), _))
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
	(   nonvar(Value)
	->  forall(recorded('$topvar', Var = _, Ref), erase(Ref)),
	    (   (   current_prolog_flag(toplevel_var_size, Count)
		->  '$term_size'(Value, Count, _)
		;   true
		)
	    ->  recorda('$topvar', Binding, _)
	    ;   true
	    )
	;   true
	),
	assert_bindings(Tail).

toplevel_var(Var, Binding) :-
	recorded('$topvar', Var=Binding).

print_toplevel_variables :-
	(   toplevel_var(Name, Value)
	*-> format('$~w =~t~12|~p~n', [Name, Value]),
	    fail
	;   format('No defined toplevel variables~n')
	).

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

