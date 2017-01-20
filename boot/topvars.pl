/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1985-2016, University of Amsterdam
                              VU University Amsterdam
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

:- module(toplevel_variables,
          [ print_toplevel_variables/0,
            verbose_expansion/1,
            '$switch_toplevel_mode'/1           % +Mode
          ]).

:- dynamic
    verbose/0.

% define the operator globally
:- op(1, fx, user:($)).

:- public
    expand_query/4,         % +Query0, -Query, +Bindings0, -Bindings
    expand_answer/2.        % +Answer0, -Answer

%!  expand_query(+Query0, -Query, +Bindings0, -Bindings) is det.
%
%   These predicates realise reuse of   toplevel variables using the
%   $Var notation. These hooks are   normally called by toplevel.pl.
%   If the user defines rules for these   hooks  in the user module,
%   these implementations may be  called  (or   not)  to  define the
%   interaction with the user hooks.

expand_query(Query, Expanded, Bindings, ExpandedBindings) :-
    phrase(expand_vars(Bindings, Query, Expanded), NewBindings),
    term_variables(Expanded, Free),
    delete_bound_vars(Bindings, Free, ExpandedBindings0),
    '$append'(ExpandedBindings0, NewBindings, ExpandedBindings),
    (   verbose,
        Query \=@= Expanded
    ->  print_query(Expanded, ExpandedBindings)
    ;   true
    ).

print_query(Query, Bindings) :-
    bind_vars(Bindings),
    writeq(Query), write('.'), nl,
    fail.                           % undo bind_vars/2.
print_query(_, _).

bind_vars([]).
bind_vars([Name=Value|Rest]) :-
    Name = Value,
    bind_vars(Rest).

%!  expand_vars(+Bindings, +Query, -Expanded)//
%
%   Replace $Var terms inside Query by   the  toplevel variable term
%   and unify the result with  Expanded. NewBindings gets Name=Value
%   terms for toplevel variables that are bound to non-ground terms.

expand_vars(_, Var, Var) -->
    { var(Var) },
    !.
expand_vars(_, Atomic, Atomic) -->
    { atomic(Atomic) },
    !.
expand_vars(Bindings, $(Var), Value) -->
    { name_var(Var, Bindings, Name),
      (   toplevel_var(Name, Value)
      ->  !
      ;   throw(error(existence_error(answer_variable, Name), _))
      )
    },
    [ Name = Value ].
expand_vars(Bindings, Term, Expanded) -->
    { compound_name_arity(Term, Name, Arity),
      !,
      compound_name_arity(Expanded, Name, Arity),
      End is Arity + 1
    },
    expand_args(1, End, Bindings, Term, Expanded).

expand_args(End, End, _, _, _) --> !.
expand_args(Arg0, End, Bindings, T0, T) -->
    { arg(Arg0, T0, V0),
      arg(Arg0, T, V1),
      Arg1 is Arg0 + 1
    },
    expand_vars(Bindings, V0, V1),
    expand_args(Arg1, End, Bindings, T0, T).

name_var(Var, [VarName = TheVar|_], VarName) :-
    Var == TheVar,
    !.
name_var(Var, [_|T], Name) :-
    name_var(Var, T, Name).


delete_bound_vars([], _, []).
delete_bound_vars([H|T0], Free, [H|T1]) :-
    H = (_Name = Value),
    v_member(Value, Free),
    !,
    delete_bound_vars(T0, Free, T1).
delete_bound_vars([_|T0], Free, T1) :-
    delete_bound_vars(T0, Free, T1).

v_member(V, [H|T]) :-
    (   V == H
    ;   v_member(V, T)
    ).

%!  expand_answer(+Answer0, -Answer) is det.
%
%   Save toplevel variable bindings.

expand_answer(Bindings, Bindings) :-
    assert_bindings(Bindings).

assert_bindings([]).
assert_bindings([Var = Value|Tail]) :-
    assert_binding(Var, Value),
    assert_bindings(Tail).

assert_binding(Var, Value) :-
    (   ( nonvar(Value) ; attvar(Value))
    ->  update_var(Var, Value)
    ;   true
    ).

update_var(Name, Value) :-
    current_prolog_flag(toplevel_mode, recursive),
    !,
    (   nb_current('$topvar', Bindings),
        Bindings \== []
    ->  true
    ;   Bindings = '$topvar'{}
    ),
    put_dict(Name, Bindings, Value, NewBindings),
    b_setval('$topvar', NewBindings).
update_var(Name, Value) :-
    delete_var(Name),
    set_var(Name, Value).

delete_var(Name) :-
    forall(recorded('$topvar', Name = _, Ref), erase(Ref)).

set_var(Name, Value) :-
    current_prolog_flag(toplevel_var_size, Count),
    !,
    (   '$term_size'(Value, Count, _)
    ->  recorda('$topvar', Name = Value, _)
    ;   true
    ).
set_var(Name, Value) :-
    recorda('$topvar', Name = Value, _).

toplevel_var(Var, Binding) :-
    current_prolog_flag(toplevel_mode, recursive),
    !,
    nb_current('$topvar', Bindings),
    Bindings \== [],
    get_dict(Var, Bindings, Binding).
toplevel_var(Var, Binding) :-
    recorded('$topvar', Var=Binding).

%!  '$switch_toplevel_mode'(+Mode) is det.
%
%   Migrate the variable database when switching   to a new toplevel
%   mode. Alternatively we may decide to wipe it as the semantics of
%   the variables may be slightly different.

'$switch_toplevel_mode'(recursive) :-
    findall(Name-Value, retract_topvar(Name, Value), Pairs),
    dict_pairs(Bindings, '$topvar', Pairs),
    b_setval('$topvar', Bindings).
'$switch_toplevel_mode'(backtracking) :-
    (   nb_current('$topvar', Dict),
        Dict \== []
    ->  forall(get_dict(Name, Dict, Value),
               recorda('$topvar', Name = Value, _))
    ),
    nb_delete('$topvar').

retract_topvar(Name, Value) :-
    recorded('$topvar', Name=Value, Ref),
    erase(Ref).

%!  print_toplevel_variables
%
%   Print known bindings for toplevel ($Var) variables.

print_toplevel_variables :-
    (   toplevel_var(Name, Value)
    *-> format('$~w =~t~12|~p~n', [Name, Value]),
        fail
    ;   format('No defined toplevel variables~n')
    ).

verbose_expansion(on) :-
    !,
    retractall(verbose),
    asserta(verbose).
verbose_expansion(off) :-
    retractall(verbose).

