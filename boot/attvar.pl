/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2004-2016, University of Amsterdam
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

:- module('$attvar',
          [ '$wakeup'/1,                % +Wakeup list
            freeze/2,                   % +Var, :Goal
            frozen/2,                   % @Var, -Goal
            call_residue_vars/2,        % :Goal, -Vars
            copy_term/3                 % +Term, -Copy, -Residue
          ]).

/** <module> Attributed variable handling

Attributed  variable  and  coroutining  support    based  on  attributed
variables. This module is complemented with C-defined predicates defined
in pl-attvar.c
*/

%!  '$wakeup'(+List)
%
%   Called from the kernel if assignments have been made to
%   attributed variables.

'$wakeup'([]).
'$wakeup'(wakeup(Attribute, Value, Rest)) :-
    call_all_attr_uhooks(Attribute, Value),
    '$wakeup'(Rest).

call_all_attr_uhooks([], _).
call_all_attr_uhooks(att(Module, AttVal, Rest), Value) :-
    uhook(Module, AttVal, Value),
    call_all_attr_uhooks(Rest, Value).


%!  uhook(+AttributeName, +AttributeValue, +Value)
%
%   Run the unify hook for attributed named AttributeName after
%   assigning an attvar with attribute AttributeValue the value
%   Value.
%
%   This predicate deals with reserved attribute names to avoid
%   the meta-call overhead.

uhook(freeze, Goal, Y) :-
    !,
    (   attvar(Y)
    ->  (   get_attr(Y, freeze, G2)
        ->  put_attr(Y, freeze, '$and'(G2, Goal))
        ;   put_attr(Y, freeze, Goal)
        )
    ;   unfreeze(Goal)
    ).
uhook(Module, AttVal, Value) :-
    Module:attr_unify_hook(AttVal, Value).


%!  unfreeze(+ConjunctionOrGoal)
%
%   Handle  unfreezing  of  conjunctions.  As  meta-calling  control
%   structures is slower than meta-interpreting them   we do this in
%   Prolog. Another advantage is that   having unfreeze/1 in between
%   makes the stacktrace and profiling   easier  to intepret. Please
%   note that we cannot use a direct conjunction as this would break
%   freeze(X, (a, !, b)).

unfreeze('$and'(A,B)) :-
    !,
    unfreeze(A),
    unfreeze(B).
unfreeze(Goal) :-
    Goal.

%!  freeze(@Var, :Goal)
%
%   Suspend execution of Goal until Var is unbound.

:- meta_predicate
    freeze(?, 0).

freeze(Var, Goal) :-
    '$freeze'(Var, Goal),
    !.        % Succeeds if delayed
freeze(_, Goal) :-
    Goal.

%!  frozen(@Var, -Goals)
%
%   Unify Goals with the goals frozen on Var or true if no
%   goals are grozen on Var.

frozen(Var, Goals) :-
    get_attr(Var, freeze, Goals0),
    !,
    make_conjunction(Goals0, Goals).
frozen(_, true).

make_conjunction('$and'(A0, B0), (A, B)) :-
    !,
    make_conjunction(A0, A),
    make_conjunction(B0, B).
make_conjunction(G, G).


                 /*******************************
                 *             PORTRAY          *
                 *******************************/

%!  portray_attvar(@Var)
%
%   Called from write_term/3 using the option attributes(portray) or
%   when the prolog flag write_attributes   equals portray. Its task
%   is the write the attributes in a human readable format.

:- public
    portray_attvar/1.

portray_attvar(Var) :-
    write('{'),
    get_attrs(Var, Attr),
    portray_attrs(Attr, Var),
    write('}').

portray_attrs([], _).
portray_attrs(att(Name, Value, Rest), Var) :-
    portray_attr(Name, Value, Var),
    (   Rest == []
    ->  true
    ;   write(', '),
        portray_attrs(Rest, Var)
    ).

portray_attr(freeze, Goal, Var) :-
    !,
    format('freeze(~w, ~W)', [ Var, Goal,
                               [ portray(true),
                                 quoted(true),
                                 attributes(ignore)
                               ]
                             ]).
portray_attr(Name, Value, Var) :-
    G = Name:attr_portray_hook(Value, Var),
    (   '$c_current_predicate'(_, G),
        G
    ->  true
    ;   format('~w = ...', [Name])
    ).


                 /*******************************
                 *          CALL RESIDUE        *
                 *******************************/

%!  call_residue_vars(:Goal, -Vars)
%
%   If Goal is  true,  Vars  is   the  set  of  residual  attributed
%   variables created by Goal. Goal  is   called  as in call/1. This
%   predicate  is  for  debugging  constraint   programs.  Assume  a
%   constraint program that creates  conflicting   constraints  on a
%   variable that is not part of the   result  variables of Goal. If
%   the solver is powerful enough it   will  detect the conflict and
%   fail. If the solver is too  weak   however  it  will succeed and
%   residual attributed variables holding the conflicting constraint
%   form a witness of this problem.

:- meta_predicate
    call_residue_vars(0, -).

call_residue_vars(Goal, Vars) :-
    prolog_current_choice(Chp),
    setup_call_cleanup(
        '$call_residue_vars_start',
        run_crv(Goal, Chp, Vars, Det),
        '$call_residue_vars_end'),
    (   Det == true
    ->  !
    ;   true
    ).
call_residue_vars(_, _) :-
    fail.

run_crv(Goal, Chp, Vars, Det) :-
    call(Goal),
    deterministic(Det),
    '$attvars_after_choicepoint'(Chp, Vars).

%!  copy_term(+Term, -Copy, -Gs) is det.
%
%   Creates a regular term Copy  as  a   copy  of  Term (without any
%   attributes), and a list Gs of goals that when executed reinstate
%   all attributes onto Copy. The nonterminal attribute_goals//1, as
%   defined in the modules the  attributes   stem  from,  is used to
%   convert attributes to lists of goals.

copy_term(Term, Copy, Gs) :-
    term_attvars(Term, Vs),
    (   Vs == []
    ->  Gs = [],
        copy_term(Term, Copy)
    ;   findall(Term-Gs,
                ( phrase(attvars_residuals(Vs), Gs),
                  delete_attributes(Term)
                ),
                [Copy-Gs])
    ).

attvars_residuals([]) --> [].
attvars_residuals([V|Vs]) -->
    (   { get_attrs(V, As) }
    ->  attvar_residuals(As, V)
    ;   []
    ),
    attvars_residuals(Vs).

attvar_residuals([], _) --> [].
attvar_residuals(att(Module,Value,As), V) -->
    (   { nonvar(V) }
    ->  % a previous projection predicate could have instantiated
        % this variable, for example, to avoid redundant goals
        []
    ;   (   { Module == freeze }
        ->  frozen_residuals(Value, V)
        ;   { current_predicate(Module:attribute_goals//1),
              phrase(Module:attribute_goals(V), Goals)
            }
        ->  list(Goals)
        ;   [put_attr(V, Module, Value)]
        )
    ),
    attvar_residuals(As, V).

list([])     --> [].
list([L|Ls]) --> [L], list(Ls).

delete_attributes(Term) :-
    term_attvars(Term, Vs),
    delete_attributes_(Vs).

delete_attributes_([]).
delete_attributes_([V|Vs]) :-
    del_attrs(V),
    delete_attributes_(Vs).


%!  frozen_residuals(+FreezeAttr, +Var)// is det.
%
%   Instantiate  a  freeze  goal  for  each    member  of  the  $and
%   conjunction. Note that we cannot  map   this  into a conjunction
%   because  freeze(X,  a),  freeze(X,  !)  would  create  freeze(X,
%   (a,!)),  which  is  fundamentally  different.  We  could  create
%   freeze(X,  (call(a),  call(!)))  or  preform  a  more  eleborate
%   analysis to validate the semantics are not changed.

frozen_residuals('$and'(X,Y), V) -->
    !,
    frozen_residuals(X, V),
    frozen_residuals(Y, V).
frozen_residuals(X, V) -->
    [ freeze(V, X) ].
