/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2011-2016, VU University Amsterdam
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

:- module(varnumbers,
          [ numbervars/1,                       % +Term
            varnumbers/2,                       % +Term, -Copy
            max_var_number/3,                   % +Term, +Start, -Max
            varnumbers/3,                       % +Term, +No, -Copy
            varnumbers_names/3                  % +Term, -Copy, -VariableNames
          ]).
:- use_module(library(error)).
:- use_module(library(assoc)).
:- use_module(library(apply)).

/** <module> Utilities for numbered terms

This  library  provides  the  inverse   functionality  of  the  built-in
numbervars/3. Note that this library suffers  from the known issues that
'$VAR'(X) is a normal Prolog term and, -unlike the built-in numbervars-,
the inverse predicates do _not_  process   cyclic  terms.  The following
predicate is true for  any  acyclic   term  that  contains no '$VAR'(X),
integer(X) terms and no constraint variables:

  ==
  always_true(X) :-
        copy_term(X, X2),
        numbervars(X),
        varnumbers(X, Copy),
        Copy =@= X2.
  ==

@see    numbervars/4, =@=/2 (variant/2).
@compat This library was introduced by Quintus and available in
        many related implementations, although not with exactly the
        same set of predicates.
*/

%!  numbervars(+Term) is det.
%
%   Number  variables  in  Term   using    $VAR(N).   Equivalent  to
%   numbervars(Term, 0, _).
%
%   @see numbervars/3, numbervars/4

numbervars(Term) :-
    numbervars(Term, 0, _).

%!  varnumbers(+Term, -Copy) is det.
%
%   Inverse  of  numbervars/1.  Equivalent  to  varnumbers(Term,  0,
%   Copy).

varnumbers(Term, Copy) :-
    varnumbers(Term, 0, Copy).

%!  varnumbers(+Term, +Start, -Copy) is det.
%
%   Inverse of numbervars/3. True when Copy is   a copy of Term with
%   all variables numbered >= Start   consistently replaced by fresh
%   variables. Variables in Term are _shared_  with Copy rather than
%   replaced by fresh variables.
%
%   @error domain_error(acyclic_term, Term) if Term is cyclic.
%   @compat Quintus, SICStus.  Not in YAP version of this library

varnumbers(Term, Min, Copy) :-
    must_be(acyclic, Term),
    MaxStart is Min-1,
    max_var_number(Term, MaxStart, Max),
    NVars is Max-MaxStart,
    (   NVars == 0
    ->  Copy = Term
    ;   roundup_next_power_two(NVars, Len),
        functor(Vars, v, Len),
        varnumbers(Term, MaxStart, Vars, Copy)
    ).

varnumbers(Var, _, _, Copy) :-
    var(Var),
    !,
    Copy = Var.
varnumbers(Var, _, _, Copy) :-
    atomic(Var),
    !,
    Copy = Var.
varnumbers('$VAR'(I), Min, Vars, Copy) :-
    integer(I),
    I > Min,
    !,
    Index is I-Min,
    arg(Index, Vars, Copy).
varnumbers(Term, Min, Vars, Copy) :-
    functor(Term, Name, Arity),
    functor(Copy, Name, Arity),
    varnumbers_args(1, Arity, Term, Min, Vars, Copy).

varnumbers_args(I, Arity, Term, Min, Vars, Copy) :-
    I =< Arity,
    !,
    arg(I, Term, AT),
    arg(I, Copy, CT),
    varnumbers(AT, Min, Vars, CT),
    I2 is I + 1,
    varnumbers_args(I2, Arity, Term, Min, Vars, Copy).
varnumbers_args(_, _, _, _, _, _).

%!  roundup_next_power_two(+Int, -NextPower) is det.
%
%   NextPower is I**2, such that NextPower >= Int.

roundup_next_power_two(1, 1) :- !.
roundup_next_power_two(N, L) :-
    L is 1<<(msb(N-1)+1).

%!  max_var_number(+Term, +Start, -Max) is det.
%
%   True when Max is the  max  of   Start  and  the highest numbered
%   $VAR(N) term.
%
%   @author Vitor Santos Costa
%   @compat YAP

max_var_number(V, Max, Max) :-
    var(V),
    !.
max_var_number('$VAR'(I), Max0, Max) :-
    integer(I),
    !,
    Max is max(I,Max0).
max_var_number(S, Max0, Max) :-
    functor(S, _, Ar),
    max_var_numberl(Ar, S, Max0, Max).

max_var_numberl(0, _, Max, Max) :- !.
max_var_numberl(I, T, Max0, Max) :-
    arg(I, T, Arg),
    I2 is I-1,
    max_var_number(Arg, Max0, Max1),
    max_var_numberl(I2, T, Max1, Max).

%!  varnumbers_names(+Term, -Copy, -VariableNames) is det.
%
%   If Term is a term with numbered   and  named variables using the
%   reserved term '$VAR'(X), Copy  is  a   copy  of  Term where each
%   '$VAR'(X) is consistently  replaced  by   a  fresh  variable and
%   Bindings is a list `X = Var`,   relating  the `X` terms with the
%   variable it is mapped to.
%
%   @see numbervars/3, varnumbers/3, read_term/3 using the
%   `variable_names` option.

varnumbers_names(Term, Copy, Bindings) :-
    must_be(acyclic, Term),
    empty_assoc(Named),
    varnumbers_names(Term, Named, BindingAssoc, Copy),
    assoc_to_list(BindingAssoc, BindingPairs),
    maplist(pair_equals, BindingPairs, Bindings).

pair_equals(N-V, N=V).

varnumbers_names(Var, Bindings, Bindings, Copy) :-
    var(Var),
    !,
    Copy = Var.
varnumbers_names(Var, Bindings, Bindings, Copy) :-
    atomic(Var),
    !,
    Copy = Var.
varnumbers_names('$VAR'(Name), Bindings0, Bindings, Copy) :-
    !,
    (   get_assoc(Name, Bindings0, Copy)
    ->  Bindings = Bindings0
    ;   put_assoc(Name, Bindings0, Copy, Bindings)
    ).
varnumbers_names(Term, Bindings0, Bindings, Copy) :-
    functor(Term, Name, Arity),
    functor(Copy, Name, Arity),
    varnumbers_names_args(1, Arity, Term, Bindings0, Bindings, Copy).

varnumbers_names_args(I, Arity, Term, Bindings0, Bindings, Copy) :-
    I =< Arity,
    !,
    arg(I, Term, AT),
    arg(I, Copy, CT),
    varnumbers_names(AT, Bindings0, Bindings1, CT),
    I2 is I + 1,
    varnumbers_names_args(I2, Arity, Term, Bindings1, Bindings, Copy).
varnumbers_names_args(_, _, _, Bindings, Bindings, _).
