/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org
    Copyright (c)  2025, SWI-Prolog Solutions b.v.
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

:- module(argnames,
          [ argnames_property/2,        % :NameOrTerm, ?Property
            op(650, xfx, of)            % Key of Argnames
          ]).
:- use_module(library(error)).
:- use_module(library(occurs)).
:- use_module(library(terms)).

:- meta_predicate
    argnames_property(:, ?).

/** <module> argnames support

This library provides additional argnames  support. Notably, it provides
term-expansion for `Property of Argnames` terms.
*/

%!  argnames_property(:NameOrTerm, ?Property) is nondet.
%
%   True when Property is a property   of the known argnames declaration
%   NameOrTerm.  Defined properties are:
%
%     - arity(?Integer)
%       Number of argument of the compound term.
%     - functor(?Functor)
%       `Name/Arity` term expressing the functor of the compound term.
%     - exported(?Boolean)
%       True when the argnames is exported from the current module.
%     - imported_from(-Module)
%       The module from which the argnames is imported.

argnames_property(M:Term, Prop),
    compound(Term) =>
    compound_name_arity(Term, Name, _),
    argnames_property_(Prop, M:Name).
argnames_property(M:Name, Prop) =>
    current_argnames(Name, M:_),
    argnames_property_(Prop, M:Name).

argnames_property_(arity(Arity), Name) :-
    '$argnames_property'(Name, arity, Arity).
argnames_property_(functor(Functor), Name) :-
    '$argnames_property'(Name, functor, Functor).
argnames_property_(exported(Exported), Name) :-
    '$argnames_property'(Name, exported, Exported).
argnames_property_(imported_from(Module), Name) :-
    '$argnames_property'(Name, imported, Module).


                /*******************************
                *        TERM EXPANSION        *
                *******************************/

:- multifile
    system:term_expansion/2,
    system:goal_expansion/2.

argnames_term_expansion(GoalIn, GoalOut) :-
    sub_term(Sub, GoalIn),
    nonvar(Sub),
    Sub = (_ of _),
    !,
    mapsubterms(map_argnames, GoalIn, GoalOut),
    GoalIn \== GoalOut.

map_argnames(Prop of ArgNames, Expanded) =>
    prolog_load_context(module, M),
    expand_property(Prop, M:ArgNames, Expanded).
map_argnames(_, _) =>
    fail.

expand_property(Key, M:ArgNames, Arg), atom(Key) =>
    (   current_argnames(ArgNames, M:Decl)
    ->  (   arg(Arg0, Decl, Key)
        ->  Arg = Arg0
        ;   existence_error(argnames, Key, M:ArgNames)
        )
    ;   existence_error(argnames, M:ArgNames)
    ).
expand_property(property(arity), ArgNames, Arity) =>
    argnames_property(ArgNames, arity(Arity)).
expand_property(property(functor), ArgNames, Functor) =>
    argnames_property(ArgNames, functor(Functor)).

system:term_expansion((Head0 :- Body), Clause) :-
    argnames_term_expansion(Head0, Head),
    Clause = (Head:-Body).

system:goal_expansion(GoalIn, GoalOut) :-
    argnames_term_expansion(GoalIn, GoalOut).
