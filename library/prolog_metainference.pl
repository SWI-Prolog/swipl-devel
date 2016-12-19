/*  Part of SWI-Prolog

    Author:        Eva Stoewe, Guenter Kniesel and Jan Wielemaker
    E-mail:        pdt@lists.iai.uni-bonn.de
    WWW:           http://sewiki.iai.uni-bonn.de/research/pdt/start
    Copyright (c)  2004-2012, CS Dept. III, University of Bonn
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

:- module(prolog_metainference,
          [ infer_meta_predicate/2,             % :Head, -MetaSpec
            inferred_meta_predicate/2           % :Head, ?MetaSpec
          ]).
:- use_module(library(lists)).
:- use_module(library(apply)).

:- meta_predicate
    inferred_meta_predicate(:, ?),
    infer_meta_predicate(:, -).

:- dynamic
    inferred_meta_pred/3.                   % Head, Module, Meta

/** <module> Infer meta-predicate properties

This module infers meta-predicate properties   by inspecting the clauses
of predicates that call other predicates.   This is extremely useful for
program analysis and refactoring because  many   programs  `in the wild'
have incomplete or incorrect meta-predicate information.

@see    This library is used by prolog_walk_code/1 to improve the
        accuracy of this analysis.
@tbd    Re-introduce some alias-analysis
@tbd    Not all missing meta-declarations are interesting.  Notably,
        meta-predicates that are private and only pass meta-arguments
        on behalve of a public meta-predicates do not need a declaration.
*/


%!  inferred_meta_predicate(:Head, ?MetaSpec) is nondet.
%
%   True when MetaSpec is an   inferred meta-predicate specification
%   for Head.

inferred_meta_predicate(M:Head, MetaSpec) :-
    inferred_meta_pred(Head, M, MetaSpec).
inferred_meta_predicate(M:Head, MetaSpec) :-
    predicate_property(M:Head, imported_from(From)),
    inferred_meta_pred(Head, From, MetaSpec).


%!  infer_meta_predicate(:Head, -MetaSpec) is semidet
%
%   True  when  MetaSpec  is  a  meta-predicate  specifier  for  the
%   predicate Head. Derived meta-predicates are   collected and made
%   available through inferred_meta_predicate/2.

infer_meta_predicate(Head, MetaSpec) :-
    inferred_meta_predicate(Head, MetaSpec),
    !.
infer_meta_predicate(M:Head, MetaSpec) :-
    predicate_property(M:Head, imported_from(From)),
    !,
    do_infer_meta_predicate(From:Head, MetaSpec),
    assertz(inferred_meta_pred(Head, From, MetaSpec)).
infer_meta_predicate(M:Head, MetaSpec) :-
    do_infer_meta_predicate(M:Head, MetaSpec),
    assertz(inferred_meta_pred(Head, M, MetaSpec)).

:- meta_predicate
    do_infer_meta_predicate(:, -).

do_infer_meta_predicate(Module:AHead, MetaSpec):-
    functor(AHead, Functor, Arity),
    functor(Head, Functor, Arity),  % Generalise the head
    findall(MetaSpec,
            meta_pred_args_in_clause(Module, Head, MetaSpec),
            MetaSpecs),
    MetaSpecs \== [],
    combine_meta_args(MetaSpecs, MetaSpec).


%!  meta_pred_args_in_clause(+Module, +Head, -MetaSpec) is nondet.

meta_pred_args_in_clause(Module, Head, MetaArgs) :-
    clause(Module:Head, Body),
    annotate_meta_vars_in_body(Body, Module),
    meta_annotation(Head, MetaArgs).


%!  annotate_meta_vars_in_body(+Term, +Module) is det
%
%   Annotate variables in Term if they appear as meta-arguments.
%
%   @tbd    Aliasing.  Previous code detected aliasing for
%           - =/2
%           - functor/3
%           - atom_concat/3
%           - =../2
%           - arg/3
%   @tbd    We can make this nondet, exploring multiple aliasing
%           paths in disjunctions.

annotate_meta_vars_in_body(A, _) :-
    atomic(A),
    !.
annotate_meta_vars_in_body(Var, _) :-
    var(Var),
    !,
    annotate(Var, 0).
annotate_meta_vars_in_body(Module:Term, _) :-
    !,
    (   atom(Module)
    ->  annotate_meta_vars_in_body(Term, Module)
    ;   var(Module)
    ->  annotate(Module, m)
    ;   true                        % may continue if Term is a system
    ).                              % predicate?
annotate_meta_vars_in_body((TermA, TermB), Module) :-
    !,
    annotate_meta_vars_in_body(TermB, Module),
    annotate_meta_vars_in_body(TermA, Module).
annotate_meta_vars_in_body((TermA; TermB), Module) :-
    !,
    annotate_meta_vars_in_body(TermB, Module),
    annotate_meta_vars_in_body(TermA, Module).
annotate_meta_vars_in_body((TermA->TermB), Module) :-
    !,
    annotate_meta_vars_in_body(TermB, Module),
    annotate_meta_vars_in_body(TermA, Module).
annotate_meta_vars_in_body((TermA*->TermB), Module) :-
    !,
    annotate_meta_vars_in_body(TermB, Module),
    annotate_meta_vars_in_body(TermA, Module).
annotate_meta_vars_in_body(A=B, _) :-
    var(A), var(B),
    !,
    A = B.
annotate_meta_vars_in_body(Goal, Module) :- % TBD: do we trust this?
    predicate_property(Module:Goal, meta_predicate(Head)),
    !,
    functor(Goal, _, Arity),
    annotate_meta_args(1, Arity, Goal, Head, Module).
annotate_meta_vars_in_body(Goal, Module) :-
    inferred_meta_predicate(Module:Goal, Head),
    !,
    functor(Goal, _, Arity),
    annotate_meta_args(1, Arity, Goal, Head, Module).
annotate_meta_vars_in_body(_, _).


%!  annotate_meta_args(+Arg, +Arity, +Goal, +MetaSpec, +Module)

annotate_meta_args(I, Arity, Goal, MetaSpec, Module) :-
    I =< Arity,
    !,
    arg(I, MetaSpec, MetaArg),
    arg(I, Goal, Arg),
    annotate_meta_arg(MetaArg, Arg, Module),
    I2 is I + 1,
    annotate_meta_args(I2, Arity, Goal, MetaSpec, Module).
annotate_meta_args(_, _, _, _, _).

annotate_meta_arg(Spec, Arg, _) :-
    var(Arg),
    !,
    annotate(Arg, Spec).
annotate_meta_arg(0, Arg, Module) :-
    !,
    annotate_meta_vars_in_body(Arg, Module).
annotate_meta_arg(N, Arg, Module) :-
    integer(N),
    callable(Arg),
    !,
    Arg =.. List,
    length(Extra, N),
    append(List, Extra, ListX),
    ArgX =.. ListX,
    annotate_meta_vars_in_body(ArgX, Module).
annotate_meta_arg(Spec, Arg, _) :-
    is_meta(Spec),
    compound(Arg),
    Arg = Module:_,
    var(Module),
    !,
    annotate(Module, m).
annotate_meta_arg(_,_,_).

annotate(Var, Annotation) :-
    get_attr(Var, prolog_metainference, Annot0),
    !,
    join_annotation(Annot0, Annotation, Joined),
    put_attr(Var, prolog_metainference, Joined).
annotate(Var, Annotation) :-
    put_attr(Var, prolog_metainference, Annotation).

join_annotation(A, A, A) :- !.
join_annotation(A, B, C) :-
    (   is_meta(A), \+ is_meta(B)
    ->  C = A
    ;   \+ is_meta(A), is_meta(B)
    ->  C = B
    ;   is_meta(A), is_meta(B)
    ->  C = (:)
    ;   C = *
    ).

attr_unify_hook(A0, Other) :-
    get_attr(Other, prolog_metainference, A1),
    !,
    join_annotation(A0, A1, A),
    put_attr(Other, prolog_metainference, A).


%!  meta_annotation(+Head, -Annotation) is semidet.
%
%   True when Annotation is an   appropriate  meta-specification for
%   Head.

meta_annotation(Head, Meta) :-
    functor(Head, Name, Arity),
    functor(Meta, Name, Arity),
    meta_args(1, Arity, Head, Meta, HasMeta),
    HasMeta == true.

meta_args(I, Arity, Head, Meta, HasMeta) :-
    I =< Arity,
    !,
    arg(I, Head, HeadArg),
    arg(I, Meta, MetaArg),
    meta_arg(HeadArg, MetaArg),
    (   is_meta(MetaArg)
    ->  HasMeta = true
    ;   true
    ),
    I2 is I + 1,
    meta_args(I2, Arity, Head, Meta, HasMeta).
meta_args(_, _, _, _, _).

is_meta(I) :- integer(I), !.
is_meta(:).
is_meta(^).
is_meta(//).

%!  meta_arg(+AnnotatedArg, -MetaSpec) is det.
%
%   True when MetaSpec is  a  proper   annotation  for  the argument
%   AnnotatedArg. This is simple if the argument is a plain argument
%   in the head (first clause). If it   is  a compound term, it must
%   unify to _:_, otherwise there is no point turning it into a meta
%   argument. If the  module  part  is   then  passed  to  a  module
%   sensitive predicate, we assume it is a meta-predicate.

meta_arg(HeadArg, MetaArg) :-
    get_attr(HeadArg, prolog_metainference, MetaArg),
    MetaArg \== m,
    !.
meta_arg(HeadArg, :) :-
    compound(HeadArg),
    HeadArg = M:_,
    get_attr(M, prolog_metainference, m),
    !.
meta_arg(_, *).

%!  combine_meta_args(+Heads, -Head) is det.
%
%   Combine multiple meta-specifications.

combine_meta_args([], []) :- !.
combine_meta_args([List], List) :- !.
combine_meta_args([Spec,Spec|Specs], CombinedArgs) :-
    !,
    combine_meta_args([Spec|Specs], CombinedArgs).
combine_meta_args([Spec1,Spec2|Specs], CombinedArgs) :-
    Spec1 =.. [Name|Args1],
    Spec2 =.. [Name|Args2],
    maplist(join_annotation, Args1, Args2, Args),
    Spec =.. [Name|Args],
    combine_meta_args([Spec|Specs], CombinedArgs).


