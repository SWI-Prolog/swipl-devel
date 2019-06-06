/*  Part of SWI-Prolog

    Author:        Paulo Moura
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2015, Paulo Moura, Kyndi Inc., VU University Amsterdam
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

:- module(yall,
          [ (>>)/2, (>>)/3, (>>)/4, (>>)/5, (>>)/6, (>>)/7, (>>)/8, (>>)/9,
            (/)/2, (/)/3, (/)/4, (/)/5, (/)/6, (/)/7, (/)/8, (/)/9,

            lambda_calls/2,                     % +LambdaExt, -Goal
            lambda_calls/3,                     % +Lambda, +Args, -Goal
            is_lambda/1                         % @Term
          ]).
:- use_module(library(error)).
:- use_module(library(lists)).

:- meta_predicate
    '>>'(?, 0),
    '>>'(?, :, ?),
    '>>'(?, :, ?, ?),
    '>>'(?, :, ?, ?, ?),
    '>>'(?, :, ?, ?, ?, ?),
    '>>'(?, :, ?, ?, ?, ?, ?),
    '>>'(?, :, ?, ?, ?, ?, ?, ?),
    '>>'(?, :, ?, ?, ?, ?, ?, ?, ?).

:- meta_predicate
    '/'(?, 0),
    '/'(?, 1, ?),
    '/'(?, 2, ?, ?),
    '/'(?, 3, ?, ?, ?),
    '/'(?, 4, ?, ?, ?, ?),
    '/'(?, 5, ?, ?, ?, ?, ?),
    '/'(?, 6, ?, ?, ?, ?, ?, ?),
    '/'(?, 7, ?, ?, ?, ?, ?, ?, ?).

/** <module> Lambda expressions

Prolog realizes _high-order_ programming  with   meta-calling.  The core
predicate of this is call/1, which simply   calls its argument. This can
be used to define higher-order predicates  such as ignore/1 or forall/2.
The call/N construct calls a _closure_  with N-1 _additional arguments_.
This is used to define  higher-order   predicates  such as the maplist/N
family or foldl/N.

The problem with higher order predicates  based   on  call/N is that the
additional arguments are always  added  to   the  end  of  the closure's
argument list. This often requires defining trivial helper predicates to
get the argument order right. For example, if   you want to add a common
postfix    to    a    list    of    atoms     you    need    to    apply
atom_concat(In,Postfix,Out),   but    maplist(x(PostFix),ListIn,ListOut)
calls x(PostFix,In,Out). This is where  this   library  comes  in, which
allows us to write

  ==
  ?- maplist([In,Out]>>atom_concat(In,'_p',Out), [a,b], ListOut).
  ListOut = [a_p, b_p].
  ==

The `{...}` specifies which variables are   _shared_  between the lambda
and the context. This allows us  to   write  the code below. Without the
`{PostFix}` a free variable would be passed to atom_concat/3.

  ==
  add_postfix(PostFix, ListIn, ListOut) :-
      maplist({PostFix}/[In,Out]>>atom_concat(In,PostFix,Out),
              ListIn, ListOut).
  ==

This introduces the second application area   of lambda expressions: the
ability to stop binding variables in   the context. This features shines
when combined with bagof/3 or setof/3 where you normally have to specify
the the variables in whose binding you   are  _not_ interested using the
`Var^Goal` construct (marking `Var` as  existential quantified). Lambdas
allow doing the  reverse:  specify  the   variables  in  which  you  are
interested.

Lambda expressions use the syntax below

  ==
  {...}/[...]>>Goal.
  ==

The `{...}` optional  part is used for lambda-free  variables. The order
of variables doesn't matter hence the `{...}` set notation.

The  `[...]`  optional  part  lists lambda  parameters.  Here  order  of
variables matters hence the list notation.

As `/` and `>>` are standard infix operators, no new operators are added
by this  library.  An  advantage of  this syntax is  that we  can simply
unify a lambda expression with Free/Parameters>>Lambda to access each of
its  components. Spaces  in  the  lambda expression  are  not a  problem
although the goal  may need to be written between  ()'s.  Goals that are
qualified by a module prefix also need to be wrapped inside parentheses.

Combined  with  library(apply_macros),  library(yall)    allows  writing
one-liners for many list operations that   have  the same performance as
hand written code.

The module name, _yall_, stands for Yet Another Lambda Library.

This  module  implements  Logtalk's   lambda  expressions  syntax.   The
development of this module was sponsored by Kyndi, Inc.

@tbd    Extend optimization support
@author Paulo Moura and Jan Wielemaker
*/

%!  >>(+Parameters, +Lambda).
%!  >>(+Parameters, +Lambda, ?A1).
%!  >>(+Parameters, +Lambda, ?A1, ?A2).
%!  >>(+Parameters, +Lambda, ?A1, ?A2, ?A3).
%!  >>(+Parameters, +Lambda, ?A1, ?A2, ?A3, ?A4).
%!  >>(+Parameters, +Lambda, ?A1, ?A2, ?A3, ?A4, ?A5).
%!  >>(+Parameters, +Lambda, ?A1, ?A2, ?A3, ?A4, ?A5, ?A6).
%!  >>(+Parameters, +Lambda, ?A1, ?A2, ?A3, ?A4, ?A5, ?A6, ?A7).
%
%   Calls a copy of Lambda. This  is similar to call(Lambda,A1,...),
%   but arguments are reordered according to the list Parameters:
%
%     - The first length(Parameters) arguments from A1, ... are
%       unified with (a copy of) Parameters, which _may_ share
%       them with variables in Lambda.
%     - Possible excess arguments are passed by position.
%
%   @arg    Parameters is either a plain list of parameters or a term
%           `{Free}/List`. `Free` represents variables that are
%           shared between the context and the Lambda term.  This
%           is needed for compiling Lambda expressions.

'>>'(Parms, Lambda) :-
    unify_lambda_parameters(Parms, [],
                            ExtraArgs, Lambda, LambdaCopy),
    Goal =.. [call, LambdaCopy| ExtraArgs],
    call(Goal).

'>>'(Parms, Lambda, A1) :-
    unify_lambda_parameters(Parms, [A1],
                            ExtraArgs, Lambda, LambdaCopy),
    Goal =.. [call, LambdaCopy| ExtraArgs],
    call(Goal).

'>>'(Parms, Lambda, A1, A2) :-
    unify_lambda_parameters(Parms, [A1,A2],
                            ExtraArgs, Lambda, LambdaCopy),
    Goal =.. [call, LambdaCopy| ExtraArgs],
    call(Goal).

'>>'(Parms, Lambda, A1, A2, A3) :-
    unify_lambda_parameters(Parms, [A1,A2,A3],
                            ExtraArgs, Lambda, LambdaCopy),
    Goal =.. [call, LambdaCopy| ExtraArgs],
    call(Goal).

'>>'(Parms, Lambda, A1, A2, A3, A4) :-
    unify_lambda_parameters(Parms, [A1,A2,A3,A4],
                            ExtraArgs, Lambda, LambdaCopy),
    Goal =.. [call, LambdaCopy| ExtraArgs],
    call(Goal).

'>>'(Parms, Lambda, A1, A2, A3, A4, A5) :-
    unify_lambda_parameters(Parms, [A1,A2,A3,A4,A5],
                            ExtraArgs, Lambda, LambdaCopy),
    Goal =.. [call, LambdaCopy| ExtraArgs],
    call(Goal).

'>>'(Parms, Lambda, A1, A2, A3, A4, A5, A6) :-
    unify_lambda_parameters(Parms, [A1,A2,A3,A4,A5,A6],
                            ExtraArgs, Lambda, LambdaCopy),
    Goal =.. [call, LambdaCopy| ExtraArgs],
    call(Goal).

'>>'(Parms, Lambda, A1, A2, A3, A4, A5, A6, A7) :-
    unify_lambda_parameters(Parms, [A1,A2,A3,A4,A5,A6,A7],
                            ExtraArgs, Lambda, LambdaCopy),
    Goal =.. [call, LambdaCopy| ExtraArgs],
    call(Goal).

%!  /(+Free, :Lambda).
%!  /(+Free, :Lambda, ?A1).
%!  /(+Free, :Lambda, ?A1, ?A2).
%!  /(+Free, :Lambda, ?A1, ?A2, ?A3).
%!  /(+Free, :Lambda, ?A1, ?A2, ?A3, ?A4).
%!  /(+Free, :Lambda, ?A1, ?A2, ?A3, ?A4, ?A5).
%!  /(+Free, :Lambda, ?A1, ?A2, ?A3, ?A4, ?A5, ?A6).
%!  /(+Free, :Lambda, ?A1, ?A2, ?A3, ?A4, ?A5, ?A6, ?A7).
%
%   Shorthand for `Free/[]>>Lambda`.  This is the same as applying
%   call/N on Lambda, except that only variables appearing in Free
%   are bound by the call.  For example
%
%     ==
%     p(1,a).
%     p(2,b).
%
%     ?- {X}/p(X,Y).
%     X = 1;
%     X = 2.
%     ==
%
%   This can in particularly be combined with bagof/3 and setof/3 to
%   _select_ particular variables to be  concerned rather than using
%   existential quantification (^/2)  to   _exclude_  variables. For
%   example, the two calls below are equivalent.
%
%     ==
%     setof(X, Y^p(X,Y), Xs)
%     setof(X, {X}/p(X,_), Xs)
%     ==


'/'(Free, Lambda) :-
    lambda_free(Free),
    copy_term_nat(Free+Lambda, Free+LambdaCopy),
    call(LambdaCopy).

'/'(Free, Lambda, A1) :-
    lambda_free(Free),
    copy_term_nat(Free+Lambda, Free+LambdaCopy),
    call(LambdaCopy, A1).

'/'(Free, Lambda, A1, A2) :-
    lambda_free(Free),
    copy_term_nat(Free+Lambda, Free+LambdaCopy),
    call(LambdaCopy, A1, A2).

'/'(Free, Lambda, A1, A2, A3) :-
    lambda_free(Free),
    copy_term_nat(Free+Lambda, Free+LambdaCopy),
    call(LambdaCopy, A1, A2, A3).

'/'(Free, Lambda, A1, A2, A3, A4) :-
    lambda_free(Free),
    copy_term_nat(Free+Lambda, Free+LambdaCopy),
    call(LambdaCopy, A1, A2, A3, A4).

'/'(Free, Lambda, A1, A2, A3, A4, A5) :-
    lambda_free(Free),
    copy_term_nat(Free+Lambda, Free+LambdaCopy),
    call(LambdaCopy, A1, A2, A3, A4, A5).

'/'(Free, Lambda, A1, A2, A3, A4, A5, A6) :-
    lambda_free(Free),
    copy_term_nat(Free+Lambda, Free+LambdaCopy),
    call(LambdaCopy, A1, A2, A3, A4, A5, A6).

'/'(Free, Lambda, A1, A2, A3, A4, A5, A6, A7) :-
    lambda_free(Free),
    copy_term_nat(Free+Lambda, Free+LambdaCopy),
    call(LambdaCopy, A1, A2, A3, A4, A5, A6, A7).


%!  unify_lambda_parameters(+ParmsAndFree, +Args, -CallArgs,
%!                          +Lambda, -LambdaCopy) is det.
%
%   @arg ParmsAndFree is the first argumen of `>>`, either a list
%        of parameters or a term `{Free}/Params`.
%   @arg Args is a list of input parameters, args 3.. from `>>`
%   @arg CallArgs are the calling arguments for the Lambda
%        expression.  I.e., we call call(LambdaCopy, CallArgs).

unify_lambda_parameters(Parms, _Args, _ExtraArgs, _Lambda, _LambdaCopy) :-
    var(Parms),
    !,
    instantiation_error(Parms).
unify_lambda_parameters(Free/Parms, Args, ExtraArgs, Lambda, LambdaCopy) :-
    !,
    lambda_free(Free),
    must_be(list, Parms),
    copy_term_nat(Free/Parms>>Lambda, Free/ParmsCopy>>LambdaCopy),
    unify_lambda_parameters_(ParmsCopy, Args, ExtraArgs,
                             Free/Parms>>Lambda).
unify_lambda_parameters(Parms, Args, ExtraArgs, Lambda, LambdaCopy) :-
    must_be(list, Parms),
    copy_term_nat(Parms>>Lambda, ParmsCopy>>LambdaCopy),
    unify_lambda_parameters_(ParmsCopy, Args, ExtraArgs,
                             Parms>>Lambda).

unify_lambda_parameters_([], ExtraArgs, ExtraArgs, _) :- !.
unify_lambda_parameters_([Parm|Parms], [Arg|Args], ExtraArgs, Culprit) :-
    !,
    Parm = Arg,
    unify_lambda_parameters_(Parms, Args, ExtraArgs, Culprit).
unify_lambda_parameters_(_,_,_,Culprit) :-
    domain_error(lambda_parameters, Culprit).

lambda_free(Free) :-
    var(Free),
    !,
    instantiation_error(Free).
lambda_free({_}) :- !.
lambda_free({}) :- !.
lambda_free(Free) :-
    type_error(lambda_free, Free).

%!  expand_lambda(+Goal, -Head) is semidet.
%
%   True if Goal is a   sufficiently  instantiated Lambda expression
%   that is compiled to the predicate   Head.  The predicate Head is
%   added    to    the    current    compilation    context    using
%   compile_aux_clauses/1.

expand_lambda(Goal, Head) :-
    Goal =.. ['>>', Parms, Lambda| ExtraArgs],
    is_callable(Lambda),
    nonvar(Parms),
    lambda_functor(Parms>>Lambda, Functor),
    (   Parms = Free/ExtraArgs
    ->  is_lambda_free(Free),
        free_to_list(Free, FreeList)
    ;   Parms = ExtraArgs,
        FreeList = []
    ),
    append(FreeList, ExtraArgs, Args),
    Head =.. [Functor|Args],
    compile_aux_clause_if_new(Head, Lambda).
expand_lambda(Goal, Head) :-
    Goal =.. ['/', Free, Closure|ExtraArgs],
    is_lambda_free(Free),
    is_callable(Closure),
    free_to_list(Free, FreeList),
    lambda_functor(Free/Closure, Functor),
    append(FreeList, ExtraArgs, Args),
    Head =.. [Functor|Args],
    Closure =.. [ClosureFunctor|ClosureArgs],
    append(ClosureArgs, ExtraArgs, LambdaArgs),
    Lambda =.. [ClosureFunctor|LambdaArgs],
    compile_aux_clause_if_new(Head, Lambda).

lambda_functor(Term, Functor) :-
    copy_term_nat(Term, Copy),
    variant_sha1(Copy, Functor0),
    atom_concat('__aux_yall_', Functor0, Functor).

free_to_list({}, []).
free_to_list({VarsConj}, Vars) :-
    conjunction_to_list(VarsConj, Vars).

conjunction_to_list(Term, [Term]) :-
    var(Term),
    !.
conjunction_to_list((Term, Conjunction), [Term|Terms]) :-
    !,
    conjunction_to_list(Conjunction, Terms).
conjunction_to_list(Term, [Term]).

compile_aux_clause_if_new(Head, Lambda) :-
    prolog_load_context(module, Context),
    (   predicate_property(Context:Head, defined)
    ->  true
    ;   expand_goal(Lambda, LambdaExpanded),
        compile_aux_clauses([(Head :- LambdaExpanded)])
    ).

lambda_like(Goal) :-
    compound(Goal),
    compound_name_arity(Goal, Name, Arity),
    lambda_functor(Name),
    Arity >= 2.

lambda_functor(>>).
lambda_functor(/).

:- dynamic system:goal_expansion/2.
:- multifile system:goal_expansion/2.

system:goal_expansion(Goal, Head) :-
    lambda_like(Goal),
    prolog_load_context(source, _),
    \+ current_prolog_flag(xref, true),
    expand_lambda(Goal, Head).

%!  is_lambda(@Term) is semidet.
%
%   True if Term is a valid Lambda expression.

is_lambda(Term) :-
    compound(Term),
    compound_name_arguments(Term, Name, Args),
    is_lambda(Name, Args).

is_lambda(>>, [Params,Lambda|_]) :-
    is_lamdba_params(Params),
    is_callable(Lambda).
is_lambda(/, [Free,Lambda|_]) :-
    is_lambda_free(Free),
    is_callable(Lambda).

is_lamdba_params(Var) :-
    var(Var), !, fail.
is_lamdba_params(Free/Params) :-
    !,
    is_lambda_free(Free),
    is_list(Params).
is_lamdba_params(Params) :-
    is_list(Params).

is_lambda_free(Free) :-
    nonvar(Free), !, (Free = {_} -> true ; Free == {}).

is_callable(Term) :-
    strip_module(Term, _, Goal),
    callable(Goal).


%!  lambda_calls(+LambdaExpression, -Goal) is det.
%!  lambda_calls(+LambdaExpression, +ExtraArgs, -Goal) is det.
%
%   Goal  is  the   goal   called   if    call/N   is   applied   to
%   LambdaExpression, where ExtraArgs are   the additional arguments
%   to call/N. ExtraArgs can be an  integer   or  a list of concrete
%   arguments. This predicate is used for cross-referencing and code
%   highlighting.

lambda_calls(LambdaExtended, Goal) :-
    compound(LambdaExtended),
    compound_name_arguments(LambdaExtended, Name, [A1,A2|Extra]),
    lambda_functor(Name),
    compound_name_arguments(Lambda, Name, [A1,A2]),
    lambda_calls(Lambda, Extra, Goal).

lambda_calls(Lambda, Extra, Goal) :-
    integer(Extra),
    !,
    length(ExtraVars, Extra),
    lambda_calls_(Lambda, ExtraVars, Goal).
lambda_calls(Lambda, Extra, Goal) :-
    must_be(list, Extra),
    lambda_calls_(Lambda, Extra, Goal).

lambda_calls_(Params>>Lambda, Args, Goal) :-
    unify_lambda_parameters(Params, Args, ExtraArgs, Lambda, LambdaCopy),
    extend(LambdaCopy, ExtraArgs, Goal).
lambda_calls_(Free/Lambda, ExtraArgs, Goal) :-
    copy_term_nat(Free+Lambda, Free+LambdaCopy),
    extend(LambdaCopy, ExtraArgs, Goal).

extend(Var, _, _) :-
    var(Var),
    !,
    instantiation_error(Var).
extend(Cyclic, _, _) :-
    cyclic_term(Cyclic),
    !,
    type_error(acyclic_term, Cyclic).
extend(M:Goal0, Extra, M:Goal) :-
    !,
    extend(Goal0, Extra, Goal).
extend(Goal0, Extra, Goal) :-
    atom(Goal0),
    !,
    Goal =.. [Goal0|Extra].
extend(Goal0, Extra, Goal) :-
    compound(Goal0),
    !,
    compound_name_arguments(Goal0, Name, Args0),
    append(Args0, Extra, Args),
    compound_name_arguments(Goal, Name, Args).


                 /*******************************
                 *     SYNTAX HIGHLIGHTING      *
                 *******************************/

:- multifile prolog_colour:goal_colours/2.

yall_colours(Lambda, built_in-[classify,body(Goal)|ArgSpecs]) :-
    catch(lambda_calls(Lambda, Goal), _, fail),
    Lambda =.. [>>,_,_|Args],
    classify_extra(Args, ArgSpecs).

classify_extra([], []).
classify_extra([_|T0], [classify|T]) :-
    classify_extra(T0, T).

prolog_colour:goal_colours(Goal, Spec) :-
    lambda_like(Goal),
    yall_colours(Goal, Spec).


                 /*******************************
                 *          XREF SUPPORT        *
                 *******************************/

:- multifile prolog:called_by/4.

prolog:called_by(Lambda, yall, _, [Goal]) :-
    lambda_like(Lambda),
    catch(lambda_calls(Lambda, Goal), _, fail).


                 /*******************************
                 *        SANDBOX SUPPORT       *
                 *******************************/

:- multifile
    sandbox:safe_meta_predicate/1,
    sandbox:safe_meta/2.

sandbox:safe_meta_predicate(yall:(/)/2).
sandbox:safe_meta_predicate(yall:(/)/3).
sandbox:safe_meta_predicate(yall:(/)/4).
sandbox:safe_meta_predicate(yall:(/)/5).
sandbox:safe_meta_predicate(yall:(/)/6).
sandbox:safe_meta_predicate(yall:(/)/7).

sandbox:safe_meta(yall:Lambda, [Goal]) :-
    compound(Lambda),
    compound_name_arity(Lambda, >>, Arity),
    Arity >= 2,
    lambda_calls(Lambda, Goal).
