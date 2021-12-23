/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2019-2020, VU University Amsterdam
                              CWI, Amsterdam
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

:- module(prolog_code,
          [ comma_list/2,                       % (A,B) <-> [A,B]
            semicolon_list/2,                   % (A;B) <-> [A,B]

            mkconj/3,                           % +A, +B, -Conjunction
            mkdisj/3,                           % +A, +B, -Disjunction

            pi_head/2,                          % :PI, :Head
            head_name_arity/3,			% ?Goal, ?Name, ?Arity

            most_general_goal/2,                % :Goal, -General
            extend_goal/3,                      % :Goal, +Extra, -GoalOut

            predicate_label/2,                  % +PI, -Label
            predicate_sort_key/2,               % +PI, -Key

            is_control_goal/1,                  % @Term
            is_predicate_indicator/1,           % @Term

            body_term_calls/2                   % :BodyTerm, -Goal
          ]).
:- autoload(library(error),[must_be/2, instantiation_error/1]).
:- autoload(library(lists),[append/3]).

:- meta_predicate
    body_term_calls(:, -).

:- multifile
    user:prolog_predicate_name/2.

/** <module> Utilities for reasoning about code

This library collects utilities to reason   about  terms commonly needed
for reasoning about Prolog code. Note   that many related facilities can
be found in the core as well as other libraries:

  - =@=/2, subsumes_term/2, etc.
  - library(occurs)
  - library(listing)
  - library(prolog_source)
  - library(prolog_xref)
  - library(prolog_codewalk)
*/

%!  comma_list(?CommaList, ?List).
%!  semicolon_list(?SemicolonList, ?List).
%
%   True if CommaList is a nested term   over  the ','/2 (';'/2) functor
%   and List is a list expressing the   elements of the conjunction. The
%   predicate  is  deterministic  if  at  least  CommaList  or  List  is
%   sufficiently  instantiated.  If  both  are   partial  structures  it
%   enumerates ever growing conjunctions  and   lists.  CommaList may be
%   left or right associative on input. When generated, the CommaList is
%   always right associative.
%
%   This predicate is typically used to reason about Prolog conjunctions
%   (disjunctions) as many operations are easier on lists than on binary
%   trees over some operator.

comma_list(CommaList, List) :-
    phrase(binlist(CommaList, ','), List).
semicolon_list(CommaList, List) :-
    phrase(binlist(CommaList, ';'), List).

binlist(Term, Functor) -->
    { nonvar(Term) },
    !,
    (   { Term =.. [Functor,A,B] }
    ->  binlist(A, Functor),
        binlist(B, Functor)
    ;   [Term]
    ).
binlist(Term, Functor) -->
    [A],
    (   var_tail
    ->  (   { Term = A }
        ;   { Term =.. [Functor,A,B] },
            binlist(B,Functor)
        )
    ;   \+ [_]
    ->  {Term = A}
    ;   binlist(B,Functor),
        {Term =.. [Functor,A,B]}
    ).

var_tail(H, H) :-
    var(H).

%!  mkconj(A,B,Conj) is det.
%!  mkdisj(A,B,Disj) is det.
%
%   Create a conjunction or  disjunction  from   two  terms.  Reduces on
%   `true` (mkconj/2) and  `false`  (mkdisj/2).   Note  that  a  `false`
%   encountered in a conjunction does __not__   cause the conjunction to
%   be `false`, i.e. semantics under side effects are preserved.
%
%   The Prolog `,` and `;` operators  are   of  type  `xfy`, i.e. _right
%   associative_. These predicates preserve this grouping. For example,
%
%       ?- mkconj((a,b), c, Conj)
%       Conj = (a,b,c)

mkconj(A,B,Conj) :-
    (   is_true(A)
    ->  Conj = B
    ;   is_true(B)
    ->  Conj = A
    ;   mkconj_(A,B,Conj)
    ).

mkconj_((A,B), C, Conj) =>
    Conj = (A,C2),
    mkconj_(B,C,C2).
mkconj_(A, B, C) =>
    C = (A,B).

mkdisj(A,B,Disj) :-
    (   is_false(A)
    ->  Disj = B
    ;   is_false(B)
    ->  Disj = A
    ;   mkdisj_(A,B,Disj)
    ).

mkdisj_((A;B), C, Disj) =>
    Disj = (A;C2),
    mkdisj_(B, C, C2).
mkdisj_(A, B, C) =>
    C = (A;B).

is_true(Goal) :- Goal == true.
is_false(Goal) :- (Goal == false -> true ; Goal == fail).

%!  is_predicate_indicator(@Term) is semidet.
%
%   True when Term is a predicate indicator

is_predicate_indicator(Var) :-
    var(Var),
    !,
    instantiation_error(Var).
is_predicate_indicator(PI) :-
    strip_module(PI, M, PI1),
    atom(M),
    (   PI1 = (Name/Arity)
    ->  true
    ;   PI1 = (Name//Arity)
    ),
    atom(Name),
    integer(Arity),
    Arity >= 0.

%!  pi_head(?PredicateIndicator, ?Goal) is det.
%
%   Translate between a PredicateIndicator and a   Goal  term. The terms
%   may have a module qualification.
%
%   @error type_error(predicate_indicator, PredicateIndicator)

pi_head(PI, Head) :-
    '$pi_head'(PI, Head).

%!  head_name_arity(?Goal, ?Name, ?Arity) is det.
%
%   Similar to functor/3, but  deals   with  SWI-Prolog's  zero-argument
%   callable terms and avoids creating a   non-callable  term if Name is
%   not an atom and Arity is zero.

head_name_arity(Goal, Name, Arity) :-
    '$head_name_arity'(Goal, Name, Arity).

%!  most_general_goal(+Goal, -General) is det.
%
%   General is the most general version of Goal.  Goal can be qualified.
%
%   @see is_most_general_term/1.

most_general_goal(Goal, General) :-
    var(Goal),
    !,
    General = Goal.
most_general_goal(Goal, General) :-
    atom(Goal),
    !,
    General = Goal.
most_general_goal(M:Goal, M:General) :-
    !,
    most_general_goal(Goal, General).
most_general_goal(Compound, General) :-
    compound_name_arity(Compound, Name, Arity),
    compound_name_arity(General, Name, Arity).


%!  extend_goal(:Goal0, +Extra, -Goal) is det.
%
%   Extend the possibly qualified Goal0   with additional arguments from
%   Extra. If Goal0 is insufficiantly instantiated (i.e., a variable), a
%   term call(Goal0, ...) is returned.

extend_goal(Goal0, Extra, Goal) :-
    var(Goal0),
    !,
    Goal =.. [call,Goal0|Extra].
extend_goal(M:Goal0, Extra, M:Goal) :-
    extend_goal(Goal0, Extra, Goal).
extend_goal(Atom, Extra, Goal) :-
    atom(Atom),
    !,
    Goal =.. [Atom|Extra].
extend_goal(Goal0, Extra, Goal) :-
    compound_name_arguments(Goal0, Name, Args0),
    append(Args0, Extra, Args),
    compound_name_arguments(Goal, Name, Args).


		 /*******************************
		 *            LABELS		*
		 *******************************/

%!  predicate_label(++PI, -Label) is det.
%
%   Create a human-readable label  for   the  given predicate indicator.
%   This notably hides the module qualification from `user` and built-in
%   predicates. This predicate  is  intended   for  reporting  predicate
%   information to the user, for example in the profiler.
%
%   First   PI   is   converted   to    a     _head_    and   the   hook
%   user:prolog_predicate_name/2 is tried.

predicate_label(PI, Label) :-
    must_be(ground, PI),
    pi_head(PI, Head),
    user:prolog_predicate_name(Head, Label),
    !.
predicate_label(M:Name/Arity, Label) :-
    !,
    (   hidden_module(M, Name/Arity)
    ->  atomic_list_concat([Name, /, Arity], Label)
    ;   atomic_list_concat([M, :, Name, /, Arity], Label)
    ).
predicate_label(M:Name//Arity, Label) :-
    !,
    (   hidden_module(M, Name//Arity)
    ->  atomic_list_concat([Name, //, Arity], Label)
    ;   atomic_list_concat([M, :, Name, //, Arity], Label)
    ).
predicate_label(Name/Arity, Label) :-
    !,
    atomic_list_concat([Name, /, Arity], Label).
predicate_label(Name//Arity, Label) :-
    !,
    atomic_list_concat([Name, //, Arity], Label).

hidden_module(system, _).
hidden_module(user, _).
hidden_module(M, Name/Arity) :-
    functor(H, Name, Arity),
    predicate_property(system:H, imported_from(M)).
hidden_module(M, Name//DCGArity) :-
    Arity is DCGArity+1,
    functor(H, Name, Arity),
    predicate_property(system:H, imported_from(M)).

%!  predicate_sort_key(+PI, -Key) is det.
%
%   Key is the (module-free) name of the predicate for sorting purposes.

predicate_sort_key(_:PI, Name) :-
    !,
    predicate_sort_key(PI, Name).
predicate_sort_key(Name/_Arity, Name).
predicate_sort_key(Name//_Arity, Name).

%!  is_control_goal(@Goal)
%
%   True if Goal is a compiled  Prolog control structure. The difference
%   between control structures and meta-predicates   is  rather unclear.
%   The constructs below are recognised by   the  compiler and cannot be
%   redefined.   Note   that   (if->then;else)     is    recognised   as
%   ((if->then);else).

is_control_goal(Goal) :-
    var(Goal),
    !, fail.
is_control_goal((_,_)).
is_control_goal((_;_)).
is_control_goal((_->_)).
is_control_goal((_|_)).
is_control_goal((_*->_)).
is_control_goal(\+(_)).

%!  body_term_calls(:BodyTerm, -Goal) is nondet.
%
%   True when BodyTerm calls Goal.  This   predicate  looks into control
%   structures as well as meta predicates based on predicate_property/2.
%
%   When a variable is  called,  this   is  normally  returned  in Goal.
%   Currently if a variable is called   with additional arguments, e.g.,
%   call(Var, a1), this call is reported as call(Var, a1).

body_term_calls(M:Body, Calls) :-
    body_term_calls(Body, M, M, Calls).

body_term_calls(Var, M, C, Calls) :-
    var(Var),
    !,
    qualify(M, C, Var, Calls).
body_term_calls(M:Goal, _, C, Calls) :-
    !,
    body_term_calls(Goal, M, C, Calls).
body_term_calls(Goal, M, C, Calls) :-
    qualify(M, C, Goal, Calls).
body_term_calls((A,B), M, C, Calls) :-
    !,
    (   body_term_calls(A, M, C, Calls)
    ;   body_term_calls(B, M, C, Calls)
    ).
body_term_calls((A;B), M, C, Calls) :-
    !,
    (   body_term_calls(A, M, C, Calls)
    ;   body_term_calls(B, M, C, Calls)
    ).
body_term_calls((A->B), M, C, Calls) :-
    !,
    (   body_term_calls(A, M, C, Calls)
    ;   body_term_calls(B, M, C, Calls)
    ).
body_term_calls((A*->B), M, C, Calls) :-
    !,
    (   body_term_calls(A, M, C, Calls)
    ;   body_term_calls(B, M, C, Calls)
    ).
body_term_calls(\+ A, M, C, Calls) :-
    !,
    body_term_calls(A, M, C, Calls).
body_term_calls(Goal, M, C, Calls) :-
    predicate_property(M:Goal, meta_predicate(Spec)),
    \+ ( functor(Goal, call, _),
         arg(1, Goal, A1),
         strip_module(A1, _, P1),
         var(P1)
       ),
    !,
    arg(I, Spec, SArg),
    arg(I, Goal, GArg),
    meta_calls(SArg, GArg, Call0),
    body_term_calls(Call0, M, C, Calls).

meta_calls(0, Goal, Goal) :-
    !.
meta_calls(I, Goal0, Goal) :-
    integer(I),
    !,
    length(Extra, I),
    extend_goal(Goal0, Extra, Goal).
meta_calls(//, Goal0, Goal) :-
    extend_goal(Goal0, [_,_], Goal).
meta_calls(^, Goal0, Goal) :-
    !,
    strip_existential(Goal0, Goal).

strip_existential(Var, Var) :-
    var(Var),
    !.
strip_existential(_^In, Out) :-
    strip_existential(In, Out).

qualify(M, C, Goal, Calls) :-
    M == C,
    !,
    Calls = Goal.
qualify(M, _, Goal, M:Goal).
