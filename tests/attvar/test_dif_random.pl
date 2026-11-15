/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2026, SWI-Prolog Solutions b.v.
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

:- module(test_dif_random,
          [ test_dif_random/0,
            test_dif_random/1                   % +Iterations
          ]).
:- use_module(library(dif)).
:- use_module(library(random_terms)).
:- use_module(library(random)).
:- use_module(library(lists)).
:- use_module(library(plunit)).

/** <module> Randomised property test for dif/2

Take a random ground term T. Produce a generalisation G by replacing
some subterms of T with fresh variables (each replacement recorded as
`Var=Value`, so that unifying all replacements makes G structurally
equal to T). Some of those variables are then aliased so that G also
carries sharing constraints. Post `dif(G, T)` and drive it with a
randomly chosen mix of

  - "match" bindings that push G towards T,
  - one "clash" binding that forces a mismatch, or
  - leaving some variables unbound.

The predicted outcome is decidable from the plan itself:

  - dif(G, T) must fail iff the plan drives G to be structurally == T
    (all replacements applied with their matching values, and no
    alias-conflict).
  - dif(G, T) must succeed in every other case: either a clash was
    applied, or some replacement was left unbound (so dif stays
    delayed), or the alias forced two positions to hold values that
    are decidably unequal in T.

Any deviation is reported with the offending T, G, plan and observed
result — a minimal regression case ready to add to test_dif.pl.
*/

test_dif_random :-
    run_tests(dif_random).

test_dif_random(N) :-
    forall(between(1, N, _), check_scenario).

:- begin_tests(dif_random).

test(scenario, [forall(between(1, 500, _))]) :-
    check_scenario.

:- end_tests(dif_random).

check_scenario :-
    random_ground_term(T),
    generalize(T, G, Bindings0),
    maybe_alias_bindings(Bindings0, Bindings, AliasBlocksMatch),
    plan(Bindings, Plan, Expected0),
    (   AliasBlocksMatch == true
    ->  Expected = ok           % G can never equal T ⇒ dif always holds
    ;   Expected = Expected0
    ),
    run(G, T, Plan, Observed),
    (   Observed == Expected
    ->  true
    ;   report(scenario, T, G, Plan, Expected, Observed),
        fail
    ).

random_ground_term(T) :-
    random_between(2, 5, Depth),
    random_between(1, 3, Arity),
    random_term(T,
                [ depth(Depth), max_arity(Arity),
                  w_var(0), w_cycle(0)
                ]).

%!  generalize(+T, -G, -Bindings) is det.
%
%   G is a fresh term structurally equal to T except that a random
%   subset of subterms has been replaced by fresh variables. Bindings
%   is the list `[Var=Value, ...]` such that applying every unification
%   makes G unifiable-with-and-then-equal-to T.

generalize(T, G, Bs) :-
    generalize_(T, G, Bs, []).

generalize_(T, G, Bs0, Bs) :-
    (   maybe(0.3)
    ->  G = _V,
        Bs0 = [G=T | Bs]
    ;   compound(T)
    ->  compound_name_arguments(T, F, As),
        generalize_args(As, Gs, Bs0, Bs),
        compound_name_arguments(G, F, Gs)
    ;   G = T,
        Bs0 = Bs
    ).

generalize_args([], [], Bs, Bs).
generalize_args([A|As], [G|Gs], Bs0, Bs) :-
    generalize_(A, G, Bs0, Bs1),
    generalize_args(As, Gs, Bs1, Bs).

%!  maybe_alias_bindings(+Bindings0, -Bindings, -AliasBlocksMatch) is det.
%
%   Optionally alias two variables in Bindings0 so that G carries a
%   sharing constraint (the same var appears at two positions).
%   AliasBlocksMatch is `true` when two aliased positions carry
%   decidably different values in T — G can then never equal T, so
%   dif(G, T) is satisfied regardless of the plan.

maybe_alias_bindings(Bindings0, Bindings, AliasBlocksMatch) :-
    (   Bindings0 = [V1=Val1 | Rest0],
        select(V2=Val2, Rest0, Rest),
        maybe(0.4)
    ->  V1 = V2,
        Bindings = [V1=Val1 | Rest],
        (   Val1 \== Val2, ground(Val1), ground(Val2)
        ->  AliasBlocksMatch = true
        ;   AliasBlocksMatch = false
        )
    ;   Bindings = Bindings0,
        AliasBlocksMatch = false
    ).

%!  plan(+Bindings, -Plan, -Expected) is det.
%
%   Pick a random subset of `Bindings` (in some order) as the plan.
%   Optionally swap exactly one of the picked bindings for a clash
%   value. Expected is decidable from the plan:
%
%     * `fail` iff the plan is a full match sequence covering every
%       unique variable — that binds G structurally to T.
%     * `ok` in every other case.

plan([], [], fail) :- !.                        % G identical to T
plan(Bindings, Plan, Expected) :-
    unique_vars(Bindings, UVs),
    length(UVs, U),
    random_permutation(Bindings, Shuffled),
    length(Shuffled, N),
    random_between(0, N, K),
    length(Prefix, K),
    append(Prefix, _Rest, Shuffled),
    plan_prefix_vars(Prefix, PrefixVs),
    length(PrefixVs, PU),
    (   K > 0, maybe(0.5)
    ->  random_between(1, K, Idx),
        nth1(Idx, Prefix, V=Val),
        clash_value(Val, Clash),
        nth1_replace(Idx, Prefix, V=Clash, Plan),
        Expected = ok
    ;   Plan = Prefix,
        (   PU =:= U
        ->  Expected = fail                     % covers all unique vars
        ;   Expected = ok
        )
    ).

unique_vars(Bindings, UVs) :-
    findall(V, member(V=_, Bindings), Vs),
    sort(Vs, UVs).

plan_prefix_vars(Prefix, PVs) :-
    findall(V, member(V=_, Prefix), Vs),
    sort(Vs, PVs).

nth1_replace(1, [_|T], X, [X|T]) :- !.
nth1_replace(N, [H|T], X, [H|R]) :-
    N1 is N - 1,
    nth1_replace(N1, T, X, R).

%!  clash_value(+Val, -Clash) is det.
%
%   Produce a value guaranteed not to unify with Val given the leaf
%   domains random_term uses (atoms `[a,b,c]`, ints in `-100..100`).

clash_value(Val, Clash) :-
    (   integer(Val)
    ->  Clash is Val + 10_000
    ;   atom(Val)
    ->  atom_concat(clashed_, Val, Clash)
    ;   compound(Val)
    ->  Clash = clashed_atom
    ;   Clash = clashed_atom
    ).

%!  run(+G, +T, +Plan, -Observed) is det.
%
%   Post dif(G, T) and apply the plan. Observed is `ok` when the whole
%   sequence succeeded, `fail` when it failed, `error(E)` on exception.

run(G, T, Plan, Observed) :-
    catch(
        (   (   dif(G, T), apply_plan(Plan)
            ->  Observed = ok
            ;   Observed = fail
            )
        ),
        Error,
        Observed = error(Error)
    ).

apply_plan([]).
apply_plan([V=X|T]) :-
    V = X,
    apply_plan(T).

report(Phase, T, G, Plan, Expected, Observed) :-
    copy_term(T-G-Plan, Tc-Gc-Pc),
    numbervars(Tc-Gc-Pc, 0, _, [singletons(false)]),
    format(user_error,
           "MISMATCH ~w: expected ~w, observed ~w~n  T=~q~n  G=~q~n  Plan=~q~n",
           [Phase, Expected, Observed, Tc, Gc, Pc]).
