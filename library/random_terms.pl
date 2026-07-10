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

:- module(random_terms,
          [ random_term/2              % -Term, +Options
          ]).
:- use_module(library(random)).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(debug)).

/** <module> Random term generator

This module generates random terms  intented   to  test  predicates that
manage  arbitrary  terms.  Think   of    variant   detection  (`=@=/2`),
factorization, etc.  The generated terms contain:

  * Actually shared subterms (same cell reachable via multiple paths).
  * Equivalent-but-not-shared subterms (fresh copies of earlier subterms).
  * Cyclic subterms (back-references to an ancestor under construction).
  * A sprinkling of fresh variables at the leaves.

## Design

During generation we thread two lists:

  * `Done` — completed compound subterms.  New subterms may either
    _share_ these (unify with the same cell) or take a _copy_
    (`duplicate_term/2`, structurally equivalent but disjoint even
    for ground subterms — `copy_term/2` would leak sharing here).
  * `Anc`  — currently under-construction ancestors.  A _cycle_
    back-reference simply unifies the new subterm with one of these,
    which produces a real cyclic term because the ancestor already
    contains the argument slot being filled.

At every non-leaf point a weighted choice   is  made among {leaf, share,
copy, cycle, compound}. Options that are unavailable (empty pool / empty
ancestor stack) are dropped from  the   choice  rather than having their
probability mass leak into  the  remaining   options  —  otherwise early
nodes, where the completed pool  is   still  empty,  would collapse into
trivial self-cycles.

The compound weight is scaled by   `depth_decay^Level`, where `Level` is
the distance from the root. This keeps  the root strongly biased towards
compounds (avoiding trivial one-leaf terms) while making the probability
of descending further vanish geometrically with depth.
*/

:- predicate_options(random_term/2, 2,
                     [ depth: nonneg,
                       depth_decay: between(0.0, 1.0),
                       max_arity: nonneg,
                       functors: list(atom),
                       atoms: list(atom),
                       max_int: nonneg,
                       p_zero_arity: between(0.0, 1.0),
                       w_leaf: between(0, 100),
                       w_share: between(0, 100),
                       w_copy: between(0, 100),
                       w_cycle: between(0, 100),
                       w_compound: between(0, 100),
                       w_var: between(0, 100),
                       w_atom: between(0, 100),
                       w_int: between(0, 100),
                       w_float: between(0, 100),
                       w_rational: between(0, 100),
                       w_string: between(0, 100)
                     ]).


%!  random_term(-Term, +Options) is det.
%
%   Generate a random term. Options override  the defaults below and may
%   be given either as an option list or   as  a dict. Weigths (w_*) are
%   handled as relative weights in two groups:
%
%     - `w_leaf`, `w_share`, `w_copy`, `w_cycle` and `w_compound`
%       control the (compound) shape of the resulting term.
%     - `w_atom`, `w_int`, `w_float`, `w_rational` and `w_string`
%       control what _leaf_ node is generated after the group above
%       decided to create a _leaf_.
%
%   Defaults:
%
%     ```
%     #{
%       depth:        5,          % maximum recursion depth
%       depth_decay:  0.5,        % w_compound is scaled by this per level
%       max_arity:    3,          % maximum compound arity
%       functors:     [f,g,h,p,q],
%       atoms:        [a,b,c],
%       max_int:      100,        % random integer leaves in 0..max_int
%	p_zero_arity: 0.1,	  % fraction on zero-arity compounds.
%       w_leaf:       10,         % choice weights
%       w_share:      30,
%       w_copy:       15,
%       w_cycle:      15,
%       w_compound:   50,
%       w_var:        10,         % leaf-type weights
%       w_atom:       10,
%       w_int:        10,
%       w_float:      10,
%       w_rational:   10,
%       w_string:     10
%     }
%     ```

random_term(Term, Options) :-
    params(Options, Params),
    gen(0, Params, [], _Done, [], Term).

default_params(
    #{ depth:        5,
       depth_decay:  0.5,
       max_arity:    3,
       functors:     [f,g,h,p,q],
       atoms:        [a,b,c],
       max_int:      100,
       p_zero_arity: 0.10,
       w_leaf:       10,
       w_share:      30,
       w_copy:       15,
       w_cycle:      15,
       w_compound:   50,
       w_var:        10,
       w_atom:       10,
       w_int:        10,
       w_float:      10,
       w_rational:   10,
       w_string:     10
     }).

params(Options, Params) :-
    default_params(Def),
    (   is_dict(Options)
    ->  User = Options
    ;   dict_create(User, #, Options)
    ),
    Params = Def.put(User).

%   gen(+Level, +Params, +Done0, -Done, +Anc, -Term)
%
%   @arg Level is the distance from the root (0 at the top).

gen(Level, Params, Done0, Done, Anc, Term) :-
    (   Level >= Params.depth
    ->  gen_leaf(Params, Term), Done = Done0
    ;   choices(Level, Params, Done0, Anc, Cs),
        weighted_pick(Cs, Action),
        debug(random_term, 'Weigths: ~p --> ~p', [Cs, Action]),
        do_action(Action, Level, Params, Done0, Done, Anc, Term)
    ).

%!  choices(+Level, +Params, +Done, +Anc, -Cs) is det.
%
%   Construct a list of choices Cs as `Weight-Action`, where `Action` is
%   one of `leaf`, `compound`, `share`, `copy`, `cycle`.

choices(Level, Params, Done, Anc, Cs) :-
    WCompound is Params.w_compound * Params.depth_decay ** Level,
    Cs0 = [Params.w_leaf-leaf, WCompound-compound],
    (   Done == []
    ->  Cs1 = Cs0
    ;   Cs1 = [Params.w_share-share, Params.w_copy-copy | Cs0]
    ),
    (   Anc == []
    ->  Cs  = Cs1
    ;   Cs  = [Params.w_cycle-cycle | Cs1]
    ).

weighted_pick(Pairs, Choice) :-
    pairs_keys(Pairs, Ws),
    sum_list(Ws, Sum),
    R is random_float * Sum,
    pick_(Pairs, R, Choice).

pick_([_-C], _, C) :- !.
pick_([W-C|_], R, C) :- R =< W, !.
pick_([W-_|Rest], R, Choice) :-
    R1 is R - W,
    pick_(Rest, R1, Choice).

do_action(leaf, _, Params, Done, Done, _, T) :-
    gen_leaf(Params, T).
do_action(share, _, _, Done, Done, _, T) :-
    random_member(T, Done).
do_action(copy, _, _, Done, Done, _, T) :-
    random_member(T0, Done),
    duplicate_term(T0, T).
do_action(cycle, _, _, Done, Done, Anc, T) :-
    random_member(T, Anc).
do_action(compound, Level, Params, Done0, Done, Anc, T) :-
    gen_compound(Level, Params, Done0, Done, Anc, T).

gen_compound(Level, Params, Done0, Done, Anc, Term) :-
    (   maybe(Params.p_zero_arity)
    ->  N = 0
    ;   random_between(1, Params.max_arity, N)
    ),
    random_member(F, Params.functors),
    length(Args, N),
    compound_name_arguments(Term, F, Args),
    L1 is Level + 1,
    gen_args(Args, L1, Params, Done0, Done1, [Term|Anc]),
    Done = [Term|Done1].

gen_args([], _, _, Done, Done, _).
gen_args([A|As], L, Params, Done0, Done, Anc) :-
    gen(L, Params, Done0, Done1, Anc, A),
    gen_args(As, L, Params, Done1, Done, Anc).

gen_leaf(Params, Leaf) :-
    pick_leaf_type(Params, Type),
    gen_leaf_of(Type, Params, Leaf).

pick_leaf_type(Params, Type) :-
    Ts = [ Params.w_var-var,
           Params.w_atom-atom,
           Params.w_int-int,
           Params.w_float-float,
           Params.w_rational-rational,
           Params.w_string-string
         ],
    weighted_pick(Ts, Type).

gen_leaf_of(var, _, _).
gen_leaf_of(atom, Params, Leaf) :-
    random_member(Leaf, Params.atoms).
gen_leaf_of(int, Params, Leaf) :-
    Neg is -Params.max_int,
    random_between(Neg, Params.max_int, Leaf).
gen_leaf_of(float, Params, Leaf) :-
    gen_leaf_of(int, Params, Int),
    Leaf is (Int*100+random(100))/100.0.
gen_leaf_of(rational, Params, Leaf) :-
    Max = Params.max_int,
    Min is -Max,
    random_between(Min, Max, N),
    random_between(1, Max, D),
    Leaf is N rdiv D.
gen_leaf_of(string, Params, Leaf) :-
    random_member(A, Params.atoms),
    atom_string(A, Leaf).
