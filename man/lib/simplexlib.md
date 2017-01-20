
## Introduction  {#simplex-intro}

A **linear programming problem** or simply **linear program** (LP)
consists of:

    - a set of _linear_ **constraints**
    - a set of **variables**
    - a _linear_ **objective function**.

The goal is to assign values to the variables so as to _maximize_ (or
minimize) the value of the objective function while satisfying all
constraints.

Many optimization problems can be modeled in this way. As one basic
example, consider a knapsack with fixed capacity C, and a number of
items with sizes `s(i)` and values `v(i)`. The goal is to put as many
items as possible in the knapsack (not exceeding its capacity) while
maximizing the sum of their values.

As another example, suppose you are given a set of _coins_ with
certain values, and you are to find the minimum number of coins such
that their values sum up to a fixed amount. Instances of these
problems are solved below.

Solving an LP or integer linear program (ILP) with this library
typically comprises 4 stages:

  1. an initial state is generated with gen_state/1
  2. all relevant constraints are added with constraint/3
  3. maximize/3 or minimize/3 are used to obtain a _solved state_ that
     represents an optimum solution
  4. variable_value/3 and objective/2 are used on the solved state to obtain
     variable values and the objective function at the optimum.

The most frequently used predicates are thus:

  - [[gen_state/1]]
  - [[constraint/3]]
  - [[maximize/3]]
  - [[minimize/3]]
  - [[variable_value/3]]
  - [[objective/2]]

All numeric quantities are converted to rationals via rationalize/1,
and rational arithmetic is used throughout solving linear programs. In
the current implementation, all variables are implicitly constrained
to be _non-negative_. This may change in future versions, and
non-negativity constraints should therefore be stated explicitly.

## Delayed column generation {#simplex-delayed-column}

_Delayed column generation_ means that more constraint columns are
added to an existing LP. The following predicates are frequently used
when this method is applied:

  - [[constraint/4]]
  - [[shadow_price/3]]
  - [[constraint_add/4]]

An example application of _delayed column generation_ to solve a _bin
packing_ task is available from:
[**metalevel.at/various/colgen/**](https://www.metalevel.at/various/colgen/)

## Solving LPs with special structure {#simplex-special-structure}

The following predicates allow you to solve specific kinds of LPs more
efficiently:


  - [[transportation/4]]
  - [[assignment/2]]

## Examples {#simplex-examples}

We include a few examples for solving LPs with this library.

### Example 1  {#simplex-ex-1}

This is the "radiation therapy" example, taken from
_Introduction to Operations Research_ by Hillier and Lieberman.

[**Prolog DCG notation**](https://www.metalevel.at/prolog/dcg) is
used to _implicitly_ thread the state through posting the
constraints:

==
:- use_module(library(simplex)).

radiation(S) :-
        gen_state(S0),
        post_constraints(S0, S1),
        minimize([0.4*x1, 0.5*x2], S1, S).

post_constraints -->
        constraint([0.3*x1, 0.1*x2] =< 2.7),
        constraint([0.5*x1, 0.5*x2] = 6),
        constraint([0.6*x1, 0.4*x2] >= 6),
        constraint([x1] >= 0),
        constraint([x2] >= 0).
==

An example query:

==
?- radiation(S), variable_value(S, x1, Val1),
                 variable_value(S, x2, Val2).
Val1 = 15 rdiv 2,
Val2 = 9 rdiv 2.
==

### Example 2 {#simplex-ex-2}

Here is an instance of the knapsack problem described above, where
`C = 8`, and we have two types of items: One item with value 7
and size 6, and 2 items each having size 4 and value 4. We introduce
two variables, `x(1)` and `x(2)` that denote how many items
to take of each type.

==
:- use_module(library(simplex)).

knapsack(S) :-
        knapsack_constraints(S0),
        maximize([7*x(1), 4*x(2)], S0, S).

knapsack_constraints(S) :-
        gen_state(S0),
        constraint([6*x(1), 4*x(2)] =< 8, S0, S1),
        constraint([x(1)] =< 1, S1, S2),
        constraint([x(2)] =< 2, S2, S).
==

An example query yields:

==
?- knapsack(S), variable_value(S, x(1), X1),
                variable_value(S, x(2), X2).
X1 = 1
X2 = 1 rdiv 2.
==

That is, we are to take the one item of the first type, and half of one of
the items of the other type to maximize the total value of items in the
knapsack.

If items can not be split, integrality constraints have to be imposed:

==
knapsack_integral(S) :-
        knapsack_constraints(S0),
        constraint(integral(x(1)), S0, S1),
        constraint(integral(x(2)), S1, S2),
        maximize([7*x(1), 4*x(2)], S2, S).
==

Now the result is different:

==
?- knapsack_integral(S), variable_value(S, x(1), X1),
                         variable_value(S, x(2), X2).

X1 = 0
X2 = 2
==

That is, we are to take only the _two_ items of the second type.
Notice in particular that always choosing the remaining item with best
performance (ratio of value to size) that still fits in the knapsack
does not necessarily yield an optimal solution in the presence of
integrality constraints.

### Example 3 {#simplex-ex-3}

We are given:

    - 3 coins each worth 1 unit
    - 20 coins each worth 5 units and
    - 10 coins each worth 20 units.

The task is to find a _minimal_ number of these coins that amount to
111 units in total. We introduce variables `c(1)`, `c(5)` and `c(20)`
denoting how many coins to take of the respective type:

==
:- use_module(library(simplex)).

coins(S) :-
        gen_state(S0),
        coins(S0, S).

coins -->
        constraint([c(1), 5*c(5), 20*c(20)] = 111),
        constraint([c(1)] =< 3),
        constraint([c(5)] =< 20),
        constraint([c(20)] =< 10),
        constraint([c(1)] >= 0),
        constraint([c(5)] >= 0),
        constraint([c(20)] >= 0),
        constraint(integral(c(1))),
        constraint(integral(c(5))),
        constraint(integral(c(20))),
        minimize([c(1), c(5), c(20)]).
==

An example query:

==
?- coins(S), variable_value(S, c(1), C1),
             variable_value(S, c(5), C5),
             variable_value(S, c(20), C20).

C1 = 1,
C5 = 2,
C20 = 5.
==
