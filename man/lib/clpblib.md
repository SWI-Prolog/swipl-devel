## Introduction                        {#clpb-intro}

This library provides CLP(B), Constraint Logic Programming over
Boolean variables. It can be used to model and solve combinatorial
problems such as verification, allocation and covering tasks.

CLP(B) is an instance of the general [CLP(_X_) scheme](<#clp>),
extending logic programming with reasoning over specialised domains.

The implementation is based on reduced and ordered Binary Decision
Diagrams (BDDs).

Benchmarks and usage examples of this library are available from:
[**https://www.metalevel.at/clpb/**](https://www.metalevel.at/clpb/)

We recommend the following reference for citing this library in
scientific publications:

==
@inproceedings{Triska2016,
  author    = "Markus Triska",
  title     = "The {Boolean} Constraint Solver of {SWI-Prolog}:
               System Description",
  booktitle = "FLOPS",
  series    = "LNCS",
  volume    = 9613,
  year      = 2016,
  pages     = "45--61"
}
==

The paper is available from
[https://www.metalevel.at/swiclpb.pdf](https://www.metalevel.at/swiclpb.pdf)

## Boolean expressions {#clpb-exprs}

A _Boolean expression_ is one of:

    | `0`                | false                                |
    | `1`                | true                                 |
    | _variable_         | unknown truth value                  |
    | _atom_             | universally quantified variable      |
    | ~ _Expr_           | logical NOT                          |
    | _Expr_ + _Expr_    | logical OR                           |
    | _Expr_ * _Expr_    | logical AND                          |
    | _Expr_ # _Expr_    | exclusive OR                         |
    | _Var_ ^ _Expr_     | existential quantification           |
    | _Expr_ =:= _Expr_  | equality                             |
    | _Expr_ =\= _Expr_  | disequality (same as #)              |
    | _Expr_ =< _Expr_   | less or equal (implication)          |
    | _Expr_ >= _Expr_   | greater or equal                     |
    | _Expr_ < _Expr_    | less than                            |
    | _Expr_ > _Expr_    | greater than                         |
    | card(Is,Exprs)     | cardinality constraint (_see below_) |
    | `+(Exprs)`         | n-fold disjunction (_see below_)     |
    | `*(Exprs)`         | n-fold conjunction (_see below_)     |

where _Expr_ again denotes a Boolean expression.

The Boolean expression card(Is,Exprs) is true iff the number of true
expressions in the list `Exprs` is a member of the list `Is` of
integers and integer ranges of the form `From-To`. For example, to
state that precisely two of the three variables `X`, `Y` and `Z` are
`true`, you can use `sat(card([2],[X,Y,Z]))`.

`+(Exprs)` and `*(Exprs)` denote, respectively, the disjunction and
conjunction of all elements in the list `Exprs` of Boolean
expressions.

Atoms denote parametric values that are universally quantified. All
universal quantifiers appear implicitly in front of the entire
expression. In residual goals, universally quantified variables always
appear on the right-hand side of equations. Therefore, they can be
used to express functional dependencies on input variables.

## Interface predicates   {#clpb-interface}

The most frequently used CLP(B) predicates are:

    * sat(+Expr)
      True iff the Boolean expression Expr is satisfiable.

    * taut(+Expr, -T)
      If Expr is a tautology with respect to the posted constraints, succeeds
      with *T = 1*. If Expr cannot be satisfied, succeeds with *T = 0*.
      Otherwise, it fails.

    * labeling(+Vs)
      Assigns truth values to the variables Vs such that all constraints
      are satisfied.

The unification of a CLP(B) variable _X_ with a term _T_ is equivalent
to posting the constraint sat(X=:=T).

## Examples                            {#clpb-examples}

Here is an example session with a few queries and their answers:

==
?- use_module(library(clpb)).
true.

?- sat(X*Y).
X = Y, Y = 1.

?- sat(X * ~X).
false.

?- taut(X * ~X, T).
T = 0,
sat(X=:=X).

?- sat(X^Y^(X+Y)).
sat(X=:=X),
sat(Y=:=Y).

?- sat(X*Y + X*Z), labeling([X,Y,Z]).
X = Z, Z = 1, Y = 0 ;
X = Y, Y = 1, Z = 0 ;
X = Y, Y = Z, Z = 1.

?- sat(X =< Y), sat(Y =< Z), taut(X =< Z, T).
T = 1,
sat(X=:=X*Y),
sat(Y=:=Y*Z).

?- sat(1#X#a#b).
sat(X=:=a#b).
==

The pending residual goals constrain remaining variables to Boolean
expressions and are declaratively equivalent to the original query.
The last example illustrates that when applicable, remaining variables
are expressed as functions of universally quantified variables.

## Obtaining BDDs {#clpb-residual-goals}

By default, CLP(B) residual goals appear in (approximately) algebraic
normal form (ANF). This projection is often computationally expensive.
We can set the Prolog flag `clpb_residuals` to the value `bdd` to see
the BDD representation of all constraints. This results in faster
projection to residual goals, and is also useful for learning more
about BDDs. For example:

==
?- set_prolog_flag(clpb_residuals, bdd).
true.

?- sat(X#Y).
node(3)- (v(X, 0)->node(2);node(1)),
node(1)- (v(Y, 1)->true;false),
node(2)- (v(Y, 1)->false;true).
==

Note that this representation cannot be pasted back on the toplevel,
and its details are subject to change. Use copy_term/3 to obtain
such answers as Prolog terms.

The variable order of the BDD is determined by the order in which the
variables first appear in constraints. To obtain different orders,
we can for example use:

==
?- sat(+[1,Y,X]), sat(X#Y).
node(3)- (v(Y, 0)->node(2);node(1)),
node(1)- (v(X, 1)->true;false),
node(2)- (v(X, 1)->false;true).
==

## Enabling monotonic CLP(B) {#clpb-monotonic}

In the default execution mode, CLP(B) constraints are _not_ monotonic.
This means that _adding_ constraints can yield new solutions. For
example:

==
?-          sat(X=:=1), X = 1+0.
false.

?- X = 1+0, sat(X=:=1), X = 1+0.
X = 1+0.
==

This behaviour is highly problematic from a logical point of view, and
it may render [**declarative
debugging**](https://www.metalevel.at/prolog/debugging)
techniques inapplicable.

Set the flag `clpb_monotonic` to `true` to make CLP(B) *monotonic*. If
this mode is enabled, then you must wrap CLP(B) variables with the
functor `v/1`. For example:

==
?- set_prolog_flag(clpb_monotonic, true).
true.

?- sat(v(X)=:=1#1).
X = 0.
==

## Example: Pigeons {#clpb-pigeons}

In this example, we are attempting to place _I_ pigeons into _J_ holes
in such a way that each hole contains at most one pigeon. One
interesting property of this task is that it can be formulated using
only _cardinality constraints_ (`card/2`). Another interesting aspect
is that this task has no short resolution refutations in general.

In the following, we use [**Prolog DCG
notation**](https://www.metalevel.at/prolog/dcg) to describe a
list `Cs` of CLP(B) constraints that must all be satisfied.

==
:- use_module(library(clpb)).
:- use_module(library(clpfd)).

pigeon(I, J, Rows, Cs) :-
        length(Rows, I), length(Row, J),
        maplist(same_length(Row), Rows),
        transpose(Rows, TRows),
        phrase((all_cards(Rows,[1]),all_cards(TRows,[0,1])), Cs).

all_cards([], _) --> [].
all_cards([Ls|Lss], Cs) --> [card(Cs,Ls)], all_cards(Lss, Cs).
==

Example queries:

==
?- pigeon(9, 8, Rows, Cs), sat(*(Cs)).
false.

?- pigeon(2, 3, Rows, Cs), sat(*(Cs)),
   append(Rows, Vs), labeling(Vs),
   maplist(portray_clause, Rows).
[0, 0, 1].
[0, 1, 0].
etc.
==

## Example: Boolean circuit  {#clpb-circuit}

Consider a Boolean circuit that express the Boolean function =|XOR|=
with 4 =|NAND|= gates. We can model such a circuit with CLP(B)
constraints as follows:

==
:- use_module(library(clpb)).

nand_gate(X, Y, Z) :- sat(Z =:= ~(X*Y)).

xor(X, Y, Z) :-
        nand_gate(X, Y, T1),
        nand_gate(X, T1, T2),
        nand_gate(Y, T1, T3),
        nand_gate(T2, T3, Z).
==

Using universally quantified variables, we can show that the circuit
does compute =|XOR|= as intended:

==
?- xor(x, y, Z).
sat(Z=:=x#y).
==

## Acknowledgments {#clpb-acknowledgments}

The interface predicates of this library follow the example of
[**SICStus Prolog**](https://sicstus.sics.se).

Use SICStus Prolog for higher performance in many cases.

## CLP(B) predicate index {#clpb-predicates}

In the following, each CLP(B) predicate is described in more detail.

We recommend the following link to refer to this manual:

http://eu.swi-prolog.org/man/clpb.html

  * [[sat/1]]
  * [[taut/2]]
  * [[labeling/1]]
  * [[sat_count/2]]
  * [[weighted_maximum/3]]
  * [[random_labeling/2]]
