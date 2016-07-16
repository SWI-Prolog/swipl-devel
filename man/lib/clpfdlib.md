### Arithmetic constraints {#clpfd-arithmetic}

_Arithmetic_ constraints are the most basic use of CLP(FD). Every time
you use is/2 or one of the low-level arithmetic comparisons (</2, >/2
etc.) over integers, consider using CLP(FD) constraints
_instead_. This can at most _increase_ the generality of your
programs. See [declarative integer
arithmetic](<#clpfd-integer-arith>).

  * [[(#=)/2]]
  * [[(#\=)/2]]
  * [[(#>=)/2]]
  * [[(#=<)/2]]
  * [[(#>)/2]]
  * [[(#<)/2]]


### Membership constraints {#clpfd-membership}

If you are using CLP(FD) to model and solve combinatorial tasks, then
you typically need to specify the admissible domains of variables.
The _membership constraints_ in/2 and ins/2 are useful in such cases.

  * [[in/2]]
  * [[ins/2]]


### Enumeration predicates {#clpfd-enumeration}

When modeling combinatorial tasks, the actual search for solutions is
typically performed by _enumeration predicates_ like labeling/2. See
the the section about _core relations_ and search for more
information.

  * [[indomain/1]]
  * [[label/1]]
  * [[labeling/2]]


### Global constraints {#clpfd-global}

A _global constraint_ expresses a relation that involves many
variables at once. The most frequently used global constraints of this
library are the combinatorial constraints all_distinct/1,
global_cardinality/2 and cumulative/2.


  * [[all_distinct/1]]
  * [[all_different/1]]
  * [[sum/3]]
  * [[scalar_product/4]]
  * [[lex_chain/1]]
  * [[tuples_in/2]]
  * [[serialized/2]]
  * [[element/3]]
  * [[global_cardinality/2]]
  * [[global_cardinality/3]]
  * [[circuit/1]]
  * [[cumulative/1]]
  * [[cumulative/2]]
  * [[disjoint2/1]]
  * [[automaton/3]]
  * [[automaton/8]]
  * [[chain/2]]


### Reification predicates {#clpfd-reification-predicates}

Many CLP(FD) constraints can be _reified_. This means that their truth
value is itself turned into a CLP(FD) variable, so that you can
explicitly reason about whether a constraint holds or not. See
[reification](<#clpfd-reification>).

  * [[(#\)/1]]
  * [[(#<==>)/2]]
  * [[(#==>)/2]]
  * [[(#<==)/2]]
  * [[(#/\)/2]]
  * [[(#\/)/2]]
  * [[(#\)/2]]
  * [[zcompare/3]]


### Reflection predicates {#clpfd-reflection-predicates}

Reflection predicates let you obtain, in a well-defined way,
information that is normally internal to this library. In addition to
the predicates explained below, also take a look at
call_residue_vars/2 and copy_term/3 to reason about CLP(FD)
constraints that arise in programs. This can be useful in program
analyzers and declarative debuggers.

  * [[fd_var/1]]
  * [[fd_inf/2]]
  * [[fd_sup/2]]
  * [[fd_size/2]]
  * [[fd_dom/2]]
