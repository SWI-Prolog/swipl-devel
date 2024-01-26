## Introduction			{#clpfd-intro}

This library provides CLP(FD): Constraint Logic Programming over
Finite Domains. This is an instance of the general [CLP(_X_)
scheme](<#clp>), extending logic programming with reasoning over
specialised domains. CLP(FD) lets us reason about **integers** in a
way that honors the relational nature of Prolog.

Read [**The Power of Prolog**](https://www.metalevel.at/prolog) to
understand how this library is meant to be used in practice.

There are two major use cases of CLP(FD) constraints:

    1. [**declarative integer arithmetic**](<#clpfd-integer-arith>)
    2. solving **combinatorial problems** such as planning, scheduling
       and allocation tasks.

The predicates of this library can be classified as:

    * _arithmetic_ constraints like #=/2, #>/2 and #\=/2 [](<#clpfd-arithmetic>)
    * the _membership_ constraints in/2 and ins/2 [](<#clpfd-membership>)
    * the _enumeration_ predicates indomain/1, label/1 and labeling/2 [](<#clpfd-enumeration>)
    * _combinatorial_ constraints like all_distinct/1 and global_cardinality/2 [](<#clpfd-global>)
    * _reification_ predicates such as #<==>/2 [](<#clpfd-reification-predicates>)
    * _reflection_ predicates such as fd_dom/2 [](<#clpfd-reflection-predicates>)

In most cases, [_arithmetic constraints_](<#clpfd-arith-constraints>)
are the only predicates you will ever need from this library. When
reasoning over integers, simply replace low-level arithmetic
predicates like `(is)/2` and `(>)/2` by the corresponding CLP(FD)
constraints like #=/2 and #>/2 to honor and preserve declarative
properties of your programs. For satisfactory performance, arithmetic
constraints are implicitly rewritten at compilation time so that
low-level fallback predicates are automatically used whenever
possible.

Almost all Prolog programs also reason about integers. Therefore, it
is highly advisable that you make CLP(FD) constraints available in all
your programs. One way to do this is to put the following directive in
your =|<config>/init.pl|= initialisation file:

==
:- use_module(library(clpfd)).
==

All example programs that appear in the CLP(FD) documentation assume
that you have done this.

Important concepts and principles of this library are illustrated by
means of usage examples that are available in a public git repository:
[**github.com/triska/clpfd**](https://github.com/triska/clpfd)

If you are used to the complicated operational considerations that
low-level arithmetic primitives necessitate, then moving to CLP(FD)
constraints may, due to their power and convenience, at first feel to
you excessive and almost like cheating. It _isn't_. Constraints are an
integral part of all popular Prolog systems, and they are designed
to help you eliminate and avoid the use of low-level and less general
primitives by providing declarative alternatives that are meant to be
used instead.

When teaching Prolog, CLP(FD) constraints should be introduced
_before_ explaining low-level arithmetic predicates and their
procedural idiosyncrasies. This is because constraints are easy to
explain, understand and use due to their purely relational nature. In
contrast, the modedness and directionality of low-level arithmetic
primitives are impure limitations that are better deferred to more
advanced lectures.

We recommend the following reference (PDF:
[metalevel.at/swiclpfd.pdf](https://www.metalevel.at/swiclpfd.pdf)) for
citing this library in scientific publications:

==
@inproceedings{Triska12,
  author    = {Markus Triska},
  title     = {The Finite Domain Constraint Solver of {SWI-Prolog}},
  booktitle = {FLOPS},
  series    = {LNCS},
  volume    = {7294},
  year      = {2012},
  pages     = {307-316}
}
==

More information about CLP(FD) constraints and their implementation is
contained in: [**metalevel.at/drt.pdf**](https://www.metalevel.at/drt.pdf)

The best way to discuss applying, improving and extending CLP(FD)
constraints is to use the dedicated `clpfd` tag on
[stackoverflow.com](http://stackoverflow.com). Several of the world's
foremost CLP(FD) experts regularly participate in these discussions
and will help you for free on this platform.

## Arithmetic constraints		{#clpfd-arith-constraints}

In modern Prolog systems, *arithmetic constraints* subsume and
supersede low-level predicates over integers. The main advantage of
arithmetic constraints is that they are true _relations_ and can be
used in all directions. For most programs, arithmetic constraints are
the only predicates you will ever need from this library.

The most important arithmetic constraint is #=/2, which subsumes both
`(is)/2` and `(=:=)/2` over integers. Use #=/2 to make your programs
more general. See [declarative integer
arithmetic](<#clpfd-integer-arith>).

In total, the arithmetic constraints are:

    | Expr1 `#=`  Expr2  | Expr1 equals Expr2                       |
    | Expr1 `#\=` Expr2  | Expr1 is not equal to Expr2              |
    | Expr1 `#>=` Expr2  | Expr1 is greater than or equal to Expr2  |
    | Expr1 `#=<` Expr2  | Expr1 is less than or equal to Expr2     |
    | Expr1 `#>` Expr2   | Expr1 is greater than Expr2              |
    | Expr1 `#<` Expr2   | Expr1 is less than Expr2                 |

`Expr1` and `Expr2` denote *arithmetic expressions*, which are:

    | _integer_          | Given value                          |
    | _variable_         | Unknown integer                      |
    | ?(_variable_)      | Unknown integer                      |
    | -Expr              | Unary minus                          |
    | Expr + Expr        | Addition                             |
    | Expr * Expr        | Multiplication                       |
    | Expr - Expr        | Subtraction                          |
    | Expr ^ Expr        | Exponentiation                       |
    | min(Expr,Expr)     | Minimum of two expressions           |
    | max(Expr,Expr)     | Maximum of two expressions           |
    | Expr `mod` Expr    | Modulo induced by floored division   |
    | Expr `rem` Expr    | Modulo induced by truncated division |
    | abs(Expr)          | Absolute value                       |
    | Expr // Expr       | Truncated integer division           |
    | Expr div Expr      | Floored integer division             |

where `Expr` again denotes an arithmetic expression.

The bitwise operations `(\)/1`, `(/\)/2`, `(\/)/2`, `(>>)/2`,
`(<<)/2`, `lsb/1`, `msb/1`, `popcount/1` and `(xor)/2` are also
supported.

## Declarative integer arithmetic		{#clpfd-integer-arith}

The [_arithmetic constraints_](<#clpfd-arith-constraints>) #=/2, #>/2
etc. are meant to be used _instead_ of the primitives `(is)/2`,
`(=:=)/2`, `(>)/2` etc. over integers. Almost all Prolog programs also
reason about integers. Therefore, it is recommended that you put the
following directive in your =|<config>/init.pl|= initialisation file to make
CLP(FD) constraints available in all your programs:

==
:- use_module(library(clpfd)).
==

Throughout the following, it is assumed that you have done this.

The most basic use of CLP(FD) constraints is _evaluation_ of
arithmetic expressions involving integers. For example:

==
?- X #= 1+2.
X = 3.
==

This could in principle also be achieved with the lower-level
predicate `(is)/2`. However, an important advantage of arithmetic
constraints is their purely relational nature: Constraints can be used
in _all directions_, also if one or more of their arguments are only
partially instantiated. For example:

==
?- 3 #= Y+2.
Y = 1.
==

This relational nature makes CLP(FD) constraints easy to explain and
use, and well suited for beginners and experienced Prolog programmers
alike. In contrast, when using low-level integer arithmetic, we get:

==
?- 3 is Y+2.
ERROR: is/2: Arguments are not sufficiently instantiated

?- 3 =:= Y+2.
ERROR: =:=/2: Arguments are not sufficiently instantiated
==

Due to the necessary operational considerations, the use of these
low-level arithmetic predicates is considerably harder to understand
and should therefore be deferred to more advanced lectures.

For supported expressions, CLP(FD) constraints are drop-in
replacements of these low-level arithmetic predicates, often yielding
more general programs. See [`n_factorial/2`](<#clpfd-factorial>) for an
example.

This library uses goal_expansion/2 to automatically rewrite
constraints at compilation time so that low-level arithmetic
predicates are _automatically_ used whenever possible. For example,
the predicate:

==
positive_integer(N) :- N #>= 1.
==

is executed as if it were written as:

==
positive_integer(N) :-
        (   integer(N)
        ->  N >= 1
        ;   N #>= 1
        ).
==

This illustrates why the performance of CLP(FD) constraints is almost
always completely satisfactory when they are used in modes that can be
handled by low-level arithmetic. To disable the automatic rewriting,
set the Prolog flag `optimise_clpfd` to `false`.

If you are used to the complicated operational considerations that
low-level arithmetic primitives necessitate, then moving to CLP(FD)
constraints may, due to their power and convenience, at first feel to
you excessive and almost like cheating. It _isn't_. Constraints are an
integral part of all popular Prolog systems, and they are designed
to help you eliminate and avoid the use of low-level and less general
primitives by providing declarative alternatives that are meant to be
used instead.


## Example: Factorial relation {#clpfd-factorial}

We illustrate the benefit of using #=/2 for more generality with a
simple example.

Consider first a rather conventional definition of `n_factorial/2`,
relating each natural number _N_ to its factorial _F_:

==
n_factorial(0, 1).
n_factorial(N, F) :-
        N #> 0,
        N1 #= N - 1,
        n_factorial(N1, F1),
        F #= N * F1.
==

This program uses CLP(FD) constraints _instead_ of low-level
arithmetic throughout, and everything that _would have worked_ with
low-level arithmetic _also_ works with CLP(FD) constraints, retaining
roughly the same performance. For example:

==
?- n_factorial(47, F).
F = 258623241511168180642964355153611979969197632389120000000000 ;
false.
==

Now the point: Due to the increased flexibility and generality of
CLP(FD) constraints, we are free to _reorder_ the goals as follows:

==
n_factorial(0, 1).
n_factorial(N, F) :-
        N #> 0,
        N1 #= N - 1,
        F #= N * F1,
        n_factorial(N1, F1).
==

In this concrete case, _termination_ properties of the predicate are
improved. For example, the following queries now both terminate:

==
?- n_factorial(N, 1).
N = 0 ;
N = 1 ;
false.

?- n_factorial(N, 3).
false.
==

To make the predicate terminate if _any_ argument is instantiated, add
the (implied) constraint `F #\= 0` before the recursive call.
Otherwise, the query `n_factorial(N, 0)` is the only non-terminating
case of this kind.

The value of CLP(FD) constraints does _not_ lie in completely freeing
us from _all_ procedural phenomena. For example, the two programs do
not even have the same _termination properties_ in all cases.
Instead, the primary benefit of CLP(FD) constraints is that they allow
you to try different execution orders and apply [**declarative
debugging**](https://www.metalevel.at/prolog/debugging)
techniques _at all_!  Reordering goals (and clauses) can significantly
impact the performance of Prolog programs, and you are free to try
different variants if you use declarative approaches. Moreover, since
all CLP(FD) constraints _always terminate_, placing them earlier can
at most _improve_, never worsen, the termination properties of your
programs. An additional benefit of CLP(FD) constraints is that they
eliminate the complexity of introducing `(is)/2` and `(=:=)/2` to
beginners, since _both_ predicates are subsumed by #=/2 when reasoning
over integers.

In the case above, the clauses are mutually exclusive _if_ the first
argument is sufficiently instantiated. To make the predicate
deterministic in such cases while retaining its generality, you can
use zcompare/3 to _reify_ a comparison, making the different cases
distinguishable by pattern matching. For example, in this concrete
case and others like it, you can use `zcompare(Comp, 0, N)` to obtain
as `Comp` the symbolic outcome (`<`, `=`, `>`) of 0 compared to N.

## Combinatorial constraints  {#clpfd-combinatorial}

In addition to subsuming and replacing low-level arithmetic
predicates, CLP(FD) constraints are often used to solve combinatorial
problems such as planning, scheduling and allocation tasks. Among the
most frequently used *combinatorial constraints* are all_distinct/1,
global_cardinality/2 and cumulative/2. This library also provides
several other constraints like disjoint2/1 and automaton/8, which are
useful in more specialized applications.

## Domains                             {#clpfd-domains}

Each CLP(FD) variable has an associated set of admissible integers,
which we call the variable's *domain*. Initially, the domain of each
CLP(FD) variable is the set of _all_ integers. CLP(FD) constraints
like #=/2, #>/2 and #\=/2 can at most reduce, and never extend, the
domains of their arguments. The constraints in/2 and ins/2 let us
explicitly state domains of CLP(FD) variables. The process of
determining and adjusting domains of variables is called constraint
*propagation*, and it is performed automatically by this library. When
the domain of a variable contains only one element, then the variable
is automatically unified to that element.

Domains are taken into account when further constraints are stated,
and by enumeration predicates like labeling/2.

## Example: Sudoku {#clpfd-sudoku}

As another example, consider _Sudoku_: It is a popular puzzle
over integers that can be easily solved with CLP(FD) constraints.

==
sudoku(Rows) :-
        length(Rows, 9), maplist(same_length(Rows), Rows),
        append(Rows, Vs), Vs ins 1..9,
        maplist(all_distinct, Rows),
        transpose(Rows, Columns),
        maplist(all_distinct, Columns),
        Rows = [As,Bs,Cs,Ds,Es,Fs,Gs,Hs,Is],
        blocks(As, Bs, Cs),
        blocks(Ds, Es, Fs),
        blocks(Gs, Hs, Is).

blocks([], [], []).
blocks([N1,N2,N3|Ns1], [N4,N5,N6|Ns2], [N7,N8,N9|Ns3]) :-
        all_distinct([N1,N2,N3,N4,N5,N6,N7,N8,N9]),
        blocks(Ns1, Ns2, Ns3).

problem(1, [[_,_,_,_,_,_,_,_,_],
            [_,_,_,_,_,3,_,8,5],
            [_,_,1,_,2,_,_,_,_],
            [_,_,_,5,_,7,_,_,_],
            [_,_,4,_,_,_,1,_,_],
            [_,9,_,_,_,_,_,_,_],
            [5,_,_,_,_,_,_,7,3],
            [_,_,2,_,1,_,_,_,_],
            [_,_,_,_,4,_,_,_,9]]).
==

Sample query:

==
?- problem(1, Rows), sudoku(Rows), maplist(portray_clause, Rows).
[9, 8, 7, 6, 5, 4, 3, 2, 1].
[2, 4, 6, 1, 7, 3, 9, 8, 5].
[3, 5, 1, 9, 2, 8, 7, 4, 6].
[1, 2, 8, 5, 3, 7, 6, 9, 4].
[6, 3, 4, 8, 9, 2, 1, 5, 7].
[7, 9, 5, 4, 6, 1, 8, 3, 2].
[5, 1, 9, 2, 8, 6, 4, 7, 3].
[4, 7, 2, 3, 1, 9, 5, 6, 8].
[8, 6, 3, 7, 4, 5, 2, 1, 9].
Rows = [[9, 8, 7, 6, 5, 4, 3, 2|...], ... , [...|...]].
==

In this concrete case, the constraint solver is strong enough to find
the unique solution without any search. For the general case, see
[search](<#clpfd-search>).


## Residual goals				{#clpfd-residual-goals}

Here is an example session with a few queries and their answers:

==
?- X #> 3.
X in 4..sup.

?- X #\= 20.
X in inf..19\/21..sup.

?- 2*X #= 10.
X = 5.

?- X*X #= 144.
X in -12\/12.

?- 4*X + 2*Y #= 24, X + Y #= 9, [X,Y] ins 0..sup.
X = 3,
Y = 6.

?- X #= Y #<==> B, X in 0..3, Y in 4..5.
B = 0,
X in 0..3,
Y in 4..5.
==

The answers emitted by the toplevel are called _residual programs_,
and the goals that comprise each answer are called **residual goals**.
In each case above, and as for all pure programs, the residual program
is declaratively equivalent to the original query. From the residual
goals, it is clear that the constraint solver has deduced additional
domain restrictions in many cases.

To inspect residual goals, it is best to let the toplevel display them
for us. Wrap the call of your predicate into call_residue_vars/2 to
make sure that all constrained variables are displayed. To make the
constraints a variable is involved in available as a Prolog term for
further reasoning within your program, use copy_term/3. For example:

==
?- X #= Y + Z, X in 0..5, copy_term([X,Y,Z], [X,Y,Z], Gs).
Gs = [clpfd: (X in 0..5), clpfd: (Y+Z#=X)],
X in 0..5,
Y+Z#=X.
==

This library also provides _reflection_ predicates (like fd_dom/2,
fd_size/2 etc.) with which we can inspect a variable's current
domain. These predicates can be useful if you want to implement your
own labeling strategies.

## Core relations and search    {#clpfd-search}

Using CLP(FD) constraints to solve combinatorial tasks typically
consists of two phases:

    1. **Modeling**. In this phase, all relevant constraints are stated.
    2. **Search**. In this phase, _enumeration predicates_ are used
       to search for concrete solutions.

It is good practice to keep the modeling part, via a dedicated
predicate called the *core relation*, separate from the actual
search for solutions. This lets us observe termination and
determinism properties of the core relation in isolation from the
search, and more easily try different search strategies.

As an example of a constraint satisfaction problem, consider the
cryptoarithmetic puzzle SEND + MORE = MONEY, where different letters
denote distinct integers between 0 and 9. It can be modeled in CLP(FD)
as follows:

==
puzzle([S,E,N,D] + [M,O,R,E] = [M,O,N,E,Y]) :-
        Vars = [S,E,N,D,M,O,R,Y],
        Vars ins 0..9,
        all_different(Vars),
                  S*1000 + E*100 + N*10 + D +
                  M*1000 + O*100 + R*10 + E #=
        M*10000 + O*1000 + N*100 + E*10 + Y,
        M #\= 0, S #\= 0.
==

Notice that we are _not_ using labeling/2 in this predicate, so that
we can first execute and observe the modeling part in isolation.
Sample query and its result (actual variables replaced for
readability):

==
?- puzzle(As+Bs=Cs).
As = [9, A2, A3, A4],
Bs = [1, 0, B3, A2],
Cs = [1, 0, A3, A2, C5],
A2 in 4..7,
all_different([9, A2, A3, A4, 1, 0, B3, C5]),
91*A2+A4+10*B3#=90*A3+C5,
A3 in 5..8,
A4 in 2..8,
B3 in 2..8,
C5 in 2..8.
==

From this answer, we see that this core relation _terminates_ and is in
fact _deterministic_. Moreover, we see from the residual goals that
the constraint solver has deduced more stringent bounds for all
variables. Such observations are only possible if modeling and search
parts are cleanly separated.

Labeling can then be used to search for solutions in a separate
predicate or goal:

==
?- puzzle(As+Bs=Cs), label(As).
As = [9, 5, 6, 7],
Bs = [1, 0, 8, 5],
Cs = [1, 0, 6, 5, 2] ;
false.
==

In this case, it suffices to label a subset of variables to find the
puzzle's unique solution, since the constraint solver is strong enough
to reduce the domains of remaining variables to singleton sets. In
general though, it is necessary to label all variables to obtain
ground solutions.

## Example: Eight queens puzzle {#clpfd-n-queens}

We illustrate the concepts of the preceding sections by means of the
so-called _eight queens puzzle_. The task is to place 8 queens on an
8x8 chessboard such that none of the queens is under attack. This
means that no two queens share the same row, column or diagonal.

To express this puzzle via CLP(FD) constraints, we must first pick a
suitable representation. Since CLP(FD) constraints reason over
_integers_, we must find a way to map the positions of queens to
integers. Several such mappings are conceivable, and it is not
immediately obvious which we should use. On top of that, different
constraints can be used to express the desired relations. For such
reasons, _modeling_ combinatorial problems via CLP(FD) constraints
often necessitates some creativity and has been described as more of
an art than a science.

In our concrete case, we observe that there must be exactly one queen
per column. The following representation therefore suggests itself: We
are looking for 8 integers, one for each column, where each integer
denotes the _row_ of the queen that is placed in the respective
column, and which are subject to certain constraints.

In fact, let us now generalize the task to the so-called _N queens
puzzle_, which is obtained by replacing 8 by _N_ everywhere it occurs
in the above description. We implement the above considerations in the
**core relation** `n_queens/2`, where the first argument is the number
of queens (which is identical to the number of rows and columns of the
generalized chessboard), and the second argument is a list of _N_
integers that represents a solution in the form described above.

==
n_queens(N, Qs) :-
        length(Qs, N),
        Qs ins 1..N,
        safe_queens(Qs).

safe_queens([]).
safe_queens([Q|Qs]) :- safe_queens(Qs, Q, 1), safe_queens(Qs).

safe_queens([], _, _).
safe_queens([Q|Qs], Q0, D0) :-
        Q0 #\= Q,
        abs(Q0 - Q) #\= D0,
        D1 #= D0 + 1,
        safe_queens(Qs, Q0, D1).
==

Note that all these predicates can be used in _all directions_: We
can use them to _find_ solutions, _test_ solutions and _complete_
partially instantiated solutions.

The original task can be readily solved with the following query:

==
?- n_queens(8, Qs), label(Qs).
Qs = [1, 5, 8, 6, 3, 7, 2, 4] .
==

Using suitable labeling strategies, we can easily find solutions with
80 queens and more:

==
?- n_queens(80, Qs), labeling([ff], Qs).
Qs = [1, 3, 5, 44, 42, 4, 50, 7, 68|...] .

?- time((n_queens(90, Qs), labeling([ff], Qs))).
% 5,904,401 inferences, 0.722 CPU in 0.737 seconds (98% CPU)
Qs = [1, 3, 5, 50, 42, 4, 49, 7, 59|...] .
==

Experimenting with different search strategies is easy because we have
separated the core relation from the actual search.



## Optimisation    {#clpfd-optimisation}

We can use labeling/2 to minimize or maximize the value of a CLP(FD)
expression, and generate solutions in increasing or decreasing order
of the value. See the labeling options `min(Expr)` and `max(Expr)`,
respectively.

Again, to easily try different labeling options in connection with
optimisation, we recommend to introduce a dedicated predicate for
posting constraints, and to use `labeling/2` in a separate goal. This
way, we can observe properties of the core relation in isolation,
and try different labeling options without recompiling our code.

If necessary, we can use `once/1` to commit to the first optimal
solution. However, it is often very valuable to see alternative
solutions that are _also_ optimal, so that we can choose among optimal
solutions by other criteria. For the sake of
[**purity**](https://www.metalevel.at/prolog/purity) and
completeness, we recommend to avoid `once/1` and other constructs that
lead to impurities in CLP(FD) programs.

Related to optimisation with CLP(FD) constraints are
[`library(simplex)`](http://eu.swi-prolog.org/man/simplex.html) and
CLP(Q) which reason about _linear_ constraints over rational numbers.

## Reification				{#clpfd-reification}

The constraints in/2, #=/2, #\=/2, #</2, #>/2, #=</2, and #>=/2 can be
_reified_, which means reflecting their truth values into Boolean
values represented by the integers 0 and 1. Let P and Q denote
reifiable constraints or Boolean variables, then:

    | #\ Q      | True iff Q is false                  |
    | P #\/ Q   | True iff either P or Q               |
    | P #/\ Q   | True iff both P and Q                |
    | P #\ Q    | True iff either P or Q, but not both |
    | P #<==> Q | True iff P and Q are equivalent      |
    | P #==> Q  | True iff P implies Q                 |
    | P #<== Q  | True iff Q implies P                 |

The constraints of this table are reifiable as well.

When reasoning over Boolean variables, also consider using
CLP(B) constraints as provided by
[`library(clpb)`](http://eu.swi-prolog.org/man/clpb.html).

## Enabling monotonic CLP(FD)		{#clpfd-monotonicity}

In the default execution mode, CLP(FD) constraints still exhibit some
non-relational properties. For example, _adding_ constraints can yield
new solutions:

==
?-          X #= 2, X = 1+1.
false.

?- X = 1+1, X #= 2, X = 1+1.
X = 1+1.
==

This behaviour is highly problematic from a logical point of view, and
it may render declarative debugging techniques inapplicable.

Set the Prolog flag `clpfd_monotonic` to `true` to make CLP(FD)
**monotonic**: This means that _adding_ new constraints _cannot_ yield
new solutions. When this flag is `true`, we must wrap variables that
occur in arithmetic expressions with the functor `(?)/1` or `(#)/1`. For
example:

==
?- set_prolog_flag(clpfd_monotonic, true).
true.

?- #(X) #= #(Y) + #(Z).
#(Y)+ #(Z)#= #(X).

?-          X #= 2, X = 1+1.
ERROR: Arguments are not sufficiently instantiated
==

The wrapper can be omitted for variables that are already constrained
to integers.

## Custom constraints			{#clpfd-custom-constraints}

We can define custom constraints. The mechanism to do this is not yet
finalised, and we welcome suggestions and descriptions of use cases
that are important to you.

As an example of how it can be done currently, let us define a new
custom constraint `oneground(X,Y,Z)`, where Z shall be 1 if at least
one of X and Y is instantiated:

==
:- multifile clpfd:run_propagator/2.

oneground(X, Y, Z) :-
        clpfd:make_propagator(oneground(X, Y, Z), Prop),
        clpfd:init_propagator(X, Prop),
        clpfd:init_propagator(Y, Prop),
        clpfd:trigger_once(Prop).

clpfd:run_propagator(oneground(X, Y, Z), MState) :-
        (   integer(X) -> clpfd:kill(MState), Z = 1
        ;   integer(Y) -> clpfd:kill(MState), Z = 1
        ;   true
        ).
==

First, clpfd:make_propagator/2 is used to transform a user-defined
representation of the new constraint to an internal form. With
clpfd:init_propagator/2, this internal form is then attached to X and
Y. From now on, the propagator will be invoked whenever the domains of
X or Y are changed. Then, clpfd:trigger_once/1 is used to give the
propagator its first chance for propagation even though the variables'
domains have not yet changed. Finally, clpfd:run_propagator/2 is
extended to define the actual propagator. As explained, this predicate
is automatically called by the constraint solver. The first argument
is the user-defined representation of the constraint as used in
clpfd:make_propagator/2, and the second argument is a mutable state
that can be used to prevent further invocations of the propagator when
the constraint has become entailed, by using clpfd:kill/1. An example
of using the new constraint:

==
?- oneground(X, Y, Z), Y = 5.
Y = 5,
Z = 1,
X in inf..sup.
==

## Applications   {#clpfd-applications}

CLP(FD) applications that we find particularly impressive and worth
studying include:

  * Michael Hendricks uses CLP(FD) constraints for flexible reasoning
    about _dates_ and _times_ in the
    [`julian`](http://www.swi-prolog.org/pack/list?p=julian) package.
  * Julien Cumin uses CLP(FD) constraints for integer arithmetic in
    [=Brachylog=](https://github.com/JCumin/Brachylog).

## Acknowledgments {#clpfd-acknowledgments}

This library gives you a glimpse of what [**SICStus
Prolog**](https://sicstus.sics.se/) can do. The API is intentionally
mostly compatible with that of SICStus Prolog, so that you can easily
switch to a much more feature-rich and much faster CLP(FD) system when
you need it. I thank [Mats Carlsson](https://www.sics.se/~matsc/), the
designer and main implementor of SICStus Prolog, for his elegant
example. I first encountered his system as part of the excellent
[**GUPU**](http://www.complang.tuwien.ac.at/ulrich/gupu/) teaching
environment by [Ulrich
Neumerkel](http://www.complang.tuwien.ac.at/ulrich/). Ulrich was also
the first and most determined tester of the present system, filing
hundreds of comments and suggestions for improvement. [Tom
Schrijvers](https://people.cs.kuleuven.be/~tom.schrijvers/) has
contributed several constraint libraries to SWI-Prolog, and I learned
a lot from his coding style and implementation examples. [Bart
Demoen](https://people.cs.kuleuven.be/~bart.demoen/) was a driving
force behind the implementation of attributed variables in SWI-Prolog,
and this library could not even have started without his prior work
and contributions. Thank you all!

## CLP(FD) predicate index			{#clpfd-predicate-index}

In the following, each CLP(FD) predicate is described in more detail.

We recommend the following link to refer to this manual:

http://eu.swi-prolog.org/man/clpfd.html



### Arithmetic constraints {#clpfd-arithmetic}

_Arithmetic_ constraints are the most basic use of CLP(FD). Every time
you use `(is)/2` or one of the low-level arithmetic comparisons
(`(<)/2`, `(>)/2` etc.) over integers, consider using CLP(FD)
constraints _instead_. This can at most _increase_ the generality of
your programs. See [declarative integer
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
value is itself turned into a CLP(FD) variable, so that we can
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

Reflection predicates let us obtain, in a well-defined way,
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
  * [[fd_degree/2]]


### FD set predicates {#clpfd-fdset-predicates}

These predicates allow operating directly on the internal representation
of CLP(FD) domains. In this context, such an internal domain
representation is called an *FD set*.

Note that the exact term representation of FD sets is unspecified and
will vary across CLP(FD) implementations or even different versions of
the same implementation. FD set terms should be manipulated *only*
using the predicates in this section. The behavior of other operations
on FD set terms is undefined. In particular, you should *not* construct
or deconstruct FD sets by unification, and you *cannot* reliably compare
FD sets using unification or generic term equality/comparison
predicates.

  * [[(in_set)/2]]
  * [[fd_set/2]]
  * [[is_fdset/1]]
  * [[empty_fdset/1]]
  * [[fdset_parts/4]]
  * [[empty_interval/2]]
  * [[fdset_interval/3]]
  * [[fdset_singleton/2]]
  * [[fdset_min/2]]
  * [[fdset_max/2]]
  * [[fdset_size/2]]
  * [[list_to_fdset/2]]
  * [[fdset_to_list/2]]
  * [[range_to_fdset/2]]
  * [[fdset_to_range/2]]
  * [[fdset_add_element/3]]
  * [[fdset_del_element/3]]
  * [[fdset_disjoint/2]]
  * [[fdset_intersect/2]]
  * [[fdset_intersection/3]]
  * [[fdset_member/2]]
  * [[fdset_eq/2]]
  * [[fdset_subset/2]]
  * [[fdset_subtract/3]]
  * [[fdset_union/3]]
  * [[fdset_union/2]]
  * [[fdset_complement/2]]


### FD miscellaneous predicates {#clpfd-misc-predicates}

The predicates in this section are not clp(fd) predicates. They ended up
in this library for historical reasons and may be moved to other libraries
in the future.

  * [[transpose/2]]


## Closing and opening words about CLP(FD) {#clpfd-closing-opening}

CLP(FD) constraints are one of the main reasons why logic programming
approaches are picked over other paradigms for solving many tasks of
high practical relevance. The usefulness of CLP(FD) constraints for
scheduling, allocation and combinatorial optimization tasks is
well-known both in academia and industry.

With this library, we take the applicability of CLP(FD) constraints
one step further, following the road that visionary systems like
SICStus Prolog have already clearly outlined: This library is designed
to completely subsume and _replace_ low-level predicates over
integers, which were in the past repeatedly found to be a major
stumbling block when introducing logic programming to beginners.

Embrace the change and new opportunities that this paradigm allows!
Use CLP(FD) constraints in your programs. The use of CLP(FD)
constraints instead of low-level arithmetic is also a good indicator
to judge the quality of any introductory Prolog text.
