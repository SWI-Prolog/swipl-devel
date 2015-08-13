/*  Part of SWI-Prolog

    Author:        Markus Triska
    E-mail:        triska@gmx.at
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2014, 2015 Markus Triska

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   CLP(B): Constraint Logic Programming over Boolean variables.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- module(clpb, [
                 op(300, fy, ~),
                 op(500, yfx, #),
                 sat/1,
                 taut/2,
                 labeling/1,
                 sat_count/2
                ]).

:- use_module(library(error)).
:- use_module(library(assoc)).
:- use_module(library(apply_macros)).

/** <module> Constraint Logic Programming over Boolean Variables

### Introduction                        {#clpb-intro}

Constraint programming is a declarative formalism that lets you state
relations between terms. This library provides CLP(B), Constraint
Logic Programming over Boolean Variables. It can be used to model and
solve combinatorial problems such as verification, allocation and
covering tasks.

The implementation is based on reduced and ordered Binary Decision
Diagrams (BDDs).


### Boolean expressions {#clpb-exprs}

A _Boolean expression_ is one of:

    | `0`                | false                                |
    | `1`                | true                                 |
    | _variable_         | unknown truth value                  |
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
    | card(Is,Exprs)     | _see below_                          |
    | `+(Exprs)`         | _see below_                          |
    | `*(Exprs)`         | _see below_                          |

where _Expr_ again denotes a Boolean expression.

The Boolean expression card(Is,Exprs) is true iff the number of true
expressions in the list `Exprs` is a member of the list `Is` of
integers and integer ranges of the form `From-To`.

`+(Exprs)` and `*(Exprs)` denote, respectively, the disjunction and
conjunction of all elements in the list `Exprs` of Boolean
expressions.

### Interface predicates   {#clpb-interface}

Important interface predicates of CLP(B) are:

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

### Examples                            {#clpb-examples}

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
sat(1#X#X*Y),
sat(1#Y#Y*Z).
==

The pending residual goals constrain remaining variables to Boolean
expressions and are declaratively equivalent to the original query.

@author Markus Triska
*/


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Each CLP(B) variable belongs to exactly one BDD. Each CLP(B)
   variable gets an attribute (in module "clpb") of the form:

        index_root(Index,Root)

   where Index is the variable's unique integer index, and Root is the
   root of the BDD that the variable belongs to.

   Each CLP(B) variable also gets an attribute in module clpb_hash: an
   association table node(LID,HID) -> Node, to keep the BDD reduced.
   The association table of each variable must be rebuilt on occasion
   to remove nodes that are no longer reachable. We rebuild the
   association tables of involved variables after BDDs are merged to
   build a new root. This only serves to reclaim memory: Keeping a
   node in a local table even when it no longer occurs in any BDD does
   not affect the solver's correctness. However, apply_shortcut/4
   relies on the invariant that every node that occurs in the relevant
   BDDs is also registered in the table of its branching variable.

   A root is a logical variable with a single attribute ("clpb_bdd")
   of the form:

        Sat-BDD

   where Sat is the SAT formula (in original form) that corresponds to
   BDD. Sat is necessary to rebuild the BDD after variable aliasing,
   and to project all remaining constraints to a list of sat/1 goals.

   Finally, a BDD is either:

      *)  The integers 0 or 1, denoting false and true, respectively, or
      *)  A node of the form

           node(ID, Var, Low, High, Aux)
               Where ID is the node's unique integer ID, Var is the
               node's branching variable, and Low and High are the
               node's low (Var = 0) and high (Var = 1) children. Aux
               is a free variable, one for each node, that can be used
               to attach attributes and store intermediate results.

   Variable aliasing is treated as a conjunction of corresponding SAT
   formulae.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Type checking.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

is_sat(V)     :- var(V), !.
is_sat(I)     :- integer(I), between(0, 1, I).
is_sat(~A)    :- is_sat(A).
is_sat(A*B)   :- is_sat(A), is_sat(B).
is_sat(A+B)   :- is_sat(A), is_sat(B).
is_sat(A#B)   :- is_sat(A), is_sat(B).
is_sat(A=:=B) :- is_sat(A), is_sat(B).
is_sat(A=\=B) :- is_sat(A), is_sat(B).
is_sat(A=<B)  :- is_sat(A), is_sat(B).
is_sat(A>=B)  :- is_sat(A), is_sat(B).
is_sat(A<B)   :- is_sat(A), is_sat(B).
is_sat(A>B)   :- is_sat(A), is_sat(B).
is_sat(+(Ls)) :- must_be(list, Ls), maplist(is_sat, Ls).
is_sat(*(Ls)) :- must_be(list, Ls), maplist(is_sat, Ls).
is_sat(X^F)   :- var(X), is_sat(F).
is_sat(card(Is,Fs)) :-
        must_be(list(ground), Is),
        must_be(list, Fs),
        maplist(is_sat, Fs).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Rewriting to canonical expressions.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

% elementary
sat_rewrite(V, V)       :- var(V), !.
sat_rewrite(I, I)       :- integer(I).
sat_rewrite(P0*Q0, P*Q) :- sat_rewrite(P0, P), sat_rewrite(Q0, Q).
sat_rewrite(P0+Q0, P+Q) :- sat_rewrite(P0, P), sat_rewrite(Q0, Q).
sat_rewrite(P0#Q0, P#Q) :- sat_rewrite(P0, P), sat_rewrite(Q0, Q).
sat_rewrite(X^F0, X^F)  :- sat_rewrite(F0, F).
sat_rewrite(card(Is,Fs0), card(Is,Fs)) :-
        maplist(sat_rewrite, Fs0, Fs).
% synonyms
sat_rewrite(~P, R)      :- sat_rewrite(1 # P, R).
sat_rewrite(P =:= Q, R) :- sat_rewrite(~P # Q, R).
sat_rewrite(P =\= Q, R) :- sat_rewrite(P # Q, R).
sat_rewrite(P =< Q, R)  :- sat_rewrite(~P + Q, R).
sat_rewrite(P >= Q, R)  :- sat_rewrite(Q =< P, R).
sat_rewrite(P < Q, R)   :- sat_rewrite(~P * Q, R).
sat_rewrite(P > Q, R)   :- sat_rewrite(Q < P, R).
sat_rewrite(+(Ls), R)   :- foldl(or, Ls, 0, F), sat_rewrite(F, R).
sat_rewrite(*(Ls), R)   :- foldl(and, Ls, 1, F), sat_rewrite(F, R).

or(A, B, B + A).

and(A, B, B * A).

must_be_sat(Sat) :-
        (   is_sat(Sat) -> true
        ;   no_truth_value(Sat)
        ).

no_truth_value(Term) :- domain_error(clpb_expr, Term).

parse_sat(Sat0, Sat) :-
        must_be_sat(Sat0),
        sat_rewrite(Sat0, Sat),
        term_variables(Sat, Vs),
        maplist(enumerate_variable, Vs).

enumerate_variable(V) :-
        (   var_index_root(V, _, _) -> true
        ;   clpb_next_id('$clpb_next_var', Index),
            put_attr(V, clpb, index_root(Index,_)),
            put_empty_hash(V)
        ).

var_index(V, I) :- var_index_root(V, I, _).

var_index_root(V, I, Root) :- get_attr(V, clpb, index_root(I,Root)).

put_empty_hash(V) :-
        empty_assoc(H0),
        put_attr(V, clpb_hash, H0).

sat_roots(Sat, Roots) :-
        term_variables(Sat, Vs),
        maplist(var_index_root, Vs, _, Roots0),
        term_variables(Roots0, Roots).

%% sat(+Expr) is semidet.
%
% True iff Expr is a satisfiable Boolean expression.

sat(Sat0) :-
        (   phrase(sat_ands(Sat0), Ands), Ands = [_,_|_] ->
            maplist(sat, Ands)
        ;   parse_sat(Sat0, Sat),
            sat_bdd(Sat, BDD),
            sat_roots(Sat, Roots),
            roots_and(Roots, Sat0-BDD, And-BDD1),
            maplist(del_bdd, Roots),
            maplist(=(Root), Roots),
            root_put_formula_bdd(Root, And, BDD1),
            is_bdd(BDD1),
            satisfiable_bdd(BDD1)
        ).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Posting many small sat/1 constraints is better than posting a huge
   conjunction (or negated disjunction), because unneeded nodes are
   removed from node tables after BDDs are merged. This is not
   possible in sat_bdd/2 because the nodes may occur in other BDDs. A
   better version of sat_bdd/2 or a proper implementation of a unique
   table including garbage collection would make this obsolete and
   also improve taut/2 and sat_count/2 in such cases.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

sat_ands(X) -->
        (   { var(X) } -> [X]
        ;   { X = (A*B) } -> sat_ands(A), sat_ands(B)
        ;   { X = *(Ls) } -> sat_ands_(Ls)
        ;   { X = ~Y } -> not_ors(Y)
        ;   [X]
        ).

sat_ands_([]) --> [].
sat_ands_([L|Ls]) --> [L], sat_ands_(Ls).

not_ors(X) -->
        (   { var(X) } -> [~X]
        ;   { X = (A+B) } -> not_ors(A), not_ors(B)
        ;   { X = +(Ls) } -> not_ors_(Ls)
        ;   [~X]
        ).

not_ors_([]) --> [].
not_ors_([L|Ls]) --> [~L], not_ors_(Ls).

del_bdd(Root) :- del_attr(Root, clpb_bdd).

root_get_formula_bdd(Root, F, BDD) :- get_attr(Root, clpb_bdd, F-BDD).

root_put_formula_bdd(Root, F, BDD) :- put_attr(Root, clpb_bdd, F-BDD).

roots_and(Roots, Sat0-BDD0, Sat-BDD) :-
        foldl(root_and, Roots, Sat0-BDD0, Sat-BDD),
        rebuild_hashes(BDD).

root_and(Root, Sat0-BDD0, Sat-BDD) :-
        (   root_get_formula_bdd(Root, F, B) ->
            Sat = F*Sat0,
            bdd_and(B, BDD0, BDD)
        ;   Sat = Sat0,
            BDD = BDD0
        ).

bdd_and(NA, NB, And) :-
        apply(*, NA, NB, And),
        is_bdd(And).

%% taut(+Expr, -T) is semidet
%
% Succeeds with T = 0 if the Boolean expression Expr cannot be
% satisfied, and with T = 1 if Expr is always true with respect to the
% current constraints. Fails otherwise.

taut(Sat0, T) :-
        parse_sat(Sat0, Sat),
        (   T = 0, \+ sat(Sat) -> true
        ;   T = 1, tautology(Sat) -> true
        ;   false
        ).

tautology(Sat) :-
        (   phrase(sat_ands(Sat), Ands), Ands = [_,_|_] ->
            maplist(tautology, Ands)
        ;   \+ sat(1#Sat)
        ).

satisfiable_bdd(BDD) :-
        (   BDD == 0 -> false
        ;   BDD == 1 -> true
        ;   (   bdd_nodes(var_unbound, BDD, Nodes) ->
                bdd_variables_classification(BDD, Nodes, Classes),
                partition(var_class, Classes, Eqs, Bs, Ds),
                domain_consistency(Eqs, Goal),
                aliasing_consistency(Bs, Ds, Goals),
                maplist(unification, [Goal|Goals])
            ;   % if any variable is instantiated, we do not perform
                % any propagation for now
                true
            )
        ).

var_class(_=_, <).
var_class(further_branching(_,_), =).
var_class(negative_decisive(_), >).

unification(true).
unification(A=B) :- A = B.      % safe_goal/1 detects safety of this call

var_unbound(Node) :-
        node_var_low_high(Node, Var, _, _),
        var(Var).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   By aliasing consistency, we mean that all unifications X=Y, where
   taut(X=:=Y, 1) holds, are posted.

   To detect this, we distinguish two kinds of variables among those
   variables that are not skipped in any branch: further-branching and
   negative-decisive. X is negative-decisive iff every node where X
   appears as a branching variable has 0 as one of its children. X is
   further-branching iff 1 is not a direct child of any node where X
   appears as a branching variable.

   Any potential aliasing must involve one further-branching, and one
   negative-decisive variable. X=Y must hold if, for each low branch
   of nodes with X as branching variable, Y has high branch 0, and for
   each high branch of nodes involving X, Y has low branch 0.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

aliasing_consistency(Bs, Ds, Goals) :-
        phrase(aliasings(Bs, Ds), Goals).

aliasings([], _) --> [].
aliasings([further_branching(B,Nodes)|Bs], Ds) -->
        { var_index(B, BI) },
        aliasings_(Ds, B, BI, Nodes),
        aliasings(Bs, Ds).

aliasings_([], _, _, _) --> [].
aliasings_([negative_decisive(D)|Ds], B, BI, Nodes) -->
        { var_index(D, DI) },
        (   { DI > BI,
              always_false(high, DI, Nodes),
              always_false(low, DI, Nodes) } ->
            [D=B]
        ;   []
        ),
        aliasings_(Ds, B, BI, Nodes).

always_false(Which, DI, Nodes) :-
        phrase(nodes_always_false(Nodes, Which, DI), Opposites),
        maplist(with_aux(unvisit), Opposites).

nodes_always_false([], _, _) --> [].
nodes_always_false([Node|Nodes], Which, DI) -->
        { which_node_child(Which, Node, Child),
          opposite(Which, Opposite) },
        opposite_always_false(Opposite, DI, Child),
        nodes_always_false(Nodes, Which, DI).

which_node_child(low, Node, Child) :-
        node_var_low_high(Node, _, Child, _).
which_node_child(high, Node, Child) :-
        node_var_low_high(Node, _, _, Child).

opposite(low, high).
opposite(high, low).

opposite_always_false(Opposite, DI, Node) -->
        (   { node_visited(Node) } -> []
        ;   { node_var_low_high(Node, Var, Low, High),
              with_aux(put_visited, Node),
              var_index(Var, VI) },
            [Node],
            (   { VI =:= DI } ->
                { which_node_child(Opposite, Node, Child),
                  Child == 0 }
            ;   opposite_always_false(Opposite, DI, Low),
                opposite_always_false(Opposite, DI, High)
            )
        ).

further_branching(Node) :-
        node_var_low_high(Node, _, Low, High),
        Low \== 1,
        High \== 1.

negative_decisive(Node) :-
        node_var_low_high(Node, _, Low, High),
        (   Low == 0 -> true
        ;   High == 0 -> true
        ;   false
        ).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Instantiate all variables that only admit a single Boolean value.

   This is the case if: The variable is not skipped in any branch
   leading to 1 (its being skipped means that it may be assigned
   either 0 or 1 and can thus not be fixed yet), and all nodes where
   it occurs as a branching variable have either lower or upper child
   fixed to 0 consistently.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

domain_consistency(Eqs, Goal) :-
        maplist(eq_a_b, Eqs, Vs, Values),
        Goal = (Vs = Values). % propagate all assignments at once

eq_a_b(A=B, A, B).

consistently_false_(Which, Node) :-
        which_node_child(Which, Node, Child),
        Child == 0.

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   In essentially one sweep of the BDD, all variables can be classified:
   Unification with 0 or 1, further branching and/or negative decisive.

   Strategy: Breadth-first traversal of the BDD, failing (and thus
   clearing all attributes) if the variable is skipped in some branch,
   and moving the frontier along each time.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

bdd_variables_classification(BDD, Nodes, Classes) :-
        nodes_variables(Nodes, Vs0),
        variables_in_index_order(Vs0, Vs),
        phrase(variables_classification(Vs, [BDD]), Classes),
        maplist(with_aux(unvisit), Nodes).

variables_classification([], _) --> [].
variables_classification([V|Vs], Nodes0) -->
        { var_index(V, Index) },
        (   { phrase(nodes_with_variable(Nodes0, Index), Nodes) } ->
            (   { maplist(consistently_false_(low), Nodes) } -> [V=1]
            ;   { maplist(consistently_false_(high), Nodes) } -> [V=0]
            ;   []
            ),
            (   { maplist(further_branching, Nodes) } ->
                [further_branching(V, Nodes)]
            ;   []
            ),
            (   { maplist(negative_decisive, Nodes) } ->
                [negative_decisive(V)]
            ;   []
            ),
            { maplist(with_aux(unvisit), Nodes) },
            variables_classification(Vs, Nodes)
        ;   variables_classification(Vs, Nodes0)
        ).

nodes_with_variable([], _) --> [].
nodes_with_variable([Node|Nodes], VI) -->
        { Node \== 1 },
        (   { node_visited(Node) } -> nodes_with_variable(Nodes, VI)
        ;   { with_aux(put_visited, Node),
              node_var_low_high(Node, OVar, Low, High),
              var_index(OVar, OVI) },
            { OVI =< VI },
            (   { OVI =:= VI } -> [Node]
            ;   nodes_with_variable([Low,High], VI)
            ),
            nodes_with_variable(Nodes, VI)
        ).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Node management. Always use an existing node, if there is one.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

make_node(Var, Low, High, Node) :-
        (   Low == High -> Node = Low
        ;   low_high_key(Low, High, Key),
            (   lookup_node(Var, Key, Node) -> true
            ;   clpb_next_id('$clpb_next_node', ID),
                Node = node(ID,Var,Low,High,_Aux),
                register_node(Var, Key, Node)
            )
        ).

make_node(Var, Low, High, Node) -->
        % make it conveniently usable within DCGs
        { make_node(Var, Low, High, Node) }.


% The key of a node for hashing is determined by the IDs of its
% children.

low_high_key(Low, High, node(LID,HID)) :-
        node_id(Low, LID),
        node_id(High, HID).


rebuild_hashes(BDD) :-
        bdd_nodes(nodevar_put_empty_hash, BDD, Nodes),
        maplist(re_register_node, Nodes).

nodevar_put_empty_hash(Node) :-
        node_var_low_high(Node, Var, _, _),
        empty_assoc(H0),
        put_attr(Var, clpb_hash, H0).

re_register_node(Node) :-
        node_var_low_high(Node, Var, Low, High),
        low_high_key(Low, High, Key),
        register_node(Var, Key, Node).

register_node(Var, Key, Node) :-
        get_attr(Var, clpb_hash, H0),
        put_assoc(Key, H0, Node, H),
        put_attr(Var, clpb_hash, H).

lookup_node(Var, Key, Node) :-
        get_attr(Var, clpb_hash, H0),
        get_assoc(Key, H0, Node).


node_id(0, false).
node_id(1, true).
node_id(node(ID,_,_,_,_), ID).

node_aux(Node, Aux) :- arg(5, Node, Aux).

node_var_low_high(Node, Var, Low, High) :-
        Node = node(_,Var,Low,High,_).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   sat_bdd/2 converts a SAT formula in canonical form to an ordered
   and reduced BDD.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

sat_bdd(V, Node)           :- var(V), !, make_node(V, 0, 1, Node).
sat_bdd(I, I)              :- integer(I), !.
sat_bdd(V^Sat, Node)       :- !, sat_bdd(Sat, BDD), existential(V, BDD, Node).
sat_bdd(card(Is,Fs), Node) :- !, counter_network(Is, Fs, Node).
sat_bdd(Sat, Node)         :- !,
        Sat =.. [F,A,B],
        sat_bdd(A, NA),
        sat_bdd(B, NB),
        apply(F, NA, NB, Node).

existential(V, BDD, Node) :-
        var_index(V, Index),
        bdd_restriction(BDD, Index, 0, NA),
        bdd_restriction(BDD, Index, 1, NB),
        apply(+, NA, NB, Node).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Counter network for card(Is,Fs).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

counter_network(Cs, Fs, Node) :-
        same_length([_|Fs], Indicators),
        fill_indicators(Indicators, 0, Cs),
        phrase(formulas_variables(Fs, Vars0), ExBDDs),
        maplist(unvisit, Vars0),
        % The counter network is built bottom-up, so variables with
        % highest index must be processed first.
        variables_in_index_order(Vars0, Vars1),
        reverse(Vars1, Vars),
        counter_network_(Vars, Indicators, Node0),
        foldl(existential_and, ExBDDs, Node0, Node).

% Introduce fresh variables for expressions that are not variables.
% These variables are later existentially quantified to remove them.
% Also, new variables are introduced for variables that are used more
% than once, as in card([0,1],[X,X,Y]), to keep the BDD ordered.

formulas_variables([], []) --> [].
formulas_variables([F|Fs], [V|Vs]) -->
        (   { var(F), \+ is_visited(F) } ->
            { V = F,
              put_visited(F) }
        ;   { enumerate_variable(V),
              sat_rewrite(V =:= F, Sat),
              sat_bdd(Sat, BDD) },
            [V-BDD]
        ),
        formulas_variables(Fs, Vs).

counter_network_([], [Node], Node).
counter_network_([Var|Vars], [I|Is0], Node) :-
        foldl(indicators_pairing(Var), Is0, Is, I, _),
        counter_network_(Vars, Is, Node).

indicators_pairing(Var, I, Node, Prev, I) :- make_node(Var, Prev, I, Node).

fill_indicators([], _, _).
fill_indicators([I|Is], Index0, Cs) :-
        (   memberchk(Index0, Cs) -> I = 1
        ;   member(A-B, Cs), between(A, B, Index0) -> I = 1
        ;   I = 0
        ),
        Index1 is Index0 + 1,
        fill_indicators(Is, Index1, Cs).

existential_and(Ex-BDD, Node0, Node) :-
        bdd_and(BDD, Node0, Node1),
        existential(Ex, Node1, Node),
        % remove attributes to avoid residual goals for variables that
        % are only used as substitutes for formulas
        del_attrs(Ex).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Compute F(NA, NB).

   We use a DCG to thread through an implicit argument G0, an
   association table F(IDA,IDB) -> Node, used for memoization.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

apply(F, NA, NB, Node) :-
        empty_assoc(G0),
        phrase(apply(F, NA, NB, Node), [G0], _).

apply(F, NA, NB, Node) -->
        (   { integer(NA), integer(NB) } -> { once(bool_op(F, NA, NB, Node)) }
        ;   { apply_shortcut(F, NA, NB, Node) } -> []
        ;   { node_id(NA, IDA), node_id(NB, IDB), Key =.. [F,IDA,IDB] },
            (   state(G0), { get_assoc(Key, G0, Node) } -> []
            ;   apply_(F, NA, NB, Node),
                state(G0, G),
                { put_assoc(Key, G0, Node, G) }
            )
        ).

apply_shortcut(+, NA, NB, Node) :-
        (   NA == 0 -> Node = NB
        ;   NA == 1 -> Node = 1
        ;   NB == 0 -> Node = NA
        ;   NB == 1 -> Node = 1
        ;   false
        ).

apply_shortcut(*, NA, NB, Node) :-
        (   NA == 0 -> Node = 0
        ;   NA == 1 -> Node = NB
        ;   NB == 0 -> Node = 0
        ;   NB == 1 -> Node = NA
        ;   false
        ).


apply_(F, NA, NB, Node) -->
        { var_less_than(NA, NB),
          !,
          node_var_low_high(NA, VA, LA, HA) },
        apply(F, LA, NB, Low),
        apply(F, HA, NB, High),
        make_node(VA, Low, High, Node).
apply_(F, NA, NB, Node) -->
        { node_var_low_high(NA, VA, LA, HA),
          node_var_low_high(NB, VB, LB, HB),
          VA == VB },
        !,
        apply(F, LA, LB, Low),
        apply(F, HA, HB, High),
        make_node(VA, Low, High, Node).
apply_(F, NA, NB, Node) --> % NB < NA
        { node_var_low_high(NB, VB, LB, HB) },
        apply(F, NA, LB, Low),
        apply(F, NA, HB, High),
        make_node(VB, Low, High, Node).


node_varindex(Node, VI) :-
        node_var_low_high(Node, V, _, _),
        var_index(V, VI).

var_less_than(NA, NB) :-
        (   integer(NB) -> true
        ;   node_varindex(NA, VAI),
            node_varindex(NB, VBI),
            VAI < VBI
        ).

bool_op(+, 0, 0, 0).
bool_op(+, 0, 1, 1).
bool_op(+, 1, 0, 1).
bool_op(+, 1, 1, 1).

bool_op(*, 0, 0, 0).
bool_op(*, 0, 1, 0).
bool_op(*, 1, 0, 0).
bool_op(*, 1, 1, 1).

bool_op(#, 0, 0, 0).
bool_op(#, 0, 1, 1).
bool_op(#, 1, 0, 1).
bool_op(#, 1, 1, 0).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Access implicit state in DCGs.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

state(S) --> state(S, S).

state(S0, S), [S] --> [S0].

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Unification. X = Expr is equivalent to sat(X =:= Expr).

   Current limitation:
   ===================

   The current interface of attributed variables is not general enough
   to express what we need. For example,

       ?- sat(A + B), A = A + 1.

   should be equivalent to

       ?- sat(A + B), sat(A =:= A + 1).

   However, attr_unify_hook/2 is only called *after* the unification
   of A with A + 1 has already taken place and turned A into a cyclic
   ground term, raised an error or failed (depending on the flag
   occurs_check), making it impossible to reason about the variable A
   in the unification hook. Therefore, a more general interface for
   attributed variables should replace the current one. In particular,
   unification filters should be able to reason about terms before
   they are unified with anything.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

attr_unify_hook(index_root(I,Root), Other) :-
        (   integer(Other) ->
            (   between(0, 1, Other) ->
                root_get_formula_bdd(Root, Sat, BDD0),
                bdd_restriction(BDD0, I, Other, BDD),
                root_put_formula_bdd(Root, Sat, BDD),
                satisfiable_bdd(BDD)
            ;   no_truth_value(Other)
            )
        ;   % due to variable aliasing, any BDDs may now be unordered,
            % so we need to rebuild the new BDD from the conjunction.
            root_get_formula_bdd(Root, Sat0, _),
            Sat = Sat0*OtherSat,
            (   var(Other), var_index_root(Other, _, OtherRoot),
                OtherRoot \== Root ->
                root_get_formula_bdd(OtherRoot, OtherSat, _),
                parse_sat(Sat, Sat1),
                sat_bdd(Sat1, BDD1),
                And = Sat1,
                sat_roots(Sat, Roots)
            ;   parse_sat(Other, OtherSat),
                sat_roots(Sat, Roots),
                maplist(root_rebuild_bdd, Roots),
                roots_and(Roots, 1-1, And-BDD1)
            ),
            maplist(del_bdd, Roots),
            maplist(=(NewRoot), Roots),
            root_put_formula_bdd(NewRoot, And, BDD1),
            is_bdd(BDD1),
            satisfiable_bdd(BDD1)
        ).

root_rebuild_bdd(Root) :-
        (   root_get_formula_bdd(Root, F0, _) ->
            parse_sat(F0, Sat),
            sat_bdd(Sat, BDD),
            root_put_formula_bdd(Root, F0, BDD)
        ;   true
        ).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Support for project_attributes/2.

   This is called by the toplevel as

      project_attributes(+QueryVars, +AttrVars)

   in order to project all remaining constraints onto QueryVars.

   All CLP(B) variables that do not occur in QueryVars or AttrVars
   need to be existentially quantified, so that they do not occur in
   residual goals. This is very easy to do in the case of CLP(B).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

project_attributes(QueryVars0, AttrVars) :-
        append(QueryVars0, AttrVars, QueryVars1),
        include(clpb_variable, QueryVars1, QueryVars),
        maplist(var_index_root, QueryVars, _, Roots0),
        sort(Roots0, Roots),
        maplist(remove_hidden_variables(QueryVars), Roots).

clpb_variable(Var) :- var_index(Var, _).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   All CLP(B) variables occurring in BDDs but not in query variables
   become existentially quantified. This must also be reflected in the
   formula. In addition, an attribute is attached to these variables
   to suppress superfluous sat(V=:=V) goals.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

remove_hidden_variables(QueryVars, Root) :-
        root_get_formula_bdd(Root, Formula, BDD0),
        maplist(put_visited, QueryVars),
        bdd_variables(BDD0, HiddenVars),
        maplist(unvisit, QueryVars),
        foldl(existential, HiddenVars, BDD0, BDD),
        foldl(quantify_existantially, HiddenVars, Formula, ExFormula),
        root_put_formula_bdd(Root, ExFormula, BDD).

quantify_existantially(E, E0, E^E0) :- put_attr(E, clpb_omit_boolean, true).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BDD restriction.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

bdd_restriction(Node, VI, Value, Res) :-
        empty_assoc(G0),
        phrase(bdd_restriction_(Node, VI, Value, Res), [G0], _),
        is_bdd(Res).

bdd_restriction_(Node, VI, Value, Res) -->
        (   { integer(Node) } -> { Res = Node }
        ;   { node_var_low_high(Node, Var, Low, High) } ->
            (   { integer(Var) } ->
                (   { Var =:= 0 } -> bdd_restriction_(Low, VI, Value, Res)
                ;   { Var =:= 1 } -> bdd_restriction_(High, VI, Value, Res)
                ;   { no_truth_value(Var) }
                )
            ;   { var_index(Var, I0),
                  node_id(Node, ID) },
                (   { I0 =:= VI } ->
                    (   { Value =:= 0 } -> { Res = Low }
                    ;   { Value =:= 1 } -> { Res = High }
                    )
                ;   { I0 > VI } -> { Res = Node }
                ;   state(G0), { get_assoc(ID, G0, Res) } -> []
                ;   bdd_restriction_(Low, VI, Value, LRes),
                    bdd_restriction_(High, VI, Value, HRes),
                    make_node(Var, LRes, HRes, Res),
                    state(G0, G),
                    { put_assoc(ID, G0, Res, G) }
                )
            )
        ;   { domain_error(node, Node) }
        ).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Relating a BDD to its elements (nodes and variables).

   Note that BDDs can become quite big (easily millions of nodes), and
   memory space is a major bottleneck for many problems. If possible,
   we therefore do not duplicate the entire BDD in memory (as in
   bdd_ites/2), but only extract its features as needed.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

bdd_nodes(BDD, Ns) :- bdd_nodes(ignore_node, BDD, Ns).

ignore_node(_).

% VPred is a unary predicate that is called for each node that has a
% branching variable (= each inner node).

bdd_nodes(VPred, BDD, Ns) :-
        phrase(bdd_nodes_(VPred, BDD), Ns),
        maplist(with_aux(unvisit), Ns).

bdd_nodes_(VPred, Node) -->
        (   { node_visited(Node) } -> []
        ;   { call(VPred, Node),
              with_aux(put_visited, Node),
              node_var_low_high(Node, _, Low, High) },
            [Node],
            bdd_nodes_(VPred, Low),
            bdd_nodes_(VPred, High)
        ).

node_visited(Node) :- integer(Node).
node_visited(Node) :- with_aux(is_visited, Node).

bdd_variables(BDD, Vs) :-
        bdd_nodes(BDD, Nodes),
        nodes_variables(Nodes, Vs).

nodes_variables(Nodes, Vs) :-
        phrase(nodes_variables_(Nodes), Vs),
        maplist(unvisit, Vs).

nodes_variables_([]) --> [].
nodes_variables_([Node|Nodes]) -->
        { node_var_low_high(Node, Var, _, _) },
        (   { integer(Var) } -> []
        ;   { is_visited(Var) } -> []
        ;   { put_visited(Var) },
            [Var]
        ),
        nodes_variables_(Nodes).

unvisit(V) :- del_attr(V, clpb_visited).

is_visited(V) :- get_attr(V, clpb_visited, true).

put_visited(V) :- put_attr(V, clpb_visited, true).

with_aux(Pred, Node) :-
        node_aux(Node, Aux),
        call(Pred, Aux).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Internal consistency checks.

   To enable these checks, set the flag clpb_validation to true.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

is_bdd(BDD) :-
        (   current_prolog_flag(clpb_validation, true) ->
            bdd_ites(BDD, ITEs),
            pairs_values(ITEs, Ls0),
            sort(Ls0, Ls1),
            (   same_length(Ls0, Ls1) -> true
            ;   domain_error(reduced_ites, (ITEs,Ls0,Ls1))
            ),
            (   member(ITE, ITEs), \+ registered_node(ITE) ->
                domain_error(registered_node, ITE)
            ;   true
            ),
            (   member(I, ITEs), \+ ordered(I) ->
                domain_error(ordered_node, I)
            ;   true
            )
        ;   true
        ).

ordered(_-ite(Var,High,Low)) :-
        (   var_index(Var, VI) ->
            greater_varindex_than(High, VI),
            greater_varindex_than(Low, VI)
        ;   true
        ).

greater_varindex_than(Node, VI) :-
        (   integer(Node) -> true
        ;   node_var_low_high(Node, Var, _, _),
            (   var_index(Var, OI) ->
                OI > VI
            ;   true
            )
        ).

registered_node(Node-ite(Var,High,Low)) :-
        (   var(Var) ->
            low_high_key(Low, High, Key),
            lookup_node(Var, Key, Node0),
            Node == Node0
        ;   true
        ).

bdd_ites(BDD, ITEs) :-
        bdd_nodes(BDD, Nodes),
        maplist(node_ite, Nodes, ITEs).

node_ite(Node, Node-ite(Var,High,Low)) :-
        node_var_low_high(Node, Var, Low, High).

%% labeling(+Vs) is multi.
%
% Assigns truth values to the Boolean variables Vs such that all
% stated constraints are satisfied.

labeling(Vs0) :-
        must_be(list, Vs0),
        variables_in_index_order(Vs0, Vs),
        maplist(indomain, Vs).

variables_in_index_order(Vs0, Vs) :-
        maplist(var_with_index, Vs0, IVs0),
        keysort(IVs0, IVs),
        pairs_values(IVs, Vs).

var_with_index(V, I-V) :-
        (   var_index_root(V, I, _) -> true
        ;   I = 0
        ).

indomain(0).
indomain(1).


%% sat_count(+Expr, -N) is det.
%
% N is the number of different assignments of truth values to the
% variables in the Boolean expression Expr, such that Expr is true and
% all posted constraints are satisfiable.
%
% Example:
%
% ==
% ?- length(Vs, 120), sat_count(+Vs, CountOr), sat_count(*(Vs), CountAnd).
% Vs = [...],
% CountOr = 1329227995784915872903807060280344575,
% CountAnd = 1.
% ==

sat_count(Sat0, N) :-
        catch((parse_sat(Sat0, Sat),
               sat_bdd(Sat, BDD),
               sat_roots(Sat, Roots),
               roots_and(Roots, _-BDD, _-BDD1),
               % we mark variables that occur in Sat0 as visited ...
               term_variables(Sat0, Vs),
               maplist(put_visited, Vs),
               % ... so that they do not appear in Vs1 ...
               bdd_variables(BDD1, Vs1),
               % ... and then remove remaining variables:
               foldl(existential, Vs1, BDD1, BDD2),
               variables_in_index_order(Vs, IVs),
               foldl(renumber_variable, IVs, 1, VNum),
               bdd_count(BDD2, VNum, Count0),
               var_u(BDD2, VNum, P),
               % Do not unify N directly, because we are not prepared
               % for propagation here in case N is a CLP(B) variable.
               N0 is 2^(P - 1)*Count0,
               % reset all attributes and Aux variables
               throw(count(N0))),
              count(N0),
              N = N0).

renumber_variable(V, I0, I) :-
        put_attr(V, clpb, index_root(I0,_)),
        I is I0 + 1.

bdd_count(Node, VNum, Count) :-
        (   integer(Node) -> Count = Node
        ;   node_aux(Node, Count),
            (   integer(Count) -> true
            ;   node_var_low_high(Node, V, Low, High),
                bdd_count(Low, VNum, LCount),
                bdd_count(High, VNum, HCount),
                bdd_pow(Low, V, VNum, LPow),
                bdd_pow(High, V, VNum, HPow),
                Count is LPow*LCount + HPow*HCount
            )
        ).


bdd_pow(Node, V, VNum, Pow) :-
        var_index(V, Index),
        var_u(Node, VNum, P),
        Pow is 2^(P - Index - 1).

var_u(Node, VNum, Index) :-
        (   integer(Node) -> Index = VNum
        ;   node_varindex(Node, Index)
        ).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Pick a solution in such a way that each solution is equally likely.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

random_solution(Seed, Vars) :-
        must_be(list, Vars),
        set_random(seed(Seed)),
        (   ground(Vars) -> true
        ;   catch((sat(+[1|Vars]), % capture all variables with a single BDD
                   once((member(Var, Vars),var(Var))),
                   var_index_root(Var, _, Root),
                   root_get_formula_bdd(Root, _, BDD),
                   bdd_variables(BDD, Vs),
                   variables_in_index_order(Vs, IVs),
                   foldl(renumber_variable, IVs, 1, VNum),
                   phrase(random_bindings(VNum, BDD), Bs),
                   maplist(del_attrs, Vs),
                   % reset all attribute modifications
                   throw(randsol(Vars, Bs))),
                  randsol(Vars, Bs),
                  true),
            maplist(call, Bs),
            % set remaining variables to 0 or 1 with equal probability
            include(var, Vars, Remaining),
            maplist(maybe_one, Remaining)
        ).

maybe_one(Var) :-
        (   maybe -> Var = 0
        ;   Var = 1
        ).

random_bindings(_, Node) --> { Node == 1 }, !.
random_bindings(VNum, Node) -->
        { node_var_low_high(Node, Var, Low, High),
          bdd_count(Node, VNum, Total),
          bdd_count(Low, VNum, LCount) },
        (   { maybe(LCount, Total) } ->
            [Var=0], random_bindings(VNum, Low)
        ;   [Var=1], random_bindings(VNum, High)
        ).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Projection to residual goals.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

attribute_goals(Var) -->
        { var_index_root(Var, _, Root) },
        (   { root_get_formula_bdd(Root, Formula, BDD) } ->
            { del_bdd(Root) },
            (   { current_prolog_flag(clpb_residuals, bdd) } ->
                { bdd_nodes(BDD, Nodes) },
                nodes(Nodes)
            ;   { phrase(sat_ands(Formula), Ands),
                  maplist(formula_anf, Ands, ANFs0),
                  sort(ANFs0, ANFs1),
                  exclude(eq_1, ANFs1, ANFs) },
                sats(ANFs)
            ),
            % formula variables not occurring in the BDD should be booleans
            { bdd_variables(BDD, Vs),
              maplist(del_clpb, Vs),
              term_variables(Formula, RestVs0),
              include(clpb_variable, RestVs0, RestVs) },
            booleans(RestVs)
        ;   boolean(Var)  % the variable may have occurred only in taut/2
        ).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Set the Prolog flag clpb_residuals to bdd to obtain the BDD nodes
   as residuals. Note that they cannot be used as regular goals.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

nodes([]) --> [].
nodes([Node|Nodes]) -->
        { node_var_low_high(Node, Var, Low, High),
          maplist(node_projection, [Node,High,Low], [ID,HID,LID]),
          var_index(Var, VI) },
        [ID-(v(Var,VI) -> HID ; LID)],
        nodes(Nodes).


node_projection(Node, Projection) :-
        node_id(Node, ID),
        (   integer(ID) -> Projection = node(ID)
        ;   Projection = ID
        ).


del_clpb(Var) :- del_attr(Var, clpb).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   By default, residual goals are sat/1 calls of the remaining formulas,
   using (mostly) algebraic normal form.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

sats([]) --> [].
sats([A|As]) -->
        { copy_term_nat(A, Copy) },
        (   { Copy =@= X#Y, A = X#Y } -> [sat(X=\=Y)]
        ;   { Copy =@= 1#X#Y, A = 1#X#Y } -> [sat(X=:=Y)]
        ;   [sat(A)]
        ),
        sats(As).

booleans([]) --> [].
booleans([B|Bs]) --> boolean(B), { del_clpb(B) }, booleans(Bs).

boolean(Var) -->
        (   { get_attr(Var, clpb_omit_boolean, true) } -> []
        ;   [sat(Var =:= Var)]
        ).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Relate a formula to its algebraic normal form (ANF).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

formula_anf(Formula0, ANF) :-
        sat_rewrite(Formula0, Formula),
        sat_bdd(Formula, Node),
        node_xors(Node, Xors),
        maplist(list_to_conjunction, Xors, [Conj|Conjs]),
        foldl(xor, Conjs, Conj, ANF).

list_to_conjunction([], 1).
list_to_conjunction([L|Ls], Conj) :- foldl(and, Ls, L, Conj).

xor(A, B, B # A).

eq_1(V) :- V == 1.

node_xors(Node, Xors) :-
        phrase(xors(Node), Xors0),
        % we remove elements that occur an even number of times (A#A --> 0)
        maplist(sort, Xors0, Xors1),
        pairs_keys_values(Pairs0, Xors1, _),
        keysort(Pairs0, Pairs),
        group_pairs_by_key(Pairs, Groups),
        exclude(even_occurrences, Groups, Odds),
        pairs_keys(Odds, Xors2),
        maplist(exclude(eq_1), Xors2, Xors).

even_occurrences(_-Ls) :- length(Ls, L), L mod 2 =:= 0.

xors(Node) -->
        (   { Node == 0 } -> []
        ;   { Node == 1 } -> [[1]]
        ;   { node_var_low_high(Node, Var, Low, High),
              node_xors(Low, Ls0),
              node_xors(High, Hs0),
              maplist(with_var(Var), Ls0, Ls),
              maplist(with_var(Var), Hs0, Hs) },
            list(Ls0),
            list(Ls),
            list(Hs)
        ).

list([]) --> [].
list([L|Ls]) --> [L], list(Ls).

with_var(Var, Ls, [Var|Ls]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Global variables for unique node and variable IDs.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

make_clpb_var('$clpb_next_var') :- nb_setval('$clpb_next_var', 0).

make_clpb_var('$clpb_next_node') :- nb_setval('$clpb_next_node', 0).

:- multifile user:exception/3.

user:exception(undefined_global_variable, Name, retry) :-
        make_clpb_var(Name), !.

clpb_next_id(Var, ID) :-
        b_getval(Var, ID),
        Next is ID + 1,
        b_setval(Var, Next).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   The variable attributes below are not used as constraints by this
   library. Project remaining attributes to empty lists of residuals.

   Because accessing these hooks is basically a cross-module call, we
   must declare them public.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- public
        clpb_hash:attr_unify_hook/2,
        clpb_bdd:attribute_goals//1,
        clpb_hash:attribute_goals//1,
        clpb_omit_boolean:attribute_goals//1.

clpb_hash:attr_unify_hook(_,_).  % this unification is always admissible

clpb_bdd:attribute_goals(_)          --> [].
clpb_hash:attribute_goals(_)         --> [].
clpb_omit_boolean:attribute_goals(_) --> [].

% clpb_hash:attribute_goals(Var) -->
%         { get_attr(Var, clpb_hash, Assoc),
%           assoc_to_list(Assoc, List0),
%           maplist(node_portray, List0, List) }, [Var-List].

% node_portray(Key-Node, Key-Node-ite(Var,High,Low)) :-
%         node_var_low_high(Node, Var, Low, High).

:- multifile
        sandbox:safe_global_variable/1.

sandbox:safe_global_variable('$clpb_next_var').
sandbox:safe_global_variable('$clpb_next_node').
