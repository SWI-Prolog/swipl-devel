/*  Part of SWI-Prolog

    Author:        Markus Triska
    E-mail:        triska@gmx.at
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2014 Markus Triska

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

### Introduction			{#clpb-intro}

Constraint programming is a declarative formalism that lets you
describe conditions a solution must satisfy. This library provides
CLP(B), Constraint Logic Programming over Boolean Variables. It can be
used to model and solve combinatorial problems such as circuit
verification, graph colouring and allocation tasks.

The implementation is based on reduced and ordered Binary Decision
Diagrams (BDDs).


### Boolean expressions {#clpb-exprs}

A _Boolean expression_ is one of:

    | *0*                | false                                |
    | *1*                | true                                 |
    | _variable_         | unknown truth value                  |
    | ~ _Expr_           | logical NOT                          |
    | _Expr_ + _Expr_    | logical OR                           |
    | _Expr_ * _Expr_    | logical AND                          |
    | _Expr_ # _Expr_    | exclusive OR                         |
    | _Var_ ^ _Expr_     | existential quantification           |
    | _Expr_ =:= _Expr_  | equality                             |
    | _Expr_ =\= _Expr_  | disequality                          |
    | _Expr_ =< _Expr_   | less or equal                        |
    | _Expr_ >= _Expr_   | greater or equal                     |
    | _Expr_ < _Expr_    | less than                            |
    | _Expr_ > _Expr_    | greater than                         |
    | card(Is,Exprs)     | _see  below_                         |

where _Expr_ again denotes a Boolean expression.

The Boolean expression card(Is,Exprs) is true iff the number of true
expressions in the list _Exprs_ is a member of the list _Is_ of
integers and integer ranges of the form _From_-_To_.

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
to posting the constraint sat(X =:= T).

### Examples				{#clpb-examples}

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
sat(X=:=X),
node(11)- (X->node(10);node(9)),
node(9)- (Y->node(8);true),
node(8)- (Z->true;false),
node(10)- (Y->node(8);false),
sat(Y=:=Y),
sat(Z=:=Z).
==

The pending residual goals constrain remaining variables to Boolean
expressions, and encode a decision diagram that determines the query's
truth value when further constraints are added.

@author Markus Triska
*/


state(S) --> state(S, S).

state(S0, S), [S] --> [S0].

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
is_sat(X^F)   :- var(X), is_sat(F).
is_sat(card(Is,Fs)) :-
        must_be(list(ground), Is),
        must_be(list, Fs),
        maplist(is_sat, Fs).

% wrap variables with v(...) and integers with i(...)
sat_nondefaulty(V, v(V)) :- var(V), !.
sat_nondefaulty(I, i(I)) :- integer(I), !.
sat_nondefaulty(~A0, ~A) :- !, sat_nondefaulty(A0, A).
sat_nondefaulty(card(Is,Fs0), card(Is,Fs)) :- !,
        maplist(sat_nondefaulty, Fs0, Fs).
sat_nondefaulty(S0, S) :-
        S0 =.. [F,X0,Y0],
        sat_nondefaulty(X0, X),
        sat_nondefaulty(Y0, Y),
        S =.. [F,X,Y].

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Rewriting to canonical expressions.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

% elementary
sat_rewrite(v(V), v(V)).
sat_rewrite(i(I), i(I)).
sat_rewrite(P0*Q0, P*Q) :- sat_rewrite(P0, P), sat_rewrite(Q0, Q).
sat_rewrite(P0+Q0, P+Q) :- sat_rewrite(P0, P), sat_rewrite(Q0, Q).
sat_rewrite(P0#Q0, P#Q) :- sat_rewrite(P0, P), sat_rewrite(Q0, Q).
sat_rewrite(X^F0, X^F)  :- sat_rewrite(F0, F).
sat_rewrite(card(Is,Fs0), card(Is,Fs)) :-
        maplist(sat_rewrite, Fs0, Fs).
% synonyms
sat_rewrite(~P, R)      :- sat_rewrite(i(1) # P, R).
sat_rewrite(P =:= Q, R) :- sat_rewrite(~P # Q, R).
sat_rewrite(P =\= Q, R) :- sat_rewrite(P # Q, R).
sat_rewrite(P =< Q, R)  :- sat_rewrite(~P + Q, R).
sat_rewrite(P >= Q, R)  :- sat_rewrite(Q =< P, R).
sat_rewrite(P < Q, R)   :- sat_rewrite(~P * Q, R).
sat_rewrite(P > Q, R)   :- sat_rewrite(Q < P, R).


must_be_sat(Sat) :-
        (   is_sat(Sat) -> true
        ;   no_truth_value(Sat)
        ).

no_truth_value(Term) :- domain_error(clpb_expr, Term).

parse_sat(Sat0, Sat) :-
        must_be_sat(Sat0),
        sat_nondefaulty(Sat0, Sat1),
        sat_rewrite(Sat1, Sat),
        term_variables(Sat, Vs),
        maplist(enumerate_variable, Vs).

sat_roots(Sat, Roots) :-
        term_variables(Sat, Vs),
        maplist(var_index_root, Vs, _, Roots0),
        term_variables(Roots0, Roots).

%% sat(+Expr) is semidet.
%
% States the constraint that Expr be a satisfiable Boolean expression.
% Fails if Expr cannot be satisfied.

sat(Sat0) :-
        parse_sat(Sat0, Sat),
        sat_bdd(Sat, BDD),
        sat_roots(Sat, Roots),
        foldl(root_and, Roots, Sat0-BDD, And-BDD1),
        maplist(del_bdd, Roots),
        maplist(=(Root), Roots),
        root_put_formula_bdd(Root, And, BDD1),
        satisfiable_bdd(BDD1).

del_bdd(Root) :- del_attr(Root, clpb_bdd).

root_get_formula_bdd(Root, F, BDD) :- get_attr(Root, clpb_bdd, F-BDD).

root_put_formula_bdd(Root, F, BDD) :- put_attr(Root, clpb_bdd, F-BDD).

root_and(Root, Sat0-BDD0, Sat-BDD) :-
        (   root_get_formula_bdd(Root, F, B) ->
            Sat = F*Sat0,
            bdd_and(B, BDD0, BDD)
        ;   Sat = Sat0,
            BDD = BDD0
        ).

%% taut(+Expr, -T) is semidet
%
% Succeeds with T = 0 if the Boolean expression Expr cannot be
% satisfied, and with T = 1 if Expr is always true with respect to the
% current constraints. Fails otherwise.

taut(Sat0, Truth) :-
        parse_sat(Sat0, Sat),
        sat_roots(Sat, Roots),
        foldl(root_and, Roots, _-1, _-Ands),
        (   sat_bdd(Sat, BDD), bdd_and(BDD, Ands, B), B == 0 ->
            Truth = 0
        ;   sat_bdd(i(1)#Sat, BDD), bdd_and(BDD, Ands, B), B == 0 ->
            Truth = 1
        ;   false
        ).

satisfiable_bdd(BDD) :-
        (   BDD == 0 -> false
        ;   BDD == 1 -> true
        ;   node_var_low_high(BDD, Var, Low, High),
            (   Low == 0 -> Var = 1
            ;   High == 0 -> Var = 0
            ;   true
            )
        ).

var_index(V, I) :- var_index_root(V, I, _).

var_index_root(V, I, Root) :- get_attr(V, clpb, index_root(I,Root)).

enumerate_variable(V) :-
        (   var_index_root(V, _, _) -> true
        ;   clpb_next_id('$clpb_next_var', Index),
            put_attr(V, clpb, index_root(Index,_))
        ).


bdd_and(NA, NB, And) :-
        empty_assoc(H0),
        empty_assoc(G0),
        phrase(apply(*, NA, NB, And), [H0-G0], _).


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

node_id(Node, ID) :-
        (   integer(Node) ->
            (   Node =:= 0 -> ID = false
            ;   Node =:= 1 -> ID = true
            ;   no_truth_value(Node)
            )
        ;   get_attr(Node, clpb_id, ID0),
            ID = node(ID0)
        ).

node_var_low_high(Node, Var, Low, High) :-
        get_attr(Node, clpb_node, node(Var,Low,High)).


make_node(Var, Low, High, Node) -->
        state(H0-G0, H-G0),
        { (   Low == High -> Node = Low, H0 = H
          ;   node_id(Low, LID),
              node_id(High, HID),
              var_index(Var, VI),
              Triple = node(VI,LID,HID),
              (   get_assoc(Triple, H0, Node) -> H0 = H
              ;   put_attr(Node, clpb_node, node(Var,Low,High)),
                  clpb_next_id('$clpb_next_node', ID),
                  put_attr(Node, clpb_id, ID),
                  put_assoc(Triple, H0, Node, H)
              )
          ) }.


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   sat_bdd/2 converts a SAT formula in canonical form to an ordered
   and reduced Binary Decision Diagram (BDD).

   Each CLP(B) variable belongs to exactly one BDD. Each CLP(B)
   variable gets an attribute (in module "clpb") of the form:

        index_root(Index,Root)

   where Index is the variable's unique integer index, and Root is the
   root of the BDD that the variable belongs to.

   A root is a logical variable with a single attribute ("clpb_bdd")
   of the form:

        Sat-BDD

   where Sat is the SAT formula (in original form) that corresponds to
   BDD. Sat is necessary to rebuild the BDD after variable aliasing.

   Finally, a BDD is either:

      *)  The integers 0 or 1, denoting false and true, respectively, or
      *)  A variable with attributes:

           "clpb_node" of the form node(Var, Low, High)
               Where Var is the node's branching variable, and Low and
               High are the node's low (Var = 0) and high (Var = 1)
               children.

           "clpb_id" denoting the node's unique integer ID.

   Variable aliasing is treated as a conjunction of corresponding SAT
   formulae.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

sat_bdd(Sat, BDD) :-
        empty_assoc(H0),
        empty_assoc(G0),
        phrase(sat_bdd(Sat, BDD), [H0-G0], _).

sat_bdd(i(I), I) --> !.
sat_bdd(v(V), Node) --> !, make_node(V, 0, 1, Node).
sat_bdd(v(V)^Sat, Node) --> !, sat_bdd(Sat, BDD), existential(V, BDD, Node).
sat_bdd(card(Is,Fs), Node) --> !, counter_network(Is, Fs, Node).
sat_bdd(Sat, Node) -->
        { Sat =.. [F,A,B] },
        sat_bdd(A, NA),
        sat_bdd(B, NB),
        apply(F, NA, NB, Node).

existential(V, BDD, Node) -->
        { var_index(V, Index),
          bdd_restriction(BDD, Index, 0, NA),
          bdd_restriction(BDD, Index, 1, NB) },
        apply(+, NA, NB, Node).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Counter network for card(Is,Fs).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

counter_network(Cs, Fs, Node) -->
        { same_length([_|Fs], Indicators),
          fill_indicators(Indicators, 0, Cs),
          same_length(Fs, Vars0),
          % enumerate variables in reverse order so that they appear
          % in the correct order in the resulting BDD
          maplist(enumerate_variable, Vars0),
          reverse(Vars0, Vars) },
        counter_network(Fs, Indicators, Vars, Node0),
        { maplist(var_index_root, Vars, _, Roots),
          maplist(=(Root), Roots),
          root_put_formula_bdd(Root, card(Cs,Fs), Node) },
        eq_and(Vars, Fs, Node0, Node1),
        all_existential(Vars, Node1, Node).

all_existential([], Node, Node) --> [].
all_existential([V|Vs], Node0, Node) -->
        existential(V, Node0, Node1),
        all_existential(Vs, Node1, Node).

eq_and([], [], Node, Node) --> [].
eq_and([X|Xs], [Y|Ys], Node0, Node) -->
        { sat_rewrite(v(X) =:= Y, Sat) },
        sat_bdd(Sat, B),
        apply(*, B, Node0, Node1),
        eq_and(Xs, Ys, Node1, Node).

counter_network([], [Node], [], Node) --> [].
counter_network([_|Fs], Is0, [Var|Vars], Node) -->
        indicators_pairing(Is0, Var, Is1),
        counter_network(Fs, Is1, Vars, Node).

indicators_pairing([], _, []) --> [].
indicators_pairing([I|Is], Var, Nodes) -->
        indicators_pairing_(Is, I, Var, Nodes).

indicators_pairing_([], _, _, []) --> [].
indicators_pairing_([I|Is], Prev, Var, [Node|Nodes]) -->
        make_node(Var, Prev, I, Node),
        indicators_pairing_(Is, I, Var, Nodes).

fill_indicators([], _, _).
fill_indicators([I|Is], Index0, Cs) :-
        (   memberchk(Index0, Cs) -> I = 1
        ;   member(A-B, Cs), between(A, B, Index0) -> I = 1
        ;   I = 0
        ),
        Index1 is Index0 + 1,
        fill_indicators(Is, Index1, Cs).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   apply//4. Uses memoization to improve performance.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

apply(+, NA, NB, Node) -->
        { (   NA == 0 -> !, Node = NB
          ;   NA == 1 -> !, Node = 1
          ;   NB == 0 -> !, Node = NA
          ;   NB == 1 -> !, Node = 1
          ;   false
          ) }.
apply(*, NA, NB, Node) -->
        { (   NA == 0 -> !, Node = 0
          ;   NA == 1 -> !, Node = NB
          ;   NB == 0 -> !, Node = 0
          ;   NB == 1 -> !, Node = NA
          ;   false
          ) }.


apply(F, NA, NB, Node) -->
        (   { integer(NA), integer(NB) } -> { once(bool_op(F, NA, NB, Node)) }
        ;   { node_id(NA, IDA), node_id(NB, IDB) },
            (   state(_-G0), { get_assoc(g(F,IDA,IDB), G0, Node) } -> []
            ;   apply_(F, NA, NB, Node),
                state(H0-G0, H0-G),
                { put_assoc(g(F,IDA,IDB), G0, Node, G) }
            )
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
apply_(F, NA, NB, Node) --> % NB > NA
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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Unification. X = Expr is equivalent to sat(X =:= Expr).
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
        ;   parse_sat(Other, OtherSat),
            root_get_formula_bdd(Root, Sat0, _),
            Sat = Sat0*OtherSat,
            sat_roots(Sat, Roots),
            maplist(root_rebuild_bdd, Roots),
            foldl(root_and, Roots, 1-1, And-BDD1),
            maplist(del_bdd, Roots),
            maplist(=(NewRoot), Roots),
            root_put_formula_bdd(NewRoot, And, BDD1),
            satisfiable_bdd(BDD1)
        ).

root_rebuild_bdd(Root) :-
        (   root_get_formula_bdd(Root, F0, _) ->
            parse_sat(F0, Sat),
            sat_bdd(Sat, BDD),
            root_put_formula_bdd(Root, F0, BDD)
        ;   true
        ).

is_bdd(BDD) :-
        bdd_ites(BDD, ITEs0),
        pairs_values(ITEs0, Ls0),
        sort(Ls0, Ls1),
        (   same_length(Ls0, Ls1) -> true
        ;   domain_error(reduced_ites, (ITEs0,Ls0,Ls1))
        ).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   BDD restriction.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

bdd_restriction(Node, VI, Value, Res) :-
        empty_assoc(H0),
        empty_assoc(G0),
        phrase(bdd_restriction_(Node, VI, Value, Res), [H0-G0], _).
        % is_bdd(Res).

bdd_restriction_(Node, VI, Value, Res) -->
        (   { integer(Node) } -> { Res = Node }
        ;   { node_var_low_high(Node, Var, Low, High) } ->
            (   { integer(Var) } ->
                (   { Var =:= 0 } -> bdd_restriction_(Low, VI, Value, Res)
                ;   { Var =:= 1 } -> bdd_restriction_(High, VI, Value, Res)
                ;   { no_truth_value(Var) }
                )
            ;   (   { var_index(Var, I0),
                      node_id(Node, ID) },
                    (   { I0 =:= VI } ->
                        (   { Value =:= 0 } ->
                            bdd_restriction_(Low, VI, Value, Res)
                        ;   { Value =:= 1 } ->
                            bdd_restriction_(High, VI, Value, Res)
                        )
                    ;   { I0 > VI } -> { Res = Node }
                    ;   state(_-G0), { get_assoc(ID, G0, Res) } -> []
                    ;   bdd_restriction_(Low, VI, Value, LRes),
                        bdd_restriction_(High, VI, Value, HRes),
                        make_node(Var, LRes, HRes, Res),
                        state(H0-G0, H0-G),
                        { put_assoc(ID, G0, Res, G) }
                    )
                )
            )
        ;   { domain_error(node, Node) }
        ).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Transformation from BDD to if-then-else terms.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

bdd_ites(Node, ITEs) :-
        phrase(bdd_ites_(Node), ITEs),
        pairs_keys(ITEs, Nodes),
        maplist(unvisit, Nodes).

bdd_ites_(Node) -->
        (   { integer(Node) ;  get_attr(Node, clpb_visited, true) } -> []
        ;   { node_var_low_high(Node, Var, Low, High),
              put_attr(Node, clpb_visited, true) },
            [Node-ite(Var, High, Low)],
            bdd_ites_(Low),
            bdd_ites_(High)
        ).

unvisit(Node) :- del_attr(Node, clpb_visited).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Projection to residual goals.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

attribute_goals(Var) -->
        { var_index_root(Var, _, Root) },
        boolean(Var),
        (   { root_get_formula_bdd(Root, _, BDD) } ->
            { bdd_ites(BDD, ITEs) },
            ites(ITEs),
            { pairs_keys(ITEs, Nodes),
              maplist(del_attrs, Nodes),
              del_bdd(Root) }
        ;   []
        ).

boolean(V) --> [sat(V =:= V)].

ites([]) --> [].
ites([Node-ite(Var,High,Low)|Is]) -->
        { maplist(node_id, [Node,High,Low], [ID,HID,LID]) },
        [ID-(Var -> HID ; LID)],
        ites(Is).

%% labeling(+Vs) is nondet.
%
% Assigns truth values to the Boolean variables Vs such that all
% stated constraints are satisfied.

labeling(Vs0) :-
        must_be(list, Vs0),
        maplist(var_with_index, Vs0, IVs0),
        keysort(IVs0, IVs),
        pairs_values(IVs, Vs),
        maplist(indomain, Vs).

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

sat_count(Sat0, N) :-
        catch((term_variables(Sat0, Vs),
               maplist(essential_variable, Vs),
               parse_sat(Sat0, Sat),
               sat_bdd(Sat, BDD),
               sat_roots(Sat, Roots),
               foldl(root_and, Roots, _-BDD, _-BDD1),
               bdd_variables(BDD1, Vs1),
               foldl(eliminate_existential, Vs1, BDD1, BDD2),
               maplist(var_with_index, Vs, IVs0),
               keysort(IVs0, IVs1),
               foldl(renumber_variable, IVs1, 1, VNum),
               bdd_count(BDD2, VNum, Count0),
               var_u(BDD2, VNum, P),
               Count is 2^(P - 1)*Count0,
               % reset all attributes
               throw(count(Count))),
              count(N),
              true).

renumber_variable(_-V, I0, I) :-
        put_attr(V, clpb, index_root(I0,_)),
        I is I0 + 1.

eliminate_existential(V, BDD0, BDD) :-
        (   get_attr(V, clpb_visited, true) -> BDD = BDD0
        ;   empty_assoc(G0),
            empty_assoc(H0),
            phrase(existential(V, BDD0, BDD), [H0-G0], _)
        ).

bdd_variables(BDD, Vs) :-
        bdd_ites(BDD, ITEs),
        maplist(ite_variable, ITEs, Vs0),
        term_variables(Vs0, Vs).

ite_variable(_-ite(V,_,_), V).

essential_variable(V) :- put_attr(V, clpb_visited, true).

bdd_count(Node, VNum, Count) :-
        (   integer(Node) -> Count = Node
        ;   get_attr(Node, clpb_count, Count) -> true
        ;   node_var_low_high(Node, V, Low, High),
            bdd_count(Low, VNum, LCount),
            bdd_count(High, VNum, HCount),
            bdd_pow(Low, V, VNum, LPow),
            bdd_pow(High, V, VNum, HPow),
            Count is LPow*LCount + HPow*HCount,
            put_attr(Node, clpb_count, Count)
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
Sandbox declarations

The variable attributes below are not used  as unification hooks by this
library. Attributes that are not accessible   from the outside are safe,
but `clpb_bdd` is exposed  and  the   sandboxing  code  does not perform
aliasing analysis to discover whether or not the others are exposed.

Therefore, we define the hooks,  so  we   know  what  will be called. In
addition, because accessing these variables  is basically a cross-module
call, we must declare them public.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- public
	clpb_node:attr_unify_hook/2,
	clpb_id:attr_unify_hook/2,
	clpb_count:attr_unify_hook/2,
	clpb_bdd:attr_unify_hook/2,
	clpb_visited:attr_unify_hook/2,

	clpb_node:attribute_goals//1,
	clpb_id:attribute_goals//1,
	clpb_count:attribute_goals//1,
	clpb_bdd:attribute_goals//1,
	clpb_visited:attribute_goals//1.

   clpb_node:attr_unify_hook(_,_) :- representation_error(cannot_unify_node).
     clpb_id:attr_unify_hook(_,_) :- representation_error(cannot_unify_id).
  clpb_count:attr_unify_hook(_,_) :- representation_error(cannot_unify_count).
    clpb_bdd:attr_unify_hook(_,_) :- representation_error(cannot_unify_bdd).
clpb_visited:attr_unify_hook(_,_) :- representation_error(cannot_unify_visited).

   clpb_node:attribute_goals(_) --> [].
     clpb_id:attribute_goals(_) --> [].
  clpb_count:attribute_goals(_) --> [].
    clpb_bdd:attribute_goals(_) --> [].
clpb_visited:attribute_goals(_) --> [].

:- multifile
	sandbox:safe_global_variable/1.

sandbox:safe_global_variable('$clpb_next_var').
sandbox:safe_global_variable('$clpb_next_node').
