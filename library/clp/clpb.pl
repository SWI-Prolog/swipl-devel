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
                 labeling/1
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

    | _0_                | Falsehood                            |
    | _1_                | Truth                                |
    | _variable_         | Unknown truth value                  |
    | ~ Expr             | Logical NOT                          |
    | Expr + Expr        | Logical OR                           |
    | Expr * Expr        | Logical AND                          |
    | Expr # Expr        | Exclusive OR                         |
    | X ^ Expr           | Existential quantification           |
    | Expr =:= Expr      | Equality                             |
    | Expr =\= Expr      | Disequality                          |
    | Expr =< Expr       | Less or equal                        |
    | Expr >= Expr       | Greater or equal                     |
    | Expr < Expr        | Less than                            |
    | Expr > Expr        | Greater than                         |

where _Expr_ again denotes a Boolean expression.

### Interface predicates   {#clpb-interface}

The interface predicates of CLP(B) are:

    * sat(+Expr)
      True iff the Boolean expression Expr is satisfiable.

    * taut(+Expr, -T)
      If Expr is a tautology with respect to the posted constraints, succeeds
      with *T = 1*. If Expr cannot be satisfied, succeeds with *T = 0*.
      Otherwise, it fails.

    * labeling(+Vs)
      Assigns truth values to the variables Vs such that all constraints
      are satisfied.


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
node(54)- (X->node(53);node(52)),
node(52)- (Y->node(51);true),
node(51)- (Z->true;false),
node(53)- (Y->node(51);false),
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
        put_attr(Root, bdd, And-BDD1),
        satisfiable_bdd(BDD1).

del_bdd(Root) :- del_attr(Root, bdd).

root_and(Root, Sat0-BDD0, Sat-BDD) :-
        (   get_attr(Root, bdd, F-B) ->
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
        ;   nb_getval('$clpb_next_var', Index0),
            put_attr(V, clpb, index_root(Index0,_)),
            Index is Index0 + 1,
            nb_setval('$clpb_next_var', Index)
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
        ;   get_attr(Node, id, ID0),
            ID = node(ID0)
        ).

node_var_low_high(Node, Var, Low, High) :-
        get_attr(Node, node, node(Var,Low,High)).


make_node(Var, Low, High, Node) -->
        state(H0-G0, H-G0),
        { (   Low == High -> Node = Low, H0 = H
          ;   node_id(Low, LID),
              node_id(High, HID),
              var_index(Var, VI),
              Triple = node(VI,LID,HID),
              (   get_assoc(Triple, H0, Node) -> H0 = H
              ;   put_attr(Node, node, node(Var,Low,High)),
                  nb_getval('$clpb_next_node', ID0),
                  put_attr(Node, id, ID0),
                  ID is ID0 + 1,
                  nb_setval('$clpb_next_node', ID),
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

   A root is a logical variable with a single attribute ("bdd") of the
   form:

        Sat-BDD

   where Sat is the SAT formula (in original form) that corresponds to
   BDD. Sat is necessary to rebuild the BDD after variable aliasing.

   Finally, a BDD is either:

      *)  The integers 0 or 1, denoting false and true, respectively, or
      *)  A variable with attributes:

           "node" of the form node(Var, Low, High)
               Where Var is the node's branching variable, and Low and
               High are the node's low (Var = 0) and high (Var = 1)
               children.

           "id" denoting the node's unique integer ID.

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
          put_attr(Root, bdd, card(Cs,Fs)-Node) },
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
                get_attr(Root, bdd, Sat-BDD0),
                bdd_restriction(BDD0, I, Other, BDD),
                put_attr(Root, bdd, Sat-BDD),
                satisfiable_bdd(BDD)
            ;   no_truth_value(Other)
            )
        ;   parse_sat(Other, OtherSat),
            get_attr(Root, bdd, Sat0-_),
            Sat = Sat0*OtherSat,
            sat_roots(Sat, Roots),
            maplist(root_rebuild_bdd, Roots),
            foldl(root_and, Roots, 1-1, And-BDD1),
            maplist(del_bdd, Roots),
            maplist(=(NewRoot), Roots),
            put_attr(NewRoot, bdd, And-BDD1),
            satisfiable_bdd(BDD1)
        ).

root_rebuild_bdd(Root) :-
        (   get_attr(Root, bdd, F0-_) ->
            parse_sat(F0, Sat),
            sat_bdd(Sat, BDD),
            put_attr(Root, bdd, F0-BDD)
        ;   true
        ).

is_bdd(BDD) :-
        catch((phrase(bdd_ite(BDD), ITEs0),
               maplist(ite_ground, ITEs0, Ls0),
               sort(Ls0, Ls1),
               (   same_length(Ls0, Ls1) -> throw(is_ok)
               ;   domain_error(reduced_ites, (ITEs0,Ls0,Ls1))
               )),
              is_ok,
              true).

ite_ground(_-(V -> HID ; LID), t(I,HID,LID)) :- var_index(V, I).


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
   Projection to residual goals.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

attribute_goals(Var) -->
        { var_index_root(Var, _, Root) },
        boolean(Var),
        (   { get_attr(Root, bdd, _-BDD) } ->
            bdd_ite(BDD),
            { del_attr(Root, bdd) }
        ;   []
        ).

boolean(V) --> [sat(V =:= V)].

bdd_ite(B) -->
        bdd_ite_(B),
        { bdd_clear(B) }.

bdd_ite_(Node) -->
        (   { integer(Node) ;  get_attr(Node, visited, true) } -> []
        ;   { node_id(Node, ID) } ->
            { node_var_low_high(Node, Var, Low, High),
              put_attr(Node, visited, true),
              node_id(High, HID),
              node_id(Low, LID) },
            [ID-(Var -> HID ; LID )],
            bdd_ite_(Low),
            bdd_ite_(High)
        ;   []
        ).

bdd_clear(Node) :-
        (   node_var_low_high(Node, _, Low, High) ->
            bdd_clear(Low),
            bdd_clear(High),
            del_attrs(Node)
        ;   true
        ).

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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   SATCount

   Currently does not take into account other constraints.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


sat_count(Sat0, N) :-
        nb_getval('$clpb_next_var', NextVar),
        nb_setval('$clpb_next_var', 1),
        catch((term_variables(Sat0, Vs),
               maplist(del_attrs, Vs),
               parse_sat(Sat0, Sat),
               sat_bdd(Sat, BDD),
               nb_getval('$clpb_next_var', VNum),
               bdd_count(BDD, VNum, Count),
               throw(count(Count))),
              count(N),
              true),
        nb_setval('$clpb_next_var', NextVar).

bdd_count(Node, VNum, Count) :-
        (   integer(Node) -> Count = Node
        ;   get_attr(Node, count, Count) -> true
        ;   node_var_low_high(Node, V, Low, High),
            bdd_count(Low, VNum, LCount),
            bdd_count(High, VNum, HCount),
            bdd_pow(Low, V, VNum, LPow),
            bdd_pow(High, V, VNum, HPow),
            Count is LPow*LCount + HPow*HCount,
            put_attr(Node, count, Count)
        ).

bdd_pow(Node, V, VNum, Pow) :-
        var_index(V, Index),
        (   integer(Node) -> P = VNum
        ;   node_varindex(Node, P)
        ),
        Pow is 2^(P - Index - 1).

make_clpb_var('$clpb_next_var') :- nb_setval('$clpb_next_var', 0).

make_clpb_var('$clpb_next_node') :- nb_setval('$clpb_next_node', 0).

:- multifile user:exception/3.

user:exception(undefined_global_variable, Name, retry) :-
        make_clpb_var(Name), !.

