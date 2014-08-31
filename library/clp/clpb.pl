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

### Introduction                        {#clpb-intro}

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
sat(X=:=X),
node(8)- (X->node(7);node(6)),
node(6)- (Y->node(5);true),
node(5)- (Z->true;false),
node(7)- (Y->node(5);false),
sat(Y=:=Y),
sat(Z=:=Z).
==

The pending residual goals constrain remaining variables to Boolean
expressions, and encode a decision diagram that determines the query's
truth value when further constraints are added.

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
   association tables of involved variables after each conjunction of
   BDDs. This only serves to reclaim memory and does not affect the
   solver's correctness.

   A root is a logical variable with a single attribute ("clpb_bdd")
   of the form:

        Sat-BDD

   where Sat is the SAT formula (in original form) that corresponds to
   BDD. Sat is necessary to rebuild the BDD after variable aliasing.

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
        (   phrase(sat_ands(Sat0), Ands), Ands = [_,_|_] ->
            maplist(sat, Ands)
        ;   parse_sat(Sat0, Sat),
            sat_bdd(Sat, BDD),
            sat_roots(Sat, Roots),
            roots_and(Roots, Sat0-BDD, And-BDD1),
            maplist(del_bdd, Roots),
            maplist(=(Root), Roots),
            root_put_formula_bdd(Root, And, BDD1),
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
        ;   { X = ~Y } -> not_ors(Y)
        ;   [X]
        ).

not_ors(X) -->
        (   { var(X) } -> [~X]
        ;   { X = (A+B) } -> not_ors(A), not_ors(B)
        ;   [~X]
        ).

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

%% taut(+Expr, -T) is semidet
%
% Succeeds with T = 0 if the Boolean expression Expr cannot be
% satisfied, and with T = 1 if Expr is always true with respect to the
% current constraints. Fails otherwise.

taut(Sat0, T) :-
        parse_sat(Sat0, Sat),
        sat_roots(Sat, Roots),
        catch((roots_and(Roots, _-1, _-Ands),
               (   T = 0, unsatisfiable_conjunction(Sat, Ands) -> true
               ;   T = 1, unsatisfiable_conjunction(i(1)#Sat, Ands) -> true
               ;   false
               ),
               % reset all attributes
               throw(truth(T))),
              truth(T),
              true).

unsatisfiable_conjunction(Sat, Ands) :-
        sat_bdd(Sat, BDD),
        bdd_and(BDD, Ands, B),
        B == 0.

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
            put_attr(V, clpb, index_root(Index,_)),
            put_empty_hash(V)
        ).

put_empty_hash(V) :-
        empty_assoc(H0),
        put_attr(V, clpb_hash, H0).


bdd_and(NA, NB, And) :-
        apply(*, NA, NB, And),
        is_bdd(And).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Node management. Always use an existing node, if there is one.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

make_node(Var, Low, High, Node) :-
        (   Low == High -> Node = Low
        ;   node_id(Low, LID),
            node_id(High, HID),
            HEntry = node(LID,HID),
            (   lookup_node(Var, HEntry, Node) -> true
            ;   clpb_next_id('$clpb_next_node', ID),
                Node = node(ID,Var,Low,High,_Aux),
                register_node(Var, HEntry, Node)
            )
        ).

make_node(Var, Low, High, Node) -->
        % make it conveniently usable within DCGs
        { make_node(Var, Low, High, Node) }.


rebuild_hashes(BDD) :-
        bdd_nodes(put_empty_hash, BDD, Nodes),
        maplist(re_register_node, Nodes).

re_register_node(Node) :-
        node_var_low_high(Node, Var, Low, High),
        node_id(Low, LID),
        node_id(High, HID),
        register_node(Var, node(LID,HID), Node).

register_node(Var, HEntry, Node) :-
        get_attr(Var, clpb_hash, H0),
        put_assoc(HEntry, H0, Node, H),
        put_attr(Var, clpb_hash, H).

lookup_node(Var, HEntry, Node) :-
        get_attr(Var, clpb_hash, H0),
        get_assoc(HEntry, H0, Node).


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

sat_bdd(i(I), I)           :- !.
sat_bdd(v(V), Node)        :- !, make_node(V, 0, 1, Node).
sat_bdd(v(V)^Sat, Node)    :- !, sat_bdd(Sat, BDD), existential(V, BDD, Node).
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
        same_length(Fs, Vars0),
        % enumerate variables in reverse order so that they appear
        % in the correct order in the resulting BDD
        maplist(enumerate_variable, Vars0),
        reverse(Vars0, Vars),
        counter_network(Fs, Indicators, Vars, Node0),
        maplist(var_index_root, Vars, _, Roots),
        maplist(=(Root), Roots),
        root_put_formula_bdd(Root, card(Cs,Fs), Node),
        eq_and(Vars, Fs, Node0, Node1),
        foldl(existential, Vars, Node1, Node),
        % remove attributes to avoid residual goals for these variables,
        % which are only used temporarily to build the counter network.
        maplist(del_attrs, Vars).

eq_and([], [], Node, Node).
eq_and([X|Xs], [Y|Ys], Node0, Node) :-
        sat_rewrite(v(X) =:= Y, Sat),
        sat_bdd(Sat, B),
        apply(*, B, Node0, Node1),
        eq_and(Xs, Ys, Node1, Node).

counter_network([], [Node], [], Node).
counter_network([_|Fs], [I|Is0], [Var|Vars], Node) :-
        indicators_pairing(Is0, I, Var, Is),
        counter_network(Fs, Is, Vars, Node).

indicators_pairing([], _, _, []).
indicators_pairing([I|Is], Prev, Var, [Node|Nodes]) :-
        make_node(Var, Prev, I, Node),
        indicators_pairing(Is, I, Var, Nodes).

fill_indicators([], _, _).
fill_indicators([I|Is], Index0, Cs) :-
        (   memberchk(Index0, Cs) -> I = 1
        ;   member(A-B, Cs), between(A, B, Index0) -> I = 1
        ;   I = 0
        ),
        Index1 is Index0 + 1,
        fill_indicators(Is, Index1, Cs).

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
            roots_and(Roots, 1-1, And-BDD1),
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
            ;   (   { var_index(Var, I0),
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
            )
        ;   { domain_error(node, Node) }
        ).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Relating a BDD to its elements (nodes and variables).

   Note that BDDs can become quite big (easily more than a million
   nodes), and memory space is a major bottleneck for many problems. If
   possible, we therefore do not duplicate the entire BDD in memory (as
   in bdd_ites/2), but only extract its features as needed.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

bdd_nodes(BDD, Ns) :- bdd_nodes(do_nothing, BDD, Ns).

do_nothing(_).

% VPred is a unary predicate that is called for each branching variable

bdd_nodes(VPred, BDD, Ns) :-
        phrase(bdd_nodes_(VPred, BDD), Ns),
        maplist(with_aux(unvisit), Ns).

bdd_nodes_(VPred, Node) -->
        (   { integer(Node) ;  with_aux(is_visited, Node) } -> []
        ;   { node_var_low_high(Node, Var, Low, High),
              call(VPred, Var),
              with_aux(put_visited, Node) },
            [Node],
            bdd_nodes_(VPred, Low),
            bdd_nodes_(VPred, High)
        ).

bdd_variables(BDD, Vs) :-
        bdd_nodes(BDD, Nodes),
        nodes_variables(Nodes, Vs).

nodes_variables(Nodes, Vs) :-
        phrase(nodes_variables_(Nodes), Vs),
        maplist(unvisit, Vs).

nodes_variables_([]) --> [].
nodes_variables_([Node|Nodes]) -->
        { node_var_low_high(Node, Var, _, _) },
        (   { is_visited(Var) } -> nodes_variables_(Nodes)
        ;   { put_visited(Var) },
            [Var],
            nodes_variables_(Nodes)
        ).

unvisit(V) :- del_attr(V, clpb_visited).

is_visited(V) :- get_attr(V, clpb_visited, true).

put_visited(V) :- put_attr(V, clpb_visited, true).

with_aux(Pred, Node) :-
        node_aux(Node, Aux),
        call(Pred, Aux).

is_bdd(BDD) :-
        (   current_prolog_flag(optimise, true) -> true % skip validation
        ;   bdd_ites(BDD, ITEs0),
            pairs_values(ITEs0, Ls0),
            sort(Ls0, Ls1),
            (   same_length(Ls0, Ls1) -> true
            ;   domain_error(reduced_ites, (ITEs0,Ls0,Ls1))
            )
        ).

bdd_ites(BDD, ITEs) :-
        bdd_nodes(BDD, Nodes),
        maplist(node_ite, Nodes, ITEs).

node_ite(Node, Node-ite(Var,High,Low)) :-
        node_var_low_high(Node, Var, Low, High).

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
%
% Example:
%
% ==
% :- use_module(library(clpb)).
%
% or(A, B, B+A).
% ==
%
% Yielding:
%
% ==
% ?- length(Vs, 120), foldl(or, Vs, 0, Expr), sat_count(Expr, N).
% Vs = [...], Expr = ... + ...,
% N = 1329227995784915872903807060280344575.
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
               maplist(var_with_index, Vs, IVs0),
               keysort(IVs0, IVs1),
               foldl(renumber_variable, IVs1, 1, VNum),
               bdd_count(BDD2, VNum, Count0),
               var_u(BDD2, VNum, P),
               Count is 2^(P - 1)*Count0,
               % reset all attributes and Aux variables
               throw(count(Count))),
              count(N),
              true).

renumber_variable(_-V, I0, I) :-
        put_attr(V, clpb, index_root(I0,_)),
        I is I0 + 1.

ite_variable(_-ite(V,_,_), V).

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
   Projection to residual goals.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

attribute_goals(Var) -->
        { var_index_root(Var, _, Root) },
        boolean(Var),
        (   { root_get_formula_bdd(Root, _, BDD) } ->
            { del_bdd(Root),
              bdd_nodes(BDD, Nodes) },
            nodes(Nodes)
        ;   []
        ).

boolean(V) --> [sat(V =:= V)].

nodes([]) --> [].
nodes([Node|Nodes]) -->
        { node_var_low_high(Node, Var, Low, High),
          maplist(node_projection, [Node,High,Low], [ID,HID,LID]) },
        [ID-(Var -> HID ; LID)],
        nodes(Nodes).

node_projection(Node, Projection) :-
        node_id(Node, ID),
        (   integer(ID) -> Projection = node(ID)
        ;   Projection = ID
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

Notice that the necessary declarations for library(sandbox) are getting
a bit out of hand for modules that use several different attributes,
like this library. There may be a better way to solve such issues, by
improving the interface to attributed variables and related predicates.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- public
        clpb_bdd:attr_unify_hook/2,
        clpb_visited:attr_unify_hook/2,
        clpb_hash:attr_unify_hook/2,

        clpb_bdd:attribute_goals//1,
        clpb_visited:attribute_goals//1,
        clpb_hash:attribute_goals//1.

    clpb_bdd:attr_unify_hook(_,_) :- representation_error(cannot_unify_bdd).
clpb_visited:attr_unify_hook(_,_) :- representation_error(cannot_unify_visited).
   clpb_hash:attr_unify_hook(_,_).  % OK

    clpb_bdd:attribute_goals(_) --> [].
clpb_visited:attribute_goals(_) --> [].
   clpb_hash:attribute_goals(_) --> [].
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
