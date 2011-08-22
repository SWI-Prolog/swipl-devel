/*  $Id$

    Part of SWI-Prolog

    Author:        Markus Triska
    E-mail:        triska@gmx.at
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2005-2009, Markus Triska

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/


:- module(simplex,
        [
                assignment/2,
                constraint/3,
                constraint/4,
                constraint_add/4,
                gen_state/1,
                gen_state_clpr/1,
                gen_state_clpr/2,
                maximize/3,
                minimize/3,
                objective/2,
                shadow_price/3,
                transportation/4,
                variable_value/3
        ]).

:- use_module(library(clpr)).
:- use_module(library(assoc)).
:- use_module(library(pio)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   CLP(R) bindings
   the (unsolved) state is stored as a structure of the form
      clpr_state(Options, Cs, Is)
   Options: list of Option=Value pairs, currently only eps=Eps
   Cs: list of constraints, i.e., structures of the form
      c(Name, Left, Op, Right)
      anonymous constraints have Name == 0
   Is: list of integral variables
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

gen_state_clpr(State) :- gen_state_clpr([], State).

gen_state_clpr(Options, State) :-
        ( memberchk(eps=_, Options) -> Options1 = Options
        ;   Options1 = [eps=1.0e-6|Options]
        ),
        State = clpr_state(Options1, [], []).

clpr_state_options(clpr_state(Os, _, _), Os).
clpr_state_constraints(clpr_state(_, Cs, _), Cs).
clpr_state_integrals(clpr_state(_, _, Is), Is).
clpr_state_add_integral(I, clpr_state(Os, Cs, Is), clpr_state(Os, Cs, [I|Is])).
clpr_state_add_constraint(C, clpr_state(Os, Cs, Is), clpr_state(Os, [C|Cs], Is)).
clpr_state_set_constraints(Cs, clpr_state(Os,_,Is), clpr_state(Os, Cs, Is)).

clpr_constraint(Name, Constraint, S0, S) :-
        (   Constraint = integral(Var) -> clpr_state_add_integral(Var, S0, S)
        ;   Constraint =.. [Op,Left,Right],
            coeff_one(Left, Left1),
            clpr_state_add_constraint(c(Name, Left1, Op, Right), S0, S)
        ).

clpr_constraint(Constraint, S0, S) :-
        clpr_constraint(0, Constraint, S0, S).

clpr_shadow_price(clpr_solved(_,_,Duals,_), Name, Value) :-
        memberchk(Name-Value0, Duals),
        Value is Value0.
        %( var(Value0) ->
        %       Value = 0
        %;
        %       Value is Value0
        %).



clpr_make_variables(Cs, Aliases) :-
        clpr_constraints_variables(Cs, Variables0, []),
        sort(Variables0, Variables1),
        clpr_aliases(Variables1, Aliases).

clpr_constraints_variables([]) --> [].
clpr_constraints_variables([c(_, Left, _, _)|Cs]) -->
        variables(Left),
        clpr_constraints_variables(Cs).

clpr_aliases([], []).
clpr_aliases([Var|Vars], [Var-_|Rest]) :-
        clpr_aliases(Vars, Rest).

clpr_set_up([], _).
clpr_set_up([C|Cs], Aliases) :-
        C = c(_Name, Left, Op, Right),
        clpr_translate_linsum(Left, Aliases, LinSum),
        CLPRConstraint =.. [Op, LinSum, Right],
        clpr:{ CLPRConstraint },
        clpr_set_up(Cs, Aliases).

clpr_set_up_noneg([], _).
clpr_set_up_noneg([Var|Vs], Aliases) :-
        memberchk(Var-CLPVar, Aliases),
        { CLPVar >= 0 },
        clpr_set_up_noneg(Vs, Aliases).

clpr_translate_linsum([], _, 0).
clpr_translate_linsum([Coeff*Var|Ls], Aliases, LinSum) :-
        memberchk(Var-CLPVar, Aliases),
        LinSum = Coeff*CLPVar + LinRest,
        clpr_translate_linsum(Ls, Aliases, LinRest).

clpr_dual(Objective0, S0, DualValues) :-
        clpr_state_constraints(S0, Cs0),
        clpr_constraints_variables(Cs0, Variables0, []),
        sort(Variables0, Variables1),
        clpr_standard_form(Cs0, Cs1),
        clpr_include_all_vars(Cs1, Variables1, Cs2),
        clpr_merge_into(Variables1, Objective0, Objective, []),
        clpr_unique_names(Cs2, 0, Names),
        clpr_constraints_coefficients(Cs2, Coefficients),
        transpose(Coefficients, TCs),
        clpr_dual_constraints(TCs, Objective, Names, DualConstraints),
        clpr_nonneg_constraints(Cs2, Names, DualNonNeg, []),
        append(DualConstraints, DualNonNeg, DualConstraints1),
        clpr_dual_objective(Cs2, Names, DualObjective),
        clpr_make_variables(DualConstraints1, Aliases),
        clpr_set_up(DualConstraints1, Aliases),
        clpr_translate_linsum(DualObjective, Aliases, LinExpr),
        minimize(LinExpr),
        Aliases = DualValues.



clpr_dual_objective([], _, []).
clpr_dual_objective([C|Cs], [Name|Names], [Right*Name|Os]) :-
        C = c(_, _, _, Right),
        clpr_dual_objective(Cs, Names, Os).

clpr_nonneg_constraints([], _, Nons, Nons).
clpr_nonneg_constraints([C|Cs], [Name|Names], Nons0, Nons) :-
        C = c(_, _, Op, _),
        (   Op == (=<) -> Nons0 = [c(0, [1*Name], (>=), 0)|Rest]
        ;   Nons0 = Rest
        ),
        clpr_nonneg_constraints(Cs, Names, Rest, Nons).


clpr_dual_constraints([], [], _, []).
clpr_dual_constraints([Coeffs|Cs], [O*_|Os], Names, [Constraint|Constraints]) :-
        clpr_dual_linsum(Coeffs, Names, Linsum),
        Constraint = c(0, Linsum, (>=), O),
        clpr_dual_constraints(Cs, Os, Names, Constraints).


clpr_dual_linsum([], [], []).
clpr_dual_linsum([Coeff|Coeffs], [Name|Names], [Coeff*Name|Rest]) :-
        clpr_dual_linsum(Coeffs, Names, Rest).


clpr_constraints_coefficients([], []).
clpr_constraints_coefficients([C|Cs], [Coeff|Coeffs]) :-
        C = c(_, Left, _, _),
        all_coeffs(Left, Coeff),
        clpr_constraints_coefficients(Cs, Coeffs).

all_coeffs([], []).
all_coeffs([Coeff*_|Cs], [Coeff|Rest]) :-
        all_coeffs(Cs, Rest).


clpr_unique_names([], _, []).
clpr_unique_names([C0|Cs0], Num, [N|Ns]) :-
        C0 = c(Name, _, _, _),
        (   Name == 0 -> N = Num, Num1 is Num + 1
        ;   N = Name, Num1 = Num
        ),
        clpr_unique_names(Cs0, Num1, Ns).

clpr_include_all_vars([], _, []).
clpr_include_all_vars([C0|Cs0], Variables, [C|Cs]) :-
        C0 = c(Name, Left0, Op, Right),
        clpr_merge_into(Variables, Left0, Left, []),
        C = c(Name, Left, Op, Right),
        clpr_include_all_vars(Cs0, Variables, Cs).

clpr_merge_into([], _, Ls, Ls).
clpr_merge_into([V|Vs], Left, Ls0, Ls) :-
        ( member(Coeff*V, Left) ->
                Ls0 = [Coeff*V|Rest]
        ;
                Ls0 = [0*V|Rest]
        ),
        clpr_merge_into(Vs, Left, Rest, Ls).




clpr_maximize(Expr0, S0, S) :-
        coeff_one(Expr0, Expr),
        clpr_state_constraints(S0, Cs),
        clpr_make_variables(Cs, Aliases),
        clpr_set_up(Cs, Aliases),
        clpr_constraints_variables(Cs, Variables0, []),
        sort(Variables0, Variables1),
        clpr_set_up_noneg(Variables1, Aliases),
        clpr_translate_linsum(Expr, Aliases, LinExpr),
        clpr_state_integrals(S0, Is),
        ( Is == [] ->
                maximize(LinExpr),
                Sup is LinExpr,
                clpr_dual(Expr, S0, DualValues),
                S = clpr_solved(Sup, Aliases, DualValues, S0)
        ;
                clpr_state_options(S0, Options),
                memberchk(eps=Eps, Options),
                clpr_fetch_vars(Is, Aliases, Vars),
                bb_inf(Vars, -LinExpr, Sup, Vertex, Eps),
                clpr_merge_vars(Is, Vertex, Values),
                % what about the dual in MIPs?
                Sup1 is -Sup,
                S = clpr_solved(Sup1, Values, [], S0)
        ).

clpr_minimize(Expr0, S0, S) :-
        coeff_one(Expr0, Expr1),
        clpr_all_negate(Expr1, Expr2),
        clpr_maximize(Expr2, S0, S1),
        S1 = clpr_solved(Sup, Values, Duals, S0),
        Inf is -Sup,
        S = clpr_solved(Inf, Values, Duals, S0).

clpr_merge_vars([], [], []).
clpr_merge_vars([I|Is], [V|Vs], [I-V|Rest]) :-
        clpr_merge_vars(Is, Vs, Rest).

clpr_fetch_vars([], _, []).
clpr_fetch_vars([Var|Vars], Aliases, [X|Xs]) :-
        memberchk(Var-X, Aliases),
        clpr_fetch_vars(Vars, Aliases, Xs).

clpr_variable_value(clpr_solved(_, Aliases, _, _), Variable, Value) :-
        memberchk(Variable-Value0, Aliases),
        Value is Value0.
        %( var(Value0) ->
        %       Value = 0
        %;
        %       Value is Value0
        %).

clpr_objective(clpr_solved(Obj, _, _, _), Obj).

clpr_standard_form([], []).
clpr_standard_form([c(Name, Left, Op, Right)|Cs], [S|Ss]) :-
        clpr_standard_form_(Op, Name, Left, Right, S),
        clpr_standard_form(Cs, Ss).

clpr_standard_form_((=), Name, Left, Right, c(Name, Left, (=), Right)).
clpr_standard_form_((>=), Name, Left, Right, c(Name, Left1, (=<), Right1)) :-
        Right1 is -Right,
        clpr_all_negate(Left, Left1).
clpr_standard_form_((=<), Name, Left, Right, c(Name, Left, (=<), Right)).

clpr_all_negate([], []).
clpr_all_negate([Coeff0*Var|As], [Coeff1*Var|Ns]) :-
        Coeff1 is -Coeff0,
        clpr_all_negate(As, Ns).



/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   General Simplex Algorithm
   Structures used:

   tableau(Objective, Variables, Indicators, Constraints)
     *) objective function, represented as row
     *) list of variables corresponding to columns
     *) indicators denoting which variables are still active
     *) constraints as rows

   row(Var, Left, Right)
     *) the basic variable corresponding to this row
     *) coefficients of the left-hand side of the constraint
     *) right-hand side of the constraint
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


find_row(Variable, [Row|Rows], R) :-
        Row = row(V, _, _),
        (   V == Variable -> R = Row
        ;   find_row(Variable, Rows, R)
        ).


variable_value(State, Variable, Value) :-
        functor(State, F, _),
        (   F == solved ->
            solved_tableau(State, Tableau),
            tableau_rows(Tableau, Rows),
            (   find_row(Variable, Rows, Row) -> Row = row(_, _, Value)
            ;   Value = 0
            )
        ;   F == clpr_solved -> clpr_variable_value(State, Variable, Value)
        ).

all_vars_zero([], _).
all_vars_zero([_Coeff*Var|Vars], State) :-
        variable_value(State, Var, 0),
        all_vars_zero(Vars, State).

list_first(Ls, F, Index) :- once(nth0(Index, Ls, F)).

shadow_price(State, Name, Value) :-
        functor(State, F, _),
        (   F == solved ->
            solved_tableau(State, Tableau),
            tableau_objective(Tableau, row(_,Left,_)),
            tableau_variables(Tableau, Variables),
            solved_names(State, Names),
            memberchk(user(Name)-Var, Names),
            list_first(Variables, Var, Nth0),
            nth0(Nth0, Left, Value)
        ;   F == clpr_solved -> clpr_shadow_price(State, Name, Value)
        ).

objective(State, Obj) :-
        functor(State, F, _),
        (   F == solved ->
            solved_tableau(State, Tableau),
            tableau_objective(Tableau, Objective),
            Objective = row(_, _, Obj)
        ;   clpr_objective(State, Obj)
        ).

   % interface functions that access tableau components

tableau_objective(tableau(Obj, _, _, _), Obj).
tableau_rows(tableau(_, _, _, Rows), Rows).
tableau_indicators(tableau(_, _, Inds, _), Inds).
tableau_variables(tableau(_, Vars, _, _), Vars).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   interface functions that access and modify state components

   state is a structure of the form
       state(Num, Names, Cs, Is)
   Num: used to obtain new unique names for slack variables in a side-effect
        free way (increased by one and threaded through)
   Names: list of Name-Var, correspondence between constraint-names and
        names of slack/artificial variables to obtain shadow prices later
   Cs: list of constraints
   Is: list of integer variables

   constraints are initially represented as c(Name, Left, Op, Right),
   and after normalizing as c(Var, Left, Right). Name of unnamed constraints
   is 0. The distinction is important for merging constraints (mainly in
   branch and bound) with existing ones.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


constraint_name(c(Name, _, _, _), Name).
constraint_op(c(_, _, Op, _), Op).
constraint_left(c(_, Left, _, _), Left).
constraint_right(c(_, _, _, Right), Right).

gen_state(state(0,[],[],[])).

state_add_constraint(C, S0, S) :-
        (   constraint_name(C, 0), constraint_left(C, [_Coeff*_Var]) ->
            state_merge_constraint(C, S0, S)
        ;   state_add_constraint_(C, S0, S)
        ).

state_add_constraint_(C, state(VID,Ns,Cs,Is), state(VID,Ns,[C|Cs],Is)).

state_merge_constraint(C, S0, S) :-
        constraint_left(C, [Coeff0*Var0]),
        constraint_right(C, Right0),
        constraint_op(C, Op),
        (   Coeff0 =:= 0 ->
            (   Op == (=) -> Right0 =:= 0, S0 = S
            ;   Op == (=<) -> S0 = S
            ;   Op == (>=) -> Right0 =:= 0, S0 = S
            )
        ;   Coeff0 < 0 -> state_add_constraint_(C, S0, S)
        ;   Right is Right0 rdiv Coeff0,
            state_constraints(S0, Cs),
            (   select(c(0, [1*Var0], Op, CRight), Cs, RestCs) ->
                (   Op == (=) -> CRight =:= Right, S0 = S
                ;   Op == (=<) ->
                    NewRight is min(Right, CRight),
                    NewCs = [c(0, [1*Var0], Op, NewRight)|RestCs],
                    state_set_constraints(NewCs, S0, S)
                ;   Op == (>=) ->
                    NewRight is max(Right, CRight),
                    NewCs = [c(0, [1*Var0], Op, NewRight)|RestCs],
                    state_set_constraints(NewCs, S0, S)
                )
            ;   state_add_constraint_(c(0, [1*Var0], Op, Right), S0, S)
            )
        ).


state_add_name(Name, Var), [state(VID,[Name-Var|Ns],Cs,Is)] -->
        [state(VID,Ns,Cs,Is)].

state_add_integral(Var, state(VID,Ns,Cs,Is), state(VID,Ns,Cs,[Var|Is])).

state_constraints(state(_, _, Cs, _), Cs).
state_names(state(_,Names,_,_), Names).
state_integrals(state(_,_,_,Is), Is).
state_set_constraints(Cs, state(VID,Ns,_,Is), state(VID,Ns,Cs,Is)).
state_set_integrals(Is, state(VID,Ns,Cs,_), state(VID,Ns,Cs,Is)).


state_next_var(VarID0), [state(VarID1,Names,Cs,Is)] -->
        [state(VarID0,Names,Cs,Is)],
        { VarID1 is VarID0 + 1 }.

solved_tableau(solved(Tableau, _, _), Tableau).
solved_names(solved(_, Names,_), Names).
solved_integrals(solved(_,_,Is), Is).

% User-named constraints are wrapped with user/1 to also allow "0" in
% constraint names.

constraint(C, S0, S) :-
        functor(S0, F, _),
        (   F == state ->
            (   C = integral(Var) -> state_add_integral(Var, S0, S)
            ;   constraint_(0, C, S0, S)
            )
        ;   F == clpr_state -> clpr_constraint(C, S0, S)
        ).

constraint(Name, C, S0, S) :- constraint_(user(Name), C, S0, S).

constraint_(Name, C, S0, S) :-
        functor(S0, F, _),
        (   F == state ->
            (   C = integral(Var) -> state_add_integral(Var, S0, S)
            ;   C =.. [Op, Left0, Right0],
                coeff_one(Left0, Left),
                Right0 >= 0,
                Right is rationalize(Right0),
                state_add_constraint(c(Name, Left, Op, Right), S0, S)
            )
        ;   F == clpr_state -> clpr_constraint(Name, C, S0, S)
        ).

constraint_add(Name, A, S0, S) :-
        functor(S0, F, _),
        (   F == state ->
            state_constraints(S0, Cs),
            add_left(Cs, user(Name), A, Cs1),
            state_set_constraints(Cs1, S0, S)
        ;   F == clpr_state ->
            clpr_state_constraints(S0, Cs),
            add_left(Cs, Name, A, Cs1),
            clpr_state_set_constraints(Cs1, S0, S)
        ).


add_left([c(Name,Left0,Op,Right)|Cs], V, A, [c(Name,Left,Op,Right)|Rest]) :-
        (   Name == V -> append(A, Left0, Left), Rest = Cs
        ;   Left0 = Left, add_left(Cs, V, A, Rest)
        ).

branching_variable(State, Variable) :-
        solved_integrals(State, Integrals),
        member(Variable, Integrals),
        variable_value(State, Variable, Value),
        \+ integer(Value).


worth_investigating(ZStar0, _, _) :- var(ZStar0).
worth_investigating(ZStar0, AllInt, Z) :-
        nonvar(ZStar0),
        (   AllInt =:= 1 -> Z1 is floor(Z)
        ;   Z1 = Z
        ),
        Z1 > ZStar0.


branch_and_bound(Objective, Solved, AllInt, ZStar0, ZStar, S0, S, Found) :-
        objective(Solved, Z),
        (   worth_investigating(ZStar0, AllInt, Z) ->
            (   branching_variable(Solved, BrVar) ->
                variable_value(Solved, BrVar, Value),
                Value1 is floor(Value),
                Value2 is Value1 + 1,
                constraint([BrVar] =< Value1, S0, SubProb1),
                (   maximize_(Objective, SubProb1, SubSolved1) ->
                    Sub1Feasible = 1,
                    objective(SubSolved1, Obj1)
                ;   Sub1Feasible = 0
                ),
                constraint([BrVar] >= Value2, S0, SubProb2),
                (   maximize_(Objective, SubProb2, SubSolved2) ->
                    Sub2Feasible = 1,
                    objective(SubSolved2, Obj2)
                ;   Sub2Feasible = 0
                ),
                (   Sub1Feasible =:= 1, Sub2Feasible =:= 1 ->
                    (   Obj1 >= Obj2 ->
                        First = SubProb1,
                        Second = SubProb2,
                        FirstSolved = SubSolved1,
                        SecondSolved = SubSolved2
                    ;   First = SubProb2,
                        Second = SubProb1,
                        FirstSolved = SubSolved2,
                        SecondSolved = SubSolved1
                    ),
                    branch_and_bound(Objective, FirstSolved, AllInt, ZStar0, ZStar1, First, Solved1, Found1),
                    branch_and_bound(Objective, SecondSolved, AllInt, ZStar1, ZStar2, Second, Solved2, Found2)
                ;   Sub1Feasible =:= 1 ->
                    branch_and_bound(Objective, SubSolved1, AllInt, ZStar0, ZStar1, SubProb1, Solved1, Found1),
                    Found2 = 0
                ;   Sub2Feasible =:= 1 ->
                    Found1 = 0,
                    branch_and_bound(Objective, SubSolved2, AllInt, ZStar0, ZStar2, SubProb2, Solved2, Found2)
                ;   Found1 = 0, Found2 = 0
                ),
                (   Found1 =:= 1, Found2 =:= 1 -> S = Solved2, ZStar = ZStar2
                ;   Found1 =:= 1 -> S = Solved1, ZStar = ZStar1
                ;   Found2 =:= 1 -> S = Solved2, ZStar = ZStar2
                ;   S = S0, ZStar = ZStar0
                ),
                Found is max(Found1, Found2)
            ;   S = Solved, ZStar = Z, Found = 1
            )
        ;   ZStar = ZStar0, S = S0, Found = 0
        ).

maximize(Z0, S0, S) :-
        coeff_one(Z0, Z1),
        functor(S0, F, _),
        (   F == state -> maximize_mip(Z1, S0, S)
        ;   F == clpr_state -> clpr_maximize(Z1, S0, S)
        ).

maximize_mip(Z, S0, S) :-
        maximize_(Z, S0, Solved),
        state_integrals(S0, Is),
        (   Is == [] -> S = Solved
        ;   % arrange it so that branch and bound branches on variables
            % in the same order the integrality constraints were stated in
            reverse(Is, Is1),
            state_set_integrals(Is1, S0, S1),
            (   all_integers(Z, Is1) -> AllInt = 1
            ;   AllInt = 0
            ),
            branch_and_bound(Z, Solved, AllInt, _, _, S1, S, 1)
        ).

all_integers([], _).
all_integers([Coeff*V|Rest], Is) :-
        integer(Coeff),
        memberchk(V, Is),
        all_integers(Rest, Is).


minimize(Z0, S0, S) :-
        coeff_one(Z0, Z1),
        functor(S0, F, _),
        (   F == state ->
            linsum_negate(Z1, Z2),
            maximize_mip(Z2, S0, S1),
            solved_tableau(S1, tableau(Obj, Vars, Inds, Rows)),
            solved_names(S1, Names),
            Obj = row(z, Left0, Right0),
            all_times(Left0, -1, Left),
            Right is -Right0,
            Obj1 = row(z, Left, Right),
            state_integrals(S0, Is),
            S = solved(tableau(Obj1, Vars, Inds, Rows), Names, Is)
        ;   F == clpr_state -> clpr_minimize(Z1, S0, S)
        ).

op_pendant(>=, =<).
op_pendant(=<, >=).

constraints_collapse([], []).
constraints_collapse([C|Cs], Colls) :-
        C = c(Name, Left, Op, Right),
        (   Name == 0, Left = [1*Var], op_pendant(Op, P) ->
            Pendant = c(0, [1*Var], P, Right),
            (   select(Pendant, Cs, Rest) ->
                Colls = [c(0, Left, (=), Right)|CollRest],
                CsLeft = Rest
            ;   Colls = [C|CollRest],
                CsLeft = Cs
            )
        ;   Colls = [C|CollRest],
            CsLeft = Cs
        ),
        constraints_collapse(CsLeft, CollRest).

% solve a (relaxed) LP in standard form

maximize_(Z, S0, S) :-
        state_constraints(S0, Cs0),
        constraints_collapse(Cs0, Cs1),
        phrase(constraints_normalize(Cs1, Cs, As0), [S0], [S1]),
        flatten(As0, As1),
        (   As1 == [] ->
            make_tableau(Z, Cs, Tableau0),
            simplex(Tableau0, Tableau),
            state_names(S1, Names),
            state_integrals(S1, Is),
            S = solved(Tableau, Names, Is)
        ;   state_names(S1, Names),
            state_integrals(S1, Is),
            two_phase_simplex(Z, Cs, As1, Names, Is, S)
        ).

make_tableau(Z, Cs, Tableau) :-
        ZC = c(_, Z, _),
        phrase(constraints_variables([ZC|Cs]), Variables0),
        sort(Variables0, Variables),
        constraints_rows(Cs, Variables, Rows),
        linsum_row(Variables, Z, Objective1),
        all_times(Objective1, -1, Obj),
        length(Variables, LVs),
        length(Ones, LVs),
        all_one(Ones),
        Tableau = tableau(row(z, Obj, 0), Variables, Ones, Rows).

all_one([]).
all_one([1|Os]) :- all_one(Os).

proper_form([], _, _, Obj, Obj).
proper_form([_Coeff*A|As], Variables, Rows, Obj0, Obj) :-
        (   find_row(A, Rows, PivotRow) ->
            list_first(Variables, A, Col),
            row_eliminate(Obj0, PivotRow, Col, Obj1)
        ;   Obj1 = Obj0
        ),
        proper_form(As, Variables, Rows, Obj1, Obj).


two_phase_simplex(Z, Cs, As, Names, Is, S) :-
        % phase 1: minimize sum of articifial variables
        make_tableau(As, Cs, Tableau0),
        Tableau0 = tableau(Obj0, Variables, Inds, Rows),
        proper_form(As, Variables, Rows, Obj0, Obj),
        simplex(tableau(Obj, Variables, Inds, Rows), Tableau1),
        all_vars_zero(As, solved(Tableau1, _, _)),
        % phase 2: remove artificial variables and solve actual LP.
        tableau_rows(Tableau1, Rows2),
        eliminate_artificial(As, As, Variables, Rows2, Rows3),
        list_nths(As, Variables, Nths0),
        nths_to_zero(Nths0, Inds, Inds1),
        linsum_row(Variables, Z, Objective),
        all_times(Objective, -1, Objective1),
        proper_form(Z, Variables, Rows3, row(z, Objective1, 0), ObjRow),
        simplex(tableau(ObjRow, Variables, Inds1, Rows3), Tableau),
        S = solved(Tableau, Names, Is).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   If artificial variables are still in the basis, replace them with
   non-artificial variables if possible. If that is not possible, the
   constraint is ignored because it is redundant.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

eliminate_artificial([], _, _, Rows, Rows).
eliminate_artificial([_Coeff*A|Rest], As, Variables, Rows0, Rows) :-
        (   select(row(A, Left, 0), Rows0, Others) ->
            (   nth0(Col, Left, Coeff),
                Coeff =\= 0,
                nth0(Col, Variables, Var),
                \+ memberchk(_*Var, As) ->
                row_divide(row(A, Left, 0), Coeff, Row),
                gauss_elimination(Rows0, Row, Col, Rows1),
                swap_basic(Rows1, A, Var, Rows2)
            ;   Rows2 = Others
            )
        ;   Rows2 = Rows0
        ),
        eliminate_artificial(Rest, As, Variables, Rows2, Rows).

nths_to_zero([], Inds, Inds).
nths_to_zero([Nth|Nths], Inds0, Inds) :-
        nth_to_zero(Inds0, 0, Nth, Inds1),
        nths_to_zero(Nths, Inds1, Inds).

nth_to_zero([], _, _, []).
nth_to_zero([I|Is], Curr, Nth, [Z|Zs]) :-
        (   Curr =:= Nth -> [Z|Zs] = [0|Is]
        ;   Z = I,
            Next is Curr + 1,
            nth_to_zero(Is, Next, Nth, Zs)
        ).


list_nths([], _, []).
list_nths([_Coeff*A|As], Variables, [Nth|Nths]) :-
        list_first(Variables, A, Nth),
        list_nths(As, Variables, Nths).


linsum_negate([], []).
linsum_negate([Coeff0*Var|Ls], [Coeff*Var|Ns]) :-
        Coeff is Coeff0 * (-1),
        linsum_negate(Ls, Ns).


linsum_row([], _, []).
linsum_row([V|Vs], Ls, [C|Cs]) :-
        (   member(Coeff*V, Ls) -> C is rationalize(Coeff)
        ;   C = 0
        ),
        linsum_row(Vs, Ls, Cs).

constraints_rows([], _, []).
constraints_rows([C|Cs], Vars, [R|Rs]) :-
        C = c(Var, Left0, Right),
        linsum_row(Vars, Left0, Left),
        R = row(Var, Left, Right),
        constraints_rows(Cs, Vars, Rs).

constraints_normalize([], [], []) --> [].
constraints_normalize([C0|Cs0], [C|Cs], [A|As]) -->
        { constraint_op(C0, Op),
          constraint_left(C0, Left),
          constraint_right(C0, Right),
          constraint_name(C0, Name),
          Con =.. [Op, Left, Right] },
        constraint_normalize(Con, Name, C, A),
        constraints_normalize(Cs0, Cs, As).

constraint_normalize(As0 =< B0, Name, c(Slack, [1*Slack|As0], B0), []) -->
        state_next_var(Slack),
        state_add_name(Name, Slack).
constraint_normalize(As0 = B0, Name, c(AID, [1*AID|As0], B0), [-1*AID]) -->
        state_next_var(AID),
        state_add_name(Name, AID).
constraint_normalize(As0 >= B0, Name, c(AID, [-1*Slack,1*AID|As0], B0), [-1*AID]) -->
        state_next_var(Slack),
        state_next_var(AID),
        state_add_name(Name, AID).

coeff_one([], []).
coeff_one([L|Ls], [Coeff*Var|Rest]) :-
        (   L = A*B -> Coeff = A, Var = B
        ;   Coeff = 1, Var = L
        ),
        coeff_one(Ls, Rest).


tableau_optimal(Tableau) :-
        tableau_objective(Tableau, Objective),
        tableau_indicators(Tableau, Indicators),
        Objective = row(_, Left, _),
        all_nonnegative(Left, Indicators).

all_nonnegative([], []).
all_nonnegative([Coeff|As], [I|Is]) :-
        (   I =:= 0 -> true
        ;   Coeff >= 0
        ),
        all_nonnegative(As, Is).

pivot_column(Tableau, PCol) :-
        tableau_objective(Tableau, row(_, Left, _)),
        tableau_indicators(Tableau, Indicators),
        first_negative(Left, Indicators, 0, Index0, Val, RestL, RestI),
        Index1 is Index0 + 1,
        pivot_column(RestL, RestI, Val, Index1, Index0, PCol).

first_negative([L|Ls], [I|Is], Index0, N, Val, RestL, RestI) :-
        Index1 is Index0 + 1,
        (   I =:= 0 -> first_negative(Ls, Is, Index1, N, Val, RestL, RestI)
        ;   (   L < 0 -> N = Index0, Val = L, RestL = Ls, RestI = Is
            ;   first_negative(Ls, Is, Index1, N, Val, RestL, RestI)
            )
        ).


pivot_column([], _, _, _, N, N).
pivot_column([L|Ls], [I|Is], Coeff0, Index0, N0, N) :-
        (   I =:= 0 -> Coeff1 = Coeff0, N1 = N0
        ;   (   L < Coeff0 -> Coeff1 = L, N1 = Index0
            ;   Coeff1 = Coeff0, N1 = N0
            )
        ),
        Index1 is Index0 + 1,
        pivot_column(Ls, Is, Coeff1, Index1, N1, N).


pivot_row(Tableau, PCol, PRow) :-
        tableau_rows(Tableau, Rows),
        pivot_row(Rows, PCol, false, _, 0, 0, PRow).

pivot_row([], _, Bounded, _, _, Row, Row) :- Bounded.
pivot_row([Row|Rows], PCol, Bounded0, Min0, Index0, PRow0, PRow) :-
        Row = row(_Var, Left, B),
        nth0(PCol, Left, Ae),
        (   Ae > 0 ->
            Bounded1 = true,
            Bound is B rdiv Ae,
            (   Bounded0 ->
                (   Bound < Min0 -> Min1 = Bound, PRow1 = Index0
                ;   Min1 = Min0, PRow1 = PRow0
                )
            ;   Min1 = Bound, PRow1 = Index0
            )
        ;   Bounded1 = Bounded0, Min1 = Min0, PRow1 = PRow0
        ),
        Index1 is Index0 + 1,
        pivot_row(Rows, PCol, Bounded1, Min1, Index1, PRow1, PRow).


row_divide(row(Var, Left0, Right0), Div, row(Var, Left, Right)) :-
        all_divide(Left0, Div, Left),
        Right is Right0 rdiv Div.


all_divide([], _, []).
all_divide([R|Rs], Div, [DR|DRs]) :-
        DR is R rdiv Div,
        all_divide(Rs, Div, DRs).

gauss_elimination([], _, _, []).
gauss_elimination([Row0|Rows0], PivotRow, Col, [Row|Rows]) :-
        PivotRow = row(PVar, _, _),
        Row0 = row(Var, _, _),
        (   PVar == Var -> Row = PivotRow
        ;   row_eliminate(Row0, PivotRow, Col, Row)
        ),
        gauss_elimination(Rows0, PivotRow, Col, Rows).

row_eliminate(row(Var, Ls0, R0), row(_, PLs, PR), Col, row(Var, Ls, R)) :-
        nth0(Col, Ls0, Coeff),
        (   Coeff =:= 0 -> Ls = Ls0, R = R0
        ;   MCoeff is -Coeff,
            all_times_plus([PR|PLs], MCoeff, [R0|Ls0], [R|Ls])
        ).

all_times_plus([], _, _, []).
all_times_plus([A|As], T, [B|Bs], [AT|ATs]) :-
        AT is A * T + B,
        all_times_plus(As, T, Bs, ATs).

all_times([], _, []).
all_times([A|As], T, [AT|ATs]) :-
        AT is A * T,
        all_times(As, T, ATs).

simplex(Tableau0, Tableau) :-
        (   tableau_optimal(Tableau0) -> Tableau0 = Tableau
        ;   pivot_column(Tableau0, PCol),
            pivot_row(Tableau0, PCol, PRow),
            Tableau0 = tableau(Obj0,Variables,Inds,Matrix0),
            nth0(PRow, Matrix0, Row0),
            Row0 = row(Leaving, Left0, _Right0),
            nth0(PCol, Left0, PivotElement),
            row_divide(Row0, PivotElement, Row1),
            gauss_elimination([Obj0|Matrix0], Row1, PCol, [Obj|Matrix1]),
            nth0(PCol, Variables, Entering),
            swap_basic(Matrix1, Leaving, Entering, Matrix),
            simplex(tableau(Obj,Variables,Inds,Matrix), Tableau)
        ).

swap_basic([Row0|Rows0], Old, New, Matrix) :-
        Row0 = row(Var, Left, Right),
        (   Var == Old -> Matrix = [row(New, Left, Right)|Rows0]
        ;   Matrix = [Row0|Rest],
            swap_basic(Rows0, Old, New, Rest)
        ).

constraints_variables([]) --> [].
constraints_variables([c(_,Left,_)|Cs]) -->
        variables(Left),
        constraints_variables(Cs).

variables([]) --> [].
variables([_Coeff*Var|Rest]) --> [Var], variables(Rest).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   A dual algorithm ("algorithm alpha-beta" in Papadimitriou and
   Steiglitz) is used for transportation and assignment problems. The
   arising max-flow problem is solved with Edmonds-Karp, itself a dual
   algorithm.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   An attributed variable is introduced for each node. Attributes:
   node: Original name of the node.
   edges: arc_to(To,F,Capacity) (F has an attribute "flow") or
          arc_from(From,F,Capacity)
   parent: used in breadth-first search
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

arcs([], Assoc, Assoc).
arcs([arc(From0,To0,C)|As], Assoc0, Assoc) :-
        (   get_assoc(From0, Assoc0, From) -> Assoc1 = Assoc0
        ;   put_assoc(From0, Assoc0, From, Assoc1),
            put_attr(From, node, From0)
        ),
        (   get_attr(From, edges, Es) -> true
        ;   Es = []
        ),
        put_attr(F, flow, 0),
        put_attr(From, edges, [arc_to(To,F,C)|Es]),
        (   get_assoc(To0, Assoc1, To) -> Assoc2 = Assoc1
        ;   put_assoc(To0, Assoc1, To, Assoc2),
            put_attr(To, node, To0)
        ),
        (   get_attr(To, edges, Es1) -> true
        ;   Es1 = []
        ),
        put_attr(To, edges, [arc_from(From,F,C)|Es1]),
        arcs(As, Assoc2, Assoc).


edmonds_karp(Arcs0, Arcs) :-
        empty_assoc(E),
        arcs(Arcs0, E, Assoc),
        get_assoc(s, Assoc, S),
        get_assoc(t, Assoc, T),
        maximum_flow(S, T),
        % fetch attvars before deleting visited edges
        term_attvars(S, AttVars),
        phrase(flow_to_arcs(S), Ls),
        arcs_assoc(Ls, Arcs),
        maplist(del_attrs, AttVars).

flow_to_arcs(V) -->
        (   { get_attr(V, edges, Es) } ->
            { del_attr(V, edges),
              get_attr(V, node, Name) },
            flow_to_arcs_(Es, Name)
        ;   []
        ).

flow_to_arcs_([], _) --> [].
flow_to_arcs_([E|Es], Name) -->
        edge_to_arc(E, Name),
        flow_to_arcs_(Es, Name).

edge_to_arc(arc_from(_,_,_), _) --> [].
edge_to_arc(arc_to(To,F,C), Name) -->
        { get_attr(To, node, NTo),
          get_attr(F, flow, Flow) },
        [arc(Name,NTo,Flow,C)],
        flow_to_arcs(To).

arcs_assoc(Arcs, Hash) :-
        empty_assoc(E),
        arcs_assoc(Arcs, E, Hash).

arcs_assoc([], Hs, Hs).
arcs_assoc([arc(From,To,F,C)|Rest], Hs0, Hs) :-
        (   get_assoc(From, Hs0, As) -> Hs1 = Hs0
        ;   put_assoc(From, Hs0, [], Hs1),
            empty_assoc(As)
        ),
        put_assoc(To, As, arc(From,To,F,C), As1),
        put_assoc(From, Hs1, As1, Hs2),
        arcs_assoc(Rest, Hs2, Hs).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Strategy: Breadth-first search until we find a free right vertex in
   the value graph, then find an augmenting path in reverse.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

maximum_flow(S, T) :-
        (   augmenting_path([[S]], Levels, T) ->
            phrase(augmenting_path(S, T), Path),
            Path = [augment(_,First,_)|Rest],
            path_minimum(Rest, First, Min),
            % format("augmenting path: ~w\n", [Min]),
            maplist(augment(Min), Path),
            maplist(maplist(clear_parent), Levels),
            maximum_flow(S, T)
        ;   true
        ).

clear_parent(V) :- del_attr(V, parent).

augmenting_path(Levels0, Levels, T) :-
        Levels0 = [Vs|_],
        Levels1 = [Tos|Levels0],
        phrase(reachables(Vs), Tos),
        Tos = [_|_],
        (   member(To, Tos), To == T -> Levels = Levels1
        ;   augmenting_path(Levels1, Levels, T)
        ).

reachables([])     --> [].
reachables([V|Vs]) -->
        { get_attr(V, edges, Es) },
        reachables_(Es, V),
        reachables(Vs).

reachables_([], _)     --> [].
reachables_([E|Es], V) -->
        reachable(E, V),
        reachables_(Es, V).

reachable(arc_from(V,F,_), P) -->
        (   { \+ get_attr(V, parent, _),
              get_attr(F, flow, Flow),
              Flow > 0 } ->
            { put_attr(V, parent, P-augment(F,Flow,-)) },
            [V]
        ;   []
        ).
reachable(arc_to(V,F,C), P) -->
        (   { \+ get_attr(V, parent, _),
              get_attr(F, flow, Flow),
              (   C == inf ; Flow < C )} ->
            { ( C == inf -> Diff = inf
              ;   Diff is C - Flow
              ),
              put_attr(V, parent, P-augment(F,Diff,+)) },
            [V]
        ;   []
        ).


path_minimum([], Min, Min).
path_minimum([augment(_,A,_)|As], Min0, Min) :-
        (   A == inf -> Min1 = Min0
        ;   Min1 is min(Min0,A)
        ),
        path_minimum(As, Min1, Min).

augment(Min, augment(F,_,Sign)) :-
        get_attr(F, flow, Flow0),
        flow_(Sign, Flow0, Min, Flow),
        put_attr(F, flow, Flow).

flow_(+, F0, A, F) :- F is F0 + A.
flow_(-, F0, A, F) :- F is F0 - A.

augmenting_path(S, V) -->
        (   { V == S } -> []
        ;   { get_attr(V, parent, V1-Augment) },
            [Augment],
            augmenting_path(S, V1)
        ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

naive_init(Supplies, _, Costs, Alphas, Betas) :-
        length(Supplies, LAs),
        length(Alphas, LAs),
        maplist(=(0), Alphas),
        transpose(Costs, TCs),
        naive_init_betas(TCs, Betas).

naive_init_betas([], []).
naive_init_betas([Ls|Lss], [B|Bs]) :-
        list_min(Ls, B),
        naive_init_betas(Lss, Bs).

list_min([F|Rest], Min) :-
        list_min(Rest, F, Min).

list_min([], Min, Min).
list_min([L|Ls], Min0, Min) :-
        Min1 is min(L,Min0),
        list_min(Ls, Min1, Min).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

transpose(Ms, Ts) :- Ms = [F|_], transpose(F, Ms, Ts).

transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
        lists_firsts_rests(Ms, Ts, Ms1),
        transpose(Rs, Ms1, Tss).

lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
        lists_firsts_rests(Rest, Fs, Oss).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   TODO: use attributed variables throughout
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

transportation(Supplies, Demands, Costs, Transport) :-
        length(Supplies, LAs),
        length(Demands, LBs),
        naive_init(Supplies, Demands, Costs, Alphas, Betas),
        network_head(Supplies, 1, SArcs, []),
        network_tail(Demands, 1, DArcs, []),
        numlist(1, LAs, Sources),
        numlist(1, LBs, Sinks0),
        maplist(make_sink, Sinks0, Sinks),
        append(SArcs, DArcs, Torso),
        alpha_beta(Torso, Sources, Sinks, Costs, Alphas, Betas, Flow),
        flow_transport(Supplies, 1, Demands, Flow, Transport).

flow_transport([], _, _, _, []).
flow_transport([_|Rest], N, Demands, Flow, [Line|Lines]) :-
        transport_line(Demands, N, 1, Flow, Line),
        N1 is N + 1,
        flow_transport(Rest, N1, Demands, Flow, Lines).

transport_line([], _, _, _, []).
transport_line([_|Rest], I, J, Flow, [L|Ls]) :-
        (   get_assoc(I, Flow, As), get_assoc(p(J), As, arc(I,p(J),F,_)) -> L = F
        ;   L = 0
        ),
        J1 is J + 1,
        transport_line(Rest, I, J1, Flow, Ls).


make_sink(N, p(N)).

network_head([], _) --> [].
network_head([S|Ss], N) -->
        [arc(s,N,S)],
        { N1 is N + 1 },
        network_head(Ss, N1).

network_tail([], _) --> [].
network_tail([D|Ds], N) -->
        [arc(p(N),t,D)],
        { N1 is N + 1 },
        network_tail(Ds, N1).

network_connections([], _, _, _) --> [].
network_connections([A|As], Betas, [Cs|Css], N) -->
        network_connections(Betas, Cs, A, N, 1),
        { N1 is N + 1 },
        network_connections(As, Betas, Css, N1).

network_connections([], _, _, _, _) --> [].
network_connections([B|Bs], [C|Cs], A, N, PN) -->
        (   { C =:= A + B } -> [arc(N,p(PN),inf)]
        ;   []
        ),
        { PN1 is PN + 1 },
        network_connections(Bs, Cs, A, N, PN1).

alpha_beta(Torso, Sources, Sinks, Costs, Alphas, Betas, Flow) :-
        network_connections(Alphas, Betas, Costs, 1, Cons, []),
        append(Torso, Cons, Arcs),
        edmonds_karp(Arcs, MaxFlow),
        mark_hashes(MaxFlow, MArcs, MRevArcs),
        all_markable(MArcs, MRevArcs, Markable),
        mark_unmark(Sources, Markable, MarkSources, UnmarkSources),
        (   MarkSources == [] -> Flow = MaxFlow
        ;   mark_unmark(Sinks, Markable, MarkSinks0, UnmarkSinks0),
            maplist(un_p, MarkSinks0, MarkSinks),
            maplist(un_p, UnmarkSinks0, UnmarkSinks),
            MarkSources = [FirstSource|_],
            UnmarkSinks = [FirstSink|_],
            theta(FirstSource, FirstSink, Costs, Alphas, Betas, TInit),
            theta(MarkSources, UnmarkSinks, Costs, Alphas, Betas, TInit, Theta),
            duals_add(MarkSources, Alphas, Theta, Alphas1),
            duals_add(UnmarkSinks, Betas, Theta, Betas1),
            Theta1 is -Theta,
            duals_add(UnmarkSources, Alphas1, Theta1, Alphas2),
            duals_add(MarkSinks, Betas1, Theta1, Betas2),
            alpha_beta(Torso, Sources, Sinks, Costs, Alphas2, Betas2, Flow)
        ).

mark_hashes(MaxFlow, Arcs, RevArcs) :-
        assoc_to_list(MaxFlow, FlowList),
        maplist(un_arc, FlowList, FlowList1),
        flatten(FlowList1, FlowList2),
        empty_assoc(E),
        mark_arcs(FlowList2, E, Arcs),
        mark_revarcs(FlowList2, E, RevArcs).

un_arc(_-Ls0, Ls) :-
        assoc_to_list(Ls0, Ls1),
        maplist(un_arc_, Ls1, Ls).

un_arc_(_-Ls, Ls).

mark_arcs([], Arcs, Arcs).
mark_arcs([arc(From,To,F,C)|Rest], Arcs0, Arcs) :-
        (   get_assoc(From, Arcs0, As) -> true
        ;   As = []
        ),
        (   C == inf -> As1 = [To|As]
        ;   F < C -> As1 = [To|As]
        ;   As1 = As
        ),
        put_assoc(From, Arcs0, As1, Arcs1),
        mark_arcs(Rest, Arcs1, Arcs).

mark_revarcs([], Arcs, Arcs).
mark_revarcs([arc(From,To,F,_)|Rest], Arcs0, Arcs) :-
        (   get_assoc(To, Arcs0, Fs) -> true
        ;   Fs = []
        ),
        (   F > 0 -> Fs1 = [From|Fs]
        ;   Fs1 = Fs
        ),
        put_assoc(To, Arcs0, Fs1, Arcs1),
        mark_revarcs(Rest, Arcs1, Arcs).


un_p(p(N), N).

duals_add([], Alphas, _, Alphas).
duals_add([S|Ss], Alphas0, Theta, Alphas) :-
        add_to_nth(1, S, Alphas0, Theta, Alphas1),
        duals_add(Ss, Alphas1, Theta, Alphas).

add_to_nth(N, N, [A0|As], Theta, [A|As]) :- !,
        A is A0 + Theta.
add_to_nth(N0, N, [A|As0], Theta, [A|As]) :-
        N1 is N0 + 1,
        add_to_nth(N1, N, As0, Theta, As).


theta(Source, Sink, Costs, Alphas, Betas, Theta) :-
        nth1(Source, Costs, Row),
        nth1(Sink, Row, C),
        nth1(Source, Alphas, A),
        nth1(Sink, Betas, B),
        Theta is (C - A - B) rdiv 2.

theta([], _, _, _, _, Theta, Theta).
theta([Source|Sources], Sinks, Costs, Alphas, Betas, Theta0, Theta) :-
        theta_(Sinks, Source, Costs, Alphas, Betas, Theta0, Theta1),
        theta(Sources, Sinks, Costs, Alphas, Betas, Theta1, Theta).

theta_([], _, _, _, _, Theta, Theta).
theta_([Sink|Sinks], Source, Costs, Alphas, Betas, Theta0, Theta) :-
        theta(Source, Sink, Costs, Alphas, Betas, Theta1),
        Theta2 is min(Theta0, Theta1),
        theta_(Sinks, Source, Costs, Alphas, Betas, Theta2, Theta).


mark_unmark(Nodes, Hash, Mark, Unmark) :-
        mark_unmark(Nodes, Hash, Mark, [], Unmark, []).

mark_unmark([], _, Mark, Mark, Unmark, Unmark).
mark_unmark([Node|Nodes], Markable, Mark0, Mark, Unmark0, Unmark) :-
        (   memberchk(Node, Markable) ->
            Mark0 = [Node|Mark1],
            Unmark0 = Unmark1
        ;   Mark0 = Mark1,
            Unmark0 = [Node|Unmark1]
        ),
        mark_unmark(Nodes, Markable, Mark1, Mark, Unmark1, Unmark).

all_markable(Flow, RevArcs, Markable) :-
        phrase(markable(s, [], _, Flow, RevArcs), Markable).

all_markable([], Visited, Visited, _, _) --> [].
all_markable([To|Tos], Visited0, Visited, Arcs, RevArcs) -->
        (   { memberchk(To, Visited0) } -> { Visited0 = Visited1 }
        ;   markable(To, [To|Visited0], Visited1, Arcs, RevArcs)
        ),
        all_markable(Tos, Visited1, Visited, Arcs, RevArcs).

markable(Current, Visited0, Visited, Arcs, RevArcs) -->
        { (   Current = p(_) ->
              (   get_assoc(Current, RevArcs, Fs) -> true
              ;   Fs = []
              )
          ;   (   get_assoc(Current, Arcs, Fs) -> true
              ;   Fs = []
              )
          ) },
        [Current],
        all_markable(Fs, [Current|Visited0], Visited, Arcs, RevArcs).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   solve(File) -- read input from File.

   Format (NS = number of sources, ND = number of demands):

   NS
   ND
   S1 S2 S3 ...
   D1 D2 D3 ...
   C11 C12 C13 ...
   C21 C22 C23 ...
   ... ... ... ...
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

input(Ss, Ds, Costs) -->
        integer(NS),
        integer(ND),
        n_integers(NS, Ss),
        n_integers(ND, Ds),
        n_kvectors(NS, ND, Costs).

n_kvectors(0, _, []) --> !.
n_kvectors(N, K, [V|Vs]) -->
        n_integers(K, V),
        { N1 is N - 1 },
        n_kvectors(N1, K, Vs).

n_integers(0, []) --> !.
n_integers(N, [I|Is]) --> integer(I), { N1 is N - 1 }, n_integers(N1, Is).


number([D|Ds]) --> digit(D), number(Ds).
number([D])    --> digit(D).

digit(D) --> [D], { between(0'0, 0'9, D) }.

integer(N) --> number(Ds), !, ws, { name(N, Ds) }.

ws --> [W], { W =< 0' }, !, ws.  % closing quote for syntax highlighting: '
ws --> [].

solve(File) :-
        time((phrase_from_file(input(Supplies, Demands, Costs), File),
              transportation(Supplies, Demands, Costs, Matrix),
              maplist(print_row, Matrix))),
        halt.

print_row(R) :- maplist(print_row_, R), nl.

print_row_(N) :- format("~w ", [N]).


% ?- call_residue_vars(transportation([12,7,14], [3,15,9,6], [[20,50,10,60],[70,40,60,30],[40,80,70,40]], Ms), Vs).
%@ Ms = [[0, 3, 9, 0], [0, 7, 0, 0], [3, 5, 0, 6]],
%@ Vs = [].


%?- call_residue_vars(simplex:solve('instance_80_80.txt'), Vs).

%?- call_residue_vars(simplex:solve('instance_3_4.txt'), Vs).

%?- call_residue_vars(simplex:solve('instance_100_100.txt'), Vs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Assignment problem - for now, reduce to transportation problem

assignment(Costs, Assignment) :-
        length(Costs, LC),
        length(Supply, LC),
        all_one(Supply),
        transportation(Supply, Supply, Costs, Assignment).

