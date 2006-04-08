/*  $Id$

    Part of SWI-Prolog

    Author:        Markus Triska
    E-mail:        triska@gmx.at
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2005, Markus Triska

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
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CLP(R) bindings
% the (unsolved) state is stored as a structure of the form
%    clpr_state(Options, Cs, Is)
% Options: list of Option=Value pairs, currently only eps=Eps
% Cs: list of constraints, i.e., structures of the form
%    c(Name, Left, Op, Right)
%    anonymous constraints have Name == 0
% Is: list of integral variables

gen_state_clpr(State) :-
	gen_state_clpr([], State).

gen_state_clpr(Options, State) :-
	( memberchk(eps=_, Options) ->
		Options1 = Options
	;
		Options1 = [eps=1e-6|Options]
	),
	State = clpr_state(Options1, [], []).

clpr_state_options(clpr_state(Os, _, _), Os).
clpr_state_constraints(clpr_state(_, Cs, _), Cs).
clpr_state_integrals(clpr_state(_, _, Is), Is).
clpr_state_add_integral(I, clpr_state(Os, Cs, Is), clpr_state(Os, Cs, [I|Is])).
clpr_state_add_constraint(C, clpr_state(Os, Cs, Is), clpr_state(Os, [C|Cs], Is)).
clpr_state_set_constraints(Cs, clpr_state(Os,_,Is), clpr_state(Os, Cs, Is)).

clpr_constraint(Name, Constraint, S0, S) :-
	( Constraint = integral(Var) ->
		clpr_state_add_integral(Var, S0, S)
	;
		Constraint =.. [Op,Left,Right],
		coeff_one(Left, Left1),
		clpr_state_add_constraint(c(Name, Left1, Op, Right), S0, S)
	).

clpr_constraint(Constraint, S0, S) :-
	clpr_constraint(0, Constraint, S0, S).

clpr_shadow_price(clpr_solved(_,_,Duals,_), Name, Value) :-
	memberchk(Name-Value0, Duals),
	Value is Value0.
	%( var(Value0) ->
	%	Value = 0
	%;
	%	Value is Value0
	%).



clpr_make_variables(Cs, Aliases) :-
	clpr_constraints_variables(Cs, Variables0, []),
	sort(Variables0, Variables1),
	clpr_aliases(Variables1, Aliases).

clpr_constraints_variables([]) -->
	[].
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
	( Op == (=<) ->
		Nons0 = [c(0, [1*Name], (>=), 0)|Rest]
	;
		Nons0 = Rest
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
	( Name == 0 ->
		N = Num,
		Num1 is Num + 1
	;
		N = Name,
		Num1 = Num
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
	%	Value = 0
	%;
	%	Value is Value0
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



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% General Simplex Algorithm
% Structures used:
%
% tableau(Objective, Variables, Indicators, Constraints)
%   *) objective function, represented as row
%   *) list of variables corresponding to columns
%   *) indicators denoting which variables are still active
%   *) constraints as rows
%
% row(Var, Left, Right)
%   *) the basic variable corresponding to this row
%   *) coefficients of the left-hand side of the constraint
%   *) right-hand side of the constraint
%


find_row(Variable, [Row|Rows], R) :-
	Row = row(V, _, _),
	( V == Variable ->
		R = Row
	;
		find_row(Variable, Rows, R)
	).


variable_value(State, Variable, Value) :-
	functor(State, F, _),
	( F == solved ->
		solved_tableau(State, Tableau),
		tableau_rows(Tableau, Rows),
		( find_row(Variable, Rows, Row) ->
			Row = row(_, _, Value)
		;
			Value = 0
		)
	; F == clpr_solved ->
		clpr_variable_value(State, Variable, Value)
	).

all_vars_zero([], _).
all_vars_zero([_Coeff*Var|Vars], State) :-
	variable_value(State, Var, 0),
	all_vars_zero(Vars, State).


list_first(Ls, F, Index) :-
	once(nth0(Index, Ls, F)).

shadow_price(State, Name, Value) :-
	functor(State, F, _),
	( F == solved ->
		solved_tableau(State, Tableau),
		tableau_objective(Tableau, Objective),
		Objective = row(_, Left, _),
		tableau_variables(Tableau, Variables),
		solved_names(State, Names),
		memberchk([Name,Var], Names),
		list_first(Variables, Var, Nth0),
		nth0(Nth0, Left, Value)
	; F == clpr_solved ->
		clpr_shadow_price(State, Name, Value)
	).



objective(State, Obj) :-
	functor(State, F, _),
	( F == solved ->
		solved_tableau(State, Tableau),
		tableau_objective(Tableau, Objective),
		Objective = row(_, _, Obj)
	;
		clpr_objective(State, Obj)
	).

   % interface functions that access tableau components

tableau_objective(tableau(Obj, _, _, _), Obj).
tableau_rows(tableau(_, _, _, Rows), Rows).
tableau_indicators(tableau(_, _, Inds, _), Inds).
tableau_variables(tableau(_, Vars, _, _), Vars).


   % interface functions that access and modify state components

   % state is a structure of the form
   %     state(Num, Names, Cs, Is)
   % Num: used to obtain new unique names for slack variables in a side-effect
   %      free way (increased by one and threaded through)
   % Names: list of [Name,Var], correspondence between constraint-names and
   %      names of slack/artificial variables to obtain shadow prices later
   % Cs: list of constraints
   % Is: list of integer variables

   % constraints are initially represented as c(Name, Left, Op, Right),
   % and after normalizing as c(Var, Left, Right). Name of unnamed constraints
   % is 0. The distinction is important for merging constraints (mainly in
   % branch and bound) with existing ones.

constraint_name(c(Name, _, _, _), Name).
constraint_op(c(_, _, Op, _), Op).
constraint_left(c(_, Left, _, _), Left).
constraint_right(c(_, _, _, Right), Right).

gen_state(state(0,[],[],[])).

state_add_constraint(C, S0, S) :-
	( constraint_name(C, 0), constraint_left(C, [_Coeff*_Var]) ->
		state_merge_constraint(C, S0, S)
	;
		state_add_constraint_(C, S0, S)
	).

state_add_constraint_(C, state(VID,Ns,Cs,Is), state(VID,Ns,[C|Cs],Is)).

state_merge_constraint(C, S0, S) :-
	constraint_left(C, [Coeff0*Var0]),
	constraint_right(C, Right0),
	constraint_op(C, Op),
	( Coeff0 =:= 0 ->
		( Op == (=) ->
			Right0 =:= 0,
			S0 = S
		; Op == (=<) ->
			S0 = S
		; Op == (>=) ->
			Right0 =:= 0,
			S0 = S
		)
	; Coeff0 < 0 ->
		state_add_constraint_(C, S0, S)
	;
		Right is Right0 rdiv Coeff0,
		state_constraints(S0, Cs),
		(  select(c(0, [1*Var0], Op, CRight), Cs, RestCs) ->
			( Op == (=) ->
				CRight =:= Right,
				S0 = S
			; Op == (=<) ->
				NewRight is min(Right, CRight),
				NewCs = [c(0, [1*Var0], Op, NewRight)|RestCs],
				state_set_constraints(NewCs, S0, S)
			; Op == (>=) ->
				NewRight is max(Right, CRight),
				NewCs = [c(0, [1*Var0], Op, NewRight)|RestCs],
				state_set_constraints(NewCs, S0, S)
			)
		;
			state_add_constraint_(c(0, [1*Var0], Op, Right), S0, S)
		)
	).


state_add_name(Name, Var, state(VID,Ns,Cs,Is), state(VID,[[Name,Var]|Ns],Cs,Is)).

state_add_integral(Var, state(VID,Ns,Cs,Is), state(VID,Ns,Cs,[Var|Is])).

state_constraints(state(_, _, Cs, _), Cs).
state_names(state(_,Names,_,_), Names).
state_integrals(state(_,_,_,Is), Is).
state_set_constraints(Cs, state(VID,Ns,_,Is), state(VID,Ns,Cs,Is)).
state_set_integrals(Is, state(VID,Ns,Cs,_), state(VID,Ns,Cs,Is)).


state_next_var(VarID0, S0, S) :-
	S0 = state(VarID0,Names,Cs,Is),
	VarID1 is VarID0 + 1,
	S = state(VarID1,Names,Cs,Is).

solved_tableau(solved(Tableau, _, _), Tableau).
solved_names(solved(_, Names,_), Names).
solved_integrals(solved(_,_,Is), Is).


constraint(C, S0, S) :-
	functor(S0, F, _),
	( F == state ->
		( C = integral(Var) ->
			state_add_integral(Var, S0, S)
		;
			constraint(0, C, S0, S)
		)
	; F == clpr_state ->
		clpr_constraint(C, S0, S)
	).


constraint(Name, C, S0, S) :-
	functor(S0, F, _),
	( F == state ->
		( C = integral(Var) ->
			state_add_integral(Var, S0, S)
		;
			C =.. [Op, Left0, Right0],
			coeff_one(Left0, Left),
			Right0 >= 0,
			Right is rationalize(Right0),
			state_add_constraint(c(Name, Left, Op, Right), S0, S)
		)
	; F == clpr_state ->
		clpr_constraint(Name, C, S0, S)
	).

constraint_add(Name, A, S0, S) :-
	functor(S0, F, _),
	( F == state ->
		state_constraints(S0, Cs),
		add_left(Cs, Name, A, Cs1),
		state_set_constraints(Cs1, S0, S)
	; F == clpr_state ->
		clpr_state_constraints(S0, Cs),
		add_left(Cs, Name, A, Cs1),
		clpr_state_set_constraints(Cs1, S0, S)
	).


add_left([c(Name,Left0,Op,Right)|Cs], V, A, [c(Name,Left,Op,Right)|Rest]) :-
	( Name == V ->
		append(A, Left0, Left),
		Rest = Cs
	;
		Left0 = Left,
		add_left(Cs, V, A, Rest)
	).

branching_variable(State, Variable) :-
	solved_integrals(State, Integrals),
	member(Variable, Integrals),
	variable_value(State, Variable, Value),
	\+ integer(Value).


worth_investigating(ZStar0, _, _) :-
	var(ZStar0).
worth_investigating(ZStar0, AllInt, Z) :-
	nonvar(ZStar0),
	( AllInt =:= 1 ->
		Z1 is floor(Z)
	;
		Z1 = Z
	),
	Z1 > ZStar0.


branch_and_bound(Objective, Solved, AllInt, ZStar0, ZStar, S0, S, Found) :-
	objective(Solved, Z),
	( worth_investigating(ZStar0, AllInt, Z) ->
		( branching_variable(Solved, BrVar) ->
			variable_value(Solved, BrVar, Value),
			Value1 is floor(Value),
			Value2 is Value1 + 1,
			constraint([BrVar] =< Value1, S0, SubProb1),
			( maximize_(Objective, SubProb1, SubSolved1) ->
				Sub1Feasible = 1,
				objective(SubSolved1, Obj1)
			;
				Sub1Feasible = 0
			),
			constraint([BrVar] >= Value2, S0, SubProb2),
			( maximize_(Objective, SubProb2, SubSolved2) ->
				Sub2Feasible = 1,
				objective(SubSolved2, Obj2)
			;
				Sub2Feasible = 0
			),
			( Sub1Feasible =:= 1, Sub2Feasible =:= 1 ->
				( Obj1 >= Obj2 ->
					First = SubProb1,
					Second = SubProb2,
					FirstSolved = SubSolved1,
					SecondSolved = SubSolved2
				;
					First = SubProb2,
					Second = SubProb1,
					FirstSolved = SubSolved2,
					SecondSolved = SubSolved1
				),
				branch_and_bound(Objective, FirstSolved, AllInt, ZStar0, ZStar1, First, Solved1, Found1),
				branch_and_bound(Objective, SecondSolved, AllInt, ZStar1, ZStar2, Second, Solved2, Found2)
			; Sub1Feasible =:= 1 ->
				branch_and_bound(Objective, SubSolved1, AllInt, ZStar0, ZStar1, SubProb1, Solved1, Found1),
				Found2 = 0
			; Sub2Feasible =:= 1 ->
				Found1 = 0,
				branch_and_bound(Objective, SubSolved2, AllInt, ZStar0, ZStar2, SubProb2, Solved2, Found2)
			;
				Found1 = 0,
				Found2 = 0
			),
			( Found1 =:= 1, Found2 =:= 1 ->
				S = Solved2,
				ZStar = ZStar2
			; Found1 =:= 1 ->
				S = Solved1,
				ZStar = ZStar1
			; Found2 =:= 1 ->
				S = Solved2,
				ZStar = ZStar2
			;
				S = S0,
				ZStar = ZStar0
			),
			Found is max(Found1, Found2)
		;
			S = Solved,
			ZStar = Z,
			Found = 1
		)
	;
		ZStar = ZStar0,
		S = S0,
		Found = 0
	).

maximize(Z0, S0, S) :-
	coeff_one(Z0, Z1),
	functor(S0, F, _),
	( F == state ->
		maximize_mip(Z1, S0, S)
	; F == clpr_state ->
		clpr_maximize(Z1, S0, S)
	).

maximize_mip(Z, S0, S) :-
	maximize_(Z, S0, Solved),
	state_integrals(S0, Is),
	( Is == [] ->
		S = Solved
	;
		% arrange it so that branch and bound branches on variables
		% in the same order the integrality constraints were stated in
		reverse(Is, Is1),
		state_set_integrals(Is1, S0, S1),
		( all_integers(Z, Is1) ->
			AllInt = 1
		;
			AllInt = 0
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
	( F == state ->
		linsum_negate(Z1, Z2),
		maximize_mip(Z2, S0, S1),
		solved_tableau(S1, Tableau),
		solved_names(S1, Names),
		Tableau = tableau(Obj, Vars, Inds, Rows),
		Obj = row(z, Left0, Right0),
		all_times(Left0, (-1), Left),
		Right is -Right0,
		Obj1 = row(z, Left, Right),
		state_integrals(S0, Is),
		S = solved(tableau(Obj1, Vars, Inds, Rows), Names, Is)
	; F == clpr_state ->
		clpr_minimize(Z1, S0, S)
	).

op_pendant((>=), (=<)).
op_pendant((=<), (>=)).

constraints_collapse([], []).
constraints_collapse([C|Cs], Colls) :-
	C = c(Name, Left, Op, Right),
	( Name == 0, Left = [1*Var], op_pendant(Op, P) ->
		Pendant = c(0, [1*Var], P, Right),
		( select(Pendant, Cs, Rest) ->
			Colls = [c(0, Left, (=), Right)|CollRest],
			CsLeft = Rest
		;
			Colls = [C|CollRest],
			CsLeft = Cs
		)
	;
		Colls = [C|CollRest],
		CsLeft = Cs
	),
	constraints_collapse(CsLeft, CollRest).

   % solve a (relaxed) LP in standard form

maximize_(Z, S0, S) :-
	state_constraints(S0, Cs0),
	constraints_collapse(Cs0, Cs1),
	constraints_normalize(Cs1, Cs, As0, [], S0, S1),
	flatten(As0, As1),
	( As1 == [] ->
		make_tableau(Z, Cs, Tableau0),
		simplex(Tableau0, Tableau),
		state_names(S1, Names),
		state_integrals(S1, Is),
		S = solved(Tableau, Names, Is)
	;
		state_names(S1, Names),
		state_integrals(S1, Is),
		two_phase_simplex(Z, Cs, As1, Names, Is, S)
	).

make_tableau(Z, Cs, Tableau) :-
	ZC = c(_, Z, _),
	phrase(constraints_variables([ZC|Cs]), Variables0),
	sort(Variables0, Variables),
	constraints_rows(Cs, Variables, Rows),
	linsum_row(Variables, Z, Objective1),
	all_times(Objective1, (-1), Obj),
	length(Variables, LVs),
	length(Ones, LVs),
	all_one(Ones),
	Tableau = tableau(row(z, Obj, 0), Variables, Ones, Rows).

all_one([]).
all_one([1|Os]) :-
	all_one(Os).


proper_form([], _, _, Obj, Obj).
proper_form([_Coeff*A|As], Variables, Rows, Obj0, Obj) :-
	( find_row(A, Rows, PivotRow) ->
		list_first(Variables, A, Col),
		row_eliminate(Obj0, PivotRow, Col, Obj1)
	;
		Obj1 = Obj0
	),
	proper_form(As, Variables, Rows, Obj1, Obj).


two_phase_simplex(Z, Cs, As, Names, Is, S) :-
        % phase 1: minimize sum of articifial variables
	make_tableau(As, Cs, Tableau0),
	Tableau0 = tableau(Obj0, Variables, Inds, Rows),
	proper_form(As, Variables, Rows, Obj0, Obj),
	Tableau1 = tableau(Obj, Variables, Inds, Rows),
	simplex(Tableau1, Tableau2),
	all_vars_zero(As, solved(Tableau2, _, _)),
        % phase 2: ignore artificial variables and solve actual LP
	tableau_rows(Tableau2, Rows2),
	eliminate_artificial(As, Rows2, Rows3),
	list_nths(As, Variables, Nths0),
	nths_to_zero(Nths0, Inds, Inds1),
	linsum_row(Variables, Z, Objective),
	all_times(Objective, (-1), Objective1),
	ObjRow0 = row(z, Objective1, 0),
	proper_form(Z, Variables, Rows3, ObjRow0, ObjRow),
	Tableau3 = tableau(ObjRow, Variables, Inds1, Rows3),
	simplex(Tableau3, Tableau),
	S = solved(Tableau, Names, Is).

eliminate_artificial([], Rows, Rows).
eliminate_artificial([_Coeff*Var|Rest], Rows0, Rows) :-
	delete(Rows0, row(Var, _, _), Rows1),
	eliminate_artificial(Rest, Rows1, Rows).


nths_to_zero([], Inds, Inds).
nths_to_zero([Nth|Nths], Inds0, Inds) :-
	nth_to_zero(Inds0, 0, Nth, Inds1),
	nths_to_zero(Nths, Inds1, Inds).

nth_to_zero([], _, _, []).
nth_to_zero([I|Is], Curr, Nth, [Z|Zs]) :-
	( Curr =:= Nth ->
		[Z|Zs] = [0|Is]
	;
		Z = I,
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
	( member(Coeff*V, Ls) ->
		C is rationalize(Coeff)
	;
		C = 0
	),
	linsum_row(Vs, Ls, Cs).

constraints_rows([], _, []).
constraints_rows([C|Cs], Vars, [R|Rs]) :-
	C = c(Var, Left0, Right),
	linsum_row(Vars, Left0, Left),
	R = row(Var, Left, Right),
	constraints_rows(Cs, Vars, Rs).

constraints_normalize([], [], As, As, S, S).
constraints_normalize([C0|Cs0], [C|Cs], [Art|AsRest], As, S0, S) :-
	constraint_op(C0, Op),
	constraint_left(C0, Left),
	constraint_right(C0, Right),
	constraint_name(C0, Name),
	Con =.. [Op, Left, Right],
	constraint_normalize(Con, Name, C, S0, S1, Art),
	constraints_normalize(Cs0, Cs, AsRest, As, S1, S).

constraint_normalize(As0 =< B0, Name, c(Slack, As1, B0), S0, S, []) :-
	As1 = [1*Slack|As0],
	state_next_var(Slack, S0, S1),
	state_add_name(Name, Slack, S1, S).
constraint_normalize(As0 = B0, Name, c(AID, As1, B0), S0, S, [(-1)*AID]) :-
	As1 = [1*AID|As0],
	state_next_var(AID, S0, S1),
	state_add_name(Name, AID, S1, S).
constraint_normalize(As0 >= B0, Name, c(AID, As1, B0), S0, S, [(-1)*AID]) :-
	state_next_var(Slack, S0, S1),
	state_next_var(AID, S1, S2),
	state_add_name(Name, AID, S2, S),
	As1 = [(-1)*Slack,1*AID|As0].


coeff_one([], []).
coeff_one([L|Ls], [Coeff*Var|Rest]) :-
	( L = A*B ->
		Coeff = A,
		Var = B
	;
		Coeff = 1,
		Var = L
	),
	coeff_one(Ls, Rest).


tableau_optimal(Tableau) :-
	tableau_objective(Tableau, Objective),
	tableau_indicators(Tableau, Indicators),
	Objective = row(_, Left, _),
	all_nonnegative(Left, Indicators).

all_nonnegative([], []).
all_nonnegative([Coeff|As], [I|Is]) :-
	( I =:= 0 ->
		true
	;
		Coeff >= 0
	),
	all_nonnegative(As, Is).

pivot_column(Tableau, PCol) :-
	tableau_objective(Tableau, Objective),
	tableau_indicators(Tableau, Indicators),
	Objective = row(_, Left, _),
	first_negative(Left, Indicators, 0, Index0, Val, RestL, RestI),
	Index1 is Index0 + 1,
	pivot_column(RestL, RestI, Val, Index1, Index0, PCol).

first_negative([L|Ls], [I|Is], Index0, N, Val, RestL, RestI) :-
	Index1 is Index0 + 1,
	( I =:= 0 ->
		first_negative(Ls, Is, Index1, N, Val, RestL, RestI)
	;
		( L < 0 ->
			N = Index0,
			Val = L,
			RestL = Ls,
			RestI = Is
		;
			first_negative(Ls, Is, Index1, N, Val, RestL, RestI)
		)
	).


pivot_column([], _, _, _, N, N).
pivot_column([L|Ls], [I|Is], Coeff0, Index0, N0, N) :-
	( I =:= 0 ->
		Coeff1 = Coeff0,
		N1 = N0
	;
		( L < Coeff0 ->
			Coeff1 = L,
			N1 = Index0
		;
			Coeff1 = Coeff0,
			N1 = N0
		)
	),
	Index1 is Index0 + 1,
	pivot_column(Ls, Is, Coeff1, Index1, N1, N).


pivot_row(Tableau, PCol, PRow) :-
	tableau_rows(Tableau, Rows),
	pivot_row(Rows, PCol, 0, _, 0, 0, PRow).

pivot_row([], _, Found, _, _, Row, Row) :-
	Found =:= 1. % otherwise: unbounded LP
pivot_row([Row|Rows], PCol, Found0, Min0, Index0, PRow0, PRow) :-
	Row = row(_Var, Left, B),
	nth0(PCol, Left, Ae),
	( Ae > 0 ->
		Found1 is 1,
		Bound is B rdiv Ae,
		( Found0 =:= 0 ->
			Min1 = Bound,
			PRow1 = Index0
		;
			( Bound < Min0 ->
				Min1 = Bound,
				PRow1 = Index0
			;
				Min1 = Min0,
				PRow1 = PRow0
			)
		)
	;
		Found1 = Found0,
		Min1 = Min0,
		PRow1 = PRow0
	),
	Index1 is Index0 + 1,
	pivot_row(Rows, PCol, Found1, Min1, Index1, PRow1, PRow).


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
	( PVar == Var ->
		Row = PivotRow
	;
		row_eliminate(Row0, PivotRow, Col, Row)
	),
	gauss_elimination(Rows0, PivotRow, Col, Rows).

row_eliminate(Row0, PivotRow, Col, Row) :-
	PivotRow = row(_, PLeft, PRight),
	Row0 = row(Var, Left, Right),
	nth0(Col, Left, Coeff),
	MCoeff is -Coeff,
	all_times([PRight|PLeft], MCoeff, MultPivotRow),
	all_add([Right|Left], MultPivotRow, NewRow),
	NewRow = [R|Ls],
	Row = row(Var, Ls, R).


all_times([], _, []).
all_times([A|As], T, [AT|ATs]) :-
	AT is A * T,
	all_times(As, T, ATs).

all_add([], [], []).
all_add([A|As], [B|Bs], [C|Cs]) :-
	C is A + B,
	all_add(As, Bs, Cs).


simplex(Tableau0, Tableau) :-
	( tableau_optimal(Tableau0) ->
		Tableau0 = Tableau
	;
		pivot_column(Tableau0, PCol),
		pivot_row(Tableau0, PCol, PRow),
		Tableau0 = tableau(Objective0,Variables,Inds,Matrix0),
		nth0(PRow, Matrix0, Row0),
		Row0 = row(Leaving, Left0, _Right0),
		nth0(PCol, Left0, PivotElement),
		row_divide(Row0, PivotElement, Row1),
		gauss_elimination([Objective0|Matrix0], Row1, PCol, Matrix1),
		Matrix1 = [Objective|Matrix2],
		nth0(PCol, Variables, Entering),
		swap_basic(Matrix2, Leaving, Entering, Matrix),
		simplex(tableau(Objective,Variables,Inds,Matrix), Tableau)
	).

swap_basic([Row0|Rows0], Old, New, Matrix) :-
	Row0 = row(Var, Left, Right),
	( Var == Old ->
		Matrix = [row(New, Left, Right)|Rows0]
	;
		Matrix = [Row0|Rest],
		swap_basic(Rows0, Old, New, Rest)
	).

constraints_variables([]) -->
	[].
constraints_variables([c(_,Left,_)|Cs]) -->
	variables(Left),
	constraints_variables(Cs).

variables([]) -->
	[].
variables([_Coeff*Var|Rest]) -->
	[Var],
	variables(Rest).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% streamlined simplex algorithm for transportation problems
% structures used:
% cost matrix:
%   *) row(Cols), Cols is a list of cells
%   *) cell(Row,Col,Cost,Diff)
% basic variables:
%   *) bv(Row, Col, Cost, Value)


make_rows([], _, []).
make_rows([C|Costs], R, [Row|Rows]) :-
	make_cols(C, R, 0, Cols),
	Row = row(Cols),
	R1 is R + 1,
	make_rows(Costs, R1, Rows).

make_cols([], _, _, []).
make_cols([Cost|Cs], R, C, [cell(R,C,Cost1,_)|Cells]) :-
	Cost1 is rationalize(Cost),
	C1 is C + 1,
	make_cols(Cs, R, C1, Cells).

all_rationalize([], []).
all_rationalize([A|As], [R|Rs]) :-
	R is rationalize(A),
	all_rationalize(As, Rs).

transportation(Supplies, Demands, Costs, Transport) :-
	make_rows(Costs, 0, Rows0),
	all_rationalize(Supplies, Supplies1),
	all_rationalize(Demands, Demands1),
	length(Supplies, NRows),
	length(Demands, NCols),
	vogel_approximation(Supplies1, Demands1, Costs, Basis),
	transportation_iterate(Rows0, NRows, NCols, Basis, Basis1),
	length(Line, NCols),
	maplist(=(0), Line),
	length(Matrix, NRows),
	maplist(=(Line), Matrix),
	basis_transport(Basis1, Matrix, Transport).

basis_transport([], Matrix, Matrix).
basis_transport([bv(NR,NC,_,Value)|Bs], Matrix0, Matrix) :-
	nth0(NR, Matrix0, Row0),
	set_nth0(NC, Row0, Value, Row1),
	set_nth0(NR, Matrix0, Row1, Matrix1),
	basis_transport(Bs, Matrix1, Matrix).

entering_variable(Rows, BBR, BBC, NRows, NCols, Var) :-
	tableau_entering(Rows, BBR, BBC, NRows, NCols, Diff, Var),
	Diff < 0.


   % compute value of the objective function (useful for debugging)

basis_objective([], N, N).
basis_objective([bv(_,_,Cost,Value)|BVs], N0, N) :-
	N1 is N0 + Cost*Value,
	basis_objective(BVs, N1, N).

transportation_iterate(Rows, NRows, NCols, Basis0, Basis) :-
	%basis_objective(Basis0, 0, Obj),
	%format("sum: ~w\n", [Obj]),
	empty_assoc(Empty),
	basis_by_col(Basis0, Empty, BBC),
	basis_by_row(Basis0, Empty, BBR),
	findall(V, entering_variable(Rows, BBR, BBC, NRows, NCols, V), Vs),
	( Vs = [Entering] ->
		Entering = v(ERow, ECol, ECost),
		once(chain_reaction(Entering, BBR, BBC, Donors, Recipients)),
		Donors = [bv(_, _, _, D0)|_],
		min_donors(Donors, D0, Min),
		bvs_add(Recipients, Min, Recipients1),
		bvs_add(Donors, -Min, Donors1),
		append(Recipients1, Donors1, ChangedBasis),
		EnterBV = bv(ERow, ECol, ECost, Min),
		merge_basis(Basis0, ChangedBasis, Basis1),
		memberchk(bv(LeavingRow,LeavingCol,_,0), Donors1),
		delete(Basis1, bv(LeavingRow,LeavingCol,_,0), Basis2),
		transportation_iterate(Rows, NRows, NCols, [EnterBV|Basis2], Basis)
	;
		Basis0 = Basis
	).


merge_basis([], _, []).
merge_basis([BV0|BVs0], Changed, [BV|BVs]) :-
	BV0 = bv(Row, Col, _, _),
	( memberchk(bv(Row, Col, Cost, Value), Changed) ->
		BV = bv(Row, Col, Cost, Value)
	;
		BV = BV0
	),
	merge_basis(BVs0, Changed, BVs).


bvs_add([], _, []).
bvs_add([bv(NR,NC,C,V0)|BVs0], Val, [bv(NR,NC,C,V1)|BVs]) :-
	V1 is V0 + Val,
	bvs_add(BVs0, Val, BVs).


min_donors([], Min, Min).
min_donors([bv(_, _, _, Val)|Ds], Min0, Min) :-
	Min1 is min(Val, Min0),
	min_donors(Ds, Min1, Min).


chain_reaction(v(Row,Col,_), BBR, BBC, Donors, Recipients) :-
	get_assoc(Col, BBC, Cs0),
	select(bv(BRow,Col,Cost,Value), Cs0, Cs1),
	BRow =\= Row,
	Curr = bv(BRow, Col, Cost, Value),
	Donors = [Curr|RestDonors],
	put_assoc(Col, BBC, Cs1, BBC1),
	get_assoc(BRow, BBR, Rs),
	delete(Rs, bv(BRow,Col,_,_), Rs1),
	put_assoc(BRow, BBR, Rs1, BBR1),
	chain_reaction_row(Row, Curr, BBR1, BBC1, RestDonors, Recipients).

chain_reaction_col(TargetRow, bv(_,FCol,_,_), BBR, BBC, Dons, Recs) :-
	get_assoc(FCol, BBC, Cs0),
	select(Curr, Cs0, Cs1),
	Curr = bv(FRow,FCol,_,_),
	put_assoc(FCol, BBC, Cs1, BBC1),
	get_assoc(FRow, BBR, Rs),
	delete(Rs, bv(FRow,FCol,_,_), Rs1),
	put_assoc(FRow, BBR, Rs1, BBR1),
	Dons = [Curr|Rest],
	chain_reaction_row(TargetRow, Curr, BBR1, BBC1, Rest, Recs).

chain_reaction_row(TargetRow, bv(FRow,_,_,_), BBR, BBC, Dons, Recs) :-
	( TargetRow =:= FRow ->
		Dons = [],
		Recs = []
	;
		get_assoc(FRow, BBR, Rs),
		select(Curr, Rs, Rs1),
		put_assoc(FRow, BBR, Rs1, BBR1),
		Curr = bv(_,FCol,_,_),
		get_assoc(FCol, BBC, Cs),
		delete(Cs, bv(FRow,FCol,_,_), Cs1),
		put_assoc(FCol, BBC, Cs1, BBC1),
		Recs = [Curr|Rest],
		chain_reaction_col(TargetRow, Curr, BBR1, BBC1, Dons, Rest)
	).



rows_number_basic(N, N, _, []) :- !.
rows_number_basic(I, N, BBR, NB0) :-
	get_assoc(I, BBR, Cols),
	length(Cols, Num),
	Num1 is -Num,
	NB0 = [Num1-I|Rest],
	I1 is I + 1,
	rows_number_basic(I1, N, BBR, Rest).

tableau_entering(Rows0, BBR, BBC, NRows, NCols, Diff, Var) :-
	length(Uis, NRows),
	length(Vjs, NCols),
	rows_number_basic(0, NRows, BBR, NB),
	keysort(NB, Sorted),
	Sorted = [_Num-NMaxRow|_],
	nth0(NMaxRow, Uis, 0),
	uis_vjs_row([NMaxRow], [], [], [], BBR, BBC, Uis, Vjs, Rows0),
	diffs(Rows0, Uis, Vjs, 0, Diff, _, Var).

basis_by_col([], BBC, BBC).
basis_by_col([bv(BR,BC,Cost,Value)|Bs], BBC0, BBC) :-
	( get_assoc(BC, BBC0, Rows) ->
		put_assoc(BC, BBC0, [bv(BR,BC,Cost,Value)|Rows], BBC1)
	;
		put_assoc(BC, BBC0, [bv(BR,BC,Cost,Value)], BBC1)
	),
	basis_by_col(Bs, BBC1, BBC).

basis_by_row([], BBR, BBR).
basis_by_row([bv(BR,BC,Cost,Value)|Bs], BBR0, BBR) :-
	( get_assoc(BR, BBR0, Cols) ->
		put_assoc(BR, BBR0, [bv(BR,BC,Cost,Value)|Cols], BBR1)
	;
		put_assoc(BR, BBR0, [bv(BR,BC,Cost,Value)], BBR1)
	),
	basis_by_row(Bs, BBR1, BBR).


uis_vjs_row(Rows, RowsVisited, Cols, ColsVisited, BBR, BBC, Uis, Vjs, Costs) :-
	( Rows == [] ->
		( Cols == [] ->
			true
		;
			uis_vjs_col(Rows, RowsVisited, Cols, ColsVisited, BBR, BBC, Uis, Vjs, Costs)
		)
	;
		Rows = [NRow|Todo],
		( memberchk(NRow, RowsVisited) ->
			uis_vjs_row(Todo, RowsVisited, Cols, ColsVisited, BBR, BBC, Uis, Vjs, Costs)
		;
			nth0(NRow, Uis, Ui),
			get_assoc(NRow, BBR, Cs),
			whole_row(Cs, Ui, Vjs, Cols, Cols1),
			sort(Cols1, Cols2), % remove dups
			uis_vjs_col(Todo, [NRow|RowsVisited], Cols2, ColsVisited, BBR, BBC, Uis, Vjs, Costs)
		)
	).

whole_row([], _, _, ColsQueue, ColsQueue).
whole_row([bv(_,VNCol,Cost,_)|Vs], Ui, Vjs, ColsQueue0, ColsQueue) :-
	nth0(VNCol, Vjs, Vj),
	Vj is Cost - Ui,
	whole_row(Vs, Ui, Vjs, [VNCol|ColsQueue0], ColsQueue).


uis_vjs_col(Rows, RowsVisited, Cols, ColsVisited, BBR, BBC, Uis, Vjs, Costs) :-
	( Cols == [] ->
		uis_vjs_row(Rows, RowsVisited, Cols, ColsVisited, BBR, BBC, Uis, Vjs, Costs)
	;
		Cols = [NCol|Todo],
		( memberchk(NCol, ColsVisited) ->
			uis_vjs_col(Rows, RowsVisited, Todo, ColsVisited, BBR, BBC, Uis, Vjs, Costs)
		;
			nth0(NCol, Vjs, Vj),
			get_assoc(NCol, BBC, Rs),
			whole_col(Rs, Vj, Uis, Rows, Rows1),
			sort(Rows1, Rows2), % remove dups
			uis_vjs_row(Rows2, RowsVisited, Todo, [NCol|ColsVisited], BBR, BBC, Uis, Vjs, Costs)
		)
	).

whole_col([], _, _, RowsQueue, RowsQueue).
whole_col([bv(VNRow,_,Cost,_)|Vs], Vj, Uis, RowsQueue0, RowsQueue) :-
	nth0(VNRow, Uis, Ui),
	Ui is Cost - Vj,
	whole_col(Vs, Vj, Uis, [VNRow|RowsQueue0], RowsQueue).


diffs([], _, _, Diff, Diff, Var, Var).
diffs([row(Cols)|Rows], [Ui|Uis], Vjs, Diff0, Diff, Var0, Var) :-
	diffs_(Cols, Ui, Vjs, Diff0, Diff1, Var0, Var1),
	diffs(Rows, Uis, Vjs, Diff1, Diff, Var1, Var).

diffs_([], _, _, Diff, Diff, Var, Var).
diffs_([cell(Row,Col,Cost,CD)|Cols], Ui, [Vj|Vjs], Diff0, Diff, Var0, Var) :-
	CD is Cost - Ui - Vj,
	( CD < Diff0 ->
		Var1 = v(Row,Col,Cost),
		Diff1 = CD
	;
		Var1 = Var0,
		Diff1 = Diff0
	),
	diffs_(Cols, Ui, Vjs, Diff1, Diff, Var1, Var).



   % Vogel's approximation method used for finding an initial solution of the
   % transportation problem


   % The following predicates compute the difference for each row/column
   % (difference = arithmetic difference between smallest and next-to-smallest
   %               unit cost still remaining in a row/column)
   % as we then want to select the row/col with the largest difference, we
   % actually compute the negative difference to sort in reverse order.
   % We also keep track of the element with least unit cost for later.

rows_with_diffs([], []).
rows_with_diffs([NR-Costs|NRs], [Diff-row(NR,NC,Costs)|Rest]) :-
	( Costs = [First-NC,Second-_|_] ->
		Diff is First - Second
	;
		Diff = 0
	),
	rows_with_diffs(NRs, Rest).

   % the same for column-differences:

cols_with_diffs([], []).
cols_with_diffs([NC-Costs|NCs], [Diff-col(NC,NR,Costs)|Rest]) :-
	( Costs = [First-NR,Second-_|_] ->
		Diff is First - Second
	;
		Diff = 0
	),
	cols_with_diffs(NCs, Rest).

recompute_diffs([], []).
recompute_diffs([_-E|Es], [R|Rs]) :-
	recompute_diff(E, R),
	recompute_diffs(Es, Rs).

recompute_diff(col(NC,_,Costs), Diff-col(NC,NR,Costs)) :-
	( Costs = [First-NR, Second-_|_] ->
		Diff is First - Second
	;
		Diff = 0
	).
recompute_diff(row(NR,_,Costs), Diff-row(NR,NC,Costs)) :-
	( Costs = [First-NC, Second-_|_] ->
		Diff is First - Second
	;
		Diff = 0
	).


   % entry-point for Vogel's approximation method

vogel_approximation(Supplies, Demands, Costs, Basis) :-
	number_list(Costs, 0, NumberedCosts),
	transpose(Costs, TCosts),
	number_list(TCosts, 0, NumberedTCosts),
	rows_with_diffs(NumberedCosts, Rows),
	cols_with_diffs(NumberedTCosts, Cols),
	keysort(Rows, Rows1),
	keysort(Cols, Cols1),
	vogel_iterate(Rows1, Cols1, Supplies, Demands, Basis, []).

   % Each row of the cost matrix is represented as Diff-row(NR,N,Costs),
   % with NR being the row number (in the original matrix), and costs are of
   % the form Cost-NC, NC being the column in the original matrix. N is
   % the column with minimal cost.
   % Columns are stored analogously. This way, it is easy to remove rows and
   % columns and still maintain information where they originally were.

number_list([], _, []).
number_list([E|Es], N0, [N0-E2|Rest]) :-
	number_list_(E, 0, E1),
	keysort(E1, E2),
	N1 is N0 + 1,
	number_list(Es, N1, Rest).

number_list_([], _, []).
number_list_([E|Es], N0, [E-N0|Rest]) :-
	N1 is N0 + 1,
	number_list_(Es, N1, Rest).


transpose(Matrix, Transposed) :-
        Matrix = [Row|_],
        transpose(Row, Matrix, Transposed).

transpose([], _, []).
transpose([_|Rest], Matrix, [Row|Rows]) :-
        firsts(Matrix, NewMatrix, Row),
        transpose(Rest, NewMatrix, Rows).

firsts([], [], []).
firsts([[F|Rs]|Rest], [Rs|Ts], [F|Fs]) :-
        firsts(Rest, Ts, Fs).


remaining_row([], _, _, _, Basis, Basis).
remaining_row([Cost-NC|Rest], NR, Supply, Demands, Basis0, Basis) :-
	nth0(NC, Demands, DemandBound),
	Amount is min(DemandBound, Supply),
	Basis0 = [bv(NR, NC, Cost, Amount)|Basis1],
	Supply1 is Supply - Amount,
	remaining_row(Rest, NR, Supply1, Demands, Basis1, Basis).

remaining_col([], _, _, _, Basis, Basis).
remaining_col([Cost-NR|Rest], NC, Supplies, Demand, Basis0, Basis) :-
	nth0(NR, Supplies, SupplyBound),
	Amount is min(SupplyBound, Demand),
	Basis0 = [bv(NR, NC, Cost, Amount)|Basis1],
	Demand1 is Demand - Amount,
	remaining_col(Rest, NC, Supplies, Demand1, Basis1, Basis).

vogel_iterate(Rows, Cols, Supplies, Demands, Basis0, Basis) :-
	( Cols = [SingleCol] ->
		SingleCol = _-col(NC,_,Costs),
		nth0(NC, Demands, Demand),
		remaining_col(Costs, NC, Supplies, Demand, Basis0, Basis)
	; Rows = [SingleRow] ->
		SingleRow = _-row(NR,_,Costs),
		nth0(NR, Supplies, Supply),
		remaining_row(Costs, NR, Supply, Demands, Basis0, Basis)
	;
		Rows = [RDiff-MinRow|_],
		Cols = [CDiff-MinCol|_],
		( RDiff < CDiff ->
			% choose the row (with smallest negative difference)
			MinRow = row(BasicRow, BasicCol, _)
		;
			% choose the column
			MinCol = col(BasicCol, BasicRow, _)
		),
		nth0(BasicCol, Demands, DemandsBound),
		nth0(BasicRow, Supplies, SupplyBound),
		Amount is min(DemandsBound, SupplyBound),
		DemandsBound1 is DemandsBound - Amount,
		SupplyBound1 is SupplyBound - Amount,
		set_nth0(BasicCol, Demands, DemandsBound1, Demands1),
		set_nth0(BasicRow, Supplies, SupplyBound1, Supplies1),
		memberchk(_-row(BasicRow,_,Cs), Rows),
		memberchk(Cost-BasicCol, Cs),
		Basis0 = [bv(BasicRow, BasicCol, Cost, Amount)|Basis1],
		( DemandsBound1 =:= 0 ->
			delete(Cols, _-col(BasicCol,_,_), Cols3),
			delete_from_all(Rows, BasicCol, Rows1),
			recompute_diffs(Rows1, Rows2),
			keysort(Rows2, Rows3)
		;
			delete(Rows, _-row(BasicRow,_,_), Rows3),
			delete_from_all(Cols, BasicRow, Cols1),
			recompute_diffs(Cols1, Cols2),
			keysort(Cols2, Cols3)
		),
		vogel_iterate(Rows3, Cols3, Supplies1, Demands1, Basis1, Basis)
	).

delete_from_all([], _, []).
delete_from_all([Diff-E0|As0], Del, [Diff-E|As]) :-
	E0 =.. [Func,N1,N2,Costs],
	delete(Costs, _-Del, Costs1),
	E =.. [Func,N1,N2,Costs1],
	delete_from_all(As0, Del, As).


set_nth0(0, [_|Ls], Element, [Element|Ls]) :- !.
set_nth0(Curr, [L|Ls], Element, [L|Ss]) :-
	Curr1 is Curr - 1,
	set_nth0(Curr1, Ls, Element, Ss).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Assignment problem - for now, reduce to transportation problem

assignment(Costs, Assignment) :-
	length(Costs, LC),
	length(Supply, LC),
	all_one(Supply),
	transportation(Supply, Supply, Costs, Assignment).

