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
		gen_state/1,
		maximize/3,
		minimize/3,
		objective/2,
		shadow_price/3,
		transportation/4,
		variable_value/3
	]).

:- use_module(library(bounds)).

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
	solved_tableau(State, Tableau),
	tableau_rows(Tableau, Rows),
	( find_row(Variable, Rows, Row) ->
		Row = row(_, _, Value)
	;
		Value = 0
	).

all_vars_zero([], _).
all_vars_zero([_Coeff*Var|Vars], State) :-
	variable_value(State, Var, 0),
	all_vars_zero(Vars, State).


list_first(Ls, F, Index) :-
	once(nth0(Index, Ls, F)).

shadow_price(State, Name, Value) :-
	solved_tableau(State, Tableau),
	tableau_objective(Tableau, Objective),
	Objective = row(_, Left, _),
	tableau_variables(Tableau, Variables),
	solved_names(State, Names),
	memberchk([Name,Var], Names),
	list_first(Variables, Var, Nth0),
	nth0(Nth0, Left, Value).


objective(State, Obj) :-
	solved_tableau(State, Tableau),
	tableau_objective(Tableau, Objective),
	Objective = row(_, _, Obj).

   % interface functions that access tableau components

tableau_objective(tableau(Obj, _, _, _), Obj).

tableau_rows(tableau(_, _, _, Rows), Rows).

tableau_indicators(tableau(_, _, Inds, _), Inds).

tableau_variables(tableau(_, Vars, _, _), Vars).


   % interface functions that access and modify state components

   % state is a structure of the form
   %     state(Num, Names, Cs, As, Is)
   % Num: used to obtain new unique names for slack variables in a side-effect
   %      free way (increased by one and threaded through)
   % Names: list of [Name,Var], correspondence between constraint-names and
   %      names of slack/artificial variables to obtain shadow prices later
   % Cs: list of normalized constraints
   % As: list of artificial variables
   % Is: list of integer variables

gen_state(state(0,[],[],[],[])).

state_add_constraint(C, state(VID,Ns,Cs,As,Is), state(VID,Ns,[C|Cs],As,Is)).

state_add_artifical(A, state(VID,Ns,Cs,As,Is), state(VID,Ns,Cs,[A|As],Is)).

state_add_name(Name, Var, state(VID,Ns,Cs,As,Is), state(VID,[[Name,Var]|Ns],Cs,As,Is)).

state_add_integral(Var, state(VID,Ns,Cs,As,Is), state(VID,Ns,Cs,As,[Var|Is])).

state_constraints(state(_, _, Cs, _, _), Cs).

state_artificials(state(_, _, _, As, _), As).

state_names(state(_,Names,_,_,_), Names).

state_integrals(state(_,_,_,_,Is), Is).

state_set_integrals(state(VID,Ns,Cs,As,_), Is, state(VID,Ns,Cs,As,Is)).


state_next_var(VarID0, S0, S) :-
	S0 = state(VarID0,Names,Cs,As,Is),
	VarID1 is VarID0 + 1,
	S = state(VarID1,Names,Cs,As,Is).

solved_tableau(solved(Tableau, _, _), Tableau).

solved_names(solved(_, Names,_), Names).

solved_integrals(solved(_,_,Is), Is).


constraint(C, S0, S) :-
	( C = integral(Var) ->
		state_add_integral(Var, S0, S)
	;
		constraint(0, C, S0, S)
	).


constraint(Name, C, S0, S) :-
	( C = integral(Var) ->
		state_add_integral(Var, S0, S)
	;
		constraint_normalize(Name, C, CN, S0, S1, Artificial),
		state_add_constraint(CN, S1, S2),
		state_add_artifical(Artificial, S2, S)
	).

branching_variable(State, Variable) :-
	solved_integrals(State, Integrals),
	member(Variable, Integrals),
	variable_value(State, Variable, Value),
	\+ integer(Value).


worth_investigating(ZStar0, _) :-
	var(ZStar0).
worth_investigating(ZStar0, Z) :-
	nonvar(ZStar0),
	Z > ZStar0.

% branch & bound could benefit greatly from reoptimization techniques!

branch_and_bound(Objective, Solved, ZStar0, ZStar, S0, S, Found) :-
	objective(Solved, Z),
	( worth_investigating(ZStar0, Z) ->
		( branching_variable(Solved, BrVar) ->
			variable_value(Solved, BrVar, Value),
			Value1 is floor(Value),
			Value2 is Value1 + 1,
			constraint([BrVar] =< Value1, S0, SubProb1),
			constraint([BrVar] >= Value2, S0, SubProb2),
			( maximize_(Objective, SubProb1, SubSolved1) ->
				Sub1Feasible = 1,
				objective(SubSolved1, Obj1)
			;
				Sub1Feasible = 0
			),
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
				branch_and_bound(Objective, FirstSolved, ZStar0, ZStar1, First, Solved1, Found1),
				branch_and_bound(Objective, SecondSolved, ZStar1, ZStar2, Second, Solved2, Found2)
			; Sub1Feasible =:= 1 ->
				branch_and_bound(Objective, SubSolved1, ZStar0, ZStar1, SubProb1, Solved1, Found1),
				Found2 = 0
			; Sub2Feasible =:= 1 ->
				Found1 = 0,
				branch_and_bound(Objective, SubSolved2, ZStar0, ZStar2, SubProb2, Solved2, Found2)
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
	maximize_mip(Z1, S0, S).

maximize_mip(Z, S0, S) :-
	maximize_(Z, S0, Solved),
	state_integrals(S0, Is),
	( Is == [] ->
		S = Solved
	;
		% order of integrality constraints play an important role
		% in branch and bound - reverse order so that branching is
		% done in the same order as the user stated them
		reverse(Is, Is1),
		state_set_integrals(S0, Is1, S1),
		branch_and_bound(Z, Solved, _, _, S1, S, 1)
	).


minimize(Z0, S0, S) :-
	coeff_one(Z0, Z1),
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
	S = solved(tableau(Obj1, Vars, Inds, Rows), Names, Is).

% solve a (relaxed) LP in standard form

maximize_(Z, S0, S) :-
	state_constraints(S0, Cs),
	state_artificials(S0, As0),
	flatten(As0, As1),
	sort(As1, As2),
	( As2 == [] ->
		make_tableau(Z, Cs, Tableau0),
		simplex(Tableau0, Tableau),
		state_names(S0, Names),
		state_integrals(S0, Is),
		S = solved(Tableau, Names, Is)
	;
		two_phase_simplex(Z, As2, S0, S)
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


two_phase_simplex(Z, As, S0, S) :-
        % phase 1: minimize sum of articifial variables
	state_constraints(S0, Cs),
	make_tableau(As, Cs, Tableau0),
	Tableau0 = tableau(Obj0, Variables, Inds, Rows),
	proper_form(As, Variables, Rows, Obj0, Obj),
	Tableau1 = tableau(Obj, Variables, Inds, Rows),
	simplex(Tableau1, Tableau2),
	all_vars_zero(As, solved(Tableau2, _, _)),
        % phase 2: ignore artificial variables and solve actual LP
	tableau_rows(Tableau2, Rows2),
	list_nths(As, Variables, Nths0),
	nths_to_zero(Nths0, Inds, Inds1),
	linsum_row(Variables, Z, Objective),
	all_times(Objective, (-1), Objective1),
	ObjRow0 = row(z, Objective1, 0),
	proper_form(Z, Variables, Rows2, ObjRow0, ObjRow),
	Tableau3 = tableau(ObjRow, Variables, Inds1, Rows2),
	simplex(Tableau3, Tableau),
	state_names(S0, Names),
	state_integrals(S0, Is),
	S = solved(Tableau,Names, Is).

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

constraint_normalize(Name, C0, C, S0, S, Artificial) :-
	C0 =.. [Op,Left0,Right0],
	coeff_one(Left0, Left),
	Right0 >= 0,
	Right is rationalize(Right0),
	C1 =.. [Op,Left,Right],
	constraint_normalize_(Name, C1, C, S0, S, Artificial).

constraint_normalize_(Name, As0 =< B0, c(Slack, As1, B0), S0, S, []) :-
	As1 = [1*Slack|As0],
	state_next_var(Slack, S0, S1),
	state_add_name(Name, Slack, S1, S).
constraint_normalize_(Name, As0 = B0, c(AID, As1, B0), S0, S, [(-1)*AID]) :-
	As1 = [1*AID|As0],
	state_next_var(AID, S0, S1),
	state_add_name(Name, AID, S1, S).
constraint_normalize_(Name, As0 >= B0, c(AID, As1, B0), S0, S, [(-1)*AID]) :-
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
	Left = [Coeff0|_],
	pivot_column(Left, Indicators, Coeff0, 0, 0, PCol).

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
   %   *) cell(Cost, Value, Diff)
   % basic variables:
   %   *) bv(Row, Col, Value)


cell_cost(cell(Cost, _, _), Cost).

cell_value(cell(_, Value, _), Value).

cell_diff(cell(_, _, Diff), Diff).

make_rows([], []).
make_rows([C|Costs], [Row|Rows]) :-
	make_cols(C, Cols),
	Row = row(Cols),
	make_rows(Costs, Rows).

make_cols([], []).
make_cols([Cost|Cs], [cell(Cost1, 0, _)|Cells]) :-
	Cost1 is rationalize(Cost),
	make_cols(Cs, Cells).

all_rationalize([], []).
all_rationalize([A|As], [R|Rs]) :-
	R is rationalize(A),
	all_rationalize(As, Rs).

transportation(Supplies, Demands, Costs, Transport) :-
	make_rows(Costs, Rows0),
	all_rationalize(Supplies, Supplies1),
	all_rationalize(Demands, Demands1),
	length(Supplies, NRows),
	length(Demands, NCols),
	vogel_approximation(Supplies1, Demands1, Costs, Basis),
	update_tableau(Basis, Rows0, Rows1),
	transportation_iterate(Rows1, Basis, NRows, NCols, Rows2),
	rows_transport(Rows2, Transport).

rows_transport([], []).
rows_transport([R|Rs], [T|Ts]) :-
	R = row(Cols),
	cols_transport(Cols, T),
	rows_transport(Rs, Ts).

cols_transport([], []).
cols_transport([Col|Cols], [T|Ts]) :-
	cell_value(Col, T),
	cols_transport(Cols, Ts).


entering_variable(Rows, Var) :-
	entering_variable(Rows, 0, 0, _, Var).

entering_variable([], _, Found, Var, Var) :-
	Found =:= 1.
entering_variable([Row|Rs], CurrRow, Found0, Var0, Var) :-
	Row = row(Cols),
	entering_variable_(Cols, CurrRow, 0, Found0, Found1, Var0, Var1),
	CurrRow1 is CurrRow + 1,
	entering_variable(Rs, CurrRow1, Found1, Var1, Var).

entering_variable_([], _, _, Found, Found, Var, Var).
entering_variable_([Col|Cols], CurrRow, CurrCol, Found0, Found, Var0, Var) :-
	Var0 = v(_, _, VarDiff),
	cell_diff(Col, Diff),
	( Diff < 0 ->
		Found1 = 1,
		( Found0 =:= 1 ->
			( Diff < VarDiff ->
				Var1 = v(CurrRow, CurrCol, Diff)
			;
				Var1 = Var0
			)
		;
			Var1 = v(CurrRow, CurrCol, Diff)
		)
	;
		Var1 = Var0,
		Found1 = Found0
	),
	CurrCol1 is CurrCol + 1,
	entering_variable_(Cols, CurrRow, CurrCol1, Found1, Found, Var1, Var).

tableau_forget_constraints([], []).
tableau_forget_constraints([Row0|Rows0], [Row|Rows]) :-
	Row0 = row(Cols0),
	cols_forget_constraints(Cols0, Cols),
	Row = row(Cols),
	tableau_forget_constraints(Rows0, Rows).

cols_forget_constraints([], []).
cols_forget_constraints([Col0|Cols0], [Col|Cols]) :-
	Col0 = cell(Cost, Value, _),
	Col = cell(Cost, Value, _),
	cols_forget_constraints(Cols0, Cols).


% compute value of the objective function (useful for debugging)

rows_sum([], _, N, N).
rows_sum([bv(NRow,NCol,_)|BVs], Rows, N0, N) :-
	nth0(NRow, Rows, Row),
	Row = row(Cols),
	nth0(NCol, Cols, Col),
	cell_cost(Col, Cost),
	cell_value(Col, Value),
	N1 is N0 + Cost*Value,
	rows_sum(BVs, Rows, N1, N).

transportation_iterate(Rows0, Basis0, NRows, NCols, Rows) :-
	%rows_sum(Basis0, Rows0, 0, Obj),
	%format("sum: ~w\n", [Obj]),
	tableau_with_diffs(Rows0, Basis0, NRows, NCols),
	( entering_variable(Rows0, Entering) ->
		Entering = v(ERow, ECol, _),
		once(chain_reaction(Entering, Basis0, Donors, Recipients)),
		Donors = [bv(_, _, D0)|_],
		min_donors(Donors, D0, Min),
		bvs_add(Recipients, Min, Recipients1),
		NegMin is -Min,
		bvs_add(Donors, NegMin, Donors1),
		append(Recipients1, Donors1, ChangedBasis),
		EnterBV = bv(ERow, ECol, Min),
		update_tableau([EnterBV|ChangedBasis], Rows0, Rows1),
		merge_basis(Basis0, ChangedBasis, Basis1),
		memberchk(bv(LeavingRow,LeavingCol,0), Donors1),
		delete(Basis1, bv(LeavingRow,LeavingCol,0), Basis2),
		tableau_forget_constraints(Rows1, Rows2),
		transportation_iterate(Rows2, [EnterBV|Basis2], NRows, NCols, Rows)
	;
		Rows0 = Rows
	).

update_tableau([], Rows, Rows).
update_tableau([bv(NRow,NCol,Value)|BVs], Rows0, Rows) :-
	nth0(NRow, Rows0, row(Cols0)),
	nth0(NCol, Cols0, cell(Cost, _, _)),
	set_nth0(NCol, Cols0, cell(Cost, Value, _), Cols1),
	set_nth0(NRow, Rows0, row(Cols1), Rows1),
	update_tableau(BVs, Rows1, Rows).


merge_basis([], _, []).
merge_basis([BV0|BVs0], Changed, [BV|BVs]) :-
	BV0 = bv(Row, Col, _),
	( member(bv(Row, Col, Value), Changed) ->
		BV = bv(Row, Col, Value)
	;
		BV0 = BV
	),
	merge_basis(BVs0, Changed, BVs).


bvs_add([], _, []).
bvs_add([BV0|BVs0], Val, [BV|BVs]) :-
	BV0 = bv(Row, Col, Value),
	Value1 is Value + Val,
	BV = bv(Row, Col, Value1),
	bvs_add(BVs0, Val, BVs).


min_donors([], Min, Min).
min_donors([bv(_, _, Val)|Ds], Min0, Min) :-
	Min1 is min(Val, Min0),
	min_donors(Ds, Min1, Min).

member_rest(E, [E|Rest], Rest).
member_rest(E, [L|Ls], [L|Rest]) :-
	member_rest(E, Ls, Rest).


chain_reaction(From, Basis, Donors, Recipients) :-
	From = v(Row, Col, _),
	member_rest(bv(BRow, Col, Value), Basis, Basis1),
	BRow =\= Row,
	Curr = bv(BRow, Col, Value),
	Donors = [Curr|RestDonors],
	chain_reaction_row(Row, Curr, Basis1, RestDonors, [], Recipients, []).

chain_reaction_col(TargetRow, From, Basis, Dons0, Dons, Recs0, Recs) :-
	From = bv(_, FCol, _),
	member_rest(bv(BRow, FCol, Value), Basis, Basis1),
	Curr = bv(BRow, FCol, Value),
	Dons0 = [Curr|Rest],
	chain_reaction_row(TargetRow, Curr, Basis1, Rest, Dons, Recs0, Recs).

chain_reaction_row(TargetRow, From, Basis, Dons0, Dons, Recs0, Recs) :-
	From = bv(FRow, _, _),
	( TargetRow =:= FRow ->
		Dons0 = Dons,
		Recs0 = Recs
	;
		member_rest(bv(FRow, BCol, Value), Basis, Basis1),
		Curr = bv(FRow, BCol, Value),
		Recs0 = [Curr|Rest],
		chain_reaction_col(TargetRow, Curr, Basis1, Dons0, Dons, Rest, Recs)
	).



rows_number_basic(N, N, _, NB, NB) :- !.
rows_number_basic(I, N, Basis, NB0, NB) :-
	num_basic_in_row(Basis, I, 0, Num),
	Num1 is -Num,
	NB0 = [Num1-I|Rest],
	I1 is I + 1,
	rows_number_basic(I1, N, Basis, Rest, NB).

num_basic_in_row([], _, N, N).
num_basic_in_row([bv(NR, _, _)|Bs], NRow, N0, N) :-
	( NR =:= NRow ->
		N1 is N0 + 1
	;
		N1 = N0
	),
	num_basic_in_row(Bs, NRow, N1, N).


tableau_with_diffs(Rows0, Basis, NRows, NCols) :-
	length(Uis, NRows),
	length(Vjs, NCols),
	rows_number_basic(0, NRows, Basis, NB, []),
	keysort(NB, Sorted),
	Sorted = [_Num-NMaxRow|_],
	nth0(NMaxRow, Uis, 0),
	% make it a bit easier for bounds.pl: constrain first the row
	% where much propagation is possible, only then the rest,
	% in the hope to avoid too many unconstrained variables
	findall(BV, (BV = bv(NMaxRow, _, _),member(BV, Basis)), BVs),
	constrain_basis(BVs, Uis, Vjs, Rows0),
	constrain_basis(Basis, Uis, Vjs, Rows0),
	constrain_diffs(Rows0, Uis, Vjs).

constrain_diffs([], _, _).
constrain_diffs([Row|Rows], [Ui|Uis], Vjs) :-
	Row = row(Cols),
	constrain_cols(Cols, Ui, Vjs),
	constrain_diffs(Rows, Uis, Vjs).

constrain_cols([], _, _).
constrain_cols([Col|Cols], Ui, [Vj|Vjs]) :-
	cell_diff(Col, Diff),
	cell_cost(Col, Cost),
	Diff #= Cost - Ui - Vj,
	constrain_cols(Cols, Ui, Vjs).

constrain_basis([], _, _, _).
constrain_basis([Var|Vs], Uis, Vjs, Rows) :-
	Var = bv(NRow, NCol, _),
	nth0(NRow, Rows, Row),
	Row = row(Cols),
	nth0(NCol, Cols, Cell),
	cell_diff(Cell, 0),
	cell_cost(Cell, Cost),
	nth0(NRow, Uis, Ui),
	nth0(NCol, Vjs, Vj),
	Vj #= Cost - Ui,
	constrain_basis(Vs, Uis, Vjs, Rows).


% Vogel's approximation method used for finding an initial solution of the
% transportation problem


% The following predicates compute the difference for each row/column
% (difference = arithmetic difference between smallest and next-to-smallest
%               unit cost still remaining in a row/column)
% as we then want to select the row/col with the largest difference, we
% actually compute the negative difference to sort in reverse order.
% We also keep track of the element with least unit cost for later.

rows_with_diffs([], []).
rows_with_diffs([NR-Costs|NRs], [Diff-(NR,NC)|Rest]) :-
	keysort(Costs, Costs1),
	Costs1 = [First-NC,Second-_|_],
	Diff is First - Second,
	rows_with_diffs(NRs, Rest).

% the same for column-differences:

cols_with_diffs([], []).
cols_with_diffs([NC-Costs|NCs], [Diff-(NR,NC)|Rest]) :-
	keysort(Costs, Costs1),
	Costs1 = [First-NR,Second-_|_],
	Diff is First - Second,
	cols_with_diffs(NCs, Rest).


% entry-point for Vogel's approximation method

vogel_approximation(Supplies, Demands, Costs, Basis) :-
	number_list(Costs, 0, NumberedCosts),
	transpose(Costs, TCosts),
	number_list(TCosts, 0, NumberedTCosts),
	vogel_iterate(NumberedCosts, NumberedTCosts, Supplies, Demands, Basis, []).

% each row of the cost matrix is represented as NR-Costs,
% with NR being the row number (in the original matrix), and costs are of
% the form Cost-NC, NC being the column in the original matrix.
% columns are stored analogously. this way, it is easy to remove rows and
% columns and still maintain information where they originally were.

number_list([], _, []).
number_list([E|Es], N0, [N0-E1|Rest]) :-
	number_list_(E, 0, E1),
	N1 is N0 + 1,
	number_list(Es, N1, Rest).

number_list_([], _, []).
number_list_([E|Es], N0, [E-N0|Rest]) :-
	N1 is N0 + 1,
	number_list_(Es, N1, Rest).


transpose(Matrix, TransPosed) :-
        Matrix = [Row|_],
        transpose(Row,Matrix, TransPosed).

transpose([], _, []).
transpose([_|R], Matrix, [Row|RestTransPosed]) :-
        takefirst(Matrix, NewMatrix, Row),
        transpose(R, NewMatrix, RestTransPosed).

takefirst([], [], []).
takefirst([[X|R]|S], [R|T], [X|Q]) :-
        takefirst(S, T, Q).


remaining_row([], _, _, _, Basis, Basis).
remaining_row([_-NC|Rest], NR, Supply, Demands, Basis0, Basis) :-
	nth0(NC, Demands, DemandBound),
	Amount is min(DemandBound, Supply),
	Basis0 = [bv(NR, NC, Amount)|Basis1],
	Supply1 is Supply - Amount,
	remaining_row(Rest, NR, Supply1, Demands, Basis1, Basis).

remaining_col([], _, _, _, Basis, Basis).
remaining_col([_-NR|Rest], NC, Supplies, Demand, Basis0, Basis) :-
	nth0(NR, Supplies, SupplyBound),
	Amount is min(SupplyBound, Demand),
	Basis0 = [bv(NR, NC, Amount)|Basis1],
	Demand1 is Demand - Amount,
	remaining_col(Rest, NC, Supplies, Demand1, Basis1, Basis).

vogel_iterate(Rows, Cols, Supplies, Demands, Basis0, Basis) :-
	( Cols = [SingleCol] ->
		SingleCol = NC-Costs,
		nth0(NC, Demands, Demand),
		keysort(Costs, Costs1),
		remaining_col(Costs1, NC, Supplies, Demand, Basis0, Basis)
	; Rows = [SingleRow] ->
		SingleRow = NR-Costs,
		nth0(NR, Supplies, Supply),
		keysort(Costs, Costs1),
		remaining_row(Costs1, NR, Supply, Demands, Basis0, Basis)
	;
		rows_with_diffs(Rows, RowsDiff),
		cols_with_diffs(Cols, ColsDiff),
		keysort(RowsDiff, RowsDiff1),
		keysort(ColsDiff, ColsDiff1),
		RowsDiff1 = [RDiff-MinRow|_],
		ColsDiff1 = [CDiff-MinCol|_],
		( RDiff < CDiff ->
			% choose the row (with smallest negative difference)
			MinRow = (BasicRow, BasicCol)
		;
			% choose the column
			MinCol = (BasicRow, BasicCol)
		),
		nth0(BasicCol, Demands, DemandsBound),
		nth0(BasicRow, Supplies, SupplyBound),
		Amount is min(DemandsBound, SupplyBound),
		DemandsBound1 is DemandsBound - Amount,
		SupplyBound1 is SupplyBound - Amount,
		set_nth0(BasicCol, Demands, DemandsBound1, Demands1),
		set_nth0(BasicRow, Supplies, SupplyBound1, Supplies1),
		Basis0 = [bv(BasicRow, BasicCol, Amount)|Basis1],
		( DemandsBound1 =:= 0 ->
			delete(Cols, BasicCol-_, Cols1),
			delete_from_all(Rows, BasicCol, Rows1)
		;
			delete(Rows, BasicRow-_, Rows1),
			delete_from_all(Cols, BasicRow, Cols1)
		),
		vogel_iterate(Rows1, Cols1, Supplies1, Demands1, Basis1, Basis)
	).

delete_from_all([], _, []).
delete_from_all([Num-A0|As0], Del, [Num-A|As]) :-
	delete(A0, _-Del, A),
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

