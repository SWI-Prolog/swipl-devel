/*  $Id$

    Part of CLP(R) (Constraint Logic Programming over Reals)

    Author:        Leslie De Koninck
    E-mail:        Leslie.DeKoninck@cs.kuleuven.be
    WWW:           http://www.swi-prolog.org
		   http://www.ai.univie.ac.at/cgi-bin/tr-online?number+95-09
    Copyright (C): 2004, K.U. Leuven and
		   1992-1995, Austrian Research Institute for
		              Artificial Intelligence (OFAI),
			      Vienna, Austria

    This software is part of Leslie De Koninck's master thesis, supervised
    by Bart Demoen and daily advisor Tom Schrijvers.  It is based on CLP(Q,R)
    by Christian Holzbaur for SICStus Prolog and distributed under the
    license details below with permission from all mentioned authors.

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


:- module(fourmotz_r,
	[
	    fm_elim/3
	]).
:- use_module(bv_r,
	[
	    allvars/2,
	    basis_add/2,
	    detach_bounds/1,
	    pivot/5,	
	    var_with_def_intern/4
	]).
:- use_module('../clpqr/class',
	[
	    class_allvars/2
	]).
:- use_module('../clpqr/project',
	[
	    drop_dep/1,
	    drop_dep_one/1,
	    make_target_indep/2
	]).
:- use_module('../clpqr/redund',
	[
	    redundancy_vars/1
	]).
:- use_module(store_r,
	[
	    add_linear_11/3,
	    add_linear_f1/4,
	    indep/2,
	    nf_coeff_of/3,
	    normalize_scalar/2
	]).
		


fm_elim(Vs,Target,Pivots) :-
	prefilter(Vs,Vsf),
	fm_elim_int(Vsf,Target,Pivots).

% prefilter(Vars,Res)
%
% filters out target variables and variables that do not occur in bounded linear equations. 
% Stores that the variables in Res are to be kept independent.

prefilter([],[]).
prefilter([V|Vs],Res) :-
	(   get_attr(V,itf,Att),
	    arg(9,Att,n),
	    occurs(V) % V is a nontarget variable that occurs in a bounded linear equation
	->  Res = [V|Tail],
	    setarg(10,Att,keep_indep),
	    prefilter(Vs,Tail)
	;   prefilter(Vs,Res)
	).

%
% the target variables are marked with an attribute, and we get a list
% of them as an argument too
%
fm_elim_int([],_,Pivots) :-	% done
	unkeep(Pivots).
fm_elim_int(Vs,Target,Pivots) :-
	Vs = [_|_],
	(   best(Vs,Best,Rest)
	->  occurences(Best,Occ),
	    elim_min(Best,Occ,Target,Pivots,NewPivots)
	;   % give up
	    NewPivots = Pivots,
	    Rest = []
	),
	fm_elim_int(Rest,Target,NewPivots).

% best(Vs,Best,Rest)
%
% Finds the variable with the best result (lowest Delta) in fm_cp_filter
% and returns the other variables in Rest.

best(Vs,Best,Rest) :-
	findall(Delta-N,fm_cp_filter(Vs,Delta,N),Deltas),
	keysort(Deltas,[_-N|_]),
	select_nth(Vs,N,Best,Rest).

% fm_cp_filter(Vs,Delta,N)
%
% For an indepenent variable V in Vs, which is the N'th element in Vs,
% find how many inequalities are generated when this variable is eliminated.
% Note that target variables and variables that only occur in unbounded equations
% should have been removed from Vs via prefilter/2

fm_cp_filter(Vs,Delta,N) :-
	length(Vs,Len),	% Len = number of variables in Vs
	mem(Vs,X,Vst),	% Selects a variable X in Vs, Vst is the list of elements after X in Vs
	get_attr(X,itf,Att),
	arg(4,Att,lin(Lin)),
	arg(5,Att,order(OrdX)),
	arg(9,Att,n),	% no target variable
	indep(Lin,OrdX),	% X is an independent variable
	occurences(X,Occ),	
	Occ = [_|_],
	cp_card(Occ,0,Lnew),
	length(Occ,Locc),
	Delta is Lnew-Locc,
	length(Vst,Vstl),
	N is Len-Vstl.	% X is the Nth element in Vs

% mem(Xs,X,XsT)
%
% If X is a member of Xs, XsT is the list of elements after X in Xs.

mem([X|Xs],X,Xs).
mem([_|Ys],X,Xs) :- mem(Ys,X,Xs).

% select_nth(List,N,Nth,Others)
%
% Selects the N th element of List, stores it in Nth and returns the rest of the list in Others.

select_nth(List,N,Nth,Others) :-
	select_nth(List,1,N,Nth,Others).

select_nth([X|Xs],N,N,X,Xs) :- !.
select_nth([Y|Ys],M,N,X,[Y|Xs]) :-
	M1 is M+1,
	select_nth(Ys,M1,N,X,Xs).

%
% fm_detach + reverse_pivot introduce indep t_none, which
% invalidates the invariants
%
elim_min(V,Occ,Target,Pivots,NewPivots) :-
	crossproduct(Occ,New,[]),
	activate_crossproduct(New),
	reverse_pivot(Pivots),
	fm_detach(Occ),
	allvars(V,All),
	redundancy_vars(All),			% only for New \== []
	make_target_indep(Target,NewPivots),
	drop_dep(All).

%
% restore NF by reverse pivoting
%
reverse_pivot([]).
reverse_pivot([I:D|Ps]) :-
	get_attr(D,itf,AttD),
	arg(2,AttD,type(Dt)),
	setarg(11,AttD,n), % no longer
	get_attr(I,itf,AttI),
	arg(2,AttI,type(It)),
	arg(5,AttI,order(OrdI)),
	arg(6,AttI,class(ClI)),
	pivot(D,ClI,OrdI,Dt,It),
	reverse_pivot(Ps).

% unkeep(Pivots)
%
% 

unkeep([]).
unkeep([_:D|Ps]) :-
	get_attr(D,itf,Att),
	setarg(11,Att,n),
	drop_dep_one(D),
	unkeep(Ps).


%
% All we drop are bounds
%
fm_detach( []).
fm_detach([V:_|Vs]) :-
	detach_bounds(V),
	fm_detach(Vs).

% activate_crossproduct(Lst)
%
% For each inequality Lin =< 0 (or Lin < 0) in Lst, a new variable is created:
% Var = Lin and Var =< 0 (or Var < 0). Var is added to the basis. 

activate_crossproduct([]).
activate_crossproduct([lez(Strict,Lin)|News]) :-
	var_with_def_intern(t_u(0.0),Var,Lin,Strict),
	% Var belongs to same class as elements in Lin
	basis_add(Var,_),
	activate_crossproduct(News).

% ------------------------------------------------------------------------------

% crossproduct(Lst,Res,ResTail)
%
% See crossproduct/4
% This predicate each time puts the next element of Lst as First in crossproduct/4
% and lets the rest be Next.

crossproduct([]) --> [].
crossproduct([A|As]) -->
	crossproduct(As,A),
	crossproduct(As).

% crossproduct(Next,First,Res,ResTail)
% 
% Eliminates a variable in linear equations First + Next and stores the generated
% inequalities in Res.
% Let's say A:K1 = First and B:K2 = first equation in Next.
% A = ... + K1*V + ...
% B = ... + K2*V + ...
% Let K = -K2/K1
% then K*A + B = ... + 0*V + ...
% from the bounds of A and B, via cross_lower/7 and cross_upper/7, new inequalities
% are generated. Then the same is done for B:K2 = next element in Next.

crossproduct([],_) --> [].
crossproduct([B:Kb|Bs],A:Ka) -->
	{
	    get_attr(A,itf,AttA),
	    arg(2,AttA,type(Ta)),
	    arg(3,AttA,strictness(Sa)),
	    arg(4,AttA,lin(LinA)),
	    get_attr(B,itf,AttB),
	    arg(2,AttB,type(Tb)),
	    arg(3,AttB,strictness(Sb)),
	    arg(4,AttB,lin(LinB)),
	    K is -Kb/Ka,
	    add_linear_f1(LinA,K,LinB,Lin)	% Lin doesn't contain the target variable anymore
	},
	(   { K > 1.0e-10 } % K > 0: signs were opposite
	->  { Strict is Sa \/ Sb },
	    cross_lower(Ta,Tb,K,Lin,Strict),
	    cross_upper(Ta,Tb,K,Lin,Strict)
	;   % La =< A =< Ua -> -Ua =< -A =< -La
    	    {
		flip(Ta,Taf),
		flip_strict(Sa,Saf),
		Strict is Saf \/ Sb
	    },
	    cross_lower(Taf,Tb,K,Lin,Strict),
	    cross_upper(Taf,Tb,K,Lin,Strict)
	),
	crossproduct(Bs,A:Ka).

% cross_lower(Ta,Tb,K,Lin,Strict,Res,ResTail)
%
% Generates a constraint following from the bounds of A and B.
% When A = LinA and B = LinB then Lin = K*LinA + LinB. Ta is the type
% of A and Tb is the type of B. Strict is the union of the strictness 
% of A and B. If K is negative, then Ta should have been flipped (flip/2).
% The idea is that if La =< A =< Ua and Lb =< B =< Ub (=< can also be <)
% then if K is positive, K*La + Lb =< K*A + B =< K*Ua + Ub.
% if K is negative, K*Ua + Lb =< K*A + B =< K*La + Ub.
% This predicate handles the first inequality and adds it to Res in the form
% lez(Sl,Lhs) meaning K*La + Lb - (K*A + B) =< 0 or K*Ua + Lb - (K*A + B) =< 0
% with Sl being the strictness and Lhs the lefthandside of the equation.
% See also cross_upper/7

cross_lower(Ta,Tb,K,Lin,Strict) -->
	{
	    lower(Ta,La),
	    lower(Tb,Lb),
	    !,
	    L is K*La+Lb,
	    normalize_scalar(L,Ln),
	    add_linear_f1(Lin,-1.0,Ln,Lhs),
	    Sl is Strict >> 1 % normalize to upper bound
	},
	[ lez(Sl,Lhs) ].
cross_lower(_,_,_,_,_) --> [].

% cross_upper(Ta,Tb,K,Lin,Strict,Res,ResTail)
%
% See cross_lower/7
% This predicate handles the second inequality:
% -(K*Ua + Ub) + K*A + B =< 0 or -(K*La + Ub) + K*A + B =< 0

cross_upper(Ta,Tb,K,Lin,Strict) -->
	{
	    upper(Ta,Ua),
	    upper(Tb,Ub),
	    !,
	    U is -(K*Ua+Ub),
	    normalize_scalar(U,Un),
	    add_linear_11(Un,Lin,Lhs),
	    Su is Strict /\ 1 % normalize to upper bound
	},
	[ lez(Su,Lhs) ].
cross_upper(_,_,_,_,_) --> [].

% lower(Type,Lowerbound)
%
% Returns the lowerbound of type Type if it has one.
% E.g. if type = t_l(L) then Lowerbound is L,
%      if type = t_lU(L,U) then Lowerbound is L,
%      if type = t_u(U) then fails

lower(t_l(L),L).
lower(t_lu(L,_),L).
lower(t_L(L),L).
lower(t_Lu(L,_),L).
lower(t_lU(L,_),L).

% upper(Type,Upperbound)
%
% Returns the upperbound of type Type if it has one.
% See lower/2

upper(t_u(U),U).
upper(t_lu(_,U),U).
upper(t_U(U),U).
upper(t_Lu(_,U),U).
upper(t_lU(_,U),U).

% flip(Type,FlippedType)
%
% Flips the lower and upperbound, so the old lowerbound becomes the new upperbound and
% vice versa.

flip(t_l(X),t_u(X)).
flip(t_u(X),t_l(X)).
flip(t_lu(X,Y),t_lu(Y,X)).
flip(t_L(X),t_u(X)).
flip(t_U(X),t_l(X)).
flip(t_lU(X,Y),t_lu(Y,X)).
flip(t_Lu(X,Y),t_lu(Y,X)).

% flip_strict(Strict,FlippedStrict)
%
% Does what flip/2 does, but for the strictness.

flip_strict(0,0).
flip_strict(1,2).
flip_strict(2,1).
flip_strict(3,3).

% cp_card(Lst,CountIn,CountOut)
%
% Counts the number of bounds that may generate an inequality in
% crossproduct/3

cp_card([],Ci,Ci).
cp_card([A|As],Ci,Co) :-
	cp_card(As,A,Ci,Cii),
	cp_card(As,Cii,Co).

% cp_card(Next,First,CountIn,CountOut)
% 
% Counts the number of bounds that may generate an inequality in
% crossproduct/4.

cp_card([],_,Ci,Ci).
cp_card([B:Kb|Bs],A:Ka,Ci,Co) :-
	get_attr(A,itf,AttA),
	arg(2,AttA,type(Ta)),
	get_attr(B,itf,AttB),
	arg(2,AttB,type(Tb)),
	K is -Kb/Ka,
	(   K > 1.0e-10 % K > 0: signs were opposite
	->  cp_card_lower(Ta,Tb,Ci,Cii),
	    cp_card_upper(Ta,Tb,Cii,Ciii)
	;   flip(Ta,Taf),
	    cp_card_lower(Taf,Tb,Ci,Cii),
	    cp_card_upper(Taf,Tb,Cii,Ciii)
	),
	cp_card(Bs,A:Ka,Ciii,Co).

% cp_card_lower(TypeA,TypeB,SIn,SOut)
%
% SOut = SIn + 1 if both TypeA and TypeB have a lowerbound.

cp_card_lower(Ta,Tb,Si,So) :-
	lower(Ta,_),
	lower(Tb,_),
	!,
	So is Si+1.
cp_card_lower(_,_,Si,Si).

% cp_card_upper(TypeA,TypeB,SIn,SOut)
%
% SOut = SIn + 1 if both TypeA and TypeB have an upperbound.

cp_card_upper(Ta,Tb,Si,So) :-
	upper(Ta,_),
	upper(Tb,_),
	!,
	So is Si+1.
cp_card_upper(_,_,Si,Si).

% ------------------------------------------------------------------------------

% occurences(V,Occ)
%
% Returns in Occ the occurrences of variable V in the linear equations of dependent variables
% with bound =\= t_none in the form of D:K where D is a dependent variable and K is the scalar
% of V in the linear equation of D.

occurences(V,Occ) :-
	get_attr(V,itf,Att),
	arg(5,Att,order(OrdV)),
	arg(6,Att,class(C)),
	class_allvars(C,All),
	occurences(All,OrdV,Occ).

% occurences(De,OrdV,Occ)
%
% Returns in Occ the occurrences of variable V with order OrdV in the linear equations of 
% dependent variables De with bound =\= t_none in the form of D:K where D is a dependent
% variable and K is the scalar of V in the linear equation of D.

occurences(De,_,[]) :- 
	var(De),
	!.
occurences([D|De],OrdV,Occ) :-
	(   get_attr(D,itf,Att),
	    arg(2,Att,type(Type)),
	    arg(4,Att,lin(Lin)),
	    occ_type_filter(Type),
	    nf_coeff_of(Lin,OrdV,K)
	->  Occ = [D:K|Occt],
	    occurences(De,OrdV,Occt)
	;   occurences(De,OrdV,Occ)
	).

% occ_type_filter(Type)
% 
% Succeeds when Type is any other type than t_none. Is used in occurences/3 and occurs/2

occ_type_filter(t_l(_)).
occ_type_filter(t_u(_)).
occ_type_filter(t_lu(_,_)).
occ_type_filter(t_L(_)).
occ_type_filter(t_U(_)).
occ_type_filter(t_lU(_,_)).
occ_type_filter(t_Lu(_,_)).

% occurs(V)
%
% Checks whether variable V occurs in a linear equation of a dependent variable with a bound
% =\= t_none.

occurs(V) :-
	get_attr(V,itf,Att),
	arg(5,Att,order(OrdV)),
	arg(6,Att,class(C)),
	class_allvars(C,All),
	occurs(All,OrdV).

% occurs(De,OrdV)
%
% Checks whether variable V with order OrdV occurs in a linear equation of any dependent variable
% in De with a bound =\= t_none.

occurs(De,_) :- 
	var(De),
	!,
	fail.
occurs([D|De],OrdV) :-
	(   get_attr(D,itf,Att),
	    arg(2,Att,type(Type)),
	    arg(4,Att,lin(Lin)),
	    occ_type_filter(Type),
	    nf_coeff_of(Lin,OrdV,_)
	->  true
	;   occurs(De,OrdV)
	).