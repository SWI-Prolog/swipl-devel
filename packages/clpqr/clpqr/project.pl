/* 

    Part of CLP(Q,R) (Constraint Logic Programming over Rationals and Reals)

    Author:        Leslie De Koninck
    E-mail:        Leslie.DeKoninck@cs.kuleuven.be
    WWW:           http://www.swi-prolog.org
		   http://www.ai.univie.ac.at/cgi-bin/tr-online?number+95-09
    Copyright (C): 2006, K.U. Leuven and
		   1992-1995, Austrian Research Institute for
		              Artificial Intelligence (OFAI),
			      Vienna, Austria

    This software is based on CLP(Q,R) by Christian Holzbaur for SICStus
    Prolog and distributed under the license details below with permission from
    all mentioned authors.

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

%
% Answer constraint projection
%

%:- public project_attributes/2. 		% xref.pl

:- module(project,
	[
	    drop_dep/1,
	    drop_dep_one/1,
	    make_target_indep/2,
	    project_attributes/2
	]).
:- use_module(class,
	[
	    class_allvars/2
	]).
:- use_module(geler,
	[
	    project_nonlin/3
	]).
:- use_module(redund,
	[
	    redundancy_vars/1,
	    systems/3
	]).
:- use_module(ordering,
	[
	    arrangement/2
	]).

%
% interface predicate
%
% May be destructive (either acts on a copy or in a failure loop)
%
project_attributes(TargetVars,Cvas) :-
	sort(TargetVars,Tvs),		% duplicates ?
	sort(Cvas,Avs),			% duplicates ?
	get_clp(TargetVars,CLP),
	(   nonvar(CLP)
	->  mark_target(Tvs),
	    project_nonlin(Tvs,Avs,NlReachable),
	    (   Tvs == []
	    ->  drop_lin_atts(Avs)
	    ;   redundancy_vars(Avs),		% removes redundant bounds (redund.pl)
		make_target_indep(Tvs,Pivots),	% pivot partners are marked to be kept during elim.	
		mark_target(NlReachable),	% after make_indep to express priority
		drop_dep(Avs),
		fm_elim(CLP,Avs,Tvs,Pivots),
		impose_ordering(Avs)
	    )
	;   true
	).

fm_elim(clpq,Avs,Tvs,Pivots) :- fourmotz_q:fm_elim(Avs,Tvs,Pivots).
fm_elim(clpr,Avs,Tvs,Pivots) :- fourmotz_r:fm_elim(Avs,Tvs,Pivots).

get_clp([],_).
get_clp([H|T],CLP) :-
	(   get_attr(H,itf,Att)
	->  arg(1,Att,CLP)
	;   true
	),
	get_clp(T,CLP).

% mark_target(Vars)
%
% Marks the variables in Vars as target variables.

mark_target([]).
mark_target([V|Vs]) :-
	(   get_attr(V,itf,Att)
	->  setarg(9,Att,target)
	;   true
	),
	mark_target(Vs).
	

% mark_keep(Vars)
%
% Mark the variables in Vars to be kept during elimination.

mark_keep([]).
mark_keep([V|Vs]) :-
	get_attr(V,itf,Att),
	setarg(11,Att,keep),
	mark_keep(Vs).

%
% Collect the pivots in reverse order
% We have to protect the target variables pivot partners
% from redundancy eliminations triggered by fm_elim,
% in order to allow for reverse pivoting.
%
make_target_indep(Ts,Ps) :- make_target_indep(Ts,[],Ps).

% make_target_indep(Targets,Pivots,PivotsTail)
%
% Tries to make as many targetvariables independent by pivoting them with a non-target
% variable. The pivots are stored as T:NT where T is a target variable and NT a non-target
% variable. The non-target variables are marked to be kept during redundancy eliminations.

make_target_indep([],Ps,Ps).
make_target_indep([T|Ts],Ps0,Pst) :-
	(   get_attr(T,itf,AttT),
	    arg(1,AttT,CLP),
	    arg(2,AttT,type(Type)),
	    arg(4,AttT,lin([_,_|H])),
	    nontarget(H,Nt)
	->  Ps1 = [T:Nt|Ps0],
	    get_attr(Nt,itf,AttN),
	    arg(2,AttN,type(IndAct)),
	    arg(5,AttN,order(Ord)),
	    arg(6,AttN,class(Class)),
	    setarg(11,AttN,keep),
	    pivot(CLP,T,Class,Ord,Type,IndAct)
	;   Ps1 = Ps0
	),
	make_target_indep(Ts,Ps1,Pst).

% nontarget(Hom,Nt)
%
% Finds a nontarget variable in homogene part Hom.
% Hom contains elements of the form l(V*K,OrdV).
% A nontarget variable has no target attribute and no keep_indep attribute.

nontarget([l(V*_,_)|Vs],Nt) :-
	(   get_attr(V,itf,Att),
	    arg(9,Att,n),
	    arg(10,Att,n)
	->  Nt = V
	;   nontarget(Vs,Nt)
	).

% drop_dep(Vars)
%
% Does drop_dep_one/1 on each variable in Vars.

drop_dep(Vs) :- 
	var(Vs),
	!.
drop_dep([]).
drop_dep([V|Vs]) :-
	drop_dep_one(V),
	drop_dep(Vs).

% drop_dep_one(V)
%
% If V is an unbounded dependent variable that isn't a target variable, shouldn't be kept
% and is not nonzero, drops all linear attributes of V.
% The linear attributes are: type, strictness, linear equation (lin), class and order.

drop_dep_one(V) :-
	get_attr(V,itf,Att),
	Att = t(CLP,type(t_none),_,lin(Lin),order(OrdV),_,_,n,n,_,n),
	\+ indep(CLP,Lin,OrdV),
	!,
	setarg(2,Att,n),
	setarg(3,Att,n),
	setarg(4,Att,n),
	setarg(5,Att,n),
	setarg(6,Att,n).
drop_dep_one(_).

indep(clpq,Lin,OrdV) :- store_q:indep(Lin,OrdV).
indep(clpr,Lin,OrdV) :- store_r:indep(Lin,OrdV).

pivot(clpq,T,Class,Ord,Type,IndAct) :- bv_q:pivot(T,Class,Ord,Type,IndAct).
pivot(clpr,T,Class,Ord,Type,IndAct) :- bv_r:pivot(T,Class,Ord,Type,IndAct).

renormalize(clpq,Lin,New) :- store_q:renormalize(Lin,New).
renormalize(clpr,Lin,New) :- store_r:renormalize(Lin,New).

% drop_lin_atts(Vs)
%
% Removes the linear attributes of the variables in Vs.
% The linear attributes are type, strictness, linear equation (lin), order and class.

drop_lin_atts([]).
drop_lin_atts([V|Vs]) :-
	get_attr(V,itf,Att),
	setarg(2,Att,n),
	setarg(3,Att,n),
	setarg(4,Att,n),
	setarg(5,Att,n),
	setarg(6,Att,n),
	drop_lin_atts(Vs).

impose_ordering(Cvas) :-
	systems(Cvas,[],Sys),
	impose_ordering_sys(Sys).

impose_ordering_sys([]).
impose_ordering_sys([S|Ss]) :-
	arrangement(S,Arr),	% ordering.pl
	arrange(Arr,S),
	impose_ordering_sys(Ss).

arrange([],_).
arrange(Arr,S) :- 
	Arr = [_|_],
	class_allvars(S,All),
	order(Arr,1,N),
	order(All,N,_),
	renorm_all(All),
	arrange_pivot(All).

order(Xs,N,M) :- 
	var(Xs),
	!,
	N = M.
order([],N,N).
order([X|Xs],N,M) :-
	(   get_attr(X,itf,Att),
	    arg(5,Att,order(O)),
	    var(O)
	->  O = N,
	    N1 is N+1,
	    order(Xs,N1,M)
	;   order(Xs,N,M)
	).

% renorm_all(Vars)
%
% Renormalizes all linear equations of the variables in difference list Vars to reflect
% their new ordering.

renorm_all(Xs) :- 
	var(Xs),
	!.
renorm_all([X|Xs]) :-
	(   get_attr(X,itf,Att),
	    arg(1,Att,CLP),
	    arg(4,Att,lin(Lin))
	->  renormalize(CLP,Lin,New),
	    setarg(4,Att,lin(New)),
	    renorm_all(Xs)
	;   renorm_all(Xs)
	).

% arrange_pivot(Vars)
%
% If variable X of Vars has type t_none and has a higher order than the first element of
% its linear equation, then it is pivoted with that element.

arrange_pivot(Xs) :- 
	var(Xs),
	!.
arrange_pivot([X|Xs]) :-
	(   get_attr(X,itf,AttX),
	    %arg(8,AttX,n), % not for nonzero
	    arg(1,AttX,CLP),
	    arg(2,AttX,type(t_none)),
	    arg(4,AttX,lin(Lin)),
	    arg(5,AttX,order(OrdX)),
	    Lin = [_,_,l(Y*_,_)|_],
	    get_attr(Y,itf,AttY),
	    arg(2,AttY,type(IndAct)),
	    arg(5,AttY,order(OrdY)),
	    arg(6,AttY,class(Class)),
	    compare(>,OrdY,OrdX)
	->  pivot(CLP,X,Class,OrdY,t_none,IndAct),
	    arrange_pivot(Xs)
	;   arrange_pivot(Xs)
	).