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

:- module(redund,
	[
	    redundancy_vars/1,
	    systems/3
	]).
:- use_module(class,
	[
	    class_allvars/2
	]).

%
% redundancy removal (semantic definition)
%
% done:
%	+) deal with active bounds
%	+) indep t_[lu] -> t_none invalidates invariants (fixed)
%

% systems(Vars,SystemsIn,SystemsOut)
%
% Returns in SystemsOut the different classes to which variables in Vars
% belong. Every class only appears once in SystemsOut.

systems([],Si,Si).
systems([V|Vs],Si,So) :-
	(   var(V),
	    get_attr(V,itf,Att),
	    arg(6,Att,class(C)),
	    not_memq(Si,C)
	->  systems(Vs,[C|Si],So)
	;   systems(Vs,Si,So)
	).

% not_memq(Lst,El)
%
% Succeeds if El is not a member of Lst (does not use unification).

not_memq([],_).
not_memq([Y|Ys],X) :-
	X \== Y,
	not_memq(Ys,X).

% redundancy_systems(Classes)
%
% Does redundancy removal via redundancy_vs/1 on all variables in the classes Classes. 

redundancy_systems([]).
redundancy_systems([S|Sys]) :-
	class_allvars(S,All),
	redundancy_vs(All),
	redundancy_systems(Sys).

% redundancy_vars(Vs)
%
% Does the same thing as redundancy_vs/1 but has some extra timing facilities that
% may be used.

redundancy_vars(Vs) :-
	!,
	redundancy_vs(Vs).
redundancy_vars(Vs) :-
	statistics(runtime,[Start|_]),
	redundancy_vs(Vs),
	statistics(runtime,[End|_]),
	Duration is End-Start,
	format(user_error,"% Redundancy elimination took ~d msec~n",Duration).


% redundancy_vs(Vs)
%
% Removes redundant bounds from the variables in Vs via redundant/3

redundancy_vs(Vs) :- 
	var(Vs),
	!.
redundancy_vs([]).
redundancy_vs([V|Vs]) :-
	(   get_attr(V,itf,Att),
	    arg(2,Att,type(Type)),
	    arg(3,Att,strictness(Strict)),
	    redundant(Type,V,Strict)
	->  redundancy_vs(Vs)
	;   redundancy_vs(Vs)
	).

% redundant(Type,Var,Strict)
%
% Removes redundant bounds from variable Var with type Type and strictness Strict.
% A redundant bound is one that is satisfied anyway (so adding the inverse of the bound
% makes the system infeasible. This predicate can either fail or succeed but a success
% doesn't necessarily mean a redundant bound.

redundant(t_l(L),X,Strict) :-
	get_attr(X,itf,Att),
	arg(1,Att,CLP),
	detach_bounds(CLP,X),	% drop temporarily
	% if not redundant, backtracking will restore bound
	negate_l(Strict,CLP,L,X),
	red_t_l.	% negate_l didn't fail, redundant bound
redundant(t_u(U),X,Strict) :-
	get_attr(X,itf,Att),
	arg(1,Att,CLP),
	detach_bounds(CLP,X),
	negate_u(Strict,CLP,U,X),
	red_t_u.
redundant(t_lu(L,U),X,Strict) :-
	strictness_parts(Strict,Sl,Su),
	(   get_attr(X,itf,Att),
	    arg(1,Att,CLP),
	    setarg(2,Att,type(t_u(U))),
	    setarg(3,Att,strictness(Su)),
	    negate_l(Strict,CLP,L,X)
	->  red_t_l,
	    (   redundant(t_u(U),X,Strict)
	    ->  true
	    ;   true
	    )
	;   get_attr(X,itf,Att),
	    arg(1,Att,CLP),
	    setarg(2,Att,type(t_l(L))),
	    setarg(3,Att,strictness(Sl)),
	    negate_u(Strict,CLP,U,X)
	->  red_t_u
	;   true
	).
redundant(t_L(L),X,Strict) :-
	get_attr(X,itf,Att),
	arg(1,Att,CLP),
	Bound is -L,
	intro_at(CLP,X,Bound,t_none),	% drop temporarily
	detach_bounds(CLP,X),
	negate_l(Strict,CLP,L,X),
	red_t_L.
redundant(t_U(U),X,Strict) :-
	get_attr(X,itf,Att),
	arg(1,Att,CLP),
	Bound is -U,
	intro_at(CLP,X,Bound,t_none),	% drop temporarily
	detach_bounds(CLP,X),
	negate_u(Strict,CLP,U,X),
	red_t_U.
redundant(t_Lu(L,U),X,Strict) :-
	strictness_parts(Strict,Sl,Su),
	(   Bound is -L,
	    get_attr(X,itf,Att),
	    arg(1,Att,CLP),
	    intro_at(CLP,X,Bound,t_u(U)),
	    get_attr(X,itf,Att2), % changed?
	    setarg(3,Att2,strictness(Su)),
	    negate_l(Strict,CLP,L,X)
	->  red_t_l,
	    (   redundant(t_u(U),X,Strict)
	    ->  true
	    ;   true
	    )
	;   get_attr(X,itf,Att),
	    arg(1,Att,CLP),
	    setarg(2,Att,type(t_L(L))),
	    setarg(3,Att,strictness(Sl)),
	    negate_u(Strict,CLP,U,X)
	->  red_t_u
	;   true
	).
redundant(t_lU(L,U),X,Strict) :-
	strictness_parts(Strict,Sl,Su),
	(   get_attr(X,itf,Att),
	    arg(1,Att,CLP),
	    setarg(2,Att,type(t_U(U))),
	    setarg(3,Att,strictness(Su)),
	    negate_l(Strict,CLP,L,X)
	->  red_t_l,
	    (   redundant(t_U(U),X,Strict)
	    ->  true
	    ;   true
	    )
	;   get_attr(X,itf,Att),
	    arg(1,Att,CLP),
	    Bound is -U,
	    intro_at(CLP,X,Bound,t_l(L)),
	    get_attr(X,itf,Att2), % changed?
	    setarg(3,Att2,strictness(Sl)),
	    negate_u(Strict,CLP,U,X)
	->  red_t_u
	;   true
	).

% strictness_parts(Strict,Lower,Upper)
%
% Splits strictness Strict into two parts: one related to the lowerbound and
% one related to the upperbound.

strictness_parts(Strict,Lower,Upper) :-
	Lower is Strict /\ 2,
	Upper is Strict /\ 1.

% negate_l(Strict,Lowerbound,X)
%
% Fails if X does not necessarily satisfy the lowerbound and strictness
% In other words: if adding the inverse of the lowerbound (X < L or X =< L)
% does not result in a failure, this predicate fails.

negate_l(0,CLP,L,X) :- 
	CLP:{L > X},
	!,
	fail.
negate_l(1,CLP,L,X) :- 
	CLP:{L > X},
	!,
	fail.
negate_l(2,CLP,L,X) :-
	CLP:{L >= X},
	!,
	fail.
negate_l(3,CLP,L,X) :-
	CLP:{L >= X},
	!,
	fail.
negate_l(_,_,_,_).

% negate_u(Strict,Upperbound,X)
%
% Fails if X does not necessarily satisfy the upperbound and strictness
% In other words: if adding the inverse of the upperbound (X > U or X >= U)
% does not result in a failure, this predicate fails.

negate_u(0,CLP,U,X) :-
	CLP:{U < X},
	!,
	fail.
negate_u(1,CLP,U,X) :- 
	CLP:{U =< X},
	!,
	fail.
negate_u(2,CLP,U,X) :- 
	CLP:{U < X},
	!,
	fail.
negate_u(3,CLP,U,X) :- 
	CLP:{U =< X},
	!,
	fail.
negate_u(_,_,_,_).

% CLP(Q,R)

detach_bounds(clpq,X) :- bv_q:detach_bounds(X).
detach_bounds(clpr,X) :- bv_r:detach_bounds(X).

intro_at(clpq,A,B,C) :- bv_q:intro_at(A,B,C).
intro_at(clpr,A,B,C) :- bv_r:intro_at(A,B,C).

% Profiling: these predicates are called during redundant and can be used
% to count the number of redundant bounds.

red_t_l.
red_t_u.
red_t_L.
red_t_U.