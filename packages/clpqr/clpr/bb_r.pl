/*  $Id$

    Part of CPL(R) (Constraint Logic Programming over Reals)

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

:- module(bb_r,
	[
	    bb_inf/3,	
	    bb_inf/5,
	    vertex_value/2
	]).
:- use_module(bv_r,
	[
	    deref/2,
	    deref_var/2,
	    determine_active_dec/1,
	    inf/2,
	    iterate_dec/2,
	    sup/2,
	    var_with_def_assign/2
	]).
:- use_module(nf_r,
	[
	    {}/1,
	    entailed/1,
	    nf/2,
	    nf_constant/2,
	    repair/2,
	    wait_linear/3		
	]).
		
% bb_inf(Ints,Term,Inf)
%
% Finds the infimum of Term where the variables Ints are to be integers.
% The infimum is stored in Inf.

bb_inf(Is,Term,Inf) :-
	bb_inf(Is,Term,Inf,_,0.001).

bb_inf(Is,Term,Inf,Vertex,Eps) :-
	nf(Eps,ENf),
	nf_constant(ENf,EpsN),
	wait_linear(Term,Nf,bb_inf_internal(Is,Nf,EpsN,Inf,Vertex)).

% ---------------------------------------------------------------------

% bb_inf_internal(Is,Lin,Eps,Inf,Vertex)
%
% Finds an infimum Inf for linear expression in normal form Lin, where
% all variables in Is are to be integers. Eps denotes the margin in which
% we accept a number as an integer (to deal with rounding errors etc.).

bb_inf_internal(Is,Lin,Eps,_,_) :-
	bb_intern(Is,IsNf,Eps),
	nb_delete(prov_opt),
	repair(Lin,LinR),	% bb_narrow ...
	deref(LinR,Lind),
	var_with_def_assign(Dep,Lind),
	determine_active_dec(Lind),
	bb_loop(Dep,IsNf,Eps),
	fail.
bb_inf_internal(_,_,_,Inf,Vertex) :-
	catch(nb_getval(prov_opt,InfVal-Vertex),_,fail),
	{Inf =:= InfVal},
	nb_delete(prov_opt).

% bb_loop(Opt,Is,Eps)
%
% Minimizes the value of Opt where variables Is have to be integer values.
% Eps denotes the rounding error that is acceptable. This predicate can be
% backtracked to try different strategies.

bb_loop(Opt,Is,Eps) :-
	bb_reoptimize(Opt,Inf),
	bb_better_bound(Inf),
	vertex_value(Is,Ivs),
	(   bb_first_nonint(Is,Ivs,Eps,Viol,Floor,Ceiling)
	->  bb_branch(Viol,Floor,Ceiling),
	    bb_loop(Opt,Is,Eps)
	;   round_values(Ivs,RoundVertex),
	    nb_setval(prov_opt,Inf-RoundVertex) % new provisional optimum
	).

% bb_reoptimize(Obj,Inf)
%
% Minimizes the value of Obj and puts the result in Inf.
% This new minimization is necessary as making a bound integer may yield a
% different optimum. The added inequalities may also have led to binding.

bb_reoptimize(Obj,Inf) :- 
	var(Obj),
	iterate_dec(Obj,Inf).
bb_reoptimize(Obj,Inf) :- 
	nonvar(Obj),
	Inf = Obj.

% bb_better_bound(Inf)
%
% Checks if the new infimum Inf is better than the previous one (if such exists).

bb_better_bound(Inf) :-
	catch((nb_getval(prov_opt,Inc-_),Inf - Inc < -1.0e-10),_,true).

% bb_branch(V,U,L)
%
% Stores that V =< U or V >= L, can be used for different strategies within bb_loop/3.

bb_branch(V,U,_) :- {V =< U}.
bb_branch(V,_,L) :- {V >= L}.

% vertex_value(Vars,Values)
%
% Returns in <Values> the current values of the variables in <Vars>.

vertex_value([],[]).
vertex_value([X|Xs],[V|Vs]) :-
	rhs_value(X,V),
	vertex_value(Xs,Vs).

% rhs_value(X,Value)
%
% Returns in <Value> the current value of variable <X>.

rhs_value(Xn,Value) :- 
	(   nonvar(Xn)
	->  Value = Xn
	;   var(Xn)
	->  deref_var(Xn,Xd),
	    Xd = [I,R|_],
	    Value is R+I
	).

% bb_first_nonint(Ints,Rhss,Eps,Viol,Floor,Ceiling)
%
% Finds the first variable in Ints which doesn't have an active integer bound.
% Rhss contain the Rhs (R + I) values corresponding to the variables.
% The first variable that hasn't got an active integer bound, is returned in
% Viol. The floor and ceiling of its actual bound is returned in Floor and Ceiling.

bb_first_nonint([I|Is],[Rhs|Rhss],Eps,Viol,F,C) :-
	(   Floor is floor(Rhs+1.0e-10),
	    Ceiling is ceiling(Rhs-1.0e-10),
	    Eps - min(Rhs-Floor,Ceiling-Rhs) < -1.0e-10
	->  Viol = I,
	    F = Floor,
	    C = Ceiling
	;   bb_first_nonint(Is,Rhss,Eps,Viol,F,C)
	).

% round_values([X|Xs],[Xr|Xrs])
%
% Rounds of the values of the first list into the second list.

round_values([],[]).
round_values([X|Xs],[Y|Ys]) :-
	Y is round(X),
	round_values(Xs,Ys).

% bb_intern([X|Xs],[Xi|Xis],Eps)
%
% Turns the elements of the first list into integers into the second
% list via bb_intern/4.

bb_intern([],[],_).
bb_intern([X|Xs],[Xi|Xis],Eps) :-
	nf(X,Xnf),
	bb_intern(Xnf,Xi,X,Eps),
	bb_intern(Xs,Xis,Eps).


% bb_intern(Nf,X,Term,Eps)
%
% Makes sure that Term which is normalized into Nf, is integer.
% X contains the possibly changed Term. If Term is a variable,
% then its bounds are hightened or lowered to the next integer.
% Otherwise, it is checked it Term is integer.

bb_intern([],X,_,_) :- 
	!,
	X = 0.0.
bb_intern([v(I,[])],X,_,Eps) :- 
	!,
	X = I,
	min(I-floor(I+1e-010),ceiling(I-1e-010)-I) - Eps < 1e-010.
bb_intern([v(One,[V^1])],X,_,_) :-
	Test is One - 1.0,
	Test =< 1e-010,
	Test >= -1e-010,
	!,
	V = X,
	bb_narrow_lower(X),
	bb_narrow_upper(X).
bb_intern(_,_,Term,_) :-
	throw(instantiation_error(bb_inf(Term,_,_),1)).

% bb_narrow_lower(X)
%
% Narrows the lower bound so that it is an integer bound.
% We do this by finding the infimum of X and asserting that X
% is larger than the first integer larger or equal to the infimum
% (second integer if X is to be strict larger than the first integer).

bb_narrow_lower(X) :-
	(   inf(X,Inf)
	->  Bound is ceiling(Inf-1.0e-10),
	    (   entailed(X > Bound)
	    ->  {X >= Bound+1}
	    ;   {X >= Bound}
	    )
	;   true
	).

% bb_narrow_upper(X)
%
% See bb_narrow_lower/1. This predicate handles the upper bound.

bb_narrow_upper(X) :-
	(   sup(X,Sup)
	->  Bound is floor(Sup+1.0e-10),
	    (   entailed(X < Bound)
	    ->  {X =< Bound-1}
	    ;   {X =< Bound}
	    )
	;   true
	).
