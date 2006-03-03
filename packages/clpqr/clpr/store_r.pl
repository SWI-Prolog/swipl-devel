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

:- module(store_r,
	[
	    add_linear_11/3,
	    add_linear_f1/4,
	    add_linear_ff/5,
	    normalize_scalar/2,
	    delete_factor/4,
	    mult_linear_factor/3,
	    nf_rhs_x/4,
	    indep/2,
	    isolate/3,
	    nf_substitute/4,
	    mult_hom/3,
	    nf2sum/3,
	    nf_coeff_of/3,
	    renormalize/2	
	]).

% normalize_scalar(S,[N,Z])
%
% Transforms a scalar S into a linear expression [S,0]

normalize_scalar(S,[S,0.0]).

% renormalize(List,Lin)
%
% Renormalizes the not normalized linear expression in List into
% a normalized one. It does so to take care of unifications.
% (e.g. when a variable X is bound to a constant, the constant is added to
% the constant part of the linear expression; when a variable X is bound to
% another variable Y, the scalars of both are added)

renormalize([I,R|Hom],Lin) :-
	length(Hom,Len),
	renormalize_log(Len,Hom,[],Lin0),
	add_linear_11([I,R],Lin0,Lin).

% renormalize_log(Len,Hom,HomTail,Lin)
%
% Logarithmically renormalizes the homogene part of a not normalized
% linear expression. See also renormalize/2.

renormalize_log(1,[Term|Xs],Xs,Lin) :-
	!,
	Term = l(X*_,_),
	renormalize_log_one(X,Term,Lin).
renormalize_log(2,[A,B|Xs],Xs,Lin) :-
	!,
	A = l(X*_,_),
	B = l(Y*_,_),
	renormalize_log_one(X,A,LinA),
	renormalize_log_one(Y,B,LinB),
	add_linear_11(LinA,LinB,Lin).
renormalize_log(N,L0,L2,Lin) :-
	P is N>>1,
	Q is N-P,
	renormalize_log(P,L0,L1,Lp),
	renormalize_log(Q,L1,L2,Lq),
	add_linear_11(Lp,Lq,Lin).

% renormalize_log_one(X,Term,Res)
%
% Renormalizes a term in X: if X is a nonvar, the term becomes a scalar. 

renormalize_log_one(X,Term,Res) :-
	var(X),
	Term = l(X*K,_),
	get_attr(X,itf,Att),
	arg(5,Att,order(OrdX)), % Order might have changed
	Res = [0.0,0.0,l(X*K,OrdX)].
renormalize_log_one(X,Term,Res) :-
	nonvar(X),
	Term = l(X*K,_),
	Xk is X*K,
	normalize_scalar(Xk,Res).

% ----------------------------- sparse vector stuff ---------------------------- %

% add_linear_ff(LinA,Ka,LinB,Kb,LinC)
%
% Linear expression LinC is the result of the addition of the 2 linear expressions
% LinA and LinB, each one multiplied by a scalar (Ka for LinA and Kb for LinB).

add_linear_ff(LinA,Ka,LinB,Kb,LinC) :-
	LinA = [Ia,Ra|Ha],
	LinB = [Ib,Rb|Hb],
	LinC = [Ic,Rc|Hc],
	Ic is Ia*Ka+Ib*Kb,
	Rc is Ra*Ka+Rb*Kb,
 	add_linear_ffh(Ha,Ka,Hb,Kb,Hc).

% add_linear_ffh(Ha,Ka,Hb,Kb,Hc)
%
% Homogene part Hc is the result of the addition of the 2 homogene parts Ha and Hb,
% each one multiplied by a scalar (Ka for Ha and Kb for Hb)

add_linear_ffh([],_,Ys,Kb,Zs) :- mult_hom(Ys,Kb,Zs).
add_linear_ffh([l(X*Kx,OrdX)|Xs],Ka,Ys,Kb,Zs) :-
	add_linear_ffh(Ys,X,Kx,OrdX,Xs,Zs,Ka,Kb).

% add_linear_ffh(Ys,X,Kx,OrdX,Xs,Zs,Ka,Kb)
%
% Homogene part Zs is the result of the addition of the 2 homogene parts Ys and
% [l(X*Kx,OrdX)|Xs], each one multiplied by a scalar (Ka for [l(X*Kx,OrdX)|Xs] and Kb for Ys)

add_linear_ffh([],X,Kx,OrdX,Xs,Zs,Ka,_) :- mult_hom([l(X*Kx,OrdX)|Xs],Ka,Zs).
add_linear_ffh([l(Y*Ky,OrdY)|Ys],X,Kx,OrdX,Xs,Zs,Ka,Kb) :-
	compare(Rel,OrdX,OrdY),
	(   Rel = (=)
	->  Kz is Kx*Ka+Ky*Kb,
	    (   % Kz =:= 0
		Kz =< 1.0e-10,
		Kz >= -1.0e-10
	    ->  add_linear_ffh(Xs,Ka,Ys,Kb,Zs)
	    ;   Zs = [l(X*Kz,OrdX)|Ztail],
		add_linear_ffh(Xs,Ka,Ys,Kb,Ztail)
	    )
	;   Rel = (<)
	->  Zs = [l(X*Kz,OrdX)|Ztail],
	    Kz is Kx*Ka,
	    add_linear_ffh(Xs,Y,Ky,OrdY,Ys,Ztail,Kb,Ka)
	;   Rel = (>)
	->  Zs = [l(Y*Kz,OrdY)|Ztail],
	    Kz is Ky*Kb,
	    add_linear_ffh(Ys,X,Kx,OrdX,Xs,Ztail,Ka,Kb)
     	).

% add_linear_f1(LinA,Ka,LinB,LinC)
%
% special case of add_linear_ff with Kb = 1

add_linear_f1(LinA,Ka,LinB,LinC) :-
	LinA = [Ia,Ra|Ha],
	LinB = [Ib,Rb|Hb],
	LinC = [Ic,Rc|Hc],
	Ic is Ia*Ka+Ib,
	Rc is Ra*Ka+Rb,
	add_linear_f1h(Ha,Ka,Hb,Hc).

% add_linear_f1h(Ha,Ka,Hb,Hc)
%
% special case of add_linear_ffh/5 with Kb = 1

add_linear_f1h([],_,Ys,Ys).
add_linear_f1h([l(X*Kx,OrdX)|Xs],Ka,Ys,Zs) :-
	add_linear_f1h(Ys,X,Kx,OrdX,Xs,Zs,Ka).

% add_linear_f1h(Ys,X,Kx,OrdX,Xs,Zs,Ka)
%
% special case of add_linear_ffh/8 with Kb = 1

add_linear_f1h([],X,Kx,OrdX,Xs,Zs,Ka) :- mult_hom([l(X*Kx,OrdX)|Xs],Ka,Zs).
add_linear_f1h([l(Y*Ky,OrdY)|Ys],X,Kx,OrdX,Xs,Zs,Ka) :-
	compare(Rel,OrdX,OrdY),
	(   Rel = (=)
	->  Kz is Kx*Ka+Ky,
	    (   % Kz =:= 0.0
		Kz =< 1.0e-10,
		Kz >= -1.0e-10
	    ->  add_linear_f1h(Xs,Ka,Ys,Zs)
	    ;   Zs = [l(X*Kz,OrdX)|Ztail],
		add_linear_f1h(Xs,Ka,Ys,Ztail)
	    )
	;   Rel = (<)
	->  Zs = [l(X*Kz,OrdX)|Ztail],
	    Kz is Kx*Ka,
	    add_linear_f1h(Xs,Ka,[l(Y*Ky,OrdY)|Ys],Ztail)
 	;   Rel = (>)
	->  Zs = [l(Y*Ky,OrdY)|Ztail],
	    add_linear_f1h(Ys,X,Kx,OrdX,Xs,Ztail,Ka)
	).

% add_linear_11(LinA,LinB,LinC)
%
% special case of add_linear_ff with Ka = 1 and Kb = 1

add_linear_11(LinA,LinB,LinC) :-
	LinA = [Ia,Ra|Ha],
	LinB = [Ib,Rb|Hb],
	LinC = [Ic,Rc|Hc],
	Ic is Ia+Ib,
	Rc is Ra+Rb,
	add_linear_11h(Ha,Hb,Hc).

% add_linear_11h(Ha,Hb,Hc)
%
% special case of add_linear_ffh/5 with Ka = 1 and Kb = 1

add_linear_11h([],Ys,Ys).
add_linear_11h([l(X*Kx,OrdX)|Xs],Ys,Zs) :-
	add_linear_11h(Ys,X,Kx,OrdX,Xs,Zs).

% add_linear_11h(Ys,X,Kx,OrdX,Xs,Zs)
%
% special case of add_linear_ffh/8 with Ka = 1 and Kb = 1

add_linear_11h([],X,Kx,OrdX,Xs,[l(X*Kx,OrdX)|Xs]).
add_linear_11h([l(Y*Ky,OrdY)|Ys],X,Kx,OrdX,Xs,Zs) :-
	compare(Rel,OrdX,OrdY),
	(   Rel = (=)
	->  Kz is Kx+Ky,
	    (   % Kz =:= 0.0
		Kz =< 1.0e-10,
		Kz >= -1.0e-10
	    ->  add_linear_11h(Xs,Ys,Zs)
	    ;   Zs = [l(X*Kz,OrdX)|Ztail],
		add_linear_11h(Xs,Ys,Ztail)
	    )
	;   Rel = (<)
	->  Zs = [l(X*Kx,OrdX)|Ztail],
	    add_linear_11h(Xs,Y,Ky,OrdY,Ys,Ztail)
	;   Rel = (>)
	->  Zs = [l(Y*Ky,OrdY)|Ztail],
	    add_linear_11h(Ys,X,Kx,OrdX,Xs,Ztail)
	).

% mult_linear_factor(Lin,K,Res)
%
% Linear expression Res is the result of multiplication of linear 
% expression Lin by scalar K

mult_linear_factor(Lin,K,Mult) :-
	TestK is K - 1.0,	% K =:= 1
	TestK =< 1.0e-10,
	TestK >= -1.0e-10,	% avoid copy
	!,
	Mult = Lin.
mult_linear_factor(Lin,K,Res) :-
	Lin = [I,R|Hom],
	Res = [Ik,Rk|Mult],
	Ik is I*K,
	Rk is R*K,
	mult_hom(Hom,K,Mult).

% mult_hom(Hom,K,Res)
%
% Homogene part Res is the result of multiplication of homogene part
% Hom by scalar K

mult_hom([],_,[]).
mult_hom([l(A*Fa,OrdA)|As],F,[l(A*Fan,OrdA)|Afs]) :-
	Fan is F*Fa,
	mult_hom(As,F,Afs).

% nf_substitute(Ord,Def,Lin,Res)
%
% Linear expression Res is the result of substitution of Var in
% linear expression Lin, by its definition in the form of linear
% expression Def

nf_substitute(OrdV,LinV,LinX,LinX1) :-
	delete_factor(OrdV,LinX,LinW,K),
	add_linear_f1(LinV,K,LinW,LinX1).

% delete_factor(Ord,Lin,Res,Coeff)
%
% Linear expression Res is the result of the deletion of the term
% Var*Coeff where Var has ordering Ord from linear expression Lin

delete_factor(OrdV,Lin,Res,Coeff) :-
	Lin = [I,R|Hom],
	Res = [I,R|Hdel],
	delete_factor_hom(OrdV,Hom,Hdel,Coeff).

% delete_factor_hom(Ord,Hom,Res,Coeff)
%
% Homogene part Res is the result of the deletion of the term
% Var*Coeff from homogene part Hom

delete_factor_hom(VOrd,[Car|Cdr],RCdr,RKoeff) :-
	Car = l(_*Koeff,Ord),
	compare(Rel,VOrd,Ord),
	(   Rel= (=)
	->  RCdr = Cdr,
	    RKoeff=Koeff
	;   Rel= (>)
	->  RCdr = [Car|RCdr1],
	    delete_factor_hom(VOrd,Cdr,RCdr1,RKoeff)
	).


% nf_coeff_of(Lin,OrdX,Coeff)
%
% Linear expression Lin contains the term l(X*Coeff,OrdX)

nf_coeff_of([_,_|Hom],VOrd,Coeff) :-
	nf_coeff_hom(Hom,VOrd,Coeff).

% nf_coeff_hom(Lin,OrdX,Coeff)
%
% Linear expression Lin contains the term l(X*Coeff,OrdX) where the
% order attribute of X = OrdX

nf_coeff_hom([l(_*K,OVar)|Vs],OVid,Coeff) :-
	compare(Rel,OVid,OVar),
	(   Rel = (=)
	->  Coeff = K
	;   Rel = (>)
	->  nf_coeff_hom(Vs,OVid,Coeff)
	).

% nf_rhs_x(Lin,OrdX,Rhs,K)
%
% Rhs = R + I where Lin = [I,R|Hom] and l(X*K,OrdX) is a term of Hom

nf_rhs_x(Lin,OrdX,Rhs,K) :-
	Lin = [I,R|Tail],
	nf_coeff_hom(Tail,OrdX,K),
	Rhs is R+I.	% late because X may not occur in H

% isolate(OrdN,Lin,Lin1)
%
% Linear expression Lin1 is the result of the transformation of linear expression
% Lin = 0 which contains the term l(New*K,OrdN) into an equivalent expression Lin1 = New.

isolate(OrdN,Lin,Lin1) :-
	delete_factor(OrdN,Lin,Lin0,Coeff),
	K is -1.0/Coeff,
	mult_linear_factor(Lin0,K,Lin1).

% indep(Lin,OrdX)
%
% succeeds if Lin = [0,_|[l(X*1,OrdX)]]

indep(Lin,OrdX) :-
	Lin = [I,_|[l(_*K,OrdY)]],
	OrdX == OrdY,
	% K =:= 1.0
	TestK is K - 1.0,
	TestK =< 1.0e-10,
	TestK >= -1.0e-10,
	% I =:= 0
	I =< 1.0e-10,
	I >= -1.0e-10.

% nf2sum(Lin,Sofar,Term)
%
% Transforms a linear expression into a sum
% (e.g. the expression [5,_,[l(X*2,OrdX),l(Y*-1,OrdY)]] gets transformed into 5 + 2*X - Y)

nf2sum([],I,I).
nf2sum([X|Xs],I,Sum) :-
	(   % I =:= 0.0
	    I =< 1.0e-10,
	    I >= -1.0e-10
	->  X = l(Var*K,_),
 	    (   % K =:= 1.0
		TestK is K - 1.0,
		TestK =< 1.0e-10,
		TestK >= -1.0e-10
	    ->  hom2sum(Xs,Var,Sum)
	    ;   % K =:= -1.0
		TestK is K + 1.0,
		TestK =< 1.0e-10,
		TestK >= -1.0e-10
	    ->  hom2sum(Xs,-Var,Sum)
	    ;	hom2sum(Xs,K*Var,Sum)
	    )
	;   hom2sum([X|Xs],I,Sum)
 	).

% hom2sum(Hom,Sofar,Term) 
% 
% Transforms a linear expression into a sum
% this predicate handles all but the first term
% (the first term does not need a concatenation symbol + or -)
% see also nf2sum/3

hom2sum([],Term,Term).
hom2sum([l(Var*K,_)|Cs],Sofar,Term) :-
	(   % K =:= 1.0
	    TestK is K - 1.0,
	    TestK =< 1.0e-10,
	    TestK >= -1.0e-10
	->  Next = Sofar + Var
	;   % K =:= -1.0
	    TestK is K + 1.0,
	    TestK =< 1.0e-10,
	    TestK >= -1.0e-10
	->  Next = Sofar - Var
	;   % K < 0.0
	    K < -1.0e-10
	->  Ka is -K,
	    Next = Sofar - Ka*Var
	;   Next = Sofar + K*Var
	),
	hom2sum(Cs,Next,Term).
