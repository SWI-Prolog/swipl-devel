/*  

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


:- module(ineq_r,
	[
	    ineq/4,
	    ineq_one/4,
	    ineq_one_n_n_0/1,
	    ineq_one_n_p_0/1,
	    ineq_one_s_n_0/1,
	    ineq_one_s_p_0/1
	]).
:- use_module(bv_r,
	[
	    backsubst/3,
	    backsubst_delta/4,
	    basis_add/2,
	    dec_step/2,
	    deref/2,
	    determine_active_dec/1,
	    determine_active_inc/1,
	    export_binding/1,
	    get_or_add_class/2,
	    inc_step/2,
	    lb/3,
	    pivot_a/4,
	    rcbl_status/6,
	    reconsider/1,
	    same_class/2,
	    solve/1,
	    ub/3,
	    unconstrained/4,
	    var_intern/3,
	    var_with_def_intern/4
	]).
:- use_module(store_r,
	[
	    add_linear_11/3,
	    add_linear_ff/5,
	    normalize_scalar/2
	]).

% ineq(H,I,Nf,Strictness)
%
% Solves the inequality Nf < 0 or Nf =< 0 where Nf is in normal form
% and H and I are the homogene and inhomogene parts of Nf.

ineq([],I,_,Strictness) :- ineq_ground(Strictness,I).
ineq([v(K,[X^1])|Tail],I,Lin,Strictness) :-
	ineq_cases(Tail,I,Lin,Strictness,X,K).

ineq_cases([],I,_,Strictness,X,K) :-	% K*X + I < 0 or K*X + I =< 0
	ineq_one(Strictness,X,K,I).
ineq_cases([_|_],_,Lin,Strictness,_,_) :-
	deref(Lin,Lind),	% Id+Hd =< 0
	Lind = [Inhom,_|Hom],
	ineq_more(Hom,Inhom,Lind,Strictness).

% ineq_ground(Strictness,I)
%
% Checks whether a grounded inequality I < 0 or I =< 0 is satisfied.

ineq_ground(strict,I) :- I < -1.0e-10.		% I < 0
ineq_ground(nonstrict,I) :- I < 1.0e-10.	% I =< 0

% ineq_one(Strictness,X,K,I)
%
% Solves the inequality K*X + I < 0 or K*X + I =< 0

ineq_one(strict,X,K,I) :-
	(   K > 1.0e-10 % K > 0.0
	->  (   I >= -1.0e-10, % I =:= 0.0
		I =< 1.0e-10
	    ->  ineq_one_s_p_0(X)	% K*X < 0, K > 0 => X < 0
	    ;   Inhom is I/K,
		ineq_one_s_p_i(X,Inhom)	% K*X + I < 0, K > 0 => X + I/K < 0
	    )
	;   (   I >= -1.0e-10, % I =:= 0.0
		I =< 1.0e-10
	    ->  ineq_one_s_n_0(X)	% K*X < 0, K < 0 => -X < 0
	    ;   Inhom is -I/K,
		ineq_one_s_n_i(X,Inhom)	% K*X + I < 0, K < 0 => -X - I/K < 0
	    )
	).
ineq_one(nonstrict,X,K,I) :-
	(   K > 1.0e-10 % K > 0.0
	->  (   I >= -1.0e-10,	% I =:= 0
		I =< 1.0e-10
	    ->  ineq_one_n_p_0(X)	% K*X =< 0, K > 0 => X =< 0
	    ;   Inhom is I/K,
		ineq_one_n_p_i(X,Inhom)	% K*X + I =< 0, K > 0 => X + I/K =< 0
	    )
	;   (   I >= -1.0e-10,	% I =:= 0
		I =< 1.0e-10
	    ->  ineq_one_n_n_0(X)	% K*X =< 0, K < 0 => -X =< 0
	    ;   Inhom is -I/K,
		ineq_one_n_n_i(X,Inhom)	% K*X + I =< 0, K < 0 => -X - I/K =< 0
	    )
	).

% --------------------------- strict ----------------------------

% ineq_one_s_p_0(X)
%
% Solves the inequality X < 0

ineq_one_s_p_0(X) :-
	get_attr(X,itf,Att),
	arg(4,Att,lin([Ix,_|OrdX])),
	!, % old variable, this is deref
	(   \+ arg(1,Att,clpr)
	->  throw(error(permission_error('mix CLP(Q) variables with',
		'CLP(R) variables:',X),context(_)))
	;   ineq_one_old_s_p_0(OrdX,X,Ix)
	).
ineq_one_s_p_0(X) :-	% new variable, nothing depends on it
	var_intern(t_u(0.0),X,1). % put a strict inactive upperbound on the variable

% ineq_one_s_n_0(X)
%
% Solves the inequality X > 0

ineq_one_s_n_0(X) :-
	get_attr(X,itf,Att),
	arg(4,Att,lin([Ix,_|OrdX])),
	!,
	(   \+ arg(1,Att,clpr)
	->  throw(error(permission_error('mix CLP(Q) variables with',
		'CLP(R) variables:',X),context(_)))
	;   ineq_one_old_s_n_0(OrdX,X,Ix)
	).
ineq_one_s_n_0(X) :-
	var_intern(t_l(0.0),X,2). % puts a strict inactive lowerbound on the variable

% ineq_one_s_p_i(X,I)
%
% Solves the inequality X < -I

ineq_one_s_p_i(X,I) :-
	get_attr(X,itf,Att),
	arg(4,Att,lin([Ix,_|OrdX])),
	!,
	(   \+ arg(1,Att,clpr)
	->  throw(error(permission_error('mix CLP(Q) variables with',
		'CLP(R) variables:',X),context(_)))
	;   ineq_one_old_s_p_i(OrdX,I,X,Ix)
	).
ineq_one_s_p_i(X,I) :-
	Bound is -I,
	var_intern(t_u(Bound),X,1). % puts a strict inactive upperbound on the variable

% ineq_one_s_n_i(X,I)
%
% Solves the inequality X > I

ineq_one_s_n_i(X,I) :-
	get_attr(X,itf,Att),
	arg(4,Att,lin([Ix,_|OrdX])),
	!,
	(   \+ arg(1,Att,clpr)
	->  throw(error(permission_error('mix CLP(Q) variables with',
		'CLP(R) variables:',X),context(_)))
	;   ineq_one_old_s_n_i(OrdX,I,X,Ix)
	).
ineq_one_s_n_i(X,I) :- var_intern(t_l(I),X,2). % puts a strict inactive lowerbound on the variable

% ineq_one_old_s_p_0(Hom,X,Inhom)
%
% Solves the inequality X < 0 where X has linear equation Hom + Inhom

ineq_one_old_s_p_0([],_,Ix) :- Ix < -1.0e-10. % X = I: Ix < 0
ineq_one_old_s_p_0([l(Y*Ky,_)|Tail],X,Ix) :-
	(   Tail = [] % X = K*Y + I
	->  Bound is -Ix/Ky,
	    update_indep(strict,Y,Ky,Bound)	% X < 0, X = K*Y + I => Y < -I/K or Y > -I/K (depending on K)
	;   Tail = [_|_]
	->  get_attr(X,itf,Att),
	    arg(2,Att,type(Type)),
	    arg(3,Att,strictness(Old)),
	    arg(4,Att,lin(Lin)),
	    udus(Type,X,Lin,0.0,Old)	% update strict upperbound
	).

% ineq_one_old_s_p_0(Hom,X,Inhom)
%
% Solves the inequality X > 0 where X has linear equation Hom + Inhom

ineq_one_old_s_n_0([],_,Ix) :- Ix > 1.0e-10. % X = I: Ix > 0
ineq_one_old_s_n_0([l(Y*Ky,_)|Tail], X, Ix) :-
	(   Tail = []	% X = K*Y + I 
	->  Coeff is -Ky,
	    Bound is Ix/Coeff,
	    update_indep(strict,Y,Coeff,Bound)
	;   Tail = [_|_]
	->  get_attr(X,itf,Att),
	    arg(2,Att,type(Type)),
	    arg(3,Att,strictness(Old)),
	    arg(4,Att,lin(Lin)),
	    udls(Type,X,Lin,0.0,Old)	% update strict lowerbound
	).

% ineq_one_old_s_p_i(Hom,C,X,Inhom)
%
% Solves the inequality X + C < 0 where X has linear equation Hom + Inhom

ineq_one_old_s_p_i([],I,_,Ix) :- Ix + I < -1.0e-10. % X = I
ineq_one_old_s_p_i([l(Y*Ky,_)|Tail],I,X,Ix) :-
	(   Tail = []	% X = K*Y + I
	->  Bound is -(Ix + I)/Ky,
	    update_indep(strict,Y,Ky,Bound)
	;   Tail = [_|_]
	->  Bound is -I,
	    get_attr(X,itf,Att),
	    arg(2,Att,type(Type)),
	    arg(3,Att,strictness(Old)),
	    arg(4,Att,lin(Lin)),
	    udus(Type,X,Lin,Bound,Old)	% update strict upperbound
	).

% ineq_one_old_s_n_i(Hom,C,X,Inhom)
%
% Solves the inequality X  - C > 0 where X has linear equation Hom + Inhom

ineq_one_old_s_n_i([],I,_,Ix) :- -Ix + I < -1.0e-10. % X = I
ineq_one_old_s_n_i([l(Y*Ky,_)|Tail],I,X,Ix) :-
	(   Tail = []	% X = K*Y + I
	->  Coeff is -Ky,
	    Bound is (Ix - I)/Coeff,
	    update_indep(strict,Y,Coeff,Bound)
	;   Tail = [_|_]
	->  get_attr(X,itf,Att),
	    arg(2,Att,type(Type)),
	    arg(3,Att,strictness(Old)),
	    arg(4,Att,lin(Lin)),
	    udls(Type,X,Lin,I,Old)	% update strict lowerbound
	).

% -------------------------- nonstrict --------------------------

% ineq_one_n_p_0(X)
%
% Solves the inequality X =< 0

ineq_one_n_p_0(X) :-
	get_attr(X,itf,Att),
	arg(4,Att,lin([Ix,_|OrdX])),
	!,	% old variable, this is deref
	(   \+ arg(1,Att,clpr)
	->  throw(error(permission_error('mix CLP(Q) variables with',
		'CLP(R) variables:',X),context(_)))
	;   ineq_one_old_n_p_0(OrdX,X,Ix)
	).
ineq_one_n_p_0(X) :-	% new variable, nothing depends on it
	var_intern(t_u(0.0),X,0).	% nonstrict upperbound

% ineq_one_n_n_0(X)
%
% Solves the inequality X >= 0

ineq_one_n_n_0(X) :-
	get_attr(X,itf,Att),
	arg(4,Att,lin([Ix,_|OrdX])),
	!,
	(   \+ arg(1,Att,clpr)
	->  throw(error(permission_error('mix CLP(Q) variables with',
		'CLP(R) variables:',X),context(_)))
	;   ineq_one_old_n_n_0(OrdX,X,Ix)
	).
ineq_one_n_n_0(X) :-
	var_intern(t_l(0.0),X,0).	% nonstrict lowerbound

% ineq_one_n_p_i(X,I)
%
% Solves the inequality X =< -I

ineq_one_n_p_i(X,I) :-
	get_attr(X,itf,Att),
	arg(4,Att,lin([Ix,_|OrdX])),
	!,
	(   \+ arg(1,Att,clpr)
	->  throw(error(permission_error('mix CLP(Q) variables with',
		'CLP(R) variables:',X),context(_)))
	;   ineq_one_old_n_p_i(OrdX,I,X,Ix)
	).
ineq_one_n_p_i(X,I) :-
	Bound is -I,
	var_intern(t_u(Bound),X,0).	% nonstrict upperbound

% ineq_one_n_n_i(X,I)
%
% Solves the inequality X >= I

ineq_one_n_n_i(X,I) :-
	get_attr(X,itf,Att),
	arg(4,Att,lin([Ix,_|OrdX])),
	!,
	(   \+ arg(1,Att,clpr)
	->  throw(error(permission_error('mix CLP(Q) variables with',
		'CLP(R) variables:',X),context(_)))
	;   ineq_one_old_n_n_i(OrdX,I,X,Ix)
	).
ineq_one_n_n_i(X,I) :-
	var_intern(t_l(I),X,0).	% nonstrict lowerbound

% ineq_one_old_n_p_0(Hom,X,Inhom)
%
% Solves the inequality X =< 0 where X has linear equation Hom + Inhom

ineq_one_old_n_p_0([],_,Ix) :- Ix < 1.0e-10. % X =I
ineq_one_old_n_p_0([l(Y*Ky,_)|Tail],X,Ix) :-
	(   Tail = []	%  X = K*Y + I
	->  Bound is -Ix/Ky,	
	    update_indep(nonstrict,Y,Ky,Bound)
	;   Tail = [_|_]
	->  get_attr(X,itf,Att),
	    arg(2,Att,type(Type)),
	    arg(3,Att,strictness(Old)),
	    arg(4,Att,lin(Lin)),
	    udu(Type,X,Lin,0.0,Old)	% update nonstrict upperbound
	).

% ineq_one_old_n_n_0(Hom,X,Inhom)
%
% Solves the inequality X >= 0 where X has linear equation Hom + Inhom

ineq_one_old_n_n_0([],_,Ix) :- Ix > -1.0e-10.	% X = I
ineq_one_old_n_n_0([l(Y*Ky,_)|Tail], X, Ix) :-
	(   Tail = []	% X = K*Y + I
	->  Coeff is -Ky,
	    Bound is Ix/Coeff,
	    update_indep(nonstrict,Y,Coeff,Bound)
	;   Tail = [_|_]
	->  get_attr(X,itf,Att),
	    arg(2,Att,type(Type)),
	    arg(3,Att,strictness(Old)),
	    arg(4,Att,lin(Lin)),
	    udl(Type,X,Lin,0.0,Old)	% update nonstrict lowerbound
	).

% ineq_one_old_n_p_i(Hom,C,X,Inhom)
%
% Solves the inequality X  + C =< 0 where X has linear equation Hom + Inhom

ineq_one_old_n_p_i([],I,_,Ix) :- Ix + I < 1.0e-10.	% X = I
ineq_one_old_n_p_i([l(Y*Ky,_)|Tail],I,X,Ix) :-
	(   Tail = []	% X = K*Y + I
	->  Bound is -(Ix + I)/Ky,
	    update_indep(nonstrict,Y,Ky,Bound)
	;   Tail = [_|_]
	->  Bound is -I,
	    get_attr(X,itf,Att),
	    arg(2,Att,type(Type)),
	    arg(3,Att,strictness(Old)),
	    arg(4,Att,lin(Lin)),
	    udu(Type,X,Lin,Bound,Old)	% update nonstrict upperbound
	).

% ineq_one_old_n_n_i(Hom,C,X,Inhom)
%
% Solves the inequality X  - C >= 0 where X has linear equation Hom + Inhom

ineq_one_old_n_n_i([],I,_,Ix) :- -Ix + I < 1.0e-10. % X = I
ineq_one_old_n_n_i([l(Y*Ky,_)|Tail],I,X,Ix) :-
	(   Tail = []
	->  Coeff is -Ky,
	    Bound is (Ix - I)/Coeff,
	    update_indep(nonstrict,Y,Coeff,Bound)
	;   Tail = [_|_]
	->  get_attr(X,itf,Att),
	    arg(2,Att,type(Type)),
	    arg(3,Att,strictness(Old)),
	    arg(4,Att,lin(Lin)),
	    udl(Type,X,Lin,I,Old)
	).

% ---------------------------------------------------------------

% ineq_more(Hom,Inhom,Lin,Strictness)
%
% Solves the inequality Lin < 0 or Lin =< 0 with Lin = Hom + Inhom

ineq_more([],I,_,Strictness) :- ineq_ground(Strictness,I).	% I < 0 or I =< 0
ineq_more([l(X*K,_)|Tail],Id,Lind,Strictness) :-
	(   Tail = []
	->  % X*K < Id or X*K =< Id
	    % one var: update bound instead of slack introduction
	    get_or_add_class(X,_),	% makes sure X belongs to a class
 	    Bound is -Id/K,	
 	    update_indep(Strictness,X,K,Bound)	% new bound
   	;   Tail = [_|_]
	->  ineq_more(Strictness,Lind)
	).

% ineq_more(Strictness,Lin)
%
% Solves the inequality Lin < 0 or Lin =< 0

ineq_more(strict,Lind) :-
	(   unconstrained(Lind,U,K,Rest)
	->  % never fails, no implied value
	    % Lind < 0 => Rest < -K*U where U has no bounds
	    var_intern(t_l(0.0),S,2),	% create slack variable S
	    get_attr(S,itf,AttS),
	    arg(5,AttS,order(OrdS)),
	    Ki is -1.0/K,
	    add_linear_ff(Rest,Ki,[0.0,0.0,l(S*1.0,OrdS)],Ki,LinU),	% U = (-1/K)*Rest + (-1/K)*S 
	    LinU = [_,_|Hu],
 	    get_or_add_class(U,Class),
	    same_class(Hu,Class),	% put all variables of new lin. eq. of U in the same class
	    get_attr(U,itf,AttU),
	    arg(5,AttU,order(OrdU)),
	    arg(6,AttU,class(ClassU)),
	    backsubst(ClassU,OrdU,LinU)	% substitute U by new lin. eq. everywhere in the class
	;   var_with_def_intern(t_u(0.0),S,Lind,1),	% Lind < 0 => Lind = S with S < 0	
	    basis_add(S,_),			% adds S to the basis
	    determine_active_dec(Lind),		% activate bounds
	    reconsider(S)			% reconsider basis
	).
ineq_more(nonstrict,Lind) :-
	(   unconstrained(Lind,U,K,Rest)
	->  % never fails, no implied value
	    % Lind =< 0 => Rest =< -K*U where U has no bounds
	    var_intern(t_l(0.0),S,0),	% create slack variable S
	    Ki is -1.0/K,
	    get_attr(S,itf,AttS),
	    arg(5,AttS,order(OrdS)),
	    add_linear_ff(Rest,Ki,[0.0,0.0,l(S*1.0,OrdS)],Ki,LinU),	% U = (-1K)*Rest + (-1/K)*S
	    LinU = [_,_|Hu],
	    get_or_add_class(U,Class),
	    same_class(Hu,Class),	% put all variables of new lin. eq of U in the same class
	    get_attr(U,itf,AttU),
	    arg(5,AttU,order(OrdU)),
	    arg(6,AttU,class(ClassU)),
	    backsubst(ClassU,OrdU,LinU)	% substitute U by new lin. eq. everywhere in the class
	;   % all variables are constrained
	    var_with_def_intern(t_u(0.0),S,Lind,0),	% Lind =< 0 => Lind = S with S =< 0
	    basis_add(S,_),				% adds S to the basis
	    determine_active_dec(Lind),
	    reconsider(S)
	).


% update_indep(Strictness,X,K,Bound)
%
% Updates the bound of independent variable X where X < Bound or X =< Bound
% or X > Bound or X >= Bound, depending on Strictness and K. 

update_indep(strict,X,K,Bound) :-
	get_attr(X,itf,Att),
	arg(2,Att,type(Type)),
	arg(3,Att,strictness(Old)),
	arg(4,Att,lin(Lin)),
	(   K < -1.0e-10
	->  uils(Type,X,Lin,Bound,Old)	% update independent lowerbound strict
	;   uius(Type,X,Lin,Bound,Old)	% update independent upperbound strict
	).
update_indep(nonstrict,X,K,Bound) :-
	get_attr(X,itf,Att),
	arg(2,Att,type(Type)),
	arg(3,Att,strictness(Old)),
	arg(4,Att,lin(Lin)),
	(   K < -1.0e-10
	->  uil(Type,X,Lin,Bound,Old)	% update independent lowerbound nonstrict
	;   uiu(Type,X,Lin,Bound,Old)	% update independent upperbound nonstrict
	).


% ---------------------------------------------------------------------------------------

%
% Update a bound on a var xi
%
%   a) independent variable
%
%	a1) update inactive bound: done
%
%	a2) update active bound:
%	    Determine [lu]b including most constraining row R
%	      If we are within: done
%	    else pivot(R,xi) and introduce bound via (b)
%
%	a3) introduce a bound on an unconstrained var:
%	    All vars that depend on xi are unconstrained (invariant) ->
%	      the bound cannot invalidate any Lhs
%
%   b) dependent variable
%
%	repair upper or lower (maybe just swap with an unconstrained var from Rhs)
%

%
% Sign = 1,0,-1 means inside,at,outside
%

% Read following predicates as update (dependent/independent) (lowerbound/upperbound) (strict)

% udl(Type,X,Lin,Bound,Strict)
%
% Updates lower bound of dependent variable X with linear equation
% Lin that had type Type and strictness Strict, to the new non-strict
% bound Bound.

udl(t_none,X,Lin,Bound,_Sold) :-
	get_attr(X,itf,AttX),
	arg(5,AttX,order(Ord)),
	setarg(2,AttX,type(t_l(Bound))),
	setarg(3,AttX,strictness(0)),
	(   unconstrained(Lin,Uc,Kuc,Rest)
	->  % X = Lin => -1/K*Rest + 1/K*X = U where U has no bounds
	    Ki is -1.0/Kuc,
	    add_linear_ff(Rest,Ki,[0.0,0.0,l(X* -1.0,Ord)],Ki,LinU),
	    get_attr(Uc,itf,AttU),
	    arg(5,AttU,order(OrdU)),
	    arg(6,AttU,class(Class)),
	    backsubst(Class,OrdU,LinU)
	;   % no unconstrained variables in Lin: make X part of basis and reconsider
	    basis_add(X,_),
	    determine_active_inc(Lin),
	    reconsider(X)
	).
udl(t_l(L),X,Lin,Bound,Sold) :-
	TestBL is Bound - L,
	(   TestBL < -1.0e-10
	->  true	% new bound is smaller than old one: keep old
	;   TestBL > 1.0e-10
	->  % new bound is larger than old one: use new and reconsider basis
	    Strict is Sold /\ 1,
	    get_attr(X,itf,Att),
	    setarg(2,Att,type(t_l(Bound))),
	    setarg(3,Att,strictness(Strict)),
	    reconsider_lower(X,Lin,Bound)	% makes sure that Lin still satisfies lowerbound Bound
	;   true	% new bound is equal to old one, new one is nonstrict: keep old
	).	
		
udl(t_u(U),X,Lin,Bound,_Sold) :-
	TestUB is U - Bound,
	(   TestUB < -1.0e-10
	->  fail	% new bound is larger than upperbound: fail
	;   TestUB > 1.0e-10
	->  % new bound is smaller than upperbound: add new and reconsider basis
	    get_attr(X,itf,Att),
	    setarg(2,Att,type(t_lu(Bound,U))),
	    reconsider_lower(X,Lin,Bound)	% makes sure that Lin still satisfies lowerbound Bound
	;   solve_bound(Lin,Bound)	% new bound is equal to upperbound: solve
	).	
udl(t_lu(L,U),X,Lin,Bound,Sold) :-
	TestBL is Bound - L,
	(   TestBL < -1.0e-10
	->  true	% smaller than lowerbound: keep 
	;   TestBL > 1.0e-10
	->  % larger than lowerbound: check upperbound
	    TestUB is U - Bound,
	    (   TestUB < -1.0e-10
	    ->  fail	% larger than upperbound: fail
	    ;   TestUB > 1.0e-10
	    ->  % smaller than upperbound: use new and reconsider basis
		Strict is Sold /\ 1,
		get_attr(X,itf,Att),
		setarg(2,Att,type(t_lu(Bound,U))),
		setarg(3,Att,strictness(Strict)),
		reconsider_lower(X,Lin,Bound)
	    ;   % equal to upperbound: if strictness matches => solve
		Sold /\ 1 =:= 0,
		solve_bound(Lin,Bound)
	    )
	;   true	% equal to lowerbound and nonstrict: keep
	).
	
% udls(Type,X,Lin,Bound,Strict)
%
% Updates lower bound of dependent variable X with linear equation
% Lin that had type Type and strictness Strict, to the new strict
% bound Bound.

udls(t_none,X,Lin,Bound,_Sold) :-
	get_attr(X,itf,AttX),
	arg(5,AttX,order(Ord)),
	setarg(2,AttX,type(t_l(Bound))),
	setarg(3,AttX,strictness(2)),
	(   unconstrained(Lin,Uc,Kuc,Rest)
	->  % X = Lin => U = -1/K*Rest + 1/K*X with U an unconstrained variable
	    Ki is -1.0/Kuc,
	    add_linear_ff(Rest,Ki,[0.0,0.0,l(X* -1.0,Ord)],Ki,LinU),
	    get_attr(Uc,itf,AttU),
	    arg(5,AttU,order(OrdU)),
	    arg(6,AttU,class(Class)),
	    backsubst(Class,OrdU,LinU)
	;   % no unconstrained variables: add X to basis and reconsider basis
	    basis_add(X,_),
	    determine_active_inc(Lin),
	    reconsider(X)
	).
udls(t_l(L),X,Lin,Bound,Sold) :-
	TestBL is Bound - L,
	(   TestBL < -1.0e-10
	->  true	% smaller than lowerbound: keep
	;   TestBL > 1.0e-10
	->  % larger than lowerbound: use new and reconsider basis
	    Strict is Sold \/ 2,
	    get_attr(X,itf,Att),
	    setarg(2,Att,type(t_l(Bound))),
	    setarg(3,Att,strictness(Strict)),
	    reconsider_lower(X,Lin,Bound)
	;   % equal to lowerbound: check strictness
	    Strict is Sold \/ 2,
	    get_attr(X,itf,Att),
	    arg(3,Att,strictness(Strict))
	).
udls(t_u(U),X,Lin,Bound,Sold) :-
	U - Bound > 1.0e-10,	% smaller than upperbound: set new bound
	Strict is Sold \/ 2,
	get_attr(X,itf,Att),
	setarg(2,Att,type(t_lu(Bound,U))),
	setarg(3,Att,strictness(Strict)),
	reconsider_lower(X,Lin,Bound).
udls(t_lu(L,U),X,Lin,Bound,Sold) :-
	TestBL is Bound - L,
	(   TestBL < -1.0e-10
	->  true	% smaller than lowerbound: keep
	;   TestBL > 1.0e-10
	->  % larger than lowerbound: check upperbound and possibly use new and reconsider basis
	    U - Bound > 1.0e-10,
	    Strict is Sold \/ 2,
	    get_attr(X,itf,Att),
	    setarg(2,Att,type(t_lu(Bound,U))),
	    setarg(3,Att,strictness(Strict)),
	    reconsider_lower(X,Lin,Bound)
	;   % equal to lowerbound: put new strictness
	    Strict is Sold \/ 2,
	    get_attr(X,itf,Att),
	    setarg(3,Att,strictness(Strict))
	).

% udu(Type,X,Lin,Bound,Strict)
%
% Updates upper bound of dependent variable X with linear equation
% Lin that had type Type and strictness Strict, to the new non-strict
% bound Bound.

udu(t_none,X,Lin,Bound,_Sold) :-
	get_attr(X,itf,AttX),
	arg(5,AttX,order(Ord)),
	setarg(2,AttX,type(t_u(Bound))),
	setarg(3,AttX,strictness(0)),
	(   unconstrained(Lin,Uc,Kuc,Rest)
	->  % X = Lin => U = -1/K*Rest + 1/K*X with U an unconstrained variable
	    Ki is -1.0/Kuc,
	    add_linear_ff(Rest,Ki,[0.0,0.0,l(X* -1.0,Ord)],Ki,LinU),
	    get_attr(Uc,itf,AttU),
	    arg(5,AttU,order(OrdU)),
	    arg(6,AttU,class(Class)),
	    backsubst(Class,OrdU,LinU)
	;   % no unconstrained variables: add X to basis and reconsider basis
	    basis_add(X,_),
	    determine_active_dec(Lin),	% try to lower R
	    reconsider(X)
	).
udu(t_u(U),X,Lin,Bound,Sold) :-
	TestUB is U - Bound,
	(   TestUB < -1.0e-10
	->  true	% larger than upperbound: keep
	;   TestUB > 1.0e-10
	->  % smaller than upperbound: update and reconsider basis
	    Strict is Sold /\ 2,
	    get_attr(X,itf,Att),
	    setarg(2,Att,type(t_u(Bound))),
	    setarg(3,Att,strictness(Strict)),
	    reconsider_upper(X,Lin,Bound)
	;   true	% equal to upperbound and nonstrict: keep
	).
udu(t_l(L),X,Lin,Bound,_Sold) :-
	TestBL is Bound - L,
	(   TestBL < -1.0e-10
	->  fail	% smaller than lowerbound: fail
	;   TestBL > 1.0e-10
	->  % larger than lowerbound: use new and reconsider basis
	    get_attr(X,itf,Att),
	    setarg(2,Att,type(t_lu(L,Bound))),
	    reconsider_upper(X,Lin,Bound)
	;   solve_bound(Lin,Bound)	% equal to lowerbound: solve
	).
udu(t_lu(L,U),X,Lin,Bound,Sold) :-
	TestUB is U - Bound,
	(   TestUB < -1.0e-10
	->  true	% larger than upperbound: keep
	;   TestUB > 1.0e-10
	->  % smaller than upperbound: check lowerbound
	    TestBL is Bound - L,
	    (   TestBL < -1.0e-10
	    ->  fail	% smaller than lowerbound: fail
	    ;   TestBL > 1.0e-10
	    ->  % larger than lowerbound: update and reconsider basis
		Strict is Sold /\ 2,
		get_attr(X,itf,Att),
		setarg(2,Att,type(t_lu(L,Bound))),
		setarg(3,Att,strictness(Strict)),
		reconsider_upper(X,Lin,Bound)
	    ;   % equal to lowerbound: check strictness and possibly solve
		Sold /\ 2 =:= 0,
		solve_bound(Lin,Bound)
	    )
	;   true	% equal to upperbound and nonstrict: keep
	).

% udus(Type,X,Lin,Bound,Strict)
%
% Updates upper bound of dependent variable X with linear equation
% Lin that had type Type and strictness Strict, to the new strict
% bound Bound.

udus(t_none,X,Lin,Bound,_Sold) :-
	get_attr(X,itf,AttX),
	arg(5,AttX,order(Ord)),
	setarg(2,AttX,type(t_u(Bound))),
	setarg(3,AttX,strictness(1)),
	(   unconstrained(Lin,Uc,Kuc,Rest)
	->   % X = Lin => U = -1/K*Rest + 1/K*X with U an unconstrained variable
	    Ki is -1.0/Kuc,
	    add_linear_ff(Rest,Ki,[0.0,0.0,l(X* -1.0,Ord)],Ki,LinU),
	    get_attr(Uc,itf,AttU),
	    arg(5,AttU,order(OrdU)),
	    arg(6,AttU,class(Class)),
	    backsubst(Class,OrdU,LinU)
	;   % no unconstrained variables: add X to basis and reconsider basis
	    basis_add(X,_),
	    determine_active_dec(Lin),
	    reconsider(X)
	).
udus(t_u(U),X,Lin,Bound,Sold) :-
	TestUB is U - Bound,
	(   TestUB < -1.0e-10
	->  true	% larger than upperbound: keep
	;   TestUB > 1.0e-10
	->  % smaller than upperbound: update bound and reconsider basis
	    Strict is Sold \/ 1,
	    get_attr(X,itf,Att),
	    setarg(2,Att,type(t_u(Bound))),
	    setarg(3,Att,strictness(Strict)),
	    reconsider_upper(X,Lin,Bound)
	;   % equal to upperbound: set new strictness
	    Strict is Sold \/ 1,
	    get_attr(X,itf,Att),
	    setarg(3,Att,strictness(Strict))
	).
udus(t_l(L),X,Lin,Bound,Sold) :-
	Bound - L > 1.0e-10,	% larger than lowerbound: update and reconsider basis
	Strict is Sold \/ 1,
	get_attr(X,itf,Att),
	setarg(2,Att,type(t_lu(L,Bound))),
	setarg(3,Att,strictness(Strict)),
	reconsider_upper(X,Lin,Bound).
udus(t_lu(L,U),X,Lin,Bound,Sold) :-
	TestUB is U - Bound,
	(   TestUB < -1.0e-10
	->  true	% larger than upperbound: keep
	;   TestUB > 1.0e-10
	->  % smaller than upperbound: check lowerbound, possibly update and reconsider basis
	    Bound - L > 1.0e-10,
	    Strict is Sold \/ 1,
	    get_attr(X,itf,Att),
	    setarg(2,Att,type(t_lu(L,Bound))),
	    setarg(3,Att,strictness(Strict)),
	    reconsider_upper(X,Lin,Bound)		
	;   % equal to upperbound: update strictness
	    Strict is Sold \/ 1,
	    get_attr(X,itf,Att),
	    setarg(3,Att,strictness(Strict))
	).

% uiu(Type,X,Lin,Bound,Strict)
%
% Updates upper bound of independent variable X with linear equation
% Lin that had type Type and strictness Strict, to the new non-strict
% bound Bound.

uiu(t_none,X,_Lin,Bound,_) :-	% X had no bounds
	get_attr(X,itf,Att),
	setarg(2,Att,type(t_u(Bound))),
	setarg(3,Att,strictness(0)).
uiu(t_u(U),X,_Lin,Bound,Sold) :-
	TestUB is U - Bound,
	(   TestUB < -1.0e-10
	->  true	% larger than upperbound: keep
	;   TestUB > 1.0e-10
	->  % smaller than upperbound: update.
	    Strict is Sold /\ 2,	% update strictness: strictness of lowerbound is kept,
	    				% strictness of upperbound is set to non-strict
	    get_attr(X,itf,Att),
	    setarg(2,Att,type(t_u(Bound))),
	    setarg(3,Att,strictness(Strict))
	;   true	% equal to upperbound and nonstrict: keep 
	).
uiu(t_l(L),X,Lin,Bound,_Sold) :-
	TestBL is Bound - L,
	(   TestBL < -1.0e-10
	->  fail	% Lowerbound was smaller than new upperbound: fail
    	;   TestBL > 1.0e-10
	->   % Upperbound is larger than lowerbound: store new bound
	    get_attr(X,itf,Att),
	    setarg(2,Att,type(t_lu(L,Bound)))
	;   solve_bound(Lin,Bound) % Lowerbound was equal to new upperbound: solve
	).
uiu(t_L(L),X,Lin,Bound,_Sold) :-
	TestBL is Bound - L,
	(   TestBL < -1.0e-10
	->  fail	% Same as for t_l
	;   TestBL > 1.0e-10
	->  % Same as for t_l (new bound becomes t_Lu)
	    get_attr(X,itf,Att),
	    setarg(2,Att,type(t_Lu(L,Bound)))
	;   solve_bound(Lin,Bound)	% Same as for t_l
	).
uiu(t_lu(L,U),X,Lin,Bound,Sold) :-
	TestUB is U - Bound,
	(   TestUB < -1.0e-10
	->  true	% Upperbound was smaller than new bound: keep
	;   TestUB > 1.0e-10
	->  TestBL is Bound - L,	% Upperbound was larger than new bound: check lowerbound
	    (   TestBL < -1.0e-10
	    ->	fail	% Lowerbound was larger than new bound: fail
	    ;   TestBL > 1.0e-10
	    ->  % Lowerbound was smaller than new bound: store new bound
		Strict is Sold /\ 2,
		get_attr(X,itf,Att),
		setarg(2,Att,type(t_lu(L,Bound))),
		setarg(3,Att,strictness(Strict))
	    ;	% Lowerbound was equal to new bound: solve
		Sold /\ 2 =:= 0,	% Only solve when strictness matches
		solve_bound(Lin,Bound)
	    )
	;   true	% Upperbound was equal to new bound and new bound non-strict: keep
	).
uiu(t_Lu(L,U),X,Lin,Bound,Sold) :-	% See t_lu case
	TestUB is U - Bound,
	(   TestUB < -1.0e-10
	->  true
	;   TestUB > 1.0e-10
	->  TestBL is Bound - L,
	    (   TestBL < -1.0e-10
	    ->  fail
	    ;   TestBL > 1.0e-10
	    ->  Strict is Sold /\ 2,
		get_attr(X,itf,Att),
		setarg(2,Att,type(t_Lu(L,Bound))),
		setarg(3,Att,strictness(Strict))
	    ;   Sold /\ 2 =:= 0,
		solve_bound(Lin,Bound)
	    )
	;   true
	).
uiu(t_U(U),X,_Lin,Bound,Sold) :-
	TestUB is U - Bound,
	(   TestUB < -1.0e-10
	->  true	% larger than upperbound: keep
	;   TestUB > 1.0e-10
	->  % smaller than active upperbound: check how much active upperbound can be lowered.
	    % if enough, just lower bound, otherwise update the bound, make X dependent and reconsider basis 
	    Strict is Sold /\ 2,
	    (   get_attr(X,itf,Att),
		arg(5,Att,order(OrdX)),
		arg(6,Att,class(ClassX)),
		lb(ClassX,OrdX,Vlb-Vb-Lb),
		Bound - (Lb + U) < 1.0e-10
	    ->  get_attr(X,itf,Att2), % changed?
		setarg(2,Att2,type(t_U(Bound))),
		setarg(3,Att2,strictness(Strict)),
		pivot_a(Vlb,X,Vb,t_u(Bound)),
		reconsider(X)
	    ;   get_attr(X,itf,Att),
		arg(5,Att,order(OrdX)),
		arg(6,Att,class(ClassX)),
		setarg(2,Att,type(t_U(Bound))),
		setarg(3,Att,strictness(Strict)),
		Delta is Bound - U,
		backsubst_delta(ClassX,OrdX,X,Delta)
	    )
	;   true	% equal to upperbound and non-strict: keep
	).
uiu(t_lU(L,U),X,Lin,Bound,Sold) :-
	TestUB is U - Bound,
	(   TestUB < -1.0e-10
	->  true	% larger than upperbound: keep
	;   TestUB > 1.0e-10
	->  TestBL is Bound-L,
	    (   TestBL < -1.0e-10
	    ->  fail	% smaller than lowerbound: fail
	    ;   TestBL > 1.0e-10
	    ->  % larger than lowerbound: see t_U case for rest
		Strict is Sold /\ 2,
		(   get_attr(X,itf,Att),
		    arg(5,Att,order(OrdX)),
		    arg(6,Att,class(ClassX)),
		    lb(ClassX,OrdX,Vlb-Vb-Lb),
		    Bound - (Lb + U) < 1.0e-10
		->  get_attr(X,itf,Att2), % changed?
		    setarg(2,Att2,type(t_lU(L,Bound))),
		    setarg(3,Att2,strictness(Strict)),
		    pivot_a(Vlb,X,Vb,t_lu(L,Bound)),
		    reconsider(X)
		;   get_attr(X,itf,Att),
		    arg(5,Att,order(OrdX)),
		    arg(6,Att,class(ClassX)),
		    setarg(2,Att,type(t_lU(L,Bound))),
		    setarg(3,Att,strictness(Strict)),
		    Delta is Bound - U,
		    backsubst_delta(ClassX,OrdX,X,Delta)
		)
	    ;	% equal to lowerbound: check strictness and solve
		Sold /\ 2 =:= 0,
		solve_bound(Lin,Bound)
	    )
	;   true	% equal to upperbound and non-strict: keep
			% smaller than upperbound: check lowerbound
	).

% uius(Type,X,Lin,Bound,Strict)
%
% Updates upper bound of independent variable X with linear equation
% Lin that had type Type and strictness Strict, to the new strict
% bound Bound. (see also uiu/5)

uius(t_none,X,_Lin,Bound,_Sold) :-
	get_attr(X,itf,Att),
	setarg(2,Att,type(t_u(Bound))),
	setarg(3,Att,strictness(1)).
uius(t_u(U),X,_Lin,Bound,Sold) :-
	TestUB is U - Bound,
	(   TestUB < -1.0e-10
	->  true
	;   TestUB > 1.0e-10
	->  Strict is Sold \/ 1,
	    get_attr(X,itf,Att),
	    setarg(2,Att,type(t_u(Bound))),
	    setarg(3,Att,strictness(Strict))
	;   Strict is Sold \/ 1,
	    get_attr(X,itf,Att),
	    setarg(3,Att,strictness(Strict))
	).
uius(t_l(L),X,_Lin,Bound,Sold) :-
	Bound - L > 1.0e-10,
	Strict is Sold \/ 1,
	get_attr(X,itf,Att),
	setarg(2,Att,type(t_lu(L,Bound))),
	setarg(3,Att,strictness(Strict)).
uius(t_L(L),X,_Lin,Bound,Sold) :-
	Bound - L > 1.0e-10,
	Strict is Sold \/ 1,
	get_attr(X,itf,Att),
	setarg(2,Att,type(t_Lu(L,Bound))),
	setarg(3,Att,strictness(Strict)).
uius(t_lu(L,U),X,_Lin,Bound,Sold) :-
	TestUB is U - Bound,
	(   TestUB < -1.0e-10
	->  true
	;   TestUB > 1.0e-10
	->  Bound - L > 1.0e-10,
	    Strict is Sold \/ 1,
	    get_attr(X,itf,Att),
	    setarg(2,Att,type(t_lu(L,Bound))),
	    setarg(3,Att,strictness(Strict))
	;   Strict is Sold \/ 1,
	    get_attr(X,itf,Att),
	    setarg(3,Att,strictness(Strict))
	).
uius(t_Lu(L,U),X,_Lin,Bound,Sold) :-
	TestUB is U - Bound,
	(   TestUB < -1.0e-10
	->  true
	;   TestUB > 1.0e-10
	->  Bound - L > 1.0e-10,
	    Strict is Sold \/ 1,
	    get_attr(X,itf,Att),
	    setarg(2,Att,type(t_Lu(L,Bound))),
	    setarg(3,Att,strictness(Strict))
	;   Strict is Sold \/ 1,
	    get_attr(X,itf,Att),
	    setarg(3,Att,strictness(Strict))
	).
uius(t_U(U),X,_Lin,Bound,Sold) :-
	TestUB is U - Bound,
	(   TestUB < -1.0e-10
	->  true
	;   TestUB > 1.0e-10
	->  Strict is Sold \/ 1,
	    (   get_attr(X,itf,Att),
		arg(5,Att,order(OrdX)),
		arg(6,Att,class(ClassX)),
		lb(ClassX,OrdX,Vlb-Vb-Lb),
		Bound - (Lb + U) < 1.0e-10
	    ->  get_attr(X,itf,Att2), % changed?
		setarg(2,Att2,type(t_U(Bound))),
		setarg(3,Att2,strictness(Strict)),
		pivot_a(Vlb,X,Vb,t_u(Bound)),
		reconsider(X)
	    ;   get_attr(X,itf,Att),
		arg(5,Att,order(OrdX)),
		arg(6,Att,class(ClassX)),
		setarg(2,Att,type(t_U(Bound))),
		setarg(3,Att,strictness(Strict)),
		Delta is Bound - U,
		backsubst_delta(ClassX,OrdX,X,Delta)
	    )
	;   Strict is Sold \/ 1,
	    get_attr(X,itf,Att),
	    setarg(3,Att,strictness(Strict))
	).
uius(t_lU(L,U),X,_Lin,Bound,Sold) :-
	TestUB is U - Bound,
	(   TestUB < -1.0e-10
	->  true
	;   TestUB > 1.0e-10
	->  Bound - L > 1.0e-10,
	    Strict is Sold \/ 1,
	    (   get_attr(X,itf,Att),
		arg(5,Att,order(OrdX)),
		arg(6,Att,class(ClassX)),
		lb(ClassX,OrdX,Vlb-Vb-Lb),
		Bound - (Lb + U) < 1.0e-10
	    ->  get_attr(X,itf,Att2), % changed?
		setarg(2,Att2,type(t_lU(L,Bound))),
		setarg(3,Att2,strictness(Strict)),
		pivot_a(Vlb,X,Vb,t_lu(L,Bound)),
		reconsider(X)
	    ;	get_attr(X,itf,Att),
		arg(5,Att,order(OrdX)),
		arg(6,Att,class(ClassX)),
		setarg(2,Att,type(t_lU(L,Bound))),
		setarg(3,Att,strictness(Strict)),
		Delta is Bound - U,
		backsubst_delta(ClassX,OrdX,X,Delta)
	    )
	;   Strict is Sold \/ 1,
	    get_attr(X,itf,Att),
	    setarg(3,Att,strictness(Strict))
	).

% uil(Type,X,Lin,Bound,Strict)
%
% Updates lower bound of independent variable X with linear equation
% Lin that had type Type and strictness Strict, to the new non-strict
% bound Bound. (see also uiu/5)


uil(t_none,X,_Lin,Bound,_Sold) :-
	get_attr(X,itf,Att),
	setarg(2,Att,type(t_l(Bound))),
	setarg(3,Att,strictness(0)).
uil(t_l(L),X,_Lin,Bound,Sold) :-
	TestBL is Bound - L,
	(   TestBL < -1.0e-10
	->  true
	;   TestBL > 1.0e-10
	->  Strict is Sold /\ 1,
	    get_attr(X,itf,Att),
	    setarg(2,Att,type(t_l(Bound))),
	    setarg(3,Att,strictness(Strict))
	;   true
	).
uil(t_u(U),X,Lin,Bound,_Sold) :-
	TestUB is U - Bound,
	(   TestUB < -1.0e-10
	->  fail
	;   TestUB > 1.0e-10
	->  get_attr(X,itf,Att),
	    setarg(2,Att,type(t_lu(Bound,U)))
	;   solve_bound(Lin,Bound)
	).
uil(t_U(U),X,Lin,Bound,_Sold) :-
	TestUB is U - Bound,
	(   TestUB < -1.0e-10
	->  fail
	;   TestUB > 1.0e-10
	->  get_attr(X,itf,Att),
	    setarg(2,Att,type(t_lU(Bound,U)))
	;   solve_bound(Lin,Bound)
	).
uil(t_lu(L,U),X,Lin,Bound,Sold) :-
	TestBL is Bound - L,
	(   TestBL < -1.0e-10
	->  true
	;   TestBL > 1.0e-10
	->  TestUB is U - Bound,
	    (   TestUB < -1.0e-10
	    ->  fail
	    ;   TestUB > 1.0e-10
	    ->  Strict is Sold /\ 1,
		get_attr(X,itf,Att),
		setarg(2,Att,type(t_lu(Bound,U))),
		setarg(3,Att,strictness(Strict))
	    ;   Sold /\ 1 =:= 0,
		solve_bound(Lin,Bound)
	    )
	;   true
	).
uil(t_lU(L,U),X,Lin,Bound,Sold) :-
	TestBL is Bound - L,
	(   TestBL < -1.0e-10
	->  true
	;   TestBL > 1.0e-10
	->  TestUB is U - Bound,
	    (   TestUB < -1.0e-10
	    ->  fail
	    ;   TestUB > 1.0e-10
	    ->  Strict is Sold /\ 1,
		get_attr(X,itf,Att),
		setarg(2,Att,type(t_lU(Bound,U))),
		setarg(3,Att,strictness(Strict))
	    ;   Sold /\ 1 =:= 0,
		solve_bound(Lin,Bound)
	    )
	;   true
	).
uil(t_L(L),X,_Lin,Bound,Sold) :-
	TestBL is Bound - L,
	(   TestBL < -1.0e-10
	->  true
	;   TestBL > 1.0e-10
	->  Strict is Sold /\ 1,
	    (   get_attr(X,itf,Att),
		arg(5,Att,order(OrdX)),
		arg(6,Att,class(ClassX)),
		ub(ClassX,OrdX,Vub-Vb-Ub),
		Bound - (Ub + L) > -1.0e-10
	    ->  get_attr(X,itf,Att2), % changed?
		setarg(2,Att2,type(t_L(Bound))),
		setarg(3,Att2,strictness(Strict)),
		pivot_a(Vub,X,Vb,t_l(Bound)),
		reconsider(X)
	    ;   get_attr(X,itf,Att),
		arg(5,Att,order(OrdX)),
		arg(6,Att,class(ClassX)),
		setarg(2,Att,type(t_L(Bound))),
		setarg(3,Att,strictness(Strict)),
		Delta is Bound - L,
		backsubst_delta(ClassX,OrdX,X,Delta)
	    )
	;   true
	).
uil(t_Lu(L,U),X,Lin,Bound,Sold) :-
	TestBL is Bound - L,
	(   TestBL < -1.0e-10
	->  true
	;   TestBL > 1.0e-10
	->  TestUB is U - Bound,
	    (   TestUB < -1.0e-10
	    ->  fail
	    ;   TestUB > 1.0e-10
	    ->  Strict is Sold /\ 1,
		(   get_attr(X,itf,Att),
		    arg(5,Att,order(OrdX)),
		    arg(6,Att,class(ClassX)),
		    ub(ClassX,OrdX,Vub-Vb-Ub),
		    Bound - (Ub + L) > -1.0e-10
		->  get_attr(X,itf,Att2), % changed?
		    setarg(2,Att2,t_Lu(Bound,U)),
		    setarg(3,Att2,strictness(Strict)),
		    pivot_a(Vub,X,Vb,t_lu(Bound,U)),
		    reconsider(X)
		;   get_attr(X,itf,Att),
		    arg(5,Att,order(OrdX)),
		    arg(6,Att,class(ClassX)),
		    setarg(2,Att,type(t_Lu(Bound,U))),
		    setarg(3,Att,strictness(Strict)),
		    Delta is Bound - L,
		    backsubst_delta(ClassX,OrdX,X,Delta)
		)
	    ;	Sold /\ 1 =:= 0,
		solve_bound(Lin,Bound)
	    )
	;   true
	).

% uils(Type,X,Lin,Bound,Strict)
%
% Updates lower bound of independent variable X with linear equation
% Lin that had type Type and strictness Strict, to the new strict
% bound Bound. (see also uiu/5)

uils(t_none,X,_Lin,Bound,_Sold) :-
	get_attr(X,itf,Att),
	setarg(2,Att,type(t_l(Bound))),
	setarg(3,Att,strictness(2)).
uils(t_l(L),X,_Lin,Bound,Sold) :-
	TestBL is Bound - L,
	(   TestBL < -1.0e-10
	->  true
	;   TestBL > 1.0e-10
	->  Strict is Sold \/ 2,
	    get_attr(X,itf,Att),
	    setarg(2,Att,type(t_l(Bound))),
	    setarg(3,Att,strictness(Strict))
	;   Strict is Sold \/ 2,
	    get_attr(X,itf,Att),
	    setarg(3,Att,strictness(Strict))
	).
uils(t_u(U),X,_Lin,Bound,Sold) :-
	U - Bound > 1.0e-10,
	Strict is Sold \/ 2,
	get_attr(X,itf,Att),
	setarg(2,Att,type(t_lu(Bound,U))),
	setarg(3,Att,strictness(Strict)).
uils(t_U(U),X,_Lin,Bound,Sold) :-
	U - Bound > 1.0e-10,
	Strict is Sold \/ 2,
	get_attr(X,itf,Att),
	setarg(2,Att,type(t_lU(Bound,U))),
	setarg(3,Att,strictness(Strict)).
uils(t_lu(L,U),X,_Lin,Bound,Sold) :-
	TestBL is Bound - L,
	(   TestBL < -1.0e-10
	->  true
	;   TestBL > 1.0e-10
	->  U - Bound > 1.0e-10,
	    Strict is Sold \/ 2,
	    get_attr(X,itf,Att),
	    setarg(2,Att,type(t_lu(Bound,U))),
	    setarg(3,Att,strictness(Strict))
	;   Strict is Sold \/ 2,
	    get_attr(X,itf,Att),
	    setarg(3,Att,strictness(Strict))
	).
uils(t_lU(L,U),X,_Lin,Bound,Sold) :-
	TestBL is Bound - L,
	(   TestBL < -1.0e-10
	->  true
	;   TestBL > 1.0e-10
	->  U - Bound > 1.0e-10,
	    Strict is Sold \/ 2,
	    get_attr(X,itf,Att),
	    setarg(2,Att,type(t_lU(Bound,U))),
	    setarg(3,Att,strictness(Strict))
	;   Strict is Sold \/ 2,
	    get_attr(X,itf,Att),
	    setarg(3,Att,strictness(Strict))
	).
uils(t_L(L),X,_Lin,Bound,Sold) :-
	TestBL is Bound - L,
	(   TestBL < -1.0e-10
	->  true
	;   TestBL > 1.0e-10
	->  Strict is Sold \/ 2,
	    (   get_attr(X,itf,Att),
		arg(5,Att,order(OrdX)),
		arg(6,Att,class(ClassX)),
		ub(ClassX,OrdX,Vub-Vb-Ub),
		Bound - (Ub + L) > -1.0e-10
	    ->  get_attr(X,itf,Att2), % changed?
		setarg(2,Att2,type(t_L(Bound))),
		setarg(3,Att2,strictness(Strict)),
		pivot_a(Vub,X,Vb,t_l(Bound)),
		reconsider(X)
	    ;   get_attr(X,itf,Att),
		arg(5,Att,order(OrdX)),
		arg(6,Att,class(ClassX)),
		setarg(2,Att,type(t_L(Bound))),
		setarg(3,Att,strictness(Strict)),
		Delta is Bound - L,
		backsubst_delta(ClassX,OrdX,X,Delta)
	    )
	;   Strict is Sold \/ 2,
	    get_attr(X,itf,Att),
	    setarg(3,Att,strictness(Strict))
	).
uils(t_Lu(L,U),X,_Lin,Bound,Sold) :-
	TestBL is Bound - L,
	(   TestBL < -1.0e-10
	->  true
	;   TestBL > 1.0e-10
	->  U - Bound > 1.0e-10,
	    Strict is Sold \/ 2,
	    (   get_attr(X,itf,Att),
		arg(5,Att,order(OrdX)),
		arg(6,Att,class(ClassX)),
	    	ub(ClassX,OrdX,Vub-Vb-Ub),
		Bound - (Ub + L) > -1.0e-10
	    ->  get_attr(X,itf,Att2), % changed?
		setarg(2,Att2,type(t_Lu(Bound,U))),
		setarg(3,Att2,strictness(Strict)),
		pivot_a(Vub,X,Vb,t_lu(Bound,U)),
		reconsider(X)
	    ;   get_attr(X,itf,Att),
		arg(5,Att,order(OrdX)),
		arg(6,Att,class(ClassX)),
		setarg(2,Att,type(t_Lu(Bound,U))),
		setarg(3,Att,strictness(Strict)),
		Delta is Bound - L,
		backsubst_delta(ClassX,OrdX,X,Delta)
	    )
	;   Strict is Sold \/ 2,
	    get_attr(X,itf,Att),
	    setarg(3,Att,strictness(Strict))
	).

% reconsider_upper(X,Lin,U)
%
% Checks if the upperbound of X which is U, satisfies the bounds
% of the variables in Lin: let R be the sum of all the bounds on 
% the variables in Lin, and I be the inhomogene part of Lin, then
% upperbound U should be larger or equal to R + I (R may contain
% lowerbounds).
% See also rcb/3 in bv.pl

reconsider_upper(X,[I,R|H],U) :-
	R + I - U > -1.0e-10,	% violation
	!,
	dec_step(H,Status),	% we want to decrement R
	rcbl_status(Status,X,[],Binds,[],u(U)),
	export_binding(Binds).
reconsider_upper( _, _, _).

% reconsider_lower(X,Lin,L)
%
% Checks if the lowerbound of X which is L, satisfies the bounds
% of the variables in Lin: let R be the sum of all the bounds on 
% the variables in Lin, and I be the inhomogene part of Lin, then
% lowerbound L should be smaller or equal to R + I (R may contain
% upperbounds).
% See also rcb/3 in bv.pl

reconsider_lower(X,[I,R|H],L) :-
	R + I - L < 1.0e-10,	% violation
	!,
	inc_step(H,Status),	% we want to increment R
	rcbl_status(Status,X,[],Binds,[],l(L)),
	export_binding(Binds).
reconsider_lower(_,_,_).

%
% lin is dereferenced
%

% solve_bound(Lin,Bound)
%
% Solves the linear equation Lin - Bound = 0
% Lin is the linear equation of X, a variable whose bounds have narrowed to value Bound

solve_bound(Lin,Bound) :-
	Bound >= -1.0e-10,
	Bound =< 1.0e-10,
	!,
	solve(Lin).
solve_bound(Lin,Bound) :-
	Nb is -Bound,
	normalize_scalar(Nb,Nbs),
	add_linear_11(Nbs,Lin,Eq),
	solve(Eq).