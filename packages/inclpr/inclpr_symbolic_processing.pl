/*  
    
    Part of INCLP(R)

    Author:        Leslie De Koninck
    E-mail:        Leslie.DeKoninck@cs.kuleuven.be
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2006, K.U. Leuven

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
:- module(inclpr_symbolic_processing,
	[
	    partial_derivative/3,
	    to_standard_form/2
	]).

% Module for converting constraints into a standard form and for calculating
% partial derivatives.

% public predicates

% partial_derivative(Function,Variable,Derivative)
%
% Returns in <Derivative> the partial derivative of function <Function> with
% respect to variable <Variable>.

partial_derivative(Expr,Var,D) :-
	pd(Expr,Var,Res),
	rewrite(Res,D).

% to_standard_form(Constraint,StandardForm)
%
% Rewrites the constraint in <Constraint> into the form Res = 0.0 (for
% equalities) or Res =< 0.0 (for inequalities).

to_standard_form(L = R,Res = 0.0) :- rewrite(L-R,Res).
to_standard_form(L =< R, Res =< 0.0) :- rewrite(L-R,Res).
to_standard_form(L >= R, Res =< 0.0) :- rewrite(R-L,Res).

% private predicates

% pd(Function,Var,Derivative)
%
% Returns in <Derivative> the partial derivative of function <Function> with
% respect to variable <Variable>.

pd(V,Var,D) :-
	(   var(V)
	->  (   V == Var
	    ->  D = 1.0
	    ;   D = 0.0
	    )
	;   number(V)
	->  D = 0.0
	;   functor(V,Op,Arity),
	    (   Arity =:= 2
	    ->  arg(1,V,L),
		arg(2,V,R),
		pd_binary(Op,L,R,Var,D)
	    ;   arg(1,V,X),
		pd_unary(Op,X,Var,D)
	    )
	).

% pd_binary(Operator,Left,Right,Variable,Derivative)
%
% Returns in <Derivative> the partial derivative with respect to variable
% <Variable> of the function made of the binary operator <Operator> applied to
% functions <Left> and <Right>.

pd_binary(+,L,R,Var,DL+DR) :-
	pd(L,Var,DL),
	pd(R,Var,DR).
pd_binary(-,L,R,Var,DL-DR) :-
	pd(L,Var,DL),
	pd(R,Var,DR).
pd_binary(*,L,R,Var,L*DR+R*DL) :-
	pd(L,Var,DL),
	pd(R,Var,DR).
pd_binary((^),X,N,Var,PD) :- pd_binary((**),X,N,Var,PD).
pd_binary((**),X,N,Var,PD) :-
	integer(N),
	pd(X,Var,PDX),
	M is N-1,
	(   M =:= 0
	->  PD = PDX
	;   M =:= 1
	->  PD = 2*X*PDX
	;   PD = N*(X**M)*PDX
	).

% pd_unary(Operator,Argument,Variable,Derivative)
%
% Returns in <Derivative> the partial derivative with respect to variable
% <Variable> of the function made of the unary operator <Operator> applied to
% the function <Argument>.

pd_unary(-,L,Var,-DL) :-
	pd(L,Var,DL).

% rewrite(Function,Rewritten)
%
% Rewrites function <Function> into an equivalent function <Rewritten> by
% applying some simplifications.

rewrite(Term,RW) :-
	(   var(Term)
	->  RW = Term
	;   number(Term)
	->  RW = Term
	;   functor(Term,Op,Arity),
	    (   Arity =:= 2
	    ->  arg(1,Term,L),
		arg(2,Term,R),
		rewrite_binary(Op,L,R,RW)
	    ;	arg(1,Term,X),
		rewrite_unary(Op,X,RW)
	    )
	).

% rewrite_binary(Operator,Left,Right,Rewritten)
%
% Rewrites the function made of binary operator <Operator> applied to functions
% <Left> and <Right> into the equivalent but more simple function <Rewritten>.

rewrite_binary(+,X,Y,Z) :-
	rewrite(X,Xr),
	rewrite(Y,Yr),
	(   number(Xr)
	->  (   number(Yr)
	    ->  Z is Xr + Yr
	    ;   Xr =:= 0.0
	    ->  Z = Yr
	    ;   nonvar(Yr),
		Yr = -Ym
	    ->  Z = Xr - Ym
	    ;   Z = Xr + Yr
	    )
	;   number(Yr)
	->  (   Yr =:= 0.0
	    ->  Z = Xr
	    ;   Yr < 0
	    ->  Ym is - Yr,
		Z = Xr - Ym
	    ;   Z = Xr + Yr
	    )
	;   nonvar(Yr),
	    Yr = -Ym
	->  Z = Xr - Ym
	;   Z = Xr + Yr
	).
rewrite_binary(-,X,Y,Z) :-
	rewrite(X,Xr),
	rewrite(Y,Yr),
	(   number(Xr)
	->  (   number(Yr)
	    ->  Z is Xr - Yr
	    ;   Xr =:= 0
	    ->  (   nonvar(Yr),
		    Yr = -Ym
		->  Z = Ym
		;   Z = -Yr
		)
	    ;   nonvar(Yr),
		Yr = -Ym
	    ->  Z = Xr + Ym
	    ;   Z = Xr - Yr
	    )
	;   number(Yr)
	->  (   Yr =:= 0.0
	    ->  Z = Xr
	    ;   Yr < 0
	    ->  Ym is -Yr,
		Z = Xr + Ym
	    ;   Z = Xr - Yr
	    )
	;   nonvar(Yr),
	    Yr = -Ym
	->  Z = Xr + Ym
	;   Z = Xr - Yr
	).
rewrite_binary(*,X,Y,Z) :-
	rewrite(X,Xr),
	rewrite(Y,Yr),
	(   number(Xr)
	->  (   number(Yr)
	    ->  Z is Xr * Yr
	    ;   Xr =:= 0.0
	    ->  Z = 0.0
	    ;   Xr =:= 1.0
	    ->  Z = Yr
	    ;   Xr =:= -1.0
	    ->  (   nonvar(Yr),
		    Yr = -Ym
		->  Z = Ym
		;   Z = -Yr
		)
	    ;   Xr < 0
	    ->  Xm is -Xr,
		(   nonvar(Yr),
		    Yr = -Ym
		->  Z = Xm * Ym
		;   Z = -(Xm * Yr)
		)
	    ;   nonvar(Yr),
		Yr = -Ym
	    ->  Z = -(Xr * Ym)
	    ;   Z = Xr * Yr
	    )
	;   number(Yr)
	->  (   Yr =:= 0.0
	    ->  Z = 0.0
	    ;   Yr =:= 1.0
	    ->  Z = Xr
	    ;   Yr =:= -1.0
	    ->  (   nonvar(Xr),
		    Xr = -Xm
		->  Z = Xm
		;   Z = -Xr
		)
	    ;   Yr < 0
	    ->  Ym is -Yr,
		(   nonvar(Xr),
		    Xr = -Xm
		->  Z = Xm * Ym
		;   Z = -(Xr * Ym)
		)
	    ;   nonvar(Xr),
		Xr = -(Xm)
	    ->  Z = -(Xm * Yr)
	    ;   Z = Xr * Yr
	    )
	;   nonvar(Xr),
	    Xr = -Xm
	->  (   nonvar(Yr),
		Yr = -Ym
	    ->  Z = Xm * Ym
	    ;   Z = -(Xm * Yr)
	    )
	;   nonvar(Yr),
	    Yr = -Ym
	->  Z = -(Xr * Ym)
	;   Z = Xr * Yr
	).
rewrite_binary(/,X,Y,Z) :-
	rewrite(X,Xr),
	rewrite(Y,Yr),
	(   number(Xr)
	->  (   number(Yr)
	    ->  Yr =\= 0.0,
		Z is Xr / Yr
	    ;   Xr =:= 0.0
	    ->  Z = 0.0
	    ;   Xr < 0.0
	    ->  Xm is -Xr,
		(   nonvar(Yr),
		    Yr = -Ym
		->  Z = Xm / Ym
		;   Z = -(Xm / Yr)
		)
	    ;   nonvar(Yr),
		Yr = -Ym
	    ->  Z = -(Xr / Ym)
	    ;   Z = Xr / Yr
	    )
	;   number(Yr)
	->  Yr =\= 0.0,
	    (   Yr =:= 1.0
	    ->  Z = Xr
	    ;   Yr =:= -1.0
	    ->  (   nonvar(Xr),
		    Xr = -Xm
		->  Z = Xm
		;   Z = -Xr
		)
	    ;   Yr < 0.0
	    ->  Ym is -Yr,
		(   nonvar(Xr),
		    Xr = -Xm
		->  Z = Xm / Ym
		;   Z = -(Xr / Ym)
		)
	    ;   nonvar(Xr),
		Xr = -Xm
	    ->  Z = -(Xm / Yr)
	    ;   Z = Xr / Yr
	    )
	;   nonvar(Xr),
	    Xr = -Xm
	->  (   nonvar(Yr),
		Yr = -Ym
	    ->  Z = Xm / Ym
	    ;   Z = -(Xm / Yr)
	    )
	;   nonvar(Yr),
	    Yr = -Ym
	->  Z = -(Xr / Ym)
	;   Z = Xr / Yr
	). 
rewrite_binary(^,X,N,Z) :- rewrite_binary(**,X,N,Z).
rewrite_binary(**,X,N,Z) :-
	integer(N),
	rewrite(X,Xr),
	(   number(Xr)
	->  Z is Xr ** N
	;   nonvar(Xr),
	    Xr = -Xm
	->  (   N mod 2 =:= 0
	    ->  Z = Xm ** N
	    ;   Z = -(Xm ** N)
	    )
	;   Z = Xr ** N
	).

% rewrite_unary(Operator,Argument,Rewritten)
%
% Rewrites the function made of unary operator <Operator> applied to function
% <Argument> into the equivalent but more simple function <Rewritten>.

rewrite_unary(-,X,Z) :-
	rewrite(X,Xr),
	(   number(Xr)
	->  Z is -Xr
	;   nonvar(Xr),
	    Xr = -Xm
	->  Z is Xm
	;   Z = -Xr
	).