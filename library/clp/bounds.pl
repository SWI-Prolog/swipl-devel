/*  $Id$

    Part of SWI-Prolog

    Author:        Tom Schrijvers
    E-mail:        tom.schrijvers@cs.kuleuven.ac.be
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2004, K.U.Leuven

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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Simple integer solver that keeps track of upper and lower bounds
%
% Author: 	Tom Schrijvers
% E-mail: 	tom.schrijvers@cs.kuleuven.ac.be
% Copyright:	2004, K.U.Leuven
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Todo:
%	- reduce redundant propagation work
%	- other labelling functions
%	- abs, mod, ...
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(bounds,
	[
		op(760,yfx,(#<=>)),
		op(750,xfy,(#=>)),
		op(750,yfx,(#<=)),
		op(740,yfx,(#\/)),
		op(730,yfx,(#\)),
		op(720,yfx,(#/\)),
		op(710, fy,(#\)),
		op(700,xfx,(#>)),
		op(700,xfx,(#<)),
		op(700,xfx,(#>=)),
		op(700,xfx,(#=<)),
		op(700,xfx,(#=)),
		op(700,xfx,(#\=)),
		op(700,xfx,(in)),
		op(550,xfx,(..)),
		(#>)/2,
		(#<)/2,
		(#>=)/2,
		(#=<)/2,
		(#=)/2,
		(#\=)/2,
		(#<=>)/2,
		(#=>)/2,
		(#<=)/2,
		(#/\)/2,
		(#\/)/2,
		(#\)/2,
		(#\)/1,
		(in)/2,
		label/1,
		all_different/1
	]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% exported predicates
X #>= Y :-
	parse_expression(X,RX),
	parse_expression(Y,RY), 
	geq(RX,RY,yes).
X #=< Y :- 
	parse_expression(X,RX),
	parse_expression(Y,RY),
	leq(RX,RY,yes).
X #= Y :-
	parse_expression(X,RX),
	parse_expression(Y,RX). 
X #\= Y :-
	parse_expression(X,RX),
	parse_expression(Y,RY), 
	neq(RX,RY,yes).
X #> Y :-
	Z #= Y + 1,
	X #>= Z.
X #< Y :-
	Y #> X.
X in L .. U :- 
	( is_list(X) ->
		domains(X,L,U)
	;
		domain(X,L,U)
	).

L #<=> R :-
	reify(L,B),
	reify(R,B).
L #=> R :-
	reify(L,BL),
	reify(R,BR),
	myimpl(BL,BR).
R #<= L :-
	reify(L,BL),
	reify(R,BR),
	myimpl(BL,BR).
L #/\ R :-
	call(L),
	call(R).
L #\/ R :-
	reify(L,BL),
	reify(R,BR),
	myor(BL,BR,1).	
L #\ R :-
	reify(L,BL),
	reify(R,BR),
	myxor(BL,BR,1).

#\ C :-
	reify(C,B),
	mynot(B,1).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
reify(B,R) :-
	var(B), !,
	R = B.
reify(B,R) :-
	number(B), !,
	R = B.
reify(X #>= Y,B) :-
	parse_expression(X,XR),
	parse_expression(Y,YR),
	reified_geq(XR,YR,B).
reify(X #> Y,B) :-
	parse_expression(X,XR),
	Z #= Y + 1,
	reified_geq(XR,Z,B).
reify(X #=< Y,B) :-
	parse_expression(X,XR),
	parse_expression(Y,YR),
	reified_geq(YR,XR,B).
reify(X #< Y,B) :-
	reify(Y #> X,B).
reify(X #= Y,B) :-
	parse_expression(X,XR),
	parse_expression(Y,YR),
	reified_eq(XR,YR,B).
reify(X #\= Y,B) :-
	parse_expression(X,XR),
	parse_expression(Y,YR),
	reified_neq(XR,YR,B).
reify((X #/\ Y),B) :-
	reify(X,BX),
	reify(Y,BY),
	myand(BX,BY,B).
reify((X #\/ Y),B) :-
	reify(X,BX),
	reify(Y,BY),
	myor(BX,BY,B).
reify((X #\ Y),B) :-
	reify(X,BX),
	reify(Y,BY),
	myxor(BX,BY,B).
reify(#\ C, B) :-
	reify(C,BC),
	mynot(BC,B).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parse_expression(Expr,Result) :-
	( var(Expr) ->
		Result = Expr
	; number(Expr) ->
		Result = Expr
	; Expr = (L + R) ->
		parse_expression(L,RL),
		parse_expression(R,RR),
		myplus(RL,RR,Result,yes)
	; Expr = (L * R) ->
		parse_expression(L,RL),
		parse_expression(R,RR),
		mytimes(RL,RR,Result,yes)
	; Expr = (L - R) ->
		parse_expression(L,RL),
		parse_expression(R,RR),
		mytimes(-1,RR,RRR,yes),
		myplus(RL,RRR,Result,yes)
	; Expr = (- E) ->
		parse_expression(E,RE),
		mytimes(-1,RE,Result,yes)
	; Expr = max(L,R) ->
		parse_expression(L,RL),
		parse_expression(R,RR),
		mymax(RL,RR,Result,yes)
	; Expr = min(L,R) ->
		parse_expression(L,RL),
		parse_expression(R,RR),
		mymin(RL,RR,Result,yes)
	; Expr = L mod R ->
		parse_expression(L,RL),
		parse_expression(R,RR),
		mymod(RL,RR,Result)
	; Expr = abs(E) ->
		parse_expression(E,RE),
		myabs(RE,Result)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
label([]).
label([V|Vs]) :-
	( get(V,L,U,_) -> 
		between(L,U,W),
		%format('\tlabelling ~w with ~w\n',[V,W]),
		V = W
	;
		true
	),
	label(Vs).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
all_different([]).
all_different([X|Xs]) :-
	different(Xs,X),
	all_different(Xs).

different([],_).
different([Y|Ys],X) :-
	neq(X,Y,yes),
	different(Ys,X).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
domain(X,L,U) :-
	( var(X) ->
		get(X,XL,XU,Exp),
		NL is max(L,XL),
		NU is min(U,XU),
		put(X,NL,NU,Exp)
	; % nonvar(X) ->
		X >= L,
		X =< U
	).

domains([],_,_).
domains([V|Vs],L,U) :-
	domain(V,L,U),
	domains(Vs,L,U).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
leq(X,Y,New) :-
	geq(Y,X,New).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
geq(X,Y,New) :-
	( X == Y ->
		true
	; nonvar(X) ->
		( nonvar(Y) ->
			X >= Y
		; 
			get(Y,YL,YU,Exp) ->
			NYU is min(X,YU),
			put(Y,YL,NYU,Exp)
		)
	; nonvar(Y) ->
		get(X,XL,XU,Exp) ->
		NXL is max(Y,XL),
		put(X,NXL,XU,Exp)
	;
		get(X,XL,XU,ExpX),
		get(Y,YL,YU,ExpY),
		XU >= YL,
		( XL > YU ->
			true
		; XU == YL ->
			X = Y
		; member(leq(Z,State),ExpX),
	          Y == Z -> 
			set_passive(State),
			X = Y	
		; 
			( New == yes ->
				active_state(State),
				put(Y,YL,YU,[leq(X,State)|ExpY]),
				ExpX1 = [geq(Y,State)|ExpX]
			;
				ExpX1 = ExpX
			),
			NXL is max(XL,YL),
			put(X,NXL,XU,ExpX1),
			( get(Y,YL2,YU2,ExpY2) ->
				NYU is min(YU2,XU),
				put(Y,YL2,NYU,ExpY2)
			;
				true
			)
		)
	).
		
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
neq(X,Y,New) :-
	X \== Y,
	( nonvar(X) ->
		( nonvar(Y) ->
			true
		;
			get(Y,L,U,Exp),
			( L == X ->
				NL is L + 1,
				put(Y,NL,U,Exp)
			; U == X ->
				NU is U - 1,
				put(Y,L,NU,Exp)
			;
				( New == yes ->
					active_state(State),
					put(Y,L,U,[neq(X,State)|Exp])
				;
					true
				)	
			)
		)
	; nonvar(Y) ->
		neq(Y,X,New)
	;
		get(X,XL,XU,XExp),
		get(Y,YL,YU,YExp),
		( XL > YU ->
			true
		; YL > XU ->
			true
		;
			( New == yes ->
				active_state(State),
				put(X,XL,XU,[neq(Y,State)|XExp]),
				put(Y,YL,YU,[neq(X,State)|YExp])
			;
				true
			)
		)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
myplus(X,Y,Z,New) :-
	( nonvar(X) ->
		( nonvar(Y) ->
			Z is X + Y
		; nonvar(Z) ->
			Y is Z - X
		;
			get(Z,ZL,ZU,ZExp),
			get(Y,YL,YU,YExp),
			( New == yes ->
				ZExp1 = [myplus2(Y,X)|ZExp],
				YExp1 = [myplus(X,Z)|YExp],
				put(Y,YL,YU,YExp1)
			;
				ZExp1 = ZExp
			),
			NZL is max(ZL,X+YL),
			NZU is min(ZU,X+YU),
			put(Z,NZL,NZU,ZExp1),
			( get(Y,YL2,YU2,YExp2) ->
				NYL is max(YL,NZL-X),
				NYU is min(YU,NZU-X),	
				put(Y,NYL,NYU,YExp2)
			;
				Z is X + Y
			)
		)
	; nonvar(Y) ->
		myplus(Y,X,Z,New)
	; nonvar(Z) ->
		get(X,XL,XU,XExp),
		get(Y,YL,YU,YExp),
		( New == yes ->
			XExp1 = [myplus(Y,Z)|XExp],
			YExp1 = [myplus(X,Z)|YExp],
			put(Y,YL,YU,YExp1)
		;
			XExp1 = XExp
		),
		NXL is max(XL,Z-YU),
		NXU is min(XU,Z-YL),
		put(X,NXL,NXU,XExp1),
		( get(Y,YL2,YU2,YExp2) ->
			NYL is max(YL2,Z-NXU),
			NYU is min(YU2,Z-NXL),
			put(Y,NYL,NYU,YExp2)
		;
			X is Z - Y
		)
	;
		get(X,XL,XU,XExp),
		get(Y,YL,YU,YExp),
		get(Z,ZL,ZU,ZExp),
		( New == yes ->
			XExp1 = [myplus(Y,Z)|XExp],
			YExp1 = [myplus(X,Z)|YExp],
			ZExp1 = [myplus2(X,Y)|ZExp],
			put(Y,YL,YU,YExp1),
			put(Z,ZL,ZU,ZExp1)
		;
			XExp1 = XExp
		),
		NXL is max(XL,ZL-YU),
		NXU is min(XU,ZU-YL),
		put(X,NXL,NXU,XExp1),
		( get(Y,YL2,YU2,YExp2) ->
			NYL is max(YL2,ZL-NXU),
			NYU is min(YU2,ZU-NXL),
			put(Y,NYL,NYU,YExp2)
		;
			NYL = Y,
			NYU = Y
		),
		( get(Z,ZL2,ZU2,ZExp2) ->
			NZL is max(ZL2,NXL+NYL),
			NZU is min(ZU2,NXU+NYU),
			put(Z,NZL,NZU,ZExp2)
		;
			true
		)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% X * Y = Z

:- arithmetic_function(div/2).
div(X,Y,Z) :-
	( max_inf(X) ->
		( Y >= 0 ->
			Z = X
		;
			min_inf(Z)
		)
	; min_inf(X) ->
		( Y >= 0 ->
			Z = X
		;
			max_inf(Z)
		)
	; Y \== 0 ->
		Z is X / Y
	; X >= 0 ->
		max_inf(Z)
	; X < 0 ->
		min_inf(Z)
	).	

mytimes(X,Y,Z,New) :-
	( nonvar(X) ->
		(nonvar(Y) ->
			Z is X * Y
		; X == 0 ->
			Z = 0
		;  nonvar(Z) ->
			0 is Z mod X,
			Y is Z / X
		;
			get(Y,YL,YU,YExp),
			get(Z,ZL,ZU,ZExp),
			NZL is max(ZL,min(X * YL,X*YU)),
			NZU is min(ZU,max(X * YU,X*YL)),
			( New == yes ->
				YExp1 = [mytimes(X,Z)|YExp],
				put(Y,YL,YU,YExp1),
				put(Z,NZL,NZU,[mytimes2(X,Y)|ZExp])
			;
				put(Z,NZL,NZU,ZExp)
			),
			( get(Y,YL2,YU2,YExp2) ->
				min_divide(ZL,ZU,X,X,NYLT),
				max_divide(ZL,ZU,X,X,NYUT),
				NYL is max(YL2,ceiling(NYLT)),
				NYU is min(YU2,floor(NYUT)),
				put(Y,NYL,NYU,YExp2)
			;
				Z is X * Y
			)		
		)
	; nonvar(Y) ->
		mytimes(Y,X,Z,New)
	; nonvar(Z) ->
		get(X,XL,XU,XExp),
		get(Y,YL,YU,YExp),
		min_divide(Z,Z,YL,YU,TNXL),
		max_divide(Z,Z,YL,YU,TNXU),
		NXL is max(XL,ceiling(TNXL)),
		NXU is min(XU,floor(TNXU)),		
		( New == yes ->
			YExp1 = [mytimes(X,Z)|YExp],
			put(Y,YL,YU,YExp1),
			put(X,NXL,NXU,[mytimes(Y,Z)|XExp])
		;
			put(X,NXL,NXU,XExp)
		),
		( get(Y,YL2,YU2,YExp2) ->
			min_divide(Z,Z,NXL,NXU,NYLT),
			max_divide(Z,Z,NXL,NXU,NYUT),
			NYL is max(YL2,ceiling(NYLT)),
			NYU is min(YU2,floor(NYUT)),
			put(Y,NYL,NYU,YExp2)
		;
			( Y \== 0 ->
				0 is Z mod Y,
				X is Z / Y
			;
				Z = 0
			)
		)
	;
		get(X,XL,XU,XExp),
		get(Y,YL,YU,YExp),
		get(Z,ZL,ZU,ZExp),
		min_divide(ZL,ZU,YL,YU,TXL),
		NXL is max(XL,ceiling(TXL)),
		max_divide(ZL,ZU,YL,YU,TXU),
		NXU is min(XU,floor(TXU)),
		( New == yes ->
			put(Y,YL,YU,[mytimes(X,Z)|YExp]),
			put(Z,ZL,ZU,[mytimes2(X,Y)|ZExp]),
			put(X,NXL,NXU,[mytimes(Y,Z)|XExp])
		;
			put(X,NXL,NXU,XExp)
		),
		( get(Y,YL2,YU2,YExp2) ->
			min_divide(ZL,ZU,XL,XU,TYL),
			NYL is max(YL2,ceiling(TYL)),
			max_divide(ZL,ZU,XL,XU,TYU),
			NYU is min(YU2,floor(TYU)),
			put(Y,NYL,NYU,YExp2)	
		;
			NYL = Y,
			NYU = Y
		),
		( get(Z,ZL2,ZU2,ZExp2) ->
			min_times(NXL,NXU,NYL,NYU,TZL),	
			NZL is max(ZL2,TZL),
			max_times(NXL,NXU,NYL,NYU,TZU),	
			NZU is min(ZU2,TZU),
			put(Z,NZL,NZU,ZExp2)
		;
			true
		)	
	).

max_times(L1,U1,L2,U2,Max) :-
	Max is max(max(L1*L2,L1*U2),max(U1*L2,U1*U2)).	
min_times(L1,U1,L2,U2,Min) :-
	Min is min(min(L1*L2,L1*U2),min(U1*L2,U1*U2)).	
max_divide(L1,U1,L2,U2,Max) :-
	( L2 =< 0 , U2 >= 0 ->
		max_inf(Max)
	;
		Max is max(max(div(L1,L2),div(L1,U2)),max(div(U1,L2),div(U1,U2)))
	).	
min_divide(L1,U1,L2,U2,Min) :-
	( L2 =< 0 , U2 >= 0 ->
		min_inf(Min)
	;	
		Min is min(min(div(L1,L2),div(L1,U2)),min(div(U1,L2),div(U1,U2)))
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mymax(X,Y,Z,New) :-
	( nonvar(X) ->
		( nonvar(Y) ->
			Z is max(X,Y)
		; nonvar(Z) ->
			( Z == X ->		
				geq(X,Y,yes)
			; Z > X ->
				Y = Z
			%; Z < X
			%	fail
			)
		;
			get(Y,YL,YU,YExp),
			get(Z,ZL,ZU,ZExp),
			( YL >= X ->
				Z = Y
			; X >= YU ->
				Z = X
			; X < ZL ->
				Y = Z
			;
				( New == yes -> 
					put(Y,YL,YU,[mymax(X,Z)|YExp]) 
				; 
					true
				),
				NZL is max(ZL,X),
				NZU is min(ZU,YU),
				( New == yes ->
					put(Z,NZL,NZU,[mymax2(X,Y)|ZExp])
				;
					put(Z,NZL,NZU,ZExp)
				),
				( get(Y,YL2,YU2,YExp2) ->
					NYU is min(YU2,NZU),
					put(Y,YL2,NYU,YExp2)
				;
					true
				)
			)
		)
	; nonvar(Y) ->
		mymax(Y,X,Z,New)
	; nonvar(Z) ->
		get(X,XL,XU,XExp),
		get(Y,YL,YU,YExp),
		( XU < Z ->
			Y = Z
		; % XU >= Z ->
		  YU < Z ->
		  	X = Z
		; % YU >= Z
		  XL > YU ->
			Z = X
		; YL > XU ->
			Z = Y
		;
			( New == yes ->
				put(Y,YL,YU,[mymax(X,Z)|YExp]),
				put(X,XL,Z,[mymax(Y,Z)|XExp])
			;
				put(X,XL,Z,XExp)
			),
			( get(Y,YL2,YU2,YExp2) ->
				NYU is min(YU2,Z),
				put(Y,YL2,NYU,YExp2)
			;
				true
			)
		)
	; X == Y ->
		Z = X
	; Z == X ->
		geq(X,Y,yes)
	; Z == Y ->
		geq(Y,X,yes)
	;
		get(X,XL,XU,XExp),
		get(Y,YL,YU,YExp),
		get(Z,ZL,ZU,ZExp),
		NZL is max(ZL,max(XL,YL)),
		NZU is min(ZU,max(XU,YU)),
		( New == yes ->
			put(X,XL,XU,[mymax(Y,Z)|XExp]),
			put(Y,YL,YU,[mymax(X,Z)|YExp]),
			put(Z,NZL,NZU,[mymax2(X,Y)|ZExp])
		;
			put(Z,NZL,NZU,ZExp)
		),
		( get(X,XL2,XU2,XExp2) ->
			( XU2 < NZL ->
				Y = Z
			; YU  < NZL ->
				X = Z
			; YU < XL2 ->
				X = Z
			; XU2 < YL ->
				Y = Z
			;
				NXU is min(XU2,NZU),
				put(X,XL2,NXU,XExp2),
				( get(Y,YL2,YU2,YExp2) ->
					NYU is min(YU2,NZU),
					put(Y,YL2,NYU,YExp2)
				;
					mymax(Y,X,Z,no)
				)
			)
		; 
			mymax(X,Y,Z,no)
		)
	).

mymin(X,Y,Z,New) :-
	( nonvar(X) ->
		( nonvar(Y) ->
			Z is min(X,Y)
		; nonvar(Z) ->
			( Z == X ->		
				leq(X,Y,yes)
			; Z < X ->
				Y = Z
			%; Z > X
			%	fail
			)
		;
			get(Y,YL,YU,YExp),
			get(Z,ZL,ZU,ZExp),
			( YL >= X ->
				Z = X
			; X >= YU ->
				Z = Y
			; X > ZU ->
				Y = Z
			;
				( New == yes -> 
					put(Y,YL,YU,[mymin(X,Z)|YExp]) 
				; 
					true
				),
				NZL is max(ZL,YL),
				NZU is min(ZU,X),
				( New == yes ->
					put(Z,NZL,NZU,[mymin2(X,Y)|ZExp])
				;
					put(Z,NZL,NZU,ZExp)
				),
				( get(Y,YL2,YU2,YExp2) ->
					NYL is max(YL2,NZL),
					put(Y,NYL,YU2,YExp2)
				;
					true
				)
			)
		)
	; nonvar(Y) ->
		mymin(Y,X,Z,New)
	; nonvar(Z) ->
		get(X,XL,XU,XExp),
		get(Y,YL,YU,YExp),
		( XL > Z ->
			Y = Z
		; % XL =< Z ->
		  YL > Z ->
		  	X = Z
		; % YL =< Z
		  XU =< YL ->
			Z = X
		; YU =< XL ->
			Z = Y
		;
			( New == yes ->
				put(Y,YL,YU,[mymin(X,Z)|YExp]),
				put(X,Z,XU,[mymin(Y,Z)|XExp])
			;
				put(X,Z,XU,XExp)
			),
			( get(Y,YL2,YU2,YExp2) ->
				NYL is max(YL2,Z),
				put(Y,NYL,YU2,YExp2)
			;
				true
			)
		)
	; X == Y ->
		Z = X
	; Z == X ->
		leq(X,Y,yes)
	; Z == Y ->
		leq(Y,X,yes)
	;
		get(X,XL,XU,XExp),
		get(Y,YL,YU,YExp),
		get(Z,ZL,ZU,ZExp),
		NZL is max(ZL,min(XL,YL)),
		NZU is min(ZU,min(XU,YU)),
		( New == yes ->
			put(X,XL,XU,[mymin(Y,Z)|XExp]),
			put(Y,YL,YU,[mymin(X,Z)|YExp]),
			put(Z,NZL,NZU,[mymin2(X,Y)|ZExp])
		;
			put(Z,NZL,NZU,ZExp)
		),
		( get(X,XL2,XU2,XExp2) ->
			( XL2 > NZU ->
				Y = Z
			; YL  > NZU ->
				X = Z
			; YU < XL2 ->
				Y = Z
			; XU2 < YL ->
				X = Z
			;
				NXL is max(XL2,NZL),
				put(X,NXL,XU2,XExp2),
				( get(Y,YL2,YU2,YExp2) ->
					NYL is max(YL2,NZL),
					put(Y,NYL,YU2,YExp2)
				;
					mymin(Y,X,Z,no)
				)
			)
		; 
			mymin(X,Y,Z,no)
		)
	).

myabs(X,Y) :-
	( nonvar(X) ->
		Y is abs(X)
	; nonvar(Y) ->
		Y >= 0,
		get(X,LX,UX,ExpX),
		( UX < Y ->
			X is (-Y)
		; LX > (-Y) ->
			X = Y
		;
			NLX is (-Y),
			put(X,NLX,Y,[myabs(Y)|ExpX])
		)
	;
		get(X,LX,UX,ExpX),
		get(Y,LY,UY,ExpY),
		UY > 0,
		put(X,LX,UX,[myabs(Y)|ExpX]),
		( LX =< 0, UX >= 0 ->
			NLY is max(0,LY)
		;
			NLY is max(min(abs(LX),abs(UX)),max(LY,0))
		),
		NUY is min(UY,max(abs(LX),abs(UX))),
		put(Y,NLY,NUY,[myabs2(X)|ExpY]),
		( var(X) ->
			get(X,LX2,UX2,ExpX2),
			( LX2 == LX, UX2 == UX ->
				( NLY == 0 ->
					NLX is max(LX,(-NUY)),
					NUX is min(UX,NUY)
				; LX > (-NLY)  ->
					NLX is max(LX,NLY),
					NUX is min(UX,NUY)
				;
					NLX is max((-NUY),LX),
					( UX < NLY ->
						NUX is min((-NLY),UX)
					;
						NUX is min(NUY,UX)	
					)
				),
				put(X,NLX,NUX,ExpX2)
			;
				true
			)
		;
			true
		)
	).

mymod(X,Y,Z) :- % Z is X mod Y
	( nonvar(X) ->
		( nonvar(Y) ->
			Z is X mod Y
		; nonvar(Z) ->
			get(Y,YL,YU,YExp),
			( Z > 0 ->
				( X == Z ->
					no_overlap(-X,X,YL,YU,NYL,NYU),
					put(Y,NYL,NYU,[mymod2(X,Z)|YExp])
				; % Z < X
					no_overlap(-Z,Z,YL,YU,YL1,YU1),
					in_between(-X,X,YL1,YU1,NYL,NYU),
					put(Y,NYL,NYU,[mymod2(X,Z)|YExp])
				)
			; Z < 0 ->
				( X == Z ->
					no_overlap(X,-X,YL,YU,NYL,NYU),
					put(Y,NYL,NYU,[mymod2(X,Z)|YExp])
				; % Z < X
					no_overlap(Z,-Z,YL,YU,YL1,YU1),
					in_between(X,-X,YL1,YU1,NYL,NYU),
					put(Y,NYL,NYU,[mymod2(X,Z)|YExp])
				)
			; % Z == 0
				( X == 0 ->
					no_overlap(0,0,YL,YU,NYL,NYU),% Y \== 0
					put(Y,NYL,NYU,[mymod2(X,Z)|YExp])
				; % X \== 0
					PX is abs(X),
					in_between(-PX,PX,YL,YU,NYL,NYU),% |Y| < |X|
					put(Y,NYL,NYU,[mymod2(X,Z)|YExp])
				)
			)
		; % var(Y), var(Z)
			get(Y,YL,YU,YExp),
			get(Z,ZL,ZU,ZExp),
			put(Y,YL,YU,[mymod2(X,Z)|YExp]),
			put(Z,ZL,ZU,[mymod3(X,Y)|ZExp])
		)
	; nonvar(Y) ->
		Y \== 0,
		( nonvar(Z) ->
			get(X,XL,XU,XExp),
			( Z > 0 ->
				NXL is max(XL,1),
				put(X,NXL,XU,[mymod1(Y,Z)|XExp])
			; Z < 0 ->
				NXU is min(XU,-1),
				put(X,XL,NXU,[mymod1(Y,Z)|XExp])
			;
				put(X,XL,XU,[mymod1(Y,Z)|XExp])	
			)
		;
			get(X,XL,XU,XExp),
			get(Z,ZL,ZU,ZExp),
			put(X,XL,XU,[mymod1(Y,Z)|XExp]),
			put(Z,ZL,ZU,[mymod3(X,Y)|ZExp])
		)
	; nonvar(Z) ->
		get(X,XL,XU,XExp),
		get(Y,YL,YU,YExp),
		put(X,XL,XU,[mymod1(Y,Z)|XExp]),
		put(Y,YL,YU,[mymod2(X,Z)|YExp])
	;
		get(X,XL,XU,XExp),
		get(Y,YL,YU,YExp),
		get(Z,ZL,ZU,ZExp),
		put(X,XL,XU,[mymod1(Y,Z)|XExp]),
		put(Y,YL,YU,[mymod2(X,Z)|YExp]),
		put(Z,ZL,ZU,[mymod3(X,Y)|ZExp])
	).
			
no_overlap(XL,XU,YL,YU,NYL,NYU) :-
	( XL =< YL ->
		NYL is XU + 1,
		NYU = YU
	; YU =< XU ->
		NYU is XL - 1,
		NYL is YL
	;
		NYL = YL,
		NYU = YU
	).
in_between(XL,XU,YL,YU,NYL,NYU) :-
	( XL >= YL ->
		NYL is XL + 1
	;
		NYL = YL
	),
	( XU =< YU ->
		NYU is XU -1
	;
		NYU = YU
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TODO
%	trigger reified constraints when geq is added
reified_geq(X,Y,B) :-
	( var(B) ->
		( nonvar(X) ->
			( nonvar(Y) ->
				( X >= Y ->
					B = 1
				;
					B = 0
				)
			;
				get(Y,L,U,Expr),
				( X >= U ->
					B = 1
				; X < L ->
					B = 0
				;
					put(Y,L,U,[reified_leq(X,B)|Expr]),
					get(B,BL,BU,BExpr),
					put(B,BL,BU,[reified_geq2(X,Y)|BExpr]),
					B in 0..1
				)
			)
		; nonvar(Y) ->
			get(X,L,U,Expr),
			( L >= Y ->
			B = 1
			; U < Y ->
				B = 0
			;
				put(X,L,U,[reified_geq(Y,B)|Expr]),
				get(B,BL,BU,BExpr),
				put(B,BL,BU,[reified_geq2(X,Y)|BExpr]),
				B in 0..1
			)
		;
			get(X,XL,XU,XExpr),
			get(Y,YL,YU,YExpr),
			( XL >= YU ->
				B = 1
			; XU < YL ->
				B = 0
			; member(geq(Z,_State),XExpr),
			  Z == Y ->
			  	B = 1
			;
				put(X,XL,XU,[reified_geq(Y,B)|XExpr]),
				put(Y,YL,YU,[reified_leq(X,B)|YExpr]),
				get(B,BL,BU,BExpr),
				put(B,BL,BU,[reified_geq2(X,Y)|BExpr]),
				B in 0..1
			)
		)
	; B == 1 ->
		X #>= Y	
	; B == 0 ->
		X #< Y
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
reified_eq(X,Y,B) :-
	( var(B) ->
		( nonvar(X) ->
			( nonvar(Y) ->
				( X == Y ->
					B = 1
				;
					B = 0
				)
			;
				get(Y,L,U,Expr),
				( L > X ->
					B = 0
				; U < X ->
					B = 0
				;
					put(Y,L,U,[reified_eq(X,B)|Expr]),
					get(B,BL,BU,BExpr),
					put(B,BL,BU,[reified_eq2(X,Y)|BExpr]),
					B in 0..1
				)
			)
		; nonvar(Y) ->
			reified_eq(Y,X,B)
		; X == Y ->
			B = 1
		;
			get(X,XL,XU,XExpr),
			get(Y,YL,YU,YExpr),
			( XL > YU ->
				B = 0
			; YL > XU ->
				B = 0
			; member(neq(Z,_),XExpr),
			  Z == Y ->
			  	B = 0
			;
				put(X,XL,XU,[reified_eq(Y,B)|XExpr]),
				put(Y,YL,YU,[reified_eq(X,B)|YExpr]),
				get(B,BL,BU,BExpr),
				put(B,BL,BU,[reified_eq2(X,Y)|BExpr]),
				B in 0..1
			)
		)
	; B == 1 ->
		X #= Y
	; B == 0 ->
		X #\= Y
	).
		
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
reified_neq(X,Y,B) :-
	mynot(B,B1),
	reified_eq(X,Y,B1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mynot(B1,B2) :-
	( nonvar(B1) ->
		( B1 == 1 ->
			B2 = 0
		; B1 == 0 ->
			B2 = 1
		)
	; nonvar(B2) ->
		mynot(B2,B1)
	;
		get(B1,L1,U1,Expr1),
		get(B2,L2,U2,Expr2),
		put(B2,L2,U2,[mynot(B1)|Expr2]),
		put(B1,L1,U1,[mynot(B2)|Expr1]),
		B1 in 0..1,
		B2 in 0..1
	).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
myimpl(B1,B2) :-
	( nonvar(B1) ->
		( B1 == 1 ->
			B2 = 1
		; B1 == 0 ->
			B2 in 0..1
		)
	; nonvar(B2) ->
		( B2 == 0 ->
			B1 = 0
		; B2 == 1 ->
			B1 in 0..1
		)
	;
		get(B1,L1,U1,Expr1),
		get(B2,L2,U2,Expr2),
		put(B1,L1,U1,[myimpl(B2)|Expr1]),
		put(B2,L2,U2,[myimpl2(B1)|Expr2]),
		B1 in 0..1,
		B2 in 0..1
	).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
myand(X,Y,Z) :-
	( nonvar(X) ->
		( X == 1 ->
			Y in 0..1,
			Y = Z
		; X == 0 ->
			Y in 0..1,
			Z = 0
		)
	; nonvar(Y) ->
		myand(Y,X,Z)
	; nonvar(Z) ->
		( Z == 1 ->
			X = 1,
			Y = 1
		; Z == 0 ->
			X in 0..1,
			Y in 0..1,
			X + Y #=< 1
		)
	;
		get(X,LX,UX,ExpX),
		get(Y,LY,UY,ExpY),
		get(Z,LZ,UZ,ExpZ),
		put(X,LX,UX,[myand(Y,Z)|ExpX]),
		put(Y,LY,UY,[myand(X,Z)|ExpY]),
		put(Z,LZ,UZ,[myand2(X,Y)|ExpZ]),
		[X,Y,Z] in 0..1
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
myor(X,Y,Z) :-
	( nonvar(X) ->
		( X == 0 ->
			Y in 0..1,
			Y = Z
		; X == 1 ->
			Y in 0..1,
			Z = 1
		)
	; nonvar(Y) ->
		myor(Y,X,Z)
	; nonvar(Z) ->
		( Z == 0 ->
			X = 0,
			Y = 0
		; Z == 1 ->
			X in 0..1,
			Y in 0..1,
			X + Y #>= 1
		)
	;
		get(X,LX,UX,ExpX),
		get(Y,LY,UY,ExpY),
		get(Z,LZ,UZ,ExpZ),
		put(X,LX,UX,[myor(Y,Z)|ExpX]),
		put(Y,LY,UY,[myor(X,Z)|ExpY]),
		put(Z,LZ,UZ,[myor2(X,Y)|ExpZ]),
		[X,Y,Z] in 0..1
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
myxor(X,Y,Z) :-
	( nonvar(X) ->
		( X == 0 ->
			Y in 0..1,
			Y = Z
		; X == 1 ->
			mynot(Y,Z)
		)
	; nonvar(Y) ->
		myxor(Y,X,Z)
	; nonvar(Z) ->
		( Z == 0 ->
			X = Y,
			[X,Y] in 0..1
		; Z == 1 ->
			X in 0..1,
			Y in 0..1,
			X + Y #= 1
		)
	;
		get(X,LX,UX,ExpX),
		get(Y,LY,UY,ExpY),
		get(Z,LZ,UZ,ExpZ),
		put(X,LX,UX,[myxor(Y,Z)|ExpX]),
		put(Y,LY,UY,[myxor(X,Z)|ExpY]),
		put(Z,LZ,UZ,[myxor2(X,Y)|ExpZ]),
		[X,Y,Z] in 0..1
	).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get(X,L,U,Exp) :-
	( get_attr(X,bounds,Attr) ->
		Attr = bounds(L,U,Exp)
	; var(X) ->
		min_inf(L),
		max_inf(U),
		Exp = []
	).

put(X,L,U,Exp) :-
	L =< U,
	( L == U ->
		X = L,
		trigger_exps(Exp,X) 
	;
		( get_attr(X,bounds,Attr) ->
			put_attr(X,bounds,bounds(L,U,Exp)),
			Attr = bounds(OldL,OldU,_),
			( OldL == L, OldU == U ->
				true
			;
				%format('\t~w in ~w .. ~w\n',[X,L,U]),
				trigger_exps(Exp,X)
			)
		; 
			%format('\t~w in ~w .. ~w\n',[X,L,U]),
			put_attr(X,bounds,bounds(L,U,Exp))
		)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
min_inf(Inf) :-
	current_prolog_flag(min_integer,MInf),
	Inf is MInf + 1.

max_inf(Inf) :-
	current_prolog_flag(max_integer,Inf).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
attr_unify_hook(bounds(L,U,Exp),Other) :-
	( get(Other,OL,OU,OExp) ->
		NL is max(L,OL),
		NU is min(U,OU),
		append(Exp,OExp,NExp),
		check_neqs(NExp,Other),
		put(Other,NL,NU,NExp)	
	; % nonvar(Other) ->
		Other >= L,
		Other =< U,
		trigger_exps(Exp,Other)
	).

check_neqs([],_).
check_neqs([E|Es],X) :-
	( E = neq(Y,_),
	  X == Y ->
		fail
	;
		check_neqs(Es,X)
	).
		

trigger_exps([],_).
trigger_exps([E|Es],X) :-
	trigger_exp(E,X),
	trigger_exps(Es,X).

trigger_exp(geq(Y,State),X) :-
	( is_active(State) ->
		geq(X,Y,no)
	;
		true
	).
trigger_exp(leq(Y,State),X) :-
	( is_active(State) ->
		leq(X,Y,no)
	;
		true
	).
trigger_exp(neq(Y,State),X) :-
	( is_active(State) ->
		neq(X,Y,no)
	;
		true
	).
trigger_exp(myplus(Y,Z),X) :-
	myplus(X,Y,Z,no).
trigger_exp(myplus2(A,B),X) :-
	myplus(A,B,X,no).

trigger_exp(mytimes(Y,Z),X) :-
	mytimes(X,Y,Z,no).
trigger_exp(mytimes2(A,B),X) :-
	mytimes(A,B,X,no).

trigger_exp(mymax(Y,Z),X) :-
	mymax(X,Y,Z,no).
trigger_exp(mymax2(X,Y),Z) :-
	mymax(X,Y,Z,no).

trigger_exp(mymin(Y,Z),X) :-
	mymin(X,Y,Z,no).
trigger_exp(mymin2(X,Y),Z) :-
	mymin(X,Y,Z,no).

trigger_exp(reified_leq(X,B),Y) :-
	reified_geq(X,Y,B).
trigger_exp(reified_geq(Y,B),X) :-
	reified_geq(X,Y,B).
trigger_exp(reified_geq2(X,Y),B) :-
	reified_geq(X,Y,B).

trigger_exp(reified_eq(Y,B),X) :-
	reified_eq(X,Y,B).
trigger_exp(reified_eq2(X,Y),B) :-
	reified_eq(X,Y,B).

trigger_exp(mynot(Y),X) :-
	mynot(X,Y).

trigger_exp(myimpl(Y),X) :-
	myimpl(X,Y).
trigger_exp(myimpl2(X),Y) :-
	myimpl(X,Y).

trigger_exp(myand(Y,Z),X) :-
	myand(X,Y,Z).
trigger_exp(myand2(X,Y),Z) :-
	myand(X,Y,Z).

trigger_exp(myor(Y,Z),X) :-
	myor(X,Y,Z).
trigger_exp(myor2(X,Y),Z) :-
	myor(X,Y,Z).

trigger_exp(myxor(Y,Z),X) :-
	myxor(X,Y,Z).
trigger_exp(myxor2(X,Y),Z) :-
	myxor(X,Y,Z).

trigger_exp(myabs(Y),X) :-
	myabs(X,Y).
trigger_exp(myabs2(X),Y) :-
	myabs(X,Y).

trigger_exp(mymod1(Y,Z),X) :-
	mymod(X,Y,Z).
trigger_exp(mymod2(X,Z),Y) :-
	mymod(X,Y,Z).
trigger_exp(mymod3(X,Y),Z) :-
	mymod(X,Y,Z).

memberchk_eq(X,[Y|Ys],Z) :-
   (   X == Y ->
       Z = Y
   ;   memberchk_eq(X,Ys,Z)
   ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
active_state(state(active)).
is_active(state(active)).
set_passive(State) :-
	setarg(1,State,passive).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
attr_portray_hook(bounds(L,U,_),_) :-
	write(L..U).

