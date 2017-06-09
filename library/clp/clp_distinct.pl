/*  Part of SWI-Prolog

    Author:        Markus Triska
    E-mail:        triska@gmx.at
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2005-2011, Markus Triska
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(clp_distinct,
	[
		vars_in/2,
		vars_in/3,
		all_distinct/1
	]).
:- use_module(library(lists)).

/** <module> Weak arc consistent all_distinct/1 constraint

@deprecated	Superseded by library(clpfd)'s all_distinct/1.
@author		Markus Triska
*/

% For details, see Neng-Fa Zhou, 2005:
%      "Programming Finite-Domain Constraint Propagators in Action Rules"

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This library uses the following arribute value:

	dom_neq(Domain, Left, Right)

Domain is an unbounded  (GMP)  integer   representing  the  domain  as a
bit-vector, meaning N is in the domain iff 0 =\= Domain /\ (1<<N).

Left and Right are both lists of lists of variables. Each of those lists
corresponds to one all_distinct constraint the  variable is involved in,
and "left" and "right" means literally which  variables are to the left,
and which to the right in the first, second etc. of those constraints.

all_distinct([A,B,C,D]), all_distinct([X,Y,C,F,E]) causes the following
attributes for "C":

	Left:  [[A,B],[X,Y]]
	Right: [[D],[F,E]]
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


vars_in(Xs, From, To) :-
	Bitvec is (1<<(To+1)) - (1<<From),
	vars_in_(Xs, Bitvec).

vars_in(Xs, Dom) :-
	domain_bitvector(Dom, 0, Bitvec),
	vars_in_(Xs, Bitvec).

vars_in_([], _).
vars_in_([V|Vs], Bitvec) :-
	( var(V) ->
		( get_attr(V, clp_distinct, dom_neq(VBV,VLeft,VRight)) ->
			Bitvec1 is VBV /\ Bitvec,
		  	Bitvec1 =\= 0,
		        ( popcount(Bitvec1) =:= 1 ->
				V is msb(Bitvec1)
			;
				put_attr(V, clp_distinct, dom_neq(Bitvec1,VLeft,VRight))
			)
		;
			( popcount(Bitvec) =:= 1 ->
				V is msb(Bitvec)
			;
				put_attr(V, clp_distinct, dom_neq(Bitvec, [], []))
			)
		)
	;
		0 =\= Bitvec /\ (1<<V)
	),
	vars_in_(Vs, Bitvec).

domain_bitvector([], Bitvec, Bitvec).
domain_bitvector([D|Ds], Bitvec0, Bitvec) :-
	Bitvec1 is Bitvec0 \/ (1 << D),
	domain_bitvector(Ds, Bitvec1, Bitvec).


all_distinct(Ls) :-
	all_distinct(Ls, []),
	outof_reducer(Ls).

outof_reducer([]).
outof_reducer([X|Xs]) :-
	( var(X) ->
		get_attr(X, clp_distinct, dom_neq(Dom,Lefts,Rights)),
		outof_reducer(Lefts, Rights, X, Dom)
	;
		true
	),
	outof_reducer(Xs).

all_distinct([], _).
all_distinct([X|Right], Left) :-
	\+ list_contains(Right, X),
	outof(X, Left, Right),
	all_distinct(Right, [X|Left]).


outof(X, Left, Right) :-
	( var(X) ->
		get_attr(X, clp_distinct, dom_neq(Dom, XLefts, XRights)),
		put_attr(X, clp_distinct, dom_neq(Dom, [Left|XLefts], [Right|XRights]))
	;
		exclude_fire([Left], [Right], X)
	).


exclude_fire(Lefts, Rights, E) :-
	Mask is \ ( 1 << E),
	exclude_fire(Lefts, Rights, E, Mask).

exclude_fire([], [], _, _).
exclude_fire([Left|Ls], [Right|Rs], E, Mask) :-
	exclude_list(Left, E, Mask),
	exclude_list(Right, E, Mask),
	exclude_fire(Ls, Rs, E, Mask).


exclude_list([], _, _).
exclude_list([V|Vs], Val, Mask) :-
	( var(V) ->
		get_attr(V, clp_distinct, dom_neq(VDom0,VLefts,VRights)),
		VDom1 is VDom0 /\ Mask,
		VDom1 =\= 0,
		( popcount(VDom1) =:= 1 ->
			V is msb(VDom1)
		;
			put_attr(V, clp_distinct, dom_neq(VDom1,VLefts,VRights))
		)
	;
		V =\= Val
	),
	exclude_list(Vs, Val, Mask).

attr_unify_hook(dom_neq(Dom,Lefts,Rights), Y) :-
	( ground(Y) ->
		Dom /\ (1 << Y) =\= 0,
		exclude_fire(Lefts, Rights, Y)
	;

		\+ lists_contain(Lefts, Y),
		\+ lists_contain(Rights, Y),
		( get_attr(Y, clp_distinct, dom_neq(YDom0,YLefts0,YRights0)) ->
			YDom1 is YDom0 /\ Dom,
			YDom1 =\= 0,
			( popcount(YDom1) =:= 1 ->
				Y is msb(YDom1)
			;
				append(YLefts0, Lefts, YLefts1),
				append(YRights0, Rights, YRights1),
				put_attr(Y, clp_distinct, dom_neq(YDom1,YLefts1,YRights1))
			)
		;
			put_attr(Y, clp_distinct, dom_neq(Dom,Lefts,Rights))
		)
	).

lists_contain([X|Xs], Y) :-
	( list_contains(X, Y) ->
		true
	;
		lists_contain(Xs, Y)
	).

list_contains([X|Xs], Y) :-
	( X == Y ->
		true
	;
		list_contains(Xs, Y)
	).


outof_reducer([], [], _, _).
outof_reducer([L|Ls], [R|Rs], Var, Dom) :-
	append(L, R, Others),
	N is popcount(Dom),
	num_subsets(Others, Dom, 0, Num),
	( Num >= N ->
		fail
	; Num =:= (N - 1) ->
		reduce_from_others(Others, Dom)
	;
		true
	),
	outof_reducer(Ls, Rs, Var, Dom).


reduce_from_others([], _).
reduce_from_others([X|Xs], Dom) :-
	( var(X) ->
		get_attr(X, clp_distinct, dom_neq(XDom,XLeft,XRight)),
		( is_subset(Dom, XDom) ->
			true
		;
			NXDom is XDom /\ \Dom,
			NXDom =\= 0,
			( popcount(NXDom) =:= 1 ->
				X is msb(NXDom)
			;
				put_attr(X, clp_distinct, dom_neq(NXDom,XLeft,XRight))
			)
		)
	;
		true
	),
	reduce_from_others(Xs, Dom).

num_subsets([], _Dom, Num, Num).
num_subsets([S|Ss], Dom, Num0, Num) :-
	( var(S) ->
		get_attr(S, clp_distinct, dom_neq(SDom,_,_)),
		( is_subset(Dom, SDom) ->
			Num1 is Num0 + 1
		;
			Num1 = Num0
		)
	;
		Num1 = Num0
	),
	num_subsets(Ss, Dom, Num1, Num).


   % true iff S is a subset of Dom - should be a GMP binding (subsumption)

is_subset(Dom, S) :-
	S \/ Dom =:= Dom.

attr_portray_hook(dom_neq(Dom,_,_), _) :-
	Max is msb(Dom),
	Min is lsb(Dom),
	write(Min-Max).
