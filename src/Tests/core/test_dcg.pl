/*  $Id$

    Part of SWI-Prolog

    Author:        Ulrich Neumerkel
    WWW:           http://www.swi-prolog.org
    Copyright (C): Ulrich Neumerkel

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/


:- module(test_dcg, [test_dcg/0]).
:- use_module(library(plunit)).
:- use_module(library(apply_macros)).

test_dcg :-
	run_tests([ expand_goal,
		    phrase,
		    phrase_expansions,
		    rule_expansions,
		    dcg_rule_expansions,
		    context
		  ]).

:- begin_tests(expand_goal).

test(1, [G == a(L,[])]) :-
	expand_goal(phrase(a,L), G).
test(2, [B == (b([x], []), []=[1])]) :-
	expand_goal(phrase({phrase(b,[x])},[1]),B).
test(3, []) :-
	expand_goal(phrase(L,L,L),G),
	(L,G) \== ([],[]=[]).
test(nonlin, [G =@= phrase(Lc,Lc,Lc)]) :-
	expand_goal(phrase(L,L,L),G).
test(nonlin, [G =@= phrase(Lc,Lc)]) :-
	expand_goal(phrase(L,L),G).
test(5, [G == quoniam]) :-
	expand_goal(quoniam,G).
test(6,[G =@= spec(_Xs2c,[],_Xs1c,[])]) :-
	expand_goal(phrase(spec(_Xs2,[]),_Xs1),G).
test(t,[]) :-
	expand_goal(phrase([1],L,L),_).
test(t,[sto(rational_trees)]) :-
	expand_goal(phrase([1],L,L),G),
	var(L),
	G,
	I = [1|I],
	L == I.
/* depending on invocation, one of these tests
   produces an error

test(absurd1,[G = (_=_)]) :-
	expand_goal(phrase([1],L,L),G).
test(absurd2,[G = (_,_)]) :-
	expand_goal(phrase([1],L,L),G).
*/
:- end_tests(expand_goal).


:- begin_tests(phrase).

test(iso_8_1_1_3, [error(instantiation_error)]) :-
	phrase(_,[],[]).
test(iso_8_1_1_3, [error(instantiation_error)]) :-
	phrase(L,L,L).
test(iso_8_1_1_3,[error(instantiation_error)]) :-
	phrase(L,L).
test(iso_8_1_1_3_OPEN, [error(type_error(_,27))]) :-
	phrase(27,[],[]).
test(iso_8_1_1_3_OPEN, [error(type_error(callable,27))]) :-
	phrase(27,[],[]).
%test(iso_8_1_1_3, [ blocked(disagreement_with_ISO_DTR_draft),
%		    error(type_error(list,_))
%		  ]) :-
%	phrase([],a).
test(uniso_8_1_1_3, [fail]) :-
	phrase([],a).
test(iso1,[]) :-
	phrase([the],[the]).
test(uwn1,[error(instantiation_error)]) :-
	phrase(L,L).
test(uwn2,[L0 == L]) :-
	phrase([],L0,L).
test(uwn3,[sto(rational_trees)]) :-
	phrase([1],L,L).
test(uwn4,[sto(finite_trees),fail]) :-
	phrase([1],L,L).

:- end_tests(phrase).

:- begin_tests(phrase_expansions).

test(1, [G == a(L,[])]) :-
	expand_goal(phrase(a,L), G).
test(2, [B == (b([x], []), []=[1])]) :-
	expand_goal(phrase({phrase(b,[x])},[1]),B).
test(3, []) :-
	expand_goal(phrase(L,L,L),G),
	(L,G) \== ([],[]=[]).
test(nonlin, [G =@= phrase(Lc,Lc,Lc)]) :-
	expand_goal(phrase(L,L,L),G).
test(nonlin, [G =@= phrase(Lc,Lc)]) :-
	expand_goal(phrase(L,L),G).
test(5, [G == quoniam]) :-
	expand_goal(quoniam,G).
test(6,[G =@= spec(_Xs2c,[],_Xs1c,[])]) :-
	expand_goal(phrase(spec(_Xs2,[]),_Xs1),G).
:- end_tests(phrase_expansions).

:- begin_tests(rule_expansions).

test(1, [R == (a :- b([1],[]))]) :-
	expand_term((a :- phrase(b,[1])),R).
test(2, [R =@= (alleq(Ys1) :- spec(Ys2,[],Ys1,[]), alleq(Ys2))]) :-
	expand_term(( alleq(Xs1) :- phrase(spec(Xs2,[]),Xs1),	alleq(Xs2) ), R).
test(3, [R0 == R]) :- % too dangerous
	R0 = (r(X,L) :-phrase(([1],x:X,[3]),L)),
	expand_term(R0, R).

:- end_tests(rule_expansions).

:- begin_tests(dcg_rule_expansions).

test(1, [R =@= (a(X0,X) :- b(X0,X1), c(X1,X))]) :-
	expand_term((a --> b, c), R).
test(2,[error(instantiation_error)]) :-
	expand_term((_ --> []), _).
test(meta0,[error(instantiation_error)]) :-
	expand_term((_, a-->[]),_).
test(meta0,[R=@= (a(L0,L) :- a(L,L0))]) :-
	expand_term((a, a-->[]),R).
test(meta0,[R=@= (a(L0,L) :- b(L,L0))]) :-
	expand_term((a, b-->[]),R).
test(meta1,[R =@= (a([1,2|L],[3|L]):-true)]) :-
	expand_term((a,[3]-->[1,2]),R).
%test(module,[R =@= ex(L,L)]) :-  % Item#282
%	expand_term((ex --> prolog:[]), R).

:- end_tests(dcg_rule_expansions).
:- begin_tests(context).
a, [_] --> !,{fail}. % ITEM
a --> [_].
test(steadfastness,[fail]) :-
	phrase(a,[quidquid],[]).

b, d --> [].
d --> [ce,ci].
test(generalcontext,[Xs0/Xs==Xs0/[ce,ci|Xs0]]) :-
	phrase(b, Xs0, Xs).

e --> [].
e,({},[]) --> [].
e,{},[] --> [].
e,{},([],{},[]) --> [].
e,({},[]),{},[] --> [].
e,[],[],[],[] --> [].
e --> b,d.
e,[] --> b,[ce,ci].
e,[] --> b,b,[ce,ci,ce,ci].
e --> b,b,[ce,ci,ce,ci].
e --> b,b,d,b,d,b,b,b,d,d,d,d.

test(epsilonness,[Xss=[[]]]) :-
	setof(Xs,phrase(e, Xs),Xss).
test(epsilonness,[set(Xs == [[]])]) :-
	phrase(e, Xs).


% Abramson & Dahl Logic Grammars

s -->
	[].
s -->
	[a],
	s,
	[b],
	c.

c -->
	[c].
c, [b] -->
	[b],
	c.


test(vd,[all(Xs==[ [],
						 [a,b,c],
						 [a,a,b,c,b,c],
						 [a,a,b,b,c,c],
						 [a,a,a,b,c,b,c,b,c],
						 [a,a,a,b,c,b,b,c,c],
						 [a,a,a,b,b,c,c,b,c],
						 [a,a,a,b,b,c,b,c,c],
						 [a,a,a,b,b,b,c,c,c]])]) :-
	between(0,11,N),
	length(Xs,N),
	phrase(s, Xs).

% From F.Kluzniak, St.Szpakowicz: Prolog for Programmers,
% Academic Press 1985

zeroes, [D] -->
	"0",
	zeroes,
	[D],
	{digit(D)}.
zeroes -->
	[].

digit(0'1).
digit(0'2).
digit(0'3).

exp("000").
exp("0x").
exp("00x").
exp("03").
exp("00003").
exp("").

test(forprogrammers,
		  [all(Xs == ["000", "0x", "00x", "3", "03", "3", "00003", []])]) :-
	exp(Xs0),
	phrase(zeroes,Xs0,Xs).

:- end_tests(context).


