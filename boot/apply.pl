/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    jan@swi.psy.uva.nl

    Purpose: calling predicates
*/

:- module($apply, [
	checklist/2, 
	maplist/3, 
	sublist/3, 
	forall/2]).

:- module_transparent
	checklist/2, 
	checklist2/2, 
	maplist/3, 
	maplist2/3, 
	sublist/3, 
	forall/2.

%	checklist(+Goal, +List)
%
%	True if Goal can succesfully be applied on all elements of List.
%	Arguments are reordered to gain performance as well as to make
%	the predicate deterministic under normal circumstances.

checklist(Goal, List) :-
	checklist2(List, Goal).

checklist2([], _).
checklist2([Elem|Tail], Goal) :-
	call(Goal, Elem), 
	checklist2(Tail, Goal).

%	maplist(+Goal, +List1, ?List2)
%	True if Goal can succesfully be applied to all succesive pairs
%	of elements of List1 and List2.

maplist(Goal, List1, List2) :-
	maplist2(List1, List2, Goal).

maplist2([], [], _).
maplist2([Elem1|Tail1], [Elem2|Tail2], Goal) :-
	call(Goal, Elem1, Elem2), 
	maplist2(Tail1, Tail2, Goal).

%	sublist(+Goal, +List1, ?List2)
%	Succeeds if List2 unifies with a list holding those terms for wich
%	apply(Goal, Elem) succeeds.

sublist(_, [], []) :- !.
sublist(Goal, [H|T], Sub) :-
	call(Goal, H), !, 
	Sub = [H|R], 
	sublist(Goal, T, R).
sublist(Goal, [_|T], R) :-
	sublist(Goal, T, R).

%	forall(+Condition, +Action)
%	True if Action if true for all variable bindings for which Condition
%	if true.

forall(Cond, Action) :-
	\+ (Cond, \+ Action).
