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
	maplist/3, 
	sublist/3, 
	forall/2.

%	checklist(+Goal, +List)
%	True if Goal can succesfully be applied on all elements of List.

checklist(_, []).
checklist(Goal, [Elem|Tail]) :-
	call(Goal, Elem), 
	checklist(Goal, Tail).

%	maplist(+Goal, +List1, ?List2)
%	True if Goal can succesfully be applied to all succesive pairs
%	of elements of List1 and List2.

maplist(_, [], []).
maplist(Goal, [Elem1|Tail1], [Elem2|Tail2]) :-
	call(Goal, Elem1, Elem2), 
	maplist(Goal, Tail1, Tail2).

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
