/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    jan@swi.psy.uva.nl

    Purpose: findall, bagof and setof
*/

:- module($bags, [
	findall/3, 
	bagof/3, 
	setof/3]).

:- module_transparent
	findall/3, 
	setof/3, 
	bagof/3, 
	assert_bag/2.

%	findall(-Var, +Goal, -Bag)
%	Bag holds all alternatives for Var  in  Goal.   Bag  might  hold
%	duplicates.   Equivalent  to bagof, using the existence operator
%	(^) on all free variables of Goal.  Succeeds with Bag  =  []  if
%	Goal fails immediately.

findall(Var, Goal, Bag) :-
	assert_bag(v-Var, Goal),
	collect_bags([], [v-VarBag]), !,
	VarBag = Bag.
findall(_, _, []).

%	setof(+Var, +Goal, -Set
%	Equivalent to bagof/3, but sorts the resulting bag  and  removes
%	duplicate answers.

setof(Var, Goal, Set) :-
	bagof(Var, Goal, Bag), 
	sort(Bag, Set).

%	bagof(+Var, +Goal, -Bag)
%	Implements Clocksin and  Melish's  bagof/3  predicate.   Bag  is
%	unified  with the alternatives of Var in Goal, Free variables of
%	Goal are bound, unless asked not to with the existence  operator
%	(^).

bagof(Gen, Goal, Bag) :-
	$e_free_variables(Gen^Goal, Vars),
	assert_bag(Vars-Gen, Goal), 
	collect_bags([], Bags), 
	member(Vars-Bag, Bags),
	Bag \== [].

assert_bag(Templ, G) :-
	$record_bag(-), 
	G,
	    $record_bag(Templ), 
	fail.
assert_bag(_, _).

collect_bags(Sofar, Result) :-
	$collect_bag(Vars, Bag), !,
	collect_bags([Vars-Bag|Sofar], Result).
collect_bags(L, L).
