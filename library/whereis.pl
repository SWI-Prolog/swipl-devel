/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    jan@swi.psy.uva.nl

    Purpose: Find predicates
*/

:- module(whereis,
	[ whereis/1
	]).

:- style_check(+dollar).

%	whereis(+Spec)
%	Find predicate definition.

whereis(Name/Arity) :- !,
	whereis(Name, Arity).
whereis(Name) :-
	atom(Name), !,
	whereis(Name, _).
whereis(Head) :-
	functor(Head, Name, Arity),
	whereis(Name, Arity).

whereis(Name, Arity) :-
	(   nonvar(Arity)
	->  functor(Head, Name, Arity)
	;   true
	),
	findall(Module:Head - 0,
		current_predicate(Name, Module:Head),
		Loaded),
	findall(Head - Where,
		find_library_predicate(Name, Head, Where),
		Library),
	append(Loaded, Library, Places),
	sort(Places, Sorted),
	checklist(output, Sorted).

find_library_predicate(Name, Head, Where) :-
	(   nonvar(Head)
	->  functor(Head, Name, Arity)
	;   true
	),
	$find_library(Name, Arity, Where).

output(Module:Head - 0) :-
	true.
output(Head - Library) :-
	true.
	

	
