/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    jan@swi.psy.uva.nl

    Purpose: provide some nice user utilities
*/

:- module(toolkit,
	[ lp/1
	, llp/1
	, lp_predicate/1
	]).
	
:- use_module(library(am_match),
	[ am_compile/1
	, am_match/1
	]).

:- module_transparent
	lp/1, 
	llp/1, 
	lp_matching_predicates/2, 
	lp_predicate/1.

:- op(900, fy, [lp, llp]).

lp(Expression) :-
	lp_matching_predicates(Expression, Predicates), 
	lp_print_predicates(Predicates, 76).

llp(Expression) :-
	lp_matching_predicates(Expression, Predicates), 
	checklist(listing, Predicates).

lp_matching_predicates(Expression, Predicates) :-
	am_compile(Expression), !, 
	bagof(Pred, lp_predicate(Pred), Predicates), !.
lp_matching_predicates(_, []).

lp_predicate(Func/Ar) :-
	current_predicate(Func, Term), 
	am_match(Func), 
	functor(Term, Func, Ar).

/*  show a list of predicates on the terminal.
*/

lp_print_predicates(RawList, Width) :-
	msort(RawList, List), 
	lp_widest_predicate(List, WA), 
	NC is Width // (WA+2),	 		% Number of collums
	WC is Width // NC, 			% Width of collums
	lp_list_predicates(List, NC, WC, 0), !.

lp_list_predicates([], _, _, 0) :- !.
lp_list_predicates([], _, _, _) :- nl, !.
lp_list_predicates([F/A|Tail], NC, WC, NC_1) :-
	succ(NC_1, NC), 
	write(F), write('/'), write(A), nl, 
	lp_list_predicates(Tail, NC, WC, 0).
lp_list_predicates([H|Tail], NC, WC, C) :-
	lp_write_left(H, WC), 
	succ(C, NewC), 
	lp_list_predicates(Tail, NC, WC, NewC), !.

lp_write_left(F/A, Width) :-
	write(F), write('/'), write(A), 
	name(F, LF), 
	length(LF, WF), 
	name(A, LA), 
	length(LA, WA), 
	tab(Width-WF-WA), !.

lp_widest_predicate([], 0).
lp_widest_predicate([F/A|T], W) :-
	name(F, LF), 
	length(LF, WF), 
	name(A, LA), 
	length(LA, WA), 
	WH is WF + WA + 1, 
	lp_widest_predicate(T, WT), 
	lp_biggest(WH, WT, W), !.

lp_biggest(A, B, A) :- A >= B, !.
lp_biggest(_, B, B).
