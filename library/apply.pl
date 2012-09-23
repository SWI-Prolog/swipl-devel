/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2012, University of Amsterdam
			      VU University Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(apply,
	  [ include/3,			% :Pred, +List, -Ok
	    exclude/3,			% :Pred. +List, -NotOk
	    partition/4,		% :Pred, +List, -Included, -Excluded
	    partition/5,		% :Pred, +List, ?Less, ?Equal, ?Greater
	    maplist/2,			% :Pred, +List
	    maplist/3,			% :Pred, ?List, ?List
	    maplist/4,			% :Pred, ?List, ?List, ?List
	    maplist/5,			% :Pred, ?List, ?List, ?List, ?List
	    foldl/4,			% :Pred, +List, ?V0, ?V
	    foldl/5,			% :Pred, +List1, +List2, ?V0, ?V
	    foldl/6,			% :Pred, +List1, +List2, +List3, ?V0, ?V
	    foldl/7,			% :Pred, +List1, +List2, +List3, +List4,
					% ?V0, ?V
	    scanl/4,			% :Pred, +List, ?V0, ?Vs
	    scanl/5,			% :Pred, +List1, +List2, ?V0, ?Vs
	    scanl/6,			% :Pred, +List1, +List2, +List3, ?V0, ?Vs
	    scanl/7			% :Pred, +List1, +List2, +List3, +List4,
					% ?V0, ?Vs
	  ]).
:- use_module(library(error)).

/** <module> Apply predicates on a list

This module defines meta-predicates  that  apply   a  predicate  on  all
members of a list.

@see	apply_macros.pl provides compile-time expansion for part of this
	library.
@see	http://www.cs.otago.ac.nz/staffpriv/ok/pllib.htm
@tbd	Add include/4, include/5, exclude/4, exclude/5
*/

:- meta_predicate
	include(1, +, -),
	exclude(1, +, -),
	partition(1, +, -, -),
	partition(2, +, -, -, -),
	maplist(1, ?),
	maplist(2, ?, ?),
	maplist(3, ?, ?, ?),
	maplist(4, ?, ?, ?, ?),
	foldl(3, +, +, -),
	foldl(4, +, +, +, -),
	foldl(5, +, +, +, +, -),
	foldl(6, +, +, +, +, +, -),
	scanl(3, +, +, -),
	scanl(4, +, +, +, -),
	scanl(5, +, +, +, +, -),
	scanl(6, +, +, +, +, +, -).


%%	include(:Goal, +List1, ?List2) is det.
%
%	Filter elements for which Goal succeeds.  True if List2 contains
%	those elements Xi of List1 for which call(Goal, Xi) succeeds.
%
%	@see	Older versions of SWI-Prolog had sublist/3 with the same
%		arguments and semantics.

include(Goal, List, Included) :-
	include_(List, Goal, Included).

include_([], _, []).
include_([X1|Xs1], P, Included) :-
	(   call(P, X1)
	->  Included = [X1|Included1]
	;   Included = Included1
	),
	include_(Xs1, P, Included1).


%%	exclude(:Goal, +List1, ?List2) is det.
%
%	Filter elements for which Goal fails.  True if List2 contains
%	those elements Xi of List1 for which call(Goal, Xi) fails.

exclude(Goal, List, Included) :-
	exclude_(List, Goal, Included).

exclude_([], _, []).
exclude_([X1|Xs1], P, Included) :-
	(   call(P, X1)
	->  Included = Included1
	;   Included = [X1|Included1]
	),
	exclude_(Xs1, P, Included1).


%%	partition(:Pred, +List, ?Included, ?Excluded) is det.
%
%	Filter elements of List according  to   Pred.  True  if Included
%	contains all elements  for  which   call(Pred,  X)  succeeds and
%	Excluded contains the remaining elements.

partition(Pred, List, Included, Excluded) :-
	partition_(List, Pred, Included, Excluded).

partition_([], _, [], []).
partition_([H|T], Pred, Incl, Excl) :-
	(   call(Pred, H)
	->  Incl = [H|I],
	    partition_(T, Pred, I, Excl)
	;   Excl = [H|E],
	    partition_(T, Pred, Incl, E)
	).


%%	partition(:Pred, +List, ?Less, ?Equal, ?Greater) is semidet.
%
%	Filter List according to Pred in three sets. For each element Xi
%	of List, its destination is determined by call(Pred, Xi, Place),
%	where Place must be unified to  one   of  =|<|=, =|=|= or =|>|=.
%	Pred must be deterministic.

partition(Pred, List, Less, Equal, Greater) :-
	partition_(List, Pred, Less, Equal, Greater).

partition_([], _, [], [], []).
partition_([H|T], Pred, L, E, G) :-
	call(Pred, H, Diff),
	partition_(Diff, H, Pred, T, L, E, G).

partition_(<, H, Pred, T, [H|L], E, G) :- !,
	partition_(T, Pred, L, E, G).
partition_(=, H, Pred, T, L, [H|E], G) :- !,
	partition_(T, Pred, L, E, G).
partition_(>, H, Pred, T, L, E, [H|G]) :- !,
	partition_(T, Pred, L, E, G).
partition_(Diff, _, _, _, _, _, _) :-
	must_be(oneof([<.=,>]), Diff).


		 /*******************************
		 *	    MAPLIST/2...	*
		 *******************************/

%%	maplist(:Goal, ?List)
%
%	True if Goal can successfully  be   applied  on  all elements of
%	List. Arguments are reordered to gain  performance as well as to
%	make the predicate deterministic under normal circumstances.

maplist(Goal, List) :-
	maplist_(List, Goal).

maplist_([], _).
maplist_([Elem|Tail], Goal) :-
	call(Goal, Elem),
	maplist_(Tail, Goal).

%%	maplist(:Goal, ?List1, ?List2)
%
%	As maplist/2, operating on pairs of elements from two lists.

maplist(Goal, List1, List2) :-
	maplist_(List1, List2, Goal).

maplist_([], [], _).
maplist_([Elem1|Tail1], [Elem2|Tail2], Goal) :-
	call(Goal, Elem1, Elem2),
	maplist_(Tail1, Tail2, Goal).

%%	maplist(:Goal, ?List1, ?List2, ?List3)
%
%	As maplist/2, operating on triples of elements from three lists.

maplist(Goal, List1, List2, List3) :-
	maplist_(List1, List2, List3, Goal).

maplist_([], [], [], _).
maplist_([Elem1|Tail1], [Elem2|Tail2], [Elem3|Tail3], Goal) :-
	call(Goal, Elem1, Elem2, Elem3),
	maplist_(Tail1, Tail2, Tail3, Goal).


%%	maplist(:Goal, ?List1, ?List2, ?List3, ?List4)
%
%	As maplist/2, operating on  quadruples   of  elements  from four
%	lists.

maplist(Goal, List1, List2, List3, List4) :-
	maplist_(List1, List2, List3, List4, Goal).

maplist_([], [], [], [], _).
maplist_([Elem1|Tail1], [Elem2|Tail2], [Elem3|Tail3], [Elem4|Tail4], Goal) :-
	call(Goal, Elem1, Elem2, Elem3, Elem4),
	maplist_(Tail1, Tail2, Tail3, Tail4, Goal).


		 /*******************************
		 *	      FOLDL		*
		 *******************************/

%%	foldl(:Goal, +List, +V0, -V).
%%	foldl(:Goal, +List1, +List2, +V0, -V).
%%	foldl(:Goal, +List1, +List2, +List3, +V0, -V).
%%	foldl(:Goal, +List1, +List2, +List3, +List4, +V0, -V).
%
%	Fold a list, using arguments of the   list as left argument. The
%	foldl family of predicates is defined by:
%
%	  ==
%	  foldl(P, [X11,...,X1n], ..., [Xm1,...,Xmn], V0, Vn) :-
%		P(X11, ..., Xm1, V0, V1),
%		...
%		P(X1n, ..., Xmn, V', Vn).
%	  ==

foldl(Goal, List, V0, V) :-
	foldl_(List, Goal, V0, V).

foldl_([], _, V, V).
foldl_([H|T], Goal, V0, V) :-
	call(Goal, H, V0, V1),
	foldl_(T, Goal, V1, V).


foldl(Goal, List1, List2, V0, V) :-
	foldl_(List1, List2, Goal, V0, V).

foldl_([], [], _, V, V).
foldl_([H1|T1], [H2|T2], Goal, V0, V) :-
	call(Goal, H1, H2, V0, V1),
	foldl_(T1, T2, Goal, V1, V).


foldl(Goal, List1, List2, List3, V0, V) :-
	foldl_(List1, List2, List3, Goal, V0, V).

foldl_([], [], [], _, V, V).
foldl_([H1|T1], [H2|T2], [H3|T3], Goal, V0, V) :-
	call(Goal, H1, H2, H3, V0, V1),
	foldl_(T1, T2, T3, Goal, V1, V).


foldl(Goal, List1, List2, List3, List4, V0, V) :-
	foldl_(List1, List2, List3, List4, Goal, V0, V).

foldl_([], [], [], [], _, V, V).
foldl_([H1|T1], [H2|T2], [H3|T3], [H4|T4], Goal, V0, V) :-
	call(Goal, H1, H2, H3, H4, V0, V1),
	foldl_(T1, T2, T3, T4, Goal, V1, V).


		 /*******************************
		 *	       SCANL		*
		 *******************************/

%%	scanl(:Goal, +List, +V0, -Values).
%%	scanl(:Goal, +List1, +List2, +V0, -Values).
%%	scanl(:Goal, +List1, +List2, +List3, +V0, -Values).
%%	scanl(:Goal, +List1, +List2, +List3, +List4, +V0, -Values).
%
%	Left scan of  list.  The  scanl   family  of  higher  order list
%	operations is defined by:
%
%	  ==
%	  scanl(P, [X11,...,X1n], ..., [Xm1,...,Xmn], V0,
%		[V0,V1,...,Vn]) :-
%		P(X11, ..., Xmn, V0, V1),
%		...
%	        P(X1n, ..., Xmn, V', Vn).
%	  ==

scanl(Goal, List, V0, [V0|Values]) :-
	scanl_(List, Goal, V0, Values).

scanl_([], _, _, []).
scanl_([H|T], Goal, V, [VH|VT]) :-
	call(Goal, H, V, VH),
	scanl_(T, Goal, VH, VT).


scanl(Goal, List1, List2, V0, [V0|Values]) :-
	scanl_(List1, List2, Goal, V0, Values).

scanl_([], [], _, _, []).
scanl_([H1|T1], [H2|T2], Goal, V, [VH|VT]) :-
	call(Goal, H1, H2, V, VH),
	scanl_(T1, T2, Goal, VH, VT).


scanl(Goal, List1, List2, List3, V0, [V0|Values]) :-
	scanl_(List1, List2, List3, Goal, V0, Values).

scanl_([], [], [], _, _, []).
scanl_([H1|T1], [H2|T2], [H3|T3], Goal, V, [VH|VT]) :-
	call(Goal, H1, H2, H3, V, VH),
	scanl_(T1, T2, T3, Goal, VH, VT).


scanl(Goal, List1, List2, List3, List4, V0, [V0|Values]) :-
	scanl_(List1, List2, List3, List4, Goal, V0, Values).

scanl_([], [], [], [], _, _, []).
scanl_([H1|T1], [H2|T2], [H3|T3], [H4|T4], Goal, V, [VH|VT]) :-
	call(Goal, H1, H2, H3, H4, V, VH),
	scanl_(T1, T2, T3, T4, Goal, VH, VT).
