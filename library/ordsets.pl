/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2005, University of Amsterdam

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

:- module(ordsets,
	  [ is_ordset/1,		% @Term
	    list_to_ord_set/2,		% +List, -OrdSet
	    ord_add_element/3,		% +Set, +Element, -NewSet
	    ord_del_element/3,		% +Set, +Element, -NewSet
	    ord_intersect/2,		% +Set1, +Set2 (test non-empty)
	    ord_intersect/3,		% +Set1, +Set2, -Intersection
	    ord_intersection/3,		% +Set1, +Set2, -Intersection
	    ord_intersection/4,		% +Set1, +Set2, -Intersection, -Diff
	    ord_disjoint/2,		% +Set1, +Set2
	    ord_subtract/3,		% +Set, +Delete, -Remaining
	    ord_union/3,		% +Set1, +Set2, -Union
	    ord_union/4,		% +Set1, +Set2, -Union, -New
	    ord_subset/2,		% +Sub, +Super (test Sub is in Super)
					% Non-Quintus extensions
	    ord_empty/1,		% ?Set
	    ord_memberchk/2		% +Element, +Set
	  ]).
:- use_module(library(oset)).
:- set_prolog_flag(generate_debug_info, false).

/** <module> Ordered set manipulation

Very incomplete implementation  of   Quintus/SICStus  compatible  ordset
library, partially based on the   contributed  SWI-Prolog library(oset).
Please complete the implementation and contribute   it to the SWI-Prolog
community.

This library was implemented to run the threetap theorem prover.  It was
extended to satisfy requirements by CHR.

@compat	De-facto standard.
@bug	Incomplete
*/

%%	is_ordset(@Term) is semidet.
%
%	True if Term is an ordered set.   All predicates in this library
%	expect ordered sets as input arguments.  Failing to fullfil this
%	assumption results in undefined   behaviour.  Typically, ordered
%	sets are created by predicates  from   this  library,  sort/2 or
%	setof/3.

is_ordset(Term) :-
	catch(sort(Term, Term), _, fail).


%%	ord_empty(List)
%	
%	True if List is the empty ordered set.  Not part of Quintus

ord_empty([]).


%%	list_to_ord_set(+List, -OrdSet)
%
%	Transform a list into an ordered set.  This is the same as
%	sorting the list.

list_to_ord_set(List, Set) :-
	sort(List, Set).


%%	ord_intersect(+Set1, +Set2)
%
%	Succeed if both ordered sets have a non-empty intersection

ord_intersect([H1|T1], L2) :-
	ord_intersect_(L2, H1, T1).

ord_intersect_([H2|T2], H1, T1) :-
	compare(Order, H1, H2),
	ord_intersect__(Order, H1, T1, H2, T2).

ord_intersect__(<, _H1, T1,  H2, T2) :-
	ord_intersect_(T1, H2, T2).
ord_intersect__(=, _H1, _T1, _H2, _T2).
ord_intersect__(>, H1, T1,  _H2, T2) :-
	ord_intersect_(T2, H1, T1).


%%	ord_disjoint(+Set1, +Set2)
%	
%	True if Set1 and Set2 have no common elements

ord_disjoint(Set1, Set2) :-
	\+ ord_intersect(Set1, Set2).


%%	ord_intersect(+Set1, +Set2, -Intersection)
%	
%	Intersection  holds  the  common  elements  of  Set1  and  Set2.
%	
%	@deprecated Use ord_intersection/3

ord_intersect(Set1, Set2, Intersection) :-
	oset_int(Set1, Set2, Intersection).


%%	ord_intersection(+Set1, +Set2, -Intersection)
%	
%	Intersection holds the common elements of Set1 and Set2.

ord_intersection(Set1, Set2, Intersection) :-
	oset_int(Set1, Set2, Intersection).


%%	ord_intersection(+Set1, +Set2, ?Intersection, ?Difference) is det.
%
%	Intersection  and  difference   between    two   ordered   sets.
%	Intersection is the intersection between   Set1  and Set2, while
%	Difference is Set2\Set1.
%	
%	@see ord_intersection/3 and ord_subtract/3.

ord_intersection([], L, [], L) :- !.
ord_intersection([_|_], [], [], []) :- !.
ord_intersection([H1|T1], [H2|T2], Intersection, Difference) :-
	compare(Diff, H1, H2),
	ord_intersection2(Diff, H1, T1, H2, T2, Intersection, Difference).

ord_intersection2(=, H1, T1, _H2, T2, [H1|T], Difference) :-
	ord_intersection(T1, T2, T, Difference).
ord_intersection2(<, _, T1, H2, T2, Intersection, Difference) :-
	ord_intersection(T1, [H2|T2], Intersection, Difference).
ord_intersection2(>, H1, T1, H2, T2, Intersection, [H2|HDiff]) :-
	ord_intersection([H1|T1], T2, Intersection, HDiff).


%%	ord_add_element(+Set1, +Element, ?Set2)
%
%	Insert an element into the set

ord_add_element(Set1, Element, Set2) :-
	oset_addel(Set1, Element, Set2).


%%	ord_del_element(+Set, +Element, -NewSet)
%	
%	Delete an element from an ordered set

ord_del_element(Set, Element, NewSet) :-
	oset_delel(Set, Element, NewSet).


%%	ord_memberchk(+Element, +Set)
%	
%	Check membership. This could stop comparing   we have passed the
%	right value, saving scanning  (on  average)   half  the  list if
%	Element is not in Set. Probably the built-in memberchk/2 will be
%	faster.  Not part of Quintus.

ord_memberchk(Element, Set) :-
	memberchk(Element, Set).


%%	ord_subset(+Sub, +Super)
%
%	Is true if all element of Sub are in Super

ord_subset([], _).
ord_subset([H1|T1], [H2|T2]) :-
	compare(Order, H1, H2),
	ord_subset_(Order, H1, T1, T2).

ord_subset_(>, H1, T1, [H2|T2]) :-
	compare(Order, H1, H2),
	ord_subset_(Order, H1, T1, T2).
ord_subset_(=, _, T1, T2) :-
	ord_subset(T1, T2).


%%	ord_subtract(+InOSet, +NotInOSet, -Diff)
%
%	Diff is the set holding all elements of InOSet that are not in
%	NotInOSet.

ord_subtract(InOSet, NotInOSet, Diff) :-
	oset_diff(InOSet, NotInOSet, Diff).


%%	ord_union(+Set1, +Set2, ?Union)
%
%	Union is the union of Set1 and Set2

ord_union(Set1, Set2, Union) :-
	oset_union(Set1, Set2, Union).


%%	ord_union(+Set1, +Set2, -Union,	-New)
%	
%	True if Union iff ord_union(Set1, Set2, Union) and
%%	ord_subtract(Set2, Set1, New).

ord_union([], Set2, Set2, Set2).
ord_union([H|T], Set2, Union, New) :-
	ord_union_1(Set2, H, T, Union, New).

ord_union_1([], H, T, [H|T], []).
ord_union_1([H2|T2], H, T, Union, New) :-
	compare(Order, H, H2),
	ord_union(Order, H, T, H2, T2, Union, New).

ord_union(<, H, T, H2, T2, [H|Union], New) :-
	ord_union_2(T, H2, T2, Union, New).
ord_union(>, H, T, H2, T2, [H2|Union], [H2|New]) :-
	ord_union_1(T2, H, T, Union, New).
ord_union(=, H, T, _, T2, [H|Union], New) :-
	ord_union(T, T2, Union, New).

ord_union_2([], H2, T2, [H2|T2], [H2|T2]).
ord_union_2([H|T], H2, T2, Union, New) :-
	compare(Order, H, H2),
	ord_union(Order, H, T, H2, T2, Union, New).

