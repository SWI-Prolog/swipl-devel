/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2002, University of Amsterdam

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
	  [ list_to_ord_set/2,
	    ord_intersect/2,
	    ord_add_element/3,
	    ord_subset/2,
	    ord_union/3
	  ]).
:- use_module(library(oset)).
:- set_prolog_flag(generate_debug_info, false).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Very incomplete implementation  of   Quintus/SICStus  compatible  ordset
library, partially based on the   contributed  SWI-Prolog library(oset).
Please complete the implementation and contribute   it to the SWI-Prolog
community.

This library was implemented to run the threetap theorem prover.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%	list_to_ord_set(+List, -OrdSet)
%
%	Transform a list into an ordered set.  This is the same as
%	sorting the list.

list_to_ord_set(List, Set) :-
	sort(List, Set).


%	ord_intersect(+Set1, +Set2)
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


%	ord_add_element(+Set1, +Element, ?Set2)
%
%	Insert an element into the set

ord_add_element(Set1, Element, Set2) :-
	oset_addel(Set1, Element, Set2).

%	ord_subset(+Sub, +Super)
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

%	ord_union(+Set1, +Set2, ?Union)
%
%	Union is the union of Set1 and Set2

ord_union(Set1, Set2, Union) :-
	oset_union(Set1, Set2, Union).
