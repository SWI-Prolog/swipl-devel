/*  Part of SWI-Prolog

    Author:        Lars Buitinck
    E-mail:        larsmans@gmail.com
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2010, Lars Buitinck

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
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(heaps,
          [ add_to_heap/4,		% +Heap0, +Priority, ?Key, -Heap
	    delete_from_heap/4,		% +Heap0, -Priority, +Key, -Heap
	    empty_heap/1,		% +Heap
	    get_from_heap/4,		% ?Heap0, ?Priority, ?Key, -Heap
	    heap_size/2,		% +Heap, -Size:int
	    heap_to_list/2,		% +Heap, -List:list
	    is_heap/1,			% +Term
	    list_to_heap/2,		% +List:list, -Heap
	    merge_heaps/3,		% +Heap0, +Heap1, -Heap
	    min_of_heap/3,		% +Heap, ?Priority, ?Key
	    min_of_heap/5		% +Heap, ?Priority1, ?Key1,
	    				%        ?Priority2, ?Key2
          ]).

/** <module> heaps/priority queues
 *
 * Heaps are data structures that return the entries inserted into them in an
 * ordered fashion, based on a priority. This makes them the data structure of
 * choice for implementing priority queues, a central element of algorithms
 * such as best-first/A* search and Kruskal's minimum-spanning-tree algorithm.
 *
 * This module implements min-heaps, meaning that items are retrieved in
 * ascending order of key/priority. It was designed to be compatible with
 * the SICStus Prolog library module of the same name. The merge_heaps/3
 * predicate is an SWI-specific extension. The portray_heap/1 predicate is
 * not implemented.
 *
 * The current version implements top-down skew heaps, as described by Sleator
 * and Tarjan (1986). All operations can be performed in at most O(lg n)
 * amortized time, except for delete_from_heap/4, heap_to_list/2 and
 * list_to_heap/2.
 *
 * @author Lars Buitinck
 */

/*
 * Heaps are represented as heap(H,Size) terms, where H is the "actual heap"
 * and Size is an integer. The "actual heaps" are either nil (empty) or terms
 * of the form t(L,TopElem,TopPrio,R), where L and R are again "actual heaps".
 */

%%	add_to_heap(+Heap0, +Priority, ?Key, -Heap) is semidet.
%
%	Adds Key with priority Priority  to   Heap0,  constructing a new
%	heap in Heap.

add_to_heap(heap(Q0,M),P,X,heap(Q1,N)) :-
	meld(Q0,t(X,P,nil,nil),Q1),
	N is M+1.

%%	delete_from_heap(+Heap0, -Priority, +Key, -Heap) is semidet.
%
%	Deletes Key from Heap0, leaving its priority in Priority and the
%	resulting data structure  in  Heap.   Useful  for  changing  the
%	priority of Key. Fails if Key is not found in Heap0.
%
%	@bug This predicate is extremely inefficient.

delete_from_heap(Q0,P,X,Q) :-
	get_from_heap(Q0,P,X,Q), !.
delete_from_heap(Q0,Px,X,Q) :-
	get_from_heap(Q0,Py,Y,Q1),
	delete_from_heap(Q1,Px,X,Q2),
	add_to_heap(Q2,Py,Y,Q).

%%	empty_heap(?Heap) is semidet.
%
%       True if Heap is an empty heap.

empty_heap(heap(nil,0)).

%%	get_from_heap(?Heap0, ?Priority, ?Key, -Heap) is semidet.
%
%	Retrieves the minimum-priority  pair   Priority-Key  from Heap0.
%	Heap is Heap0 with that pair removed.

get_from_heap(heap(t(X,P,L,R),M),P,X,heap(Q,N)) :-
	meld(L,R,Q),
	N is M-1.

%%	heap_size(+Heap, -Size:int) is det.
%
%	Determines the number of elements in Heap.

heap_size(heap(_,N),N).

%%	heap_to_list(+Heap, -List:list) is det.
%
%	Constructs a list List  of   Priority-Element  terms, ordered by
%	(ascending) priority.

heap_to_list(Q,L) :-
	to_list(Q,L).
to_list(heap(nil,0),[]) :- !.
to_list(Q0,[P-X|Xs]) :-
	get_from_heap(Q0,P,X,Q),
	heap_to_list(Q,Xs).

%%	is_heap(+X) is semidet.
%
%	Returns true is X is a heap.
%
%	@bug does not test the integrity of the actual heap.

is_heap(V) :-
	var(V), !, fail.
is_heap(heap(_,N)) :-
	integer(N).

%%	list_to_heap(+List:list, -Heap) is det.
%
%	If List is a list of   Priority-Element terms, constructs a heap
%	out of List.

list_to_heap(Xs,Q) :-
	empty_heap(Empty),
	list_to_heap(Xs,Empty,Q).

list_to_heap([],Q,Q).
list_to_heap([P-X|Xs],Q0,Q) :-
	add_to_heap(Q0,P,X,Q1),
	list_to_heap(Xs,Q1,Q).

%%	min_of_heap(+Heap, ?Priority, ?Key) is semidet.
%
%	Unifies Key with  the  minimum-priority   element  of  Heap  and
%	Priority with its priority value.

min_of_heap(heap(t(X,P,_,_),_),P,X).

%%	min_of_heap(+Heap, ?Priority1, ?Key1, ?Priority2, ?Key2) is semidet.
%
%	Gets the two minimum-priority elements from Heap.

min_of_heap(Q,Px,X,Py,Y) :-
	get_from_heap(Q,Px,X,Q0),
	min_of_heap(Q0,Py,Y).

%%	merge_heaps(+Heap0, +Heap1, -Heap) is det.
%
%	Merge the two heaps Heap0 and Heap1 in Heap.

merge_heaps(heap(L,K),heap(R,M),heap(Q,N)) :-
	meld(L,R,Q),
	N is K+M.

/*
 * The meld/3 predicate is the backbone of the implementation.
 * It merges two heaps according to the skew heap definition.
 */

meld(nil,Q,Q) :- !.
meld(Q,nil,Q).
meld(L,R,Q) :-
	L = t(X,Px,LL,LR),
	R = t(Y,Py,RL,RR),
	(   Px @< Py
	->  Q = t(X,Px,L1,LL),
	    meld(LR,R,L1)
	;   Q = t(Y,Py,L1,RL),
	    meld(L,RR,L1)
	).
