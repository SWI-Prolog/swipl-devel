%   File   : HEAPS.PL
%   Author : R.A.O'Keefe
%   Updated: 29 November 1983
%   Purpose: Implement heaps in Prolog.

:- module(heaps, [
		  add_to_heap/4,
		  empty_heap/1,
		  get_from_heap/4,
		  heap_size/2,
		  heap_to_list/2,
		  list_to_heap/2,
		  min_of_heap/3,
		  min_of_heap/5
		 ]).

/** <module> Priority Queues

    A heap is a labelled binary tree where the key of each node is less
    than or equal to the keys of its sons.  The point of a heap is that
    we can keep on adding new elements to the heap and we can keep on
    taking out the minimum element.  If there are N elements total, the
    total time is O(NlgN).  If you know all the elements in advance, you
    are better off doing a merge-sort, but this file is for when you
    want to do say a best-first search, and have no idea when you start
    how many elements there will be, let alone what they are.

    A heap is represented as a triple t(N, Free, Tree) where N is the
    number of elements in the tree, Free is a list of integers which
    specifies unused positions in the tree, and Tree is a tree made of

	| t			 | terms for empty subtrees and |
	| t(Key,Datum,Lson,Rson) | terms for the rest		|

    The nodes of the tree are notionally numbered like this:

    ==
				    1
		     2				    3
             4               6               5               7
         8      12      10     14       9       13      11     15
      ..  ..  ..  ..  ..  ..  ..  ..  ..  ..  ..  ..  ..  ..  ..  ..
    ==

    The idea is that if the maximum number of elements that have been in
    the heap so far is M, and the tree currently has K elements, the tree
    is some subtreee of the tree of this form having exactly M elements,
    and the Free list is a list of K-M integers saying which of the
    positions in the M-element tree are currently unoccupied.  This free
    list is needed to ensure that the cost of passing N elements through
    the heap is O(NlgM) instead of O(NlgN).  For M say 100 and N say 10^4
    this means a factor of two.  The cost of the free list is slight.
    The storage cost of a heap in a copying Prolog (which Dec-10 Prolog is
    not) is 2K+3M words.

    @tbd   Missing for SICStus compatibility:

   * delete_from_heap(+OldHeap, +Key, ?Datum, ?NewHeap)
	  deletes a single Key-Datum pair in OldHeap producing
	  NewHeap.

   * is_heap(+Heap)
          is true if Heap is a valid heap
	  
   @author R.A.O'Keefe
*/

%%  add_to_heap(+OldHeap, +Key, +Datum, -NewHeap)
%   inserts the new Key-Datum pair into the heap.  The insertion is
%   not stable, that is, if you insert several pairs with the same
%   Key it is not defined which of them will come out first, and it
%   is possible for any of them to come out first depending on the
%   history of the heap.  If you need a stable heap, you could add
%   a counter to the heap and include the counter at the time of
%   insertion in the key.  If the free list is empty, the tree will
%   be grown, otherwise one of the empty slots will be re-used.  (I
%   use imperative programming language, but the heap code is as
%   pure as the trees code, you can create any number of variants
%   starting from the same heap, and they will share what common
%   structure they can without interfering with each other.)

add_to_heap(t(M,[],OldTree), Key, Datum, t(N,[],NewTree)) :- !,
	N is M+1,
	add_to_heap(N, Key, Datum, OldTree, NewTree).
add_to_heap(t(M,[H|T],OldTree), Key, Datum, t(N,T,NewTree)) :-
	N is M+1,
	add_to_heap(H, Key, Datum, OldTree, NewTree).


add_to_heap(1, Key, Datum, _, t(Key,Datum,t,t)) :- !.
add_to_heap(N, Key, Datum, t(K1,D1,L1,R1), t(K2,D2,L2,R2)) :-
	E is N mod 2,
	M is N // 2,
	sort2(Key, Datum, K1, D1, K2, D2, K3, D3),
	add_to_heap(E, M, K3, D3, L1, R1, L2, R2).


add_to_heap(0, N, Key, Datum, L1, R, L2, R) :- !,
	add_to_heap(N, Key, Datum, L1, L2).
add_to_heap(1, N, Key, Datum, L, R1, L, R2) :- !,
	add_to_heap(N, Key, Datum, R1, R2).


sort2(Key1, Datum1, Key2, Datum2, Key1, Datum1, Key2, Datum2) :-
	Key1 @< Key2,
	!.
sort2(Key1, Datum1, Key2, Datum2, Key2, Datum2, Key1, Datum1).


%%  empty_heap(?Heap)
%   is true if Heap is the empty heap

empty_heap(t(0,[],t)).



%%	get_from_heap(+OldHeap, ?Key, ?Datum, -NewHeap)
%
%	returns the Key-Datum pair in OldHeap with the smallest Key, and
%	also a New Heap which is the   Old  Heap with that pair deleted.
%	The easy part is picking off the smallest element. The hard part
%	is repairing the heap structure using repair_heap/4.

get_from_heap(t(N,Free,t(Key,Datum,L,R)), Key, Datum, t(M,[Hole|Free],Tree)) :-
	M is N-1,
	repair_heap(L, R, Tree, Hole).


%%	repair_heap(+Heap1, +Heap2, -Heap, -GapPos)
%
%	repair_heap/4 takes a pair of heaps and returns a new heap built
%	from their elements, and the position number   of the gap in the
%	new tree. Note that repair_heap is *not* tail-recursive.

repair_heap(t(K1,D1,L1,R1), t(K2,D2,L2,R2), t(K2,D2,t(K1,D1,L1,R1),R3), N) :-
	K2 @< K1,
	!,
	repair_heap(L2, R2, R3, M),
	N is 2*M+1.
repair_heap(t(K1,D1,L1,R1), t(K2,D2,L2,R2), t(K1,D1,L3,t(K2,D2,L2,R2)), N) :- !,
	repair_heap(L1, R1, L3, M),
	N is 2*M.
repair_heap(t(K1,D1,L1,R1), t, t(K1,D1,L3,t), N) :- !,
	repair_heap(L1, R1, L3, M),
	N is 2*M.
repair_heap(t, t(K2,D2,L2,R2), t(K2,D2,t,R3), N) :- !,
	repair_heap(L2, R2, R3, M),
	N is 2*M+1.
repair_heap(t, t, t, 1) :- !.



%%  heap_size(+Heap, ?Size)
%   reports the number of elements currently in the heap.

heap_size(t(Size,_,_), Size).



%%  heap_to_list(+Heap, -List)
%   returns the current set of Key-Datum pairs in the Heap as a
%   List, sorted into ascending order of Keys.  This is included
%   simply because I think every data structure foo ought to have
%   a foo_to_list and list_to_foo relation (where, of course, it
%   makes sense!) so that conversion between arbitrary data
%   structures is as easy as possible.  This predicate is basically
%   just a merge sort, where we can exploit the fact that the tops
%   of the subtrees are smaller than their descendants.

heap_to_list(t(_,_,Tree), List) :-
	heap_tree_to_list(Tree, List).


heap_tree_to_list(t, []) :- !.
heap_tree_to_list(t(Key,Datum,Lson,Rson), [Key-Datum|Merged]) :-
	heap_tree_to_list(Lson, Llist),
	heap_tree_to_list(Rson, Rlist),
	heap_tree_to_list(Llist, Rlist, Merged).


heap_tree_to_list([H1|T1], [H2|T2], [H2|T3]) :-
	H2 @< H1,
	!,
	heap_tree_to_list([H1|T1], T2, T3).
heap_tree_to_list([H1|T1], T2, [H1|T3]) :- !,
	heap_tree_to_list(T1, T2, T3).
heap_tree_to_list([], T, T) :- !.
heap_tree_to_list(T, [], T).



%%  list_to_heap(+List, -Heap)
%   takes a list of Key-Datum pairs (such as keysort could be used to
%   sort) and forms them into a heap.  We could do that a wee bit
%   faster by keysorting the list and building the tree directly, but
%   this algorithm makes it obvious that the result is a heap, and
%   could be adapted for use when the ordering predicate is not @<
%   and hence keysort is inapplicable.

list_to_heap(List, Heap) :-
	list_to_heap(List, 0, t, Heap).


list_to_heap([], N, Tree, t(N,[],Tree)) :- !.
list_to_heap([Key-Datum|Rest], M, OldTree, Heap) :-
	N is M+1,
	add_to_heap(N, Key, Datum, OldTree, MidTree),
	list_to_heap(Rest, N, MidTree, Heap).



%%  min_of_heap(+Heap, ?Key, ?Datum)
%   returns the Key-Datum pair at the top of the heap (which is of
%   course the pair with the smallest Key), but does not remove it
%   from the heap.  It fails if the heap is empty.

%%  min_of_heap(+Heap, ?Key1, ?Datum1, ?Key2, ?Datum2)
%   returns the smallest (Key1) and second smallest (Key2) pairs in
%   the heap, without deleting them.  It fails if the heap does not
%   have at least two elements.

min_of_heap(t(_,_,t(Key,Datum,_,_)), Key, Datum).


min_of_heap(t(_,_,t(Key1,Datum1,Lson,Rson)), Key1, Datum1, Key2, Datum2) :-
	min_of_heap(Lson, Rson, Key2, Datum2).


min_of_heap(t(Ka,_,_,_), t(Kb,Db,_,_), Kb, Db) :-
	Kb @< Ka, !.
min_of_heap(t(Ka,Da,_,_), _, Ka, Da).
min_of_heap(t, t(Kb,Db,_,_), Kb, Db).

