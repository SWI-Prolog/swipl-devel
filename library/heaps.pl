/*  Part of SWI-Prolog

    Author:        Lars Buitinck
    E-mail:        larsmans@gmail.com
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2006-2015, Lars Buitinck
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(heaps,
          [ add_to_heap/4,              % +Heap0, +Priority, ?Key, -Heap
            delete_from_heap/4,         % +Heap0, -Priority, +Key, -Heap
            empty_heap/1,               % +Heap
            get_from_heap/4,            % ?Heap0, ?Priority, ?Key, -Heap
            heap_size/2,                % +Heap, -Size:int
            heap_to_list/2,             % +Heap, -List:list
            is_heap/1,                  % +Term
            list_to_heap/2,             % +List:list, -Heap
            merge_heaps/3,              % +Heap0, +Heap1, -Heap
            min_of_heap/3,              % +Heap, ?Priority, ?Key
            min_of_heap/5,              % +Heap, ?Priority1, ?Key1,
                                        %        ?Priority2, ?Key2
            singleton_heap/3            % ?Heap, ?Priority, ?Key
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
 * the SICStus Prolog library module of the same name. merge_heaps/3 and
 * singleton_heap/3 are SWI-specific extension. The portray_heap/1 predicate
 * is not implemented.
 *
 * Although the data items can be arbitrary Prolog data, keys/priorities must
 * be ordered by @=</2. Be careful when using variables as keys, since binding
 * them in between heap operations may change the ordering.
 *
 * The current version implements pairing heaps. These support insertion and
 * merging both in constant time, deletion of the minimum in logarithmic
 * amortized time (though delete-min, i.e., get_from_heap/3, takes linear time
 * in the worst case).
 *
 * @author Lars Buitinck
 */

/*
 * Heaps are represented as heap(H,Size) terms, where H is a pairing heap and
 * Size is an integer. A pairing heap is either nil or a term
 * t(X,PrioX,Sub) where Sub is a list of pairing heaps t(Y,PrioY,Sub) s.t.
 * PrioX @< PrioY. See predicate is_heap/2, below.
 */

%!  add_to_heap(+Heap0, +Priority, ?Key, -Heap) is semidet.
%
%   Adds Key with priority Priority  to   Heap0,  constructing a new
%   heap in Heap.

add_to_heap(heap(Q0,M),P,X,heap(Q1,N)) :-
    meld(Q0,t(X,P,[]),Q1),
    N is M+1.

%!  delete_from_heap(+Heap0, -Priority, +Key, -Heap) is semidet.
%
%   Deletes Key from Heap0, leaving its priority in Priority and the
%   resulting data structure in Heap.   Fails if Key is not found in
%   Heap0.
%
%   @bug This predicate is extremely inefficient and exists only for
%        SICStus compatibility.

delete_from_heap(Q0,P,X,Q) :-
    get_from_heap(Q0,P,X,Q),
    !.
delete_from_heap(Q0,Px,X,Q) :-
    get_from_heap(Q0,Py,Y,Q1),
    delete_from_heap(Q1,Px,X,Q2),
    add_to_heap(Q2,Py,Y,Q).

%!  empty_heap(?Heap) is semidet.
%
%   True if Heap is an empty heap. Complexity: constant.

empty_heap(heap(nil,0)).

%!  singleton_heap(?Heap, ?Priority, ?Key) is semidet.
%
%   True if Heap is a heap with the single element Priority-Key.
%
%   Complexity: constant.

singleton_heap(heap(t(X,P,[]), 1), P, X).

%!  get_from_heap(?Heap0, ?Priority, ?Key, -Heap) is semidet.
%
%   Retrieves the minimum-priority  pair   Priority-Key  from Heap0.
%   Heap is Heap0 with that pair removed.   Complexity:  logarithmic
%   (amortized), linear in the worst case.

get_from_heap(heap(t(X,P,Sub),M), P, X, heap(Q,N)) :-
    pairing(Sub,Q),
    N is M-1.

%!  heap_size(+Heap, -Size:int) is det.
%
%   Determines the number of elements in Heap. Complexity: constant.

heap_size(heap(_,N),N).

%!  heap_to_list(+Heap, -List:list) is det.
%
%   Constructs a list List  of   Priority-Element  terms, ordered by
%   (ascending) priority. Complexity: $O(n \log n)$.

heap_to_list(Q,L) :-
    to_list(Q,L).
to_list(heap(nil,0),[]) :- !.
to_list(Q0,[P-X|Xs]) :-
    get_from_heap(Q0,P,X,Q),
    heap_to_list(Q,Xs).

%!  is_heap(+X) is semidet.
%
%   Returns true if X is a heap.  Validates the consistency of the
%   entire heap. Complexity: linear.

is_heap(V) :-
    var(V), !, fail.
is_heap(heap(Q,N)) :-
    integer(N),
    nonvar(Q),
    (   Q == nil
    ->  N == 0
    ;   N > 0,
        Q = t(_,MinP,Sub),
        are_pairing_heaps(Sub, MinP)
    ).

% True iff 1st arg is a pairing heap with min key @=< 2nd arg,
% where min key of nil is logically @> any term.
is_pairing_heap(V, _) :-
    var(V),
    !,
    fail.
is_pairing_heap(nil, _).
is_pairing_heap(t(_,P,Sub), MinP) :-
    MinP @=< P,
    are_pairing_heaps(Sub, P).

% True iff 1st arg is a list of pairing heaps, each with min key @=< 2nd arg.
are_pairing_heaps(V, _) :-
    var(V),
    !,
    fail.
are_pairing_heaps([], _).
are_pairing_heaps([Q|Qs], MinP) :-
    is_pairing_heap(Q, MinP),
    are_pairing_heaps(Qs, MinP).

%!  list_to_heap(+List:list, -Heap) is det.
%
%   If List is a list of  Priority-Element  terms, constructs a heap
%   out of List. Complexity: linear.

list_to_heap(Xs,Q) :-
    empty_heap(Empty),
    list_to_heap(Xs,Empty,Q).

list_to_heap([],Q,Q).
list_to_heap([P-X|Xs],Q0,Q) :-
    add_to_heap(Q0,P,X,Q1),
    list_to_heap(Xs,Q1,Q).

%!  min_of_heap(+Heap, ?Priority, ?Key) is semidet.
%
%   Unifies Key with  the  minimum-priority   element  of  Heap  and
%   Priority with its priority value. Complexity: constant.

min_of_heap(heap(t(X,P,_),_), P, X).

%!  min_of_heap(+Heap, ?Priority1, ?Key1, ?Priority2, ?Key2) is semidet.
%
%   Gets the two minimum-priority elements from Heap. Complexity: logarithmic
%   (amortized).
%
%   Do not use this predicate; it exists for compatibility with earlier
%   implementations of this library and the SICStus counterpart. It performs
%   a linear amount of work in the worst case that a following get_from_heap
%   has to re-do.

min_of_heap(Q,Px,X,Py,Y) :-
    get_from_heap(Q,Px,X,Q0),
    min_of_heap(Q0,Py,Y).

%!  merge_heaps(+Heap0, +Heap1, -Heap) is det.
%
%   Merge the two heaps Heap0 and Heap1 in Heap. Complexity: constant.

merge_heaps(heap(L,K),heap(R,M),heap(Q,N)) :-
    meld(L,R,Q),
    N is K+M.


% Merge two pairing heaps according to the pairing heap definition.
meld(nil,Q,Q) :- !.
meld(Q,nil,Q) :- !.
meld(L,R,Q) :-
    L = t(X,Px,SubL),
    R = t(Y,Py,SubR),
    (   Px @< Py
    ->  Q = t(X,Px,[R|SubL])
    ;   Q = t(Y,Py,[L|SubR])
    ).

% "Pair up" (recursively meld) a list of pairing heaps.
pairing([], nil).
pairing([Q], Q) :- !.
pairing([Q0,Q1|Qs], Q) :-
    meld(Q0, Q1, Q2),
    pairing(Qs, Q3),
    meld(Q2, Q3, Q).
