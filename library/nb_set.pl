/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2005-2025, VU University Amsterdam
                              CWI, Amsterdam
                              SWI-Prolog Solutions b.v.
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

:- module(nb_set,
          [ empty_nb_set/1,		 % -EmptySet
            add_nb_set/2,		 % +Key, !Set
            add_nb_set/3,		 % +Key, !Set, ?New
            size_nb_set/2,		 % +Set, -Size
            nb_set_to_list/2,		 % +Set, -List
            gen_nb_set/2                 % +Set, -Key
          ]).
:- autoload(library(terms), [term_factorized/3]).
:- use_module(library(debug), [assertion/1]).

/** <module> Non-backtrackable sets

This library provides  a  non-backtrackabe  _set_   of  terms  that  are
variants of each other. It is primarily intended to implement distinct/1
from library(solution_sequences). The set is implemented as a hash table
that is built using non-backtrackable primitives, notably nb_setarg/3.

The original version of this library   used  binary trees which provides
immediate ordering. As the trees were   not  balanced, performance could
get   really   poor.   The   complexity   of   balancing   trees   using
non-backtrackable primitives is too high. The  next iteration used _open
hash tables_, while the current incarnation   uses _closed hash tables_,
providing better perfomance and less space usage.

@author Jan Wielemaker
*/

initial_capacity(4).                       % initial hash-table size

%!  empty_nb_set(-Set)
%
%   Create an empty non-backtrackable set.

empty_nb_set(NbSet) :-
    initial_capacity(Capacity),
    Empty = empty(1),
    '$filled_array'(Buckets, buckets, Capacity, Empty),
    NbSet = nb_set(Empty, Capacity, 0, Buckets).

%!  add_nb_set(+Key, !Set) is det.
%!  add_nb_set(+Key, !Set, ?New) is semidet.
%
%   Insert Key into the set. If  a   variant  (see  =@=/2) of Key is
%   already in the set, the set is unchanged and New is unified with
%   `false`. Otherwise, New is unified with   `true` and a _copy of_
%   Key is added to the set.
%
%   @tbd    Computing the hash for cyclic terms is performed with
%           the help of term_factorized/3, which performs rather
%           poorly.

add_nb_set(Key, Set) :-
    add_nb_set(Key, Set, _).
add_nb_set(Key, Set, New) :-
    Set = nb_set(Empty, Capacity, Size, Buckets),
    key_hash(Key, Hash),
    index(Hash, Capacity, KIndex),
        arg(KIndex, Buckets, StoredKey),
        (   same_term(StoredKey, Empty)
        ->  !,
            New = true,
            nb_setarg(KIndex, Buckets, Key),
            NSize is Size+1,
            nb_setarg(3, Set, NSize),
            (   NSize > Capacity//2
            ->  rehash(Set)
            ;   true
            )
        ;   Key =@= StoredKey
        ->  !,
            New = false
        ).

%!  index(+Hash, +Capacity, -Index) is nondet.
%
%   Generate  candidate  values  for  Index,  starting  from  `Hash  mod
%   Capacity`, round tripping to 1 when Capacity is reached.

index(Hash, Capacity, KIndex) :-
    KIndex0 is (Hash mod Capacity) + 1,
    next(KIndex0, Capacity, KIndex).

next(KIndex, _, KIndex).
next(KIndex0, Capacity, KIndex) :-
    KIndex1 is 1+(KIndex0 mod Capacity),
    next(KIndex1, Capacity, KIndex).

rehash(Set) :-
    Set = nb_set(Empty, Capacity, Size, Buckets),
    NCapacity is Capacity*2,
    '$filled_array'(NBuckets, buckets, NCapacity, Empty),
    nb_setarg(2, Set, NCapacity),
    nb_setarg(3, Set, 0),
    nb_linkarg(4, Set, NBuckets),
    forall(between(1, Capacity, I),
           reinsert(I, Empty, Buckets, Set)),
    arg(3, Set, NewSize),
    assertion(NewSize == Size).

:- det(reinsert/4).
reinsert(KIndex, Empty, Buckets, Set) :-
    arg(KIndex, Buckets, Key),
    (   same_term(Key, Empty)
    ->  true
    ;   add_nb_set(Key, Set, true)
    ).

%!  hash_key(+Key, -Hash:integer) is det.
%
%   Compute a hash for Term. Note that variant_hash/2 currently does
%   not handle cyclic terms, so use  term_factorized/3 to get rid of
%   the cycles. This means that  this   library  is rather slow when
%   cyclic terms are involved.

:- if(catch((A = f(A), variant_hash(A,_)), _, fail)).
key_hash(Key, Hash) :-
    variant_hash(Key, Hash).
:- else.
key_hash(Key, Hash) :-
    acyclic_term(Key),
    !,
    variant_hash(Key, Hash).
key_hash(Key, Hash) :-
    term_factorized(Key, Skeleton, Substiution),
    variant_hash(Skeleton+Substiution, Hash).
:- endif.

%!  nb_set_to_list(+NBSet, -OrdSet) is det.
%!  nb_set_to_list(-NBSet, +List) is det.
%
%   Get the elements of a an nb_set.   OrdSet  is sorted to the standard
%   order of terms, providing a set representation that is compatible to
%   library(ordsets).

nb_set_to_list(NBSet, Set),
    NBSet = nb_set(Empty, Capacity, _Size, Buckets) =>
    buckets_to_list(1, Empty, Capacity, Buckets, List0),
    sort(List0, Set).

buckets_to_list(KIndex, Empty, Capacity, Buckets, List) :-
    (   arg(KIndex, Buckets, Key)
    ->  (   same_term(Empty, Key)
        ->  KIndex1 is KIndex+1,
            buckets_to_list(KIndex1, Empty, Capacity, Buckets, List)
        ;   List = [Key|List1],
            KIndex1 is KIndex+1,
            buckets_to_list(KIndex1, Empty, Capacity, Buckets, List1)
        )
    ;   List = []
    ).

%!  gen_nb_set(+Set, -Key) is nondet.
%
%   Enumerate the members of a set in the standard order of terms.

gen_nb_set(nb_set(Empty, Capacity, _Size, Buckets), Key) =>
    between(1, Capacity, KIndex),
    arg(KIndex, Buckets, Key),
    \+ same_term(Empty, Key).

%!  size_nb_set(+Set, -Size) is det.
%
%   Unify Size with the number of elements in the set

size_nb_set(nb_set(_Empty, _Capacity, Sz, _Buckets), Size) =>
    Size = Sz.
