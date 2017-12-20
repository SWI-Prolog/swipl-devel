/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2005-2015, VU University Amsterdam
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
          [ empty_nb_set/1,             % -EmptySet
            add_nb_set/2,               % +Key, !Set
            add_nb_set/3,               % +Key, !Set, ?New
            gen_nb_set/2,               % +Set, -Key
            size_nb_set/2,              % +Set, -Size
            nb_set_to_list/2            % +Set, -List
          ]).
:- use_module(library(lists)).
:- use_module(library(terms)).
:- use_module(library(apply_macros), []).

/** <module> Non-backtrackable sets

This library provides  a  non-backtrackabe  _set_   of  terms  that  are
variants of each other. It is primarily intended to implement distinct/1
from library(solution_sequences). The set is implemented as a hash table
that is built using non-backtrackable primitives, notably nb_setarg/3.

The original version of this library   used  binary trees which provides
immediate ordering. As the trees were   not  balanced, performance could
get   really   poor.   The   complexity   of   balancing   trees   using
non-backtrackable primitives is too high.

@author Jan Wielemaker
*/

initial_size(32).                       % initial hash-table size

%!  empty_nb_set(-Set)
%
%   Create an empty non-backtrackable set.

empty_nb_set(nb_set(Buckets, 0)) :-
    initial_size(Size),
    '$filled_array'(Buckets, buckets, Size, []).

%!  add_nb_set(+Key, !Set) is det.
%!  add_nb_set(+Key, !Set, ?New) is semidet.
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
    arg(1, Set, Buckets),
    compound_name_arity(Buckets, _, BCount),
    hash_key(Key, BCount, Hash),
    arg(Hash, Buckets, Bucket),
    (   member(X, Bucket),
        Key =@= X
    ->  New = false
    ;   New = true,
        duplicate_term(Key, Copy),
        nb_linkarg(Hash, Buckets, [Copy|Bucket]),
        arg(2, Set, Size0),
        Size is Size0+1,
        nb_setarg(2, Set, Size),
        (   Size > BCount
        ->  rehash(Set)
        ;   true
        )
    ).

%!  hash_key(+Term, +BucketCount, -Key) is det.
%
%   Compute a hash for Term. Note that variant_hash/2 currently does
%   not handle cyclic terms, so use  term_factorized/3 to get rid of
%   the cycles. This means that  this   library  is rather slow when
%   cyclic terms are involved.

:- if(catch((A = f(A), variant_hash(A,_)), _, fail)).
hash_key(Term, BCount, Key) :-
    variant_hash(Term, IntHash),
    Key is (IntHash mod BCount)+1.
:- else.
hash_key(Term, BCount, Key) :-
    acyclic_term(Key),
    !,
    variant_hash(Term, IntHash),
    Key is (IntHash mod BCount)+1.
hash_key(Term, BCount, Key) :-
    term_factorized(Term, Skeleton, Substiution),
    variant_hash(Skeleton+Substiution, IntHash),
    Key is (IntHash mod BCount)+1.
:- endif.

rehash(Set) :-
    arg(1, Set, Buckets0),
    compound_name_arity(Buckets0, Name, Arity0),
    Arity is Arity0*2,
    '$filled_array'(Buckets, Name, Arity, []),
    nb_setarg(1, Set, Buckets),
    nb_setarg(2, Set, 0),
    forall(( arg(_, Buckets0, Chain),
             member(Key, Chain)
           ),
           add_nb_set(Key, Set, _)).

%!  nb_set_to_list(+Set, -List)
%
%   Get the elements of a an nb_set. List is sorted to the standard
%   order of terms.

nb_set_to_list(nb_set(Buckets, _Size), OrdSet) :-
    compound_name_arguments(Buckets, _, Args),
    append(Args, List),
    sort(List, OrdSet).

%!  gen_nb_set(+Set, -Key)
%
%   Enumerate the members of a set in the standard order of terms.

gen_nb_set(Set, Key) :-
    nb_set_to_list(Set, OrdSet),
    member(Key, OrdSet).

%!  size_nb_set(+Set, -Size)
%
%   Unify Size with the number of elements in the set

size_nb_set(nb_set(_, Size), Size).
