/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2020, VU University Amsterdam
                         CWI, Amsterdam
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

:- module(hashtable,
          [ ht_new/1,                   % --HT
            ht_is_hashtable/1,          % @HT
            ht_size/2,                  % +HT, -Size

            ht_put/3,                   % !HT, +Key, +Value
            ht_update/4,                % +HT, +Key, ?Old, +New
            ht_put_new/3,               % !HT, +Key, +Value
            ht_put/5,                   % !HT, +Key, +Value, +IfNew, -Old
            ht_del/3,                   % !HT, +Key, -Value

            ht_get/3,                   % +HT, +Key, -Value
            ht_gen/3,                   % +HT, ?Key, ?Value
            ht_pairs/2,                 % ?HT, ?Pairs
            ht_keys/2                   % +HT, -Keys
          ]).
:- autoload(library(error), [must_be/2]).

/** <module> Hash tables

Hash tables are one of the   many key-value representations available to
SWI-Prolog.

This module implements a hash table   as a _mutable_ and _backtrackable_
data structure. The hash table is implemented  as a _closed hash table_,
where the _buckets_ array  is  implemented   using  an  unbounded  arity
compound term. Elements in this array are manipulated using setarg/3.

Hash tables allow for any Prolog data   types  as keys or values, except
that the key cannot be a  variable.   Applications  that require a plain
variable as key can do so by  wrapping   all  keys  in a compound, e.g.,
k(Var).
*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Data structure

    ht(Load, Size, Table)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%!  ht_new(--HT)
%
%   Create a new hash table.

ht_new(ht(0,0,[](_))).

%!  ht_is_hashtable(@HT) is semidet.
%
%   True when HT is a hash table.

ht_is_hashtable(HT) :-
    nonvar(HT),
    HT = ht(Load, Size, Buckets),
    integer(Load),
    integer(Size),
    compound_name_arity(Buckets, [], Arity),
    Arity =:= Size*2+1.

%!  ht_size(+HT, -Count) is det.
%
%   True when Size is the number of key-value pairs in HT.

ht_size(ht(Count, _Size, _Buckets), Count).

%!  ht_put(!HT, +Key, +Value) is det.
%
%   Add a Key-Value to HT. The binding is undone on backtracking.

ht_put(HT, Key, Value) :-
    must_be(nonvar, Key),
    ht_put(HT, Key, Value, _, _, _).

%!  ht_put_new(!HT, +Key, +Value) is semidet.
%
%   As ht_put/3, but fails if Key is   already in HT instead of updating
%   the associated value.

ht_put_new(HT, Key, Value) :-
    must_be(nonvar, Key),
    ht_put(HT, Key, Value, _, _, true).

%!  ht_update(+HT, +Key, ?Old, +New) is semidet.
%
%   True when HT holds Key-Old before and  Key-New after this call. Note
%   that it is possible to update  to   a  variable  and the instantiate
%   this. For example, a word-count update could be implemented as:
%
%   ```
%   update_word_count(HT, Word) :-
%       (   ht_update(HT, Word, Old, New)
%       ->  New is Old+1
%       ;   ht_put(HT, Word, 1)
%       ).
%   ```

ht_update(HT, Key, Old, New) :-
    must_be(nonvar, Key),
    ht_put(HT, Key, New, _, Old, false).

%!  ht_put(!HT, +Key, +Value, +IfNew, -Old) is det.
%
%   Add Key-Value to HT. Old is unified   with  the old value associated
%   with Key or, if Key  is  new,  with   IfNew.  This  can  be  used to
%   bootstrap managing a list of values, e.g.
%
%       ht_put_list(HT, Key, Value) :-
%           ht_put(HT, Key, [Value|Tail], [], Tail).

ht_put(HT, Key, Value, IfNew, Old) :-
    must_be(nonvar, Key),
    ht_put(HT, Key, Value, IfNew, Old, _).

ht_put(HT, Key, Value, IfNew, Old, IsNew) :-
    HT = ht(Load, Size, Buckets),
    (   Load >= Size//2
    ->  ht_resize(HT),
        ht_put(HT, Key, Value, IfNew, Old, IsNew)
    ;   variant_hash(Key, I0),
        I is I0 mod Size,
        put_(Buckets, I, Size, Key, Old, IfNew, Value, IsNew),
        (   IsNew == true
        ->  Load2 is Load+1,
            setarg(1, HT, Load2)
        ;   true
        )
    ).

put_(Buckets, I, Size, Key, Old, IfNew, Value, IsNew) :-
    ht_kv(Buckets, I, K, V),
    (   var(K)
    ->  IsNew = true,
        Old = IfNew,
        K = Key,
        V = Value
    ;   K == Key
    ->  IsNew = false,
        Old = V,
        ht_put_v(Buckets, I, Value)
    ;   I2 is (I+1) mod Size,
        put_(Buckets, I2, Size, Key, Old, IfNew, Value, IsNew)
    ).

ht_resize(HT) :-
    HT = ht(_Load, Size, Buckets),
    NewSize is max(4, Size*2),
    NewArity is NewSize*2+1,
    compound_name_arity(NewBuckets, [], NewArity),
    copy_members(0, Size, Buckets, NewSize, NewBuckets),
    setarg(2, HT, NewSize),
    setarg(3, HT, NewBuckets).

copy_members(I, OSize, OBuckets, NSize, NBuckets) :-
    I < OSize,
    !,
    ht_kv(OBuckets, I, K, V),
    (   nonvar(K)
    ->  variant_hash(K, I0),
        NI is I0 mod NSize,
        copy_(NBuckets, NI, NSize, K, V)
    ;   true
    ),
    I2 is I+1,
    copy_members(I2, OSize, OBuckets, NSize, NBuckets).
copy_members(_, _, _, _, _).

copy_(Buckets, I, Size, Key, Value) :-
    ht_kv(Buckets, I, K, V),
    (   var(K)
    ->  K = Key,
        V = Value
    ;   I2 is (I+1) mod Size,
        copy_(Buckets, I2, Size, Key, Value)
    ).


%!  ht_del(!HT, +Key, -Value) is semidet.
%
%   Delete Key-Value from HT. Fails if  Key   does  not  appear in HT or
%   Value does not unify with the old associated value.

ht_del(HT, Key, Value) :-
    must_be(nonvar, Key),
    HT = ht(Load, Size, Buckets),
    Load > 0,
    variant_hash(Key, I0),
    I is I0 mod Size,
    del_(Buckets, I, Size, Key, Value),
    Load2 is Load - 1,
    setarg(1, HT, Load2).

del_(Buckets, I, Size, Key, Value) :-
    ht_kv(Buckets, I, K, V),
    (   var(K)
    ->  fail
    ;   K == Key
    ->  V = Value,
        ht_put_kv(Buckets, I, _, _),
        del_shift(Buckets, I, I, Size)
    ;   I2 is (I+1) mod Size,
        del_(Buckets, I2, Size, Key, Value)
    ).

del_shift(Buckets, I0, J, Size) :-
    I is (I0+1) mod Size,
    ht_kv(Buckets, I, K, V),
    (   var(K)
    ->  true
    ;   variant_hash(K, Hash),
        R is Hash mod Size,
        (   (   I >= R, R > J
            ;   R > J, J > I
            ;   J > I, I >= R
            )
        ->  del_shift(Buckets, I, J, Size)
        ;   ht_put_kv(Buckets, J, K, V),
            ht_put_kv(Buckets, I, _, _),
            del_shift(Buckets, I, I, Size)
        )
    ).


%!  ht_get(+HT, +Key, -Value) is semidet.
%
%   True when Key is in HT and associated with Value.

ht_get(ht(Load, Size, Buckets), Key, Value) :-
    Load > 0,
    must_be(nonvar, Key),
    variant_hash(Key, I0),
    I is I0 mod Size,
    get_(Buckets, I, Size, Key, Value).

get_(Buckets, I, Size, Key, Value) :-
    ht_kv(Buckets, I, K, V),
    (   Key == K
    ->  Value = V
    ;   nonvar(K)
    ->  I2 is (I+1) mod Size,
        get_(Buckets, I2, Size, Key, Value)
    ).

ht_k(Buckets, I, K) :-
    IK is I*2+1,
    arg(IK, Buckets, K).

ht_kv(Buckets, I, K, V) :-
    IK is I*2+1,
    IV is IK+1,
    arg(IK, Buckets, K),
    arg(IV, Buckets, V).

ht_put_kv(Buckets, I, K, V) :-
    IK is I*2+1,
    IV is IK+1,
    setarg(IK, Buckets, K),
    setarg(IV, Buckets, V).

ht_put_v(Buckets, I, V) :-
    IV is I*2+2,
    setarg(IV, Buckets, V).

%!  ht_gen(+HT, ?Key, ?Value) is nondet.
%
%   True when Key-Value is in HT.   Pairs are enumerated on backtracking
%   using the hash table order.

ht_gen(HT, Key, Value) :-
    HT = ht(_, Size, Buckets),
    End is Size - 1,
    between(0, End, I),
    ht_kv(Buckets, I, K, V),
    nonvar(K),
    K = Key,
    V = Value.

%!  ht_pairs(?HT, ?Pairs) is det.
%
%   True when Pairs and HT represent the  same association. When used in
%   mode (+,-), Pairs is an ordered set.

ht_pairs(HT, Pairs) :-
    ht_is_hashtable(HT),
    !,
    HT = ht(_Load, Size, Buckets),
    pairs_(0, Size, Buckets, Pairs0),
    sort(Pairs0, Pairs).
ht_pairs(HT, Pairs) :-
    must_be(list(pair), Pairs),
    ht_new(HT),
    ht_fill(Pairs, HT).

pairs_(I, Size, Buckets, Pairs) :-
    (   I < Size
    ->  ht_kv(Buckets, I, K, V),
        (   nonvar(K)
        ->  Pairs = [K-V|T],
            I2 is I+1,
            pairs_(I2, Size, Buckets, T)
        ;   I2 is I+1,
            pairs_(I2, Size, Buckets, Pairs)
        )
    ;   Pairs = []
    ).

ht_fill([], _).
ht_fill([K-V|T], HT) :-
    ht_put(HT, K, V),
    ht_fill(T, HT).

%!  ht_keys(+HT, -Keys) is det.
%
%   True when Keys is an ordered set of all keys in HT.

ht_keys(HT, Keys) :-
    HT = ht(_Load, Size, Buckets),
    keys_(0, Size, Buckets, Keys0),
    sort(Keys0, Keys).

keys_(I, Size, Buckets, Keys) :-
    (   I < Size
    ->  ht_k(Buckets, I, K),
        (   nonvar(K)
        ->  Keys = [K|T],
            I2 is I+1,
            keys_(I2, Size, Buckets, T)
        ;   I2 is I+1,
            keys_(I2, Size, Buckets, Keys)
        )
    ;   Keys = []
    ).
