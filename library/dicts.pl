/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2015-2025, VU University Amsterdam
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

:- module(dicts,
          [ mapdict/2,                  % :Goal, +Dict
            mapdict/3,                  % :Goal, ?Dict1, ?Dict2
            mapdict/4,                  % :Goal, ?Dict1, ?Dict2, ?Dict3
            dicts_same_tag/2,           % +List, -Tag
            dict_size/2,                % +Dict, -KeyCount
            dict_keys/2,                % +Dict, -Keys
            dicts_same_keys/2,          % +DictList, -Keys
            dicts_to_same_keys/3,       % +DictsIn, :OnEmpty, -DictsOut
            dict_fill/4,                % +Value, +Key, +Dict, -Value
            dict_no_fill/3,             % +Key, +Dict, -Value
            dicts_join/3,               % +Key, +DictsIn, -Dicts
            dicts_join/4,               % +Key, +Dicts1, +Dicts2, -Dicts
            dicts_slice/3,              % +Keys, +DictsIn, -DictsOut
            dicts_to_compounds/4        % ?Dicts, +Keys, :OnEmpty, ?Compounds
          ]).
:- autoload(library(apply),[maplist/2,maplist/3]).
:- autoload(library(lists),[append/2,append/3]).
:- autoload(library(ordsets),[ord_subtract/3]).
:- autoload(library(pairs),[pairs_keys/2,pairs_keys_values/3]).
:- autoload(library(error), [domain_error/2, must_be/2]).

:- set_prolog_flag(generate_debug_info, false).

:- meta_predicate
    mapdict(2, +),
    mapdict(3, ?, ?),
    mapdict(4, ?, ?, ?),
    dicts_to_same_keys(+,3,-),
    dicts_to_compounds(?,+,3,?).

/** <module> Dict utilities

This library defines utilities that operate   on lists of dicts, notably
to make lists of dicts  consistent   by  adding missing keys, converting
between lists of compounds and lists of dicts, joining and slicing lists
of dicts.
*/

%!  mapdict(:Goal, +Dict).
%!  mapdict(:Goal, ?Dict, ?Dict2).
%!  mapdict(:Goal, ?Dict, ?Dict2, ?Dict3).
%
%   True when all dicts have the same   set  of keys and call(Goal, Key,
%   V1, ...) is true for all keys  in   the  dicts.  At least one of the
%   dicts must be instantiated.
%
%   @error instantiation_error if no dict is bound
%   @error type_error(dict, Culprit) if one of the dict arguments is not
%   a dict.
%   @error domain_error(incompatible_dict, Culprit) if Culprit does not
%   have the same keys as one of the other dicts.

mapdict(Goal, Dict) :-
    mapdict_(1, Goal, Dict).

mapdict_(I, Goal, D1) :-
    (   '$get_dict_kv'(I, D1, K, V1)
    ->  call(Goal, K, V1),
        I2 is I+1,
        mapdict_(I2, Goal, D1)
    ;   true
    ).

mapdict(Goal, Dict1, Dict2) :-
    (   dict_same_keys(Dict1, Dict2)
    ->  mapdict_(1, Goal, Dict1, Dict2)
    ;   domain_error(incompatible_dict, Dict2)
    ).

mapdict_(I, Goal, D1, D2) :-
    (   '$get_dict_kv'(I, D1, D2, K, V1, V2)
    ->  call(Goal, K, V1, V2),
        I2 is I+1,
        mapdict_(I2, Goal, D1, D2)
    ;   true
    ).


mapdict(Goal, Dict1, Dict2, Dict3) :-
    (   nonvar(Dict1)
    ->  dict_same_keys(Dict1, Dict2),
        dict_same_keys(Dict1, Dict3)
    ;   nonvar(Dict2)
    ->  dict_same_keys(Dict1, Dict2),
        dict_same_keys(Dict1, Dict3)
    ;   dict_same_keys(Dict3, Dict2),
        dict_same_keys(Dict3, Dict1)
    ),
    !,
    mapdict_(1, Goal, Dict1, Dict2, Dict3).
mapdict(_Goal, Dict1, Dict2, Dict3) :-
    (   nonvar(Dict3)
    ->  domain_error(incompatible_dict, Dict3)
    ;   nonvar(Dict2)
    ->  domain_error(incompatible_dict, Dict2)
    ;   domain_error(incompatible_dict, Dict1)
    ).

mapdict_(I, Goal, D1, D2, D3) :-
    (   '$get_dict_kv'(I, D1, D2, D3, K, V1, V2, V3)
    ->  call(Goal, K, V1, V2, V3),
        I2 is I+1,
        mapdict_(I2, Goal, D1, D2, D3)
    ;   true
    ).


%!  dicts_same_tag(+List, -Tag) is semidet.
%
%   True when List is a list of dicts that all have the tag Tag.

dicts_same_tag(List, Tag) :-
    maplist(keys_tag(Tag), List).

keys_tag(Tag, Dict) :-
    is_dict(Dict, Tag).

%!  dict_size(+Dict, -KeyCount) is det.
%
%   True when KeyCount is the number of keys in Dict.

dict_size(Dict, KeyCount) :-
    must_be(dict,Dict),
    compound_name_arity(Dict,_,Arity),
    KeyCount is (Arity-1)//2.

%!  dict_keys(+Dict, -Keys) is det.
%
%   True when Keys is an ordered set of the keys appearing in Dict.

dict_keys(Dict, Keys) :-
    dict_pairs(Dict, _Tag, Pairs),
    pairs_keys(Pairs, Keys).


%!  dicts_same_keys(+List, -Keys) is semidet.
%
%   True if List is a list of dicts  that all have the same keys and
%   Keys is an ordered set of these keys.

dicts_same_keys(List, Keys) :-
    maplist(keys_dict(Keys), List).

keys_dict(Keys, Dict) :-
    dict_keys(Dict, Keys).

%!  dicts_to_same_keys(+DictsIn, :OnEmpty, -DictsOut)
%
%   DictsOut is a copy of DictsIn, where each dict contains all keys
%   appearing in all dicts of  DictsIn.   Values  for  keys that are
%   added to a dict are produced by   calling  OnEmpty as below. The
%   predicate dict_fill/4 provides an implementation  that fills all
%   new cells with a predefined value.
%
%     ==
%     call(:OnEmpty, +Key, +Dict, -Value)
%     ==

dicts_to_same_keys(Dicts, _, Table) :-
    dicts_same_keys(Dicts, _),
    !,
    Table = Dicts.
dicts_to_same_keys(Dicts, OnEmpty, Table) :-
    maplist(dict_keys, Dicts, KeysList),
    append(KeysList, Keys0),
    sort(Keys0, Keys),
    maplist(extend_dict(Keys, OnEmpty), Dicts, Table).

extend_dict(Keys, OnEmpty, Dict0, Dict) :-
    dict_pairs(Dict0, Tag, Pairs),
    pairs_keys(Pairs, DictKeys),
    ord_subtract(Keys, DictKeys, Missing),
    (   Missing == []
    ->  Dict = Dict0
    ;   maplist(key_value_pair(Dict0, OnEmpty), Missing, NewPairs),
        append(NewPairs, Pairs, AllPairs),
        dict_pairs(Dict, Tag, AllPairs)
    ).

key_value_pair(Dict, OnEmpty, Key, Key-Value) :-
    call(OnEmpty, Key, Dict, Value).

%!  dict_fill(+ValueIn, +Key, +Dict, -Value) is det.
%
%   Implementation for the dicts_to_same_keys/3   `OnEmpty`  closure
%   that  fills  new  cells  with  a  copy  of  ValueIn.  Note  that
%   copy_term/2 does not really copy  ground   terms.  Below are two
%   examples. Note that when filling empty   cells  with a variable,
%   each empty cell is bound to a new variable.
%
%     ==
%     ?- dicts_to_same_keys([r{x:1}, r{y:2}], dict_fill(null), L).
%     L = [r{x:1, y:null}, r{x:null, y:2}].
%     ?- dicts_to_same_keys([r{x:1}, r{y:2}], dict_fill(_), L).
%     L = [r{x:1, y:_G2005}, r{x:_G2036, y:2}].
%     ==
%
%   Use dict_no_fill/3 to raise an error if a dict is missing a key.

dict_fill(ValueIn, _, _, Value) :-
    copy_term(ValueIn, Value).

%!  dict_no_fill is det.
%
%   Can be used instead of dict_fill/4 to raise an exception if some
%   dict is missing a key.

dict_no_fill(Key, Dict, Value) :-
    Value = Dict.Key.

%!  dicts_join(+Key, +DictsIn, -Dicts) is semidet.
%
%   Join dicts in Dicts that have the   same value for Key, provided
%   they do not have conflicting values on other keys.  For example:
%
%   ==
%   ?- dicts_join(x, [r{x:1, y:2}, r{x:1, z:3}, r{x:2,y:4}], L).
%   L = [r{x:1, y:2, z:3}, r{x:2, y:4}].
%   ==
%
%   @error  existence_error(key, Key, Dict) if a dict in Dicts1
%           or Dicts2 does not contain Key.

dicts_join(Join, Dicts0, Dicts) :-
    sort(Join, @=<, Dicts0, Dicts1),
    join(Dicts1, Join, Dicts).

join([], _, []) :- !.
join([H0|T0], Key, [H|T]) :-
    !,
    get_dict(Key, H0, V0),
    join_same(T0, Key, V0, H0, H, T1),
    join(T1, Key, T).
join([One], _, [One]) :- !.

join_same([H|T0], Key, V0, D0, D, T) :-
    get_dict(Key, H, V),
    V == V0,
    !,
    D0 >:< H,
    put_dict(H, D0, D1),
    join_same(T0, Key, V0, D1, D, T).
join_same(DL, _, _, D, D, DL).

%!  dicts_join(+Key, +Dicts1, +Dicts2, -Dicts) is semidet.
%
%   Join two lists of dicts (Dicts1 and   Dicts2)  on Key. Each pair
%   D1-D2 from Dicts1 and Dicts2 that have   the same (==) value for
%   Key creates a new dict D with the  union of the keys from D1 and
%   D2, provided D1 and D2 to not   have conflicting values for some
%   key.  For example:
%
%     ==
%     ?- DL1 = [r{x:1,y:1},r{x:2,y:4}],
%        DL2 = [r{x:1,z:2},r{x:3,z:4}],
%        dicts_join(x, DL1, DL2, DL).
%        DL = [r{x:1, y:1, z:2}, r{x:2, y:4}, r{x:3, z:4}].
%     ==
%
%   @error  existence_error(key, Key, Dict) if a dict in Dicts1
%           or Dicts2 does not contain Key.

dicts_join(Join, Dicts1, Dicts2, Dicts) :-
    sort(Join, @=<, Dicts1, Dicts11),
    sort(Join, @=<, Dicts2, Dicts21),
    join(Dicts11, Dicts21, Join, Dicts).

join([], [], _, []) :- !.
join([D1|T1], [D2|T2], Join, [DNew|MoreDicts]) :-
    !,
    get_dict(Join, D1, K1),
    get_dict(Join, D2, K2),
    compare(Diff, K1, K2),
    (   Diff == (=)
    ->  D1 >:< D2,
        put_dict(D1, D2, DNew),
        join(T1, T2, Join, MoreDicts)
    ;   Diff == (<)
    ->  DNew = D1,
        join(T1, [D2|T2], Join, MoreDicts)
    ;   DNew = D2,
        join([D1|T1], T2, Join, MoreDicts)
    ).
join([], Dicts, _, Dicts) :- !.
join(Dicts, [], _, Dicts).


%!  dicts_slice(+Keys, +DictsIn, -DictsOut) is det.
%
%   DictsOut is a list of Dicts only containing values for Keys.

dicts_slice(Keys, DictsIn, DictsOut) :-
    sort(Keys, SortedKeys),
    maplist(dict_slice(SortedKeys), DictsIn, DictsOut).

dict_slice(Keys, DictIn, DictOut) :-
    dict_pairs(DictIn, Tag, PairsIn),
    slice_pairs(Keys, PairsIn, PairsOut),
    dict_pairs(DictOut, Tag, PairsOut).

slice_pairs([], _, []) :- !.
slice_pairs(_, [], []) :- !.
slice_pairs([H|T0], [P|PL], Pairs) :-
    P = K-_,
    compare(D, H, K),
    (   D == (=)
    ->  Pairs = [P|More],
        slice_pairs(T0, PL, More)
    ;   D == (<)
    ->  slice_pairs(T0, [P|PL], Pairs)
    ;   slice_pairs([H|T0], PL, Pairs)
    ).

%!  dicts_to_compounds(?Dicts, +Keys, :OnEmpty, ?Compounds) is semidet.
%
%   True when Dicts and Compounds are lists   of the same length and
%   each element of Compounds is  a   compound  term whose arguments
%   represent the values associated with   the corresponding keys in
%   Keys. When converting from  dict  to   row,  OnEmpty  is used to
%   compute missing values. The functor for the compound is the same
%   as the tag of the pair. When converting from dict to row and the
%   dict has no tag, the functor `row` is used. For example:
%
%     ==
%     ?- Dicts = [_{x:1}, _{x:2, y:3}],
%        dicts_to_compounds(Dicts, [x], dict_fill(null), Compounds).
%     Compounds = [row(1), row(2)].
%     ?- Dicts = [_{x:1}, _{x:2, y:3}],
%        dicts_to_compounds(Dicts, [x,y], dict_fill(null), Compounds).
%     Compounds = [row(1, null), row(2, 3)].
%     ?- Compounds = [point(1,1), point(2,4)],
%        dicts_to_compounds(Dicts, [x,y], dict_fill(null), Compounds).
%     Dicts = [point{x:1, y:1}, point{x:2, y:4}].
%     ==
%
%   When converting from Dicts to  Compounds   Keys  may be computed by
%   dicts_same_keys/2.

dicts_to_compounds(Dicts, Keys, OnEmpty, Compounds) :-
    maplist(dict_to_compound(Keys, OnEmpty), Dicts, Compounds).

dict_to_compound(Keys, OnEmpty, Dict, Row) :-
    is_dict(Dict, Tag0),
    !,
    default_tag(Tag0, row, Tag),
    maplist(key_value(Dict, OnEmpty), Keys, Values),
    compound_name_arguments(Row, Tag, Values).
dict_to_compound(Keys, _, Dict, Row) :-
    compound(Row),
    compound_name_arguments(Row, Tag, Values),
    pairs_keys_values(Pairs, Keys, Values),
    dict_pairs(Dict, Tag, Pairs).

default_tag(Tag0, _, Tag), atom(Tag0) =>
    Tag = Tag0.
default_tag(_, Default, Tag) =>
    Tag = Default.

key_value(Dict, OnEmpty, Key, Value) :-
    (   get_dict(Key, Dict, Value0)
    ->  Value = Value0
    ;   call(OnEmpty, Key, Dict, Value)
    ).
