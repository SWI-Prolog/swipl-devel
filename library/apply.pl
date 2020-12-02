/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2007-2015, University of Amsterdam
                              VU University Amsterdam
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

:- module(apply,
          [ include/3,                  % :Pred, +List, -Ok
            exclude/3,                  % :Pred. +List, -NotOk
            partition/4,                % :Pred, +List, -Included, -Excluded
            partition/5,                % :Pred, +List, ?Less, ?Equal, ?Greater
            maplist/2,                  % :Pred, +List
            maplist/3,                  % :Pred, ?List, ?List
            maplist/4,                  % :Pred, ?List, ?List, ?List
            maplist/5,                  % :Pred, ?List, ?List, ?List, ?List
            convlist/3,                 % :Pred, +List, -List
            foldl/4,                    % :Pred, +List, ?V0, ?V
            foldl/5,                    % :Pred, +List1, +List2, ?V0, ?V
            foldl/6,                    % :Pred, +List1, +List2, +List3, ?V0, ?V
            foldl/7,                    % :Pred, +List1, +List2, +List3, +List4,
                                        % ?V0, ?V
            foldr/4,                    % :Pred, +List, ?V0, ?V
            foldr/5,                    % :Pred, +List1, +List2, ?V0, ?V
            foldr/6,                    % :Pred, +List1, +List2, +List3, ?V0, ?V
            foldr/7,                    % :Pred, +List1, +List2, +List3, +List4,
                                        % ?V0, ?V
            scanl/4,                    % :Pred, +List, ?V0, ?Vs
            scanl/5,                    % :Pred, +List1, +List2, ?V0, ?Vs
            scanl/6,                    % :Pred, +List1, +List2, +List3, ?V0, ?Vs
            scanl/7                     % :Pred, +List1, +List2, +List3, +List4,
                                        % ?V0, ?Vs
          ]).
:- autoload(library(error),[must_be/2]).

/** <module> Apply predicates on a list

This module defines meta-predicates  that  apply   a  predicate  on  all
members of a list.

@see    apply_macros.pl provides compile-time expansion for part of this
        library.
@see    http://www.cs.otago.ac.nz/staffpriv/ok/pllib.htm
@see    Unit test code in src/Tests/library/test_apply.pl
@tbd    Add include/4, include/5, exclude/4, exclude/5
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
    convlist(2, +, -),
    foldl(3, +, +, -),
    foldl(4, +, +, +, -),
    foldl(5, +, +, +, +, -),
    foldl(6, +, +, +, +, +, -),
    foldr(3, +, +, -),
    foldr(4, +, +, +, -),
    foldr(5, +, +, +, +, -),
    foldr(6, +, +, +, +, +, -),
    scanl(3, +, +, -),
    scanl(4, +, +, +, -),
    scanl(5, +, +, +, +, -),
    scanl(6, +, +, +, +, +, -).


%!  include(:Goal, +List1, ?List2) is det.
%
%   Filter elements for which Goal succeeds.  True if List2 contains
%   those elements Xi of List1 for which call(Goal, Xi) succeeds.
%
%   @see    Older versions of SWI-Prolog had sublist/3 with the same
%           arguments and semantics.

include(Goal, List, Included) :-
    include_(List, Goal, Included).

include_([], _, []).
include_([X1|Xs1], P, Included) :-
    (   call(P, X1)
    ->  Included = [X1|Included1]
    ;   Included = Included1
    ),
    include_(Xs1, P, Included1).


%!  exclude(:Goal, +List1, ?List2) is det.
%
%   Filter elements for which Goal fails.  True if List2 contains
%   those elements Xi of List1 for which call(Goal, Xi) fails.

exclude(Goal, List, Included) :-
    exclude_(List, Goal, Included).

exclude_([], _, []).
exclude_([X1|Xs1], P, Included) :-
    (   call(P, X1)
    ->  Included = Included1
    ;   Included = [X1|Included1]
    ),
    exclude_(Xs1, P, Included1).


%!  partition(:Pred, +List, ?Included, ?Excluded) is det.
%
%   Filter elements of List according  to   Pred.  True  if Included
%   contains all elements  for  which   call(Pred,  X)  succeeds and
%   Excluded contains the remaining elements.

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


%!  partition(:Pred, +List, ?Less, ?Equal, ?Greater) is semidet.
%
%   Filter List according to Pred in three sets. For each element Xi
%   of List, its destination is determined by call(Pred, Xi, Place),
%   where Place must be unified to  one   of  =|<|=, =|=|= or =|>|=.
%   Pred must be deterministic.

partition(Pred, List, Less, Equal, Greater) :-
    partition_(List, Pred, Less, Equal, Greater).

partition_([], _, [], [], []).
partition_([H|T], Pred, L, E, G) :-
    call(Pred, H, Diff),
    partition_(Diff, H, Pred, T, L, E, G).

partition_(<, H, Pred, T, L, E, G) :-
    !,
    L = [H|Rest],
    partition_(T, Pred, Rest, E, G).
partition_(=, H, Pred, T, L, E, G) :-
    !,
    E = [H|Rest],
    partition_(T, Pred, L, Rest, G).
partition_(>, H, Pred, T, L, E, G) :-
    !,
    G = [H|Rest],
    partition_(T, Pred, L, E, Rest).
partition_(Diff, _, _, _, _, _, _) :-
    must_be(oneof([<,=,>]), Diff).


                 /*******************************
                 *          MAPLIST             *
                 *******************************/

%!  maplist(:Goal, ?List1).
%!  maplist(:Goal, ?List1, ?List2).
%!  maplist(:Goal, ?List1, ?List2, ?List3).
%!  maplist(:Goal, ?List1, ?List2, ?List3, ?List4).
%
%   True if Goal is successfully applied on all matching elements of the
%   list. The maplist family of predicates is defined as:
%
%     ==
%     maplist(G, [X_11, ..., X_1n], 
%                [X_21, ..., X_2n],
%                ...,
%                [X_m1, ..., X_mn]) :-
%        G(X_11, ..., X_m1),
%        G(X_12, ..., X_m2),
%        ...
%        G(X_1n, ..., X_mn).
%     ==
%
%   This family of predicates is deterministic iff Goal is deterministic
%   and List1 is a proper list, i.e., a list that ends in `[]`.

maplist(Goal, List) :-
    maplist_(List, Goal).

maplist_([], _).
maplist_([Elem|Tail], Goal) :-
    call(Goal, Elem),
    maplist_(Tail, Goal).

maplist(Goal, List1, List2) :-
    maplist_(List1, List2, Goal).

maplist_([], [], _).
maplist_([Elem1|Tail1], [Elem2|Tail2], Goal) :-
    call(Goal, Elem1, Elem2),
    maplist_(Tail1, Tail2, Goal).

maplist(Goal, List1, List2, List3) :-
    maplist_(List1, List2, List3, Goal).

maplist_([], [], [], _).
maplist_([Elem1|Tail1], [Elem2|Tail2], [Elem3|Tail3], Goal) :-
    call(Goal, Elem1, Elem2, Elem3),
    maplist_(Tail1, Tail2, Tail3, Goal).

maplist(Goal, List1, List2, List3, List4) :-
    maplist_(List1, List2, List3, List4, Goal).

maplist_([], [], [], [], _).
maplist_([Elem1|Tail1], [Elem2|Tail2], [Elem3|Tail3], [Elem4|Tail4], Goal) :-
    call(Goal, Elem1, Elem2, Elem3, Elem4),
    maplist_(Tail1, Tail2, Tail3, Tail4, Goal).


%!  convlist(:Goal, +ListIn, -ListOut) is det.
%
%   Similar to maplist/3, but elements for   which call(Goal, ElemIn, _)
%   fails are omitted from ListOut.  For example (using library(yall)):
%
%   ```
%   ?- convlist([X,Y]>>(integer(X), Y is X^2),
%               [3, 5, foo, 2], L).
%   L = [9, 25, 4].
%   ```
%
%   @compat  Also  appears  in  YAP   =|library(maplist)|=  and  SICStus
%   =|library(lists)|=.

convlist(Goal, ListIn, ListOut) :-
    convlist_(ListIn, ListOut, Goal).

convlist_([], [], _).
convlist_([H0|T0], ListOut, Goal) :-
    (   call(Goal, H0, H)
    ->  ListOut = [H|T],
        convlist_(T0, T, Goal)
    ;   convlist_(T0, ListOut, Goal)
    ).


                 /*******************************
                 *            FOLDL             *
                 *******************************/

%!  foldl(:Goal, +List, +V0, -V).
%!  foldl(:Goal, +List1, +List2, +V0, -V).
%!  foldl(:Goal, +List1, +List2, +List3, +V0, -V).
%!  foldl(:Goal, +List1, +List2, +List3, +List4, +V0, -V).
%
%   Fold an ensemble of _m_ (0 <= _m_ <= 4) lists of length _n_
%   head-to-tail ("fold-left"), using columns of _m_ list elements 
%   as arguments for Goal. The `foldl` family of predicates is
%   defined as follows, with `V0` an initial value and `V` the
%   final value of the folding operation:
%
%     ==
%     foldl(G, [X_11, ..., X_1n], 
%              [X_21, ..., X_2n],
%              ...,
%              [X_m1, ..., X_mn], V0, V) :-
%        G(X_11, ..., X_m1, V0, V1),
%        G(X_12, ..., X_m2, V1, V2),
%        ...
%        G(X_1n, ..., X_mn, V<n-1>, V).
%     ==
%
%   As expected with fold-left, the implementation performs a
%   tail-recursive call. `foldl` can handle _open lists_, instantiating
%   them to longer and longer lists on backtracking.
%
%   Note that the list element is the first argument to Goal.
%   For example, using atom_concat/3:
%
%     ==
%     ?- foldl(atom_concat,[a,b,c,d],start,V).
%     V = dcbastart.
%     ==
%
%   With library(yall), argument order can be rearranged inline:
%
%     ==
%     ?- foldl(
%         [E,FromLeft,ToRight]>>atom_concat(FromLeft,E,ToRight),
%         [a,b,c,d],"start",V).
%     V = startabcd.
%     ==
%
%   @see  library(yall), atom_concat/3

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
                 *             SCANL            *
                 *******************************/

%!  scanl(:Goal, +List, +V0, -Values).
%!  scanl(:Goal, +List1, +List2, +V0, -Values).
%!  scanl(:Goal, +List1, +List2, +List3, +V0, -Values).
%!  scanl(:Goal, +List1, +List2, +List3, +List4, +V0, -Values).
%
%   Scan an ensemble of _m_ (0 <= _m_ <= 4) lists of length _n_
%   head-to-tail ("scan-left"), using columns of _m_ list elements 
%   as arguments for Goal. The `scanl` family of predicates is
%   defined as follows, with `V0` an initial value and `V` the
%   final value of the scanning operation:
%
%     ==
%     scanl(G, [X_11, ..., X_1n], 
%              [X_21, ..., X_2n],
%              ...,
%              [X_m1, ..., X_mn], V0, [V0, V1, ..., Vn] ) :-
%        G(X_11, ..., X_m1, V0, V1),
%        G(X_12, ..., X_m2, V1, V2),
%        ...
%        G(X_1n, ..., X_mn, V<n-1>, Vn).
%     ==
%
%  `scanl` behaves like a `foldl` that collects the sequence of 
%  values taken on by the `Vx` accumulator into a list.

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

                 /*******************************
                 *            FOLDR             *
                 *******************************/

%!  foldr(:Goal, +List, +V0, -V).
%!  foldr(:Goal, +List1, +List2, +V0, -V).
%!  foldr(:Goal, +List1, +List2, +List3, +V0, -V).
%!  foldr(:Goal, +List1, +List2, +List3, +List4, +V0, -V).
%
%   Fold an ensemble of _m_ (0 <= _m_ <= 4) lists of length _n_
%   tail-to-head ("fold-right"), using columns of _m_ list elements 
%   as arguments for Goal. The `foldr` family of predicates is
%   defined as follows, with `V0` an initial value and `V` the
%   final value of the folding operation:
%
%     ==
%     foldr(G, [X_11, ..., X_1n], 
%              [X_21, ..., X_2n],
%              ...,
%              [X_m1, ..., X_mn], V0, V) :-
%        G(X_1n,      ..., X_mn,     V0,      V1),
%        G(X_1<n-1>,  ..., X_m<n-1>, V1,      V2),
%        ...
%        G(X_11,      ..., X_m1,     V<n-1>,  V).
%     ==
%
%   As expected with fold-right, the implementation is not
%   subject to tail-call optimization and using `foldr` is 
%   expensive in stack usage. `foldr` can handle _open lists_, 
%   instantiating them to longer and longer lists on backtracking.
%
%   Note that the list element is the first argument to Goal.
%   For example:
%
%     ==
%     ?- foldr(atom_concat,[a,b,c,d],start,V).
%     V = abcdstart.
%     ==
%
%   With library(yall), argument order can be rearranged inline:
%
%     ==
%     ?- foldr(
%         [E,FromRight,ToLeft]>>atom_concat(FromRight,E,ToLeft),
%         [a,b,c,d],"start",V).
%     V = startdcba.
%     ==
%
%   @see  library(yall), atom_concat/3

% Approach: choose the ordering of the arguments to Goal
% to have identical "in" and "out" parameter positions as for foldl
% foldl: Goal(Element, FromLeft,ToRight)
% foldr: Goal(Element, FromRight,ToLeft)

foldr(Goal, List, V0, V) :-
    foldr_(List, Goal, V0, V).

foldr_([], _, V0, V0).
foldr_([H|T], Goal, V0, V) :-
    foldr_(T, Goal, V0, V2),    
    call(Goal, H, V2, V).

% ---

foldr(Goal, List1, List2, V0, V) :-
    foldr_(List1, List2, Goal, V0, V).

foldr_([], [], _, V0, V0).
foldr_([H1|T1], [H2|T2], Goal, V0, V) :-
    foldr_(T1, T2, Goal, V0, V2),
    call(Goal, H1, H2, V2, V).

% ---

foldr(Goal, List1, List2, List3, V0, V) :-
    foldr_(List1, List2, List3, Goal, V0, V).

foldr_([], [], [], _, V0, V0).
foldr_([H1|T1], [H2|T2], [H3|T3], Goal, V0, V) :-
    foldr_(T1, T2, T3, Goal, V0, V2),
    call(Goal, H1, H2, H3, V2, V).

% ---

foldr(Goal, List1, List2, List3, List4, V0, V) :-
    foldr_(List1, List2, List3, List4, Goal, V0, V).

foldr_([], [], [], [], _, V0, V0).
foldr_([H1|T1], [H2|T2], [H3|T3], [H4|T4], Goal, V0, V) :-
    foldr_(T1, T2, T3, T4, Goal, V0, V2),
    call(Goal, H1, H2, H3, H4, V2, V).


                 /*******************************
                 *            SANDBOX           *
                 *******************************/

:- multifile
    sandbox:safe_meta_predicate/1.

safe_api(Name/Arity, sandbox:safe_meta_predicate(apply:Name/Arity)).

term_expansion(safe_api, Clauses) :-
    module_property(apply, exports(API)),
    maplist(safe_api, API, Clauses).

safe_api.
