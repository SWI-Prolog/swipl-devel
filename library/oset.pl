/*  Part of SWI-Prolog

    Author:        Jon Jagger
    E-mail:        J.R.Jagger@shu.ac.uk
    Copyright (c)  1993-2011, Jon Jagger
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

:- module(oset, [  oset_is/1,
                    oset_union/3,
                    oset_int/3,
                    oset_diff/3,
                    oset_dint/2,
                    oset_dunion/2,
                    oset_addel/3,
                    oset_delel/3,
                    oset_power/2
                 ]).


/** <module> Ordered set manipulation

This library defines set operations on sets represented as ordered
lists.

@author Jon Jagger
@deprecated Use the de-facto library ordsets.pl
*/


%% oset_is(+OSet)
%   check that OSet in correct format (standard order)

oset_is(-) :- !, fail.    % var filter
oset_is([]).
oset_is([H|T]) :-
    oset_is(T, H).

oset_is(-, _) :- !, fail.  % var filter
oset_is([], _H).
oset_is([H|T], H0) :-
    H0 @< H,               % use standard order
    oset_is(T, H).



%% oset_union(+OSet1, +OSet2, -Union).

oset_union([], Union, Union).
oset_union([H1|T1], L2, Union) :-
    union2(L2, H1, T1, Union).

union2([], H1, T1, [H1|T1]).
union2([H2|T2], H1, T1, Union) :-
    compare(Order, H1, H2),
    union3(Order, H1, T1, H2, T2, Union).

union3(<, H1, T1,  H2, T2, [H1|Union]) :-
    union2(T1, H2, T2, Union).
union3(=, H1, T1, _H2, T2, [H1|Union]) :-
    oset_union(T1, T2, Union).
union3(>, H1, T1,  H2, T2, [H2|Union]) :-
    union2(T2, H1, T1, Union).


%% oset_int(+OSet1, +OSet2, -Int)
%   ordered set intersection

oset_int([], _Int, []).
oset_int([H1|T1], L2, Int) :-
    isect2(L2, H1, T1, Int).

isect2([], _H1, _T1, []).
isect2([H2|T2], H1, T1, Int) :-
    compare(Order, H1, H2),
    isect3(Order, H1, T1, H2, T2, Int).

isect3(<, _H1, T1,  H2, T2, Int) :-
    isect2(T1, H2, T2, Int).
isect3(=, H1, T1, _H2, T2, [H1|Int]) :-
    oset_int(T1, T2, Int).
isect3(>, H1, T1,  _H2, T2, Int) :-
    isect2(T2, H1, T1, Int).


%% oset_diff(+InOSet, +NotInOSet, -Diff)
%   ordered set difference

oset_diff([], _Not, []).
oset_diff([H1|T1], L2, Diff) :-
    diff21(L2, H1, T1, Diff).

diff21([], H1, T1, [H1|T1]).
diff21([H2|T2], H1, T1, Diff) :-
    compare(Order, H1, H2),
    diff3(Order, H1, T1, H2, T2, Diff).

diff12([], _H2, _T2, []).
diff12([H1|T1], H2, T2, Diff) :-
    compare(Order, H1, H2),
    diff3(Order, H1, T1, H2, T2, Diff).

diff3(<,  H1, T1,  H2, T2, [H1|Diff]) :-
    diff12(T1, H2, T2, Diff).
diff3(=, _H1, T1, _H2, T2, Diff) :-
    oset_diff(T1, T2, Diff).
diff3(>,  H1, T1, _H2, T2, Diff) :-
    diff21(T2, H1, T1, Diff).


%% oset_dunion(+SetofSets, -DUnion)
%   distributed union

oset_dunion([], []).
oset_dunion([H|T], DUnion) :-
    oset_dunion(T, H, DUnion).

oset_dunion([], DUnion, DUnion).
oset_dunion([H|T], DUnion0, DUnion) :-
    oset_union(H, DUnion0, DUnion1),
    oset_dunion(T, DUnion1, DUnion).


%% oset_dint(+SetofSets, -DInt)
%   distributed intersection

oset_dint([], []).
oset_dint([H|T], DInt) :-
    dint(T, H, DInt).

dint([], DInt, DInt).
dint([H|T], DInt0, DInt) :-
    oset_int(H, DInt0, DInt1),
    dint(T, DInt1, DInt).


%!  oset_power(+Set, -PSet)
%
%   True when PSet is the powerset of Set. That is, Pset is a set of
%   all subsets of Set, where each subset is a proper ordered set.

oset_power(S, PSet) :-
    reverse(S, R),
    pset(R, [[]], PSet0),
    sort(PSet0, PSet).

% The powerset of a set  is  the  powerset   of  a  set  of one smaller,
% together with the set of one  smaller   where  each subset is extended
% with the new element.  Note that this produces the elements of the set
% in reverse order.  Hence the reverse in oset_power/2.

pset([], PSet, PSet).
pset([H|T], PSet0, PSet) :-
    happ(PSet0, H, PSet1),
    pset(T, PSet1, PSet).

happ([], _, []).
happ([S|Ss], H, [[H|S],S|Rest]) :-
    happ(Ss, H, Rest).



%% oset_addel(+Set, +El, -Add)
%   ordered set element addition

oset_addel([], El, [El]).
oset_addel([H|T], El, Add) :-
    compare(Order, H, El),
    addel(Order, H, T, El, Add).

addel(<, H, T,  El, [H|Add]) :-
    oset_addel(T, El, Add).
addel(=, H, T, _El, [H|T]).
addel(>, H, T,  El, [El,H|T]).


%% oset_delel(+Set, +El, -Del)
%   ordered set element deletion

oset_delel([], _El, []).
oset_delel([H|T], El, Del) :-
    compare(Order, H, El),
    delel(Order, H, T, El, Del).

delel(<,  H, T,  El, [H|Del]) :-
    oset_delel(T, El, Del).
delel(=, _H, T, _El, T).
delel(>,  H, T, _El, [H|T]).

