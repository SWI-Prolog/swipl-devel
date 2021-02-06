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

:- module(oset,
          [ oset_is/1,
            oset_union/3,
            oset_int/3,
            oset_diff/3,
            oset_dint/2,
            oset_dunion/2,
            oset_addel/3,
            oset_delel/3,
            oset_power/2
          ]).
:- autoload(library(lists), [reverse/2]).
:- autoload(library(ordsets),
            [ is_ordset/1,
              ord_union/3,
              ord_intersection/3,
              ord_subtract/3,
              ord_add_element/3,
              ord_del_element/3,
              ord_union/2,
              ord_intersection/2
            ]).

/** <module> Ordered set manipulation

This library defines set  operations  on   sets  represented  as ordered
lists. This current library is a   thin wrapper around library(ordsets).
Many of the  implementations  of   library(ordsets)  originate  from the
library.

@author Jon Jagger
@deprecated Use the de-facto library(ordsets)
*/

%!  oset_is(@OSet)
%
%   check that OSet in correct format (standard order)
%
%   @deprecated Use is_ordset/1 from library(ordsets)

oset_is(OSet) :-
    is_ordset(OSet).

%! oset_union(+OSet1, +OSet2, -Union)
%
%  Union is the union of OSet1 and OSet2.
%
%  @deprecated Use ord_union/3 from library(ordsets)

oset_union(OSet1, OSet2, Union) :-
    ord_union(OSet1, OSet2, Union).

%!  oset_int(+OSet1, +OSet2, -Int)
%
%   ordered set intersection

oset_int(Set1, Set2, Intersection) :-
    ord_intersection(Set1, Set2, Intersection).

%!  oset_diff(+InOSet, +NotInOSet, -Diff)
%
%   Ordered set difference
%
%   @deprecated Use ord_subtract/3 from library(ordsets)

oset_diff(InOSet, NotInOSet, Diff) :-
    ord_subtract(InOSet, NotInOSet, Diff).

%!  oset_dunion(+SetofSets, -DUnion)
%
%   Distributed union.
%
%   @deprecated Use ord_union/2 from library(ordsets)

oset_dunion(SetofSets, DUnion) :-
    ord_union(SetofSets, DUnion).

%!  oset_dint(+SetofSets, -DInt)
%
%   Distributed intersection.
%
%   @deprecated Use ord_intersection/2 from library(ordsets)

oset_dint(SetofSets, DInt) :-
    ord_intersection(SetofSets, DInt).

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

%!  oset_addel(+Set, +El, -Add)
%
%   Ordered set element addition.
%
%   @deprecated Use ord_add_element/3 from library(ordsets)

oset_addel(Set1, Element, Set2) :-
    ord_add_element(Set1, Element, Set2).

%!  oset_delel(+Set, +El, -Del)
%
%   Ordered set element deletion.
%
%   @deprecated Use ord_del_element/3 from library(ordsets)

oset_delel(Set, Element, NewSet) :-
    ord_del_element(Set, Element, NewSet).
