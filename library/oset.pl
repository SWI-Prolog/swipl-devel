% Filename: oset.pl
% Author--: Jon Jagger,  J.R.Jagger@shu.ac.uk
% Created-: 05/03/93
% Version-: 1.0
% Updates-: Mon Oct 21 12:39:41 1996
%	    Fix in oset_int/3 by Robert van Engelen.
% Notes---: This file provides some basic set manipulation
%           predicates. The representation of the sets is
%           assumed to be ordered with no duplication. You
%           can create an ordered set from a free form list
%           by using the sort/2 predicate. The advantage of
%           using an ordered representation is that the algorithms
%           are order sum of the sizes of the operands, rather than
%           product of the sizes of the operands.
%
%           I have tried to make all the predicates as efficient as
%           possible with respect to first argument indexing, and tail 
%           clause determinacy.
%
%           These routines are provided as is, with no guarantees.
%           They have undergone minimal testing.

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


% oset_is(+OSet)
%   check that OSet in correct format (standard order)
% -------------------
oset_is(-) :- !, fail.    % var filter
oset_is([]).
oset_is([H|T]) :-
    oset_is(T, H).

oset_is(-, _) :- !, fail.  % var filter
oset_is([], _H).
oset_is([H|T], H0) :-
    H0 @< H,               % use standard order
    oset_is(T, H).



% oset_union(+OSet1, +OSet2, -Union).
% -----------------------------
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


% oset_int(+OSet1, +OSet2, -Int)
%   ordered set intersection
% ------------------------------
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


% oset_diff(+InOSet, +NotInOSet, -Diff)
%   ordered set difference
% --------------------------------------
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


% oset_dunion(+SetofSets, -DUnion)    
%   distributed union
% --------------------------------
oset_dunion([], []).
oset_dunion([H|T], DUnion) :-
    oset_dunion(T, H, DUnion).

oset_dunion([], _DUnion, _DUnion).
oset_dunion([H|T], DUnion0, DUnion) :-
    oset_union(H, DUnion0, DUnion1),
    oset_dunion(T, DUnion1, DUnion).


% oset_dint(+SetofSets, -DInt)    
%   distributed intersection
% ---------------------------- 
oset_dint([], []).
oset_dint([H|T], DInt) :-
    dint(T, H, DInt).

dint([], DInt, DInt).
dint([H|T], DInt0, DInt) :-
    oset_int(H, DInt0, DInt1),
    dint(T, DInt1, DInt).


% oset_power(+Set, -PSet)
%   ordered set powerset
% -----------------------
oset_power(S, PSet) :-
    pset(S, [[]], PSet0),
    sort(PSet0, PSet).

pset([], PSet, PSet).
pset([H|T], PSet0, PSet) :-
    happ(PSet0, H, PSet1),
    pset(T, PSet1, PSet).

happ([], _, []).
happ([S|Ss], H, [[H|S],S|Rest]) :-
    happ(Ss, H, Rest).



% oset_addel(+Set, +El, -Add)  
%   ordered set element addition
% ------------------------------
oset_addel([], El, [El]). 
oset_addel([H|T], El, Add) :-
    compare(Order, H, El),
    addel(Order, H, T, El, Add).

addel(<, H, T,  El, [H|Add]) :-
    oset_addel(T, El, Add).
addel(=, H, T, _El, [H|T]). 
addel(>, H, T,  El, [El,H|T]).


% oset_delel(+Set, +el, -Del)  
%   ordered set element deletion
% ------------------------------
oset_delel([], _El, []).
oset_delel([H|T], El, Del) :-
    compare(Order, H, El),
    delel(Order, H, T, El, Del).

delel(<,  H, T,  El, [H|Del]) :-
    oset_delel(T, El, Del).
delel(=, _H, T, _El, T).
delel(>,  H, T, _El, [H|T]).

