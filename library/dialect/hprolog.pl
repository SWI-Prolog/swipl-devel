/*  Part of SWI-Prolog

    Author:        Tom Schrijvers, Bart Demoen, Jan Wielemaker
    E-mail:        Tom.Schrijvers@cs.kuleuven.be
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2004-2008, K.U. Leuven

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/


:- module(hprolog,
	  [ substitute_eq/4,		% +OldVal, +OldList, +NewVal, -NewList
	    memberchk_eq/2,		% +Val, +List
	    intersect_eq/3,		% +List1, +List2, -Intersection
	    list_difference_eq/3,	% +List, -Subtract, -Rest
	    take/3,			% +N, +List, -FirstElements
	    drop/3,			% +N, +List, -LastElements
	    split_at/4,			% +N, +List, -FirstElements, -LastElements
	    max_go_list/2,		% +List, -Max
	    or_list/2,			% +ListOfInts, -BitwiseOr
	    sublist/2,			% ?Sublist, +List
	    bounded_sublist/3,		% ?Sublist, +List, +Bound
	    chr_delete/3,
	    init_store/2,
	    get_store/2,
	    update_store/2,
	    make_get_store_goal/3,
	    make_update_store_goal/3,
	    make_init_store_goal/3,

	    empty_ds/1,
	    ds_to_list/2,
	    get_ds/3,
	    put_ds/4
%	    lookup_ht1/4
	  ]).
:- use_module(library(lists)).
:- use_module(library(assoc)).

/** <module> hProlog compatibility library

This library has been developed mainly for porting the CHR package.

@author Tom Schrijvers
@author Bart Demoen
@author Jan Wielemaker
@tbd	Ultimately, this must disappear.  Generally useful predicates
	must be moved to their appropriate library.  Others must be moved
	into the CHR utilities.
*/

empty_ds(DS) :- empty_assoc(DS).
ds_to_list(DS,LIST) :- assoc_to_list(DS,LIST).
get_ds(A,B,C) :- get_assoc(A,B,C).
put_ds(A,B,C,D) :- put_assoc(A,B,C,D).


init_store(Name,Value) :- nb_setval(Name,Value).

get_store(Name,Value) :- nb_getval(Name,Value).

update_store(Name,Value) :- b_setval(Name,Value).

make_init_store_goal(Name,Value,Goal) :- Goal = nb_setval(Name,Value).

make_get_store_goal(Name,Value,Goal) :- Goal = nb_getval(Name,Value).

make_update_store_goal(Name,Value,Goal) :- Goal = b_setval(Name,Value).


		 /*******************************
		 *      MORE LIST OPERATIONS	*
		 *******************************/

%%	substitute_eq(+OldVal, +OldList, +NewVal, -NewList)
%
%	Substitute OldVal by NewVal in OldList and unify the result
%	with NewList.

substitute_eq(_, [], _, []) :- ! .
substitute_eq(X, [U|Us], Y, [V|Vs]) :-
        (   X == U
	->  V = Y,
            substitute_eq(X, Us, Y, Vs)
        ;   V = U,
            substitute_eq(X, Us, Y, Vs)
        ).

%%	memberchk_eq(+Val, +List)
%
%	Deterministic check of membership using == rather than
%	unification.

memberchk_eq(X, [Y|Ys]) :-
   (   X == Y
   ->  true
   ;   memberchk_eq(X, Ys)
   ).

% :- load_foreign_library(chr_support).

%%	list_difference_eq(+List, -Subtract, -Rest)
%
%	Delete all elements of Subtract from List and unify the result
%	with Rest.  Element comparision is done using ==/2.

list_difference_eq([],_,[]).
list_difference_eq([X|Xs],Ys,L) :-
	(   memberchk_eq(X,Ys)
	->  list_difference_eq(Xs,Ys,L)
	;   L = [X|T],
	    list_difference_eq(Xs,Ys,T)
	).

%%	intersect_eq(+List1, +List2, -Intersection)
%
%	Determine the intersection of two lists without unifying values.

intersect_eq([], _, []).
intersect_eq([X|Xs], Ys, L) :-
	(   memberchk_eq(X, Ys)
	->  L = [X|T],
	    intersect_eq(Xs, Ys, T)
	;   intersect_eq(Xs, Ys, L)
	).


%%	take(+N, +List, -FirstElements)
%
%	Take the first  N  elements  from   List  and  unify  this  with
%	FirstElements. The definition is based   on the GNU-Prolog lists
%	library. Implementation by Jan Wielemaker.

take(0, _, []) :- !.
take(N, [H|TA], [H|TB]) :-
	N > 0,
	N2 is N - 1,
	take(N2, TA, TB).

%%	drop(+N, +List, -ListMinFirstN) is semidet.
%
%	Drop the first N elements from List and unify the remainder with
%	LastElements.

drop(0,LastElements,LastElements) :- !.
drop(N,[_|Tail],LastElements) :-
	N > 0,
	N1 is N  - 1,
	drop(N1,Tail,LastElements).

%%	split_at(+N, +List, +FirstN, -Rest) is semidet.
%
%	Combines take/3 and drop/3.

split_at(0,L,[],L) :- !.
split_at(N,[H|T],[H|L1],L2) :-
	M is N -1,
	split_at(M,T,L1,L2).

%%	max_go_list(+List, -Max)
%
%	Return the maximum of List in the standard order of terms.

max_go_list([H|T], Max) :-
	max_go_list(T, H, Max).

max_go_list([], Max, Max).
max_go_list([H|T], X, Max) :-
        (   H @=< X
	->  max_go_list(T, X, Max)
        ;   max_go_list(T, H, Max)
        ).

%%	or_list(+ListOfInts, -BitwiseOr)
%
%	Do a bitwise disjuction over all integer members of ListOfInts.

or_list(L, Or) :-
	or_list(L, 0, Or).

or_list([], Or, Or).
or_list([H|T], Or0, Or) :-
	Or1 is H \/ Or0,
	or_list(T, Or1, Or).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%	sublist(?Sub, +List) is nondet.
%
%	True if all elements of Sub appear in List in the same order.

sublist(L, L).
sublist(Sub, [H|T]) :-
	'$sublist1'(T, H, Sub).

'$sublist1'(Sub, _, Sub).
'$sublist1'([H|T], _, Sub) :-
	'$sublist1'(T, H, Sub).
'$sublist1'([H|T], X, [X|Sub]) :-
	'$sublist1'(T, H, Sub).

%%	bounded_sublist(?Sub, +List, +Bound:integer)
%
%	As sublist/2, but Sub has at most  Bound elements. E.g. the call
%	below generates all 21 sublists of length   =< 2 from the second
%	argument.
%
%	==
%	?- bounded_sublist(List, [a,b,c,d], 2).
%	X = [] ;
%	X = [a] ;
%	X = [a, b] ;
%	X = [a] ;
%	...
%	==

bounded_sublist(Sublist,_,_) :-
	Sublist = [].
bounded_sublist(Sublist,[H|List],Bound) :-
	Bound > 0,
	(
		Sublist = [H|Rest],
		NBound is Bound - 1,
		bounded_sublist(Rest,List,NBound)
	;
		bounded_sublist(Sublist,List,Bound)
	).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%	chr_delete(+List, +Element, -Rest) is det.
%
%	Rest is a copy of List   without elements matching Element using
%	==.

chr_delete([], _, []).
chr_delete([H|T], X, L) :-
        (   H==X ->
            chr_delete(T, X, L)
        ;   L=[H|RT],
            chr_delete(T, X, RT)
        ).

