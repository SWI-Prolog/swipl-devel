/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker and Richard O'Keefe
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2002, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(lists,
	[ member/2,
	  append/3,
	  select/3,
	  nextto/3,			% ?X, ?Y, ?List
	  delete/3,
	  nth0/3,
	  nth1/3,
	  last/2,			% +List, -Element
	  reverse/2,			% +List, -Reversed
	  permutation/2,		% ?List, ?Permutation
	  flatten/2,			% +Nested, -Flat
	  sumlist/2,			% +List, -Sum
	  numlist/3,			% +Low, +High, -List

	  is_set/1,			% set manipulation
	  list_to_set/2,		% +List, -Set
	  intersection/3,
	  union/3,
	  subset/2,
	  subtract/3
	]).
%:- system_module.			% hide details

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Some of these predicates are copied from   "The  Craft of Prolog" and/or
the DEC-10 Prolog library (LISTRO.PL). Contributed by Richard O'Keefe.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%	member(?Elem, ?List)
%	
%	True if Elem is a member of List

member(X, [X|_]).
member(X, [_|T]) :-
	member(X, T).

%	append(?List1, ?List2, ?List1AndList2)
%	
%	List1AndList2 is the concatination of List1 and List2

append([], L, L).
append([H|T], L, [H|R]) :-
	append(T, L, R).

%	select(?Elem, ?List1, ?List2)
%
%	Is true when List1, with Elem removed results in List2.

select(X, [X|Tail], Tail).
select(Elem, [Head|Tail], [Head|Rest]) :-
	select(Elem, Tail, Rest).


%	nextto(?X, ?Y, ?List)
%	
%	True of Y follows X in List.

nextto(X, Y, [X,Y|_]).
nextto(X, Y, [_|Zs]) :-
	nextto(X, Y, Zs).

%	delete(?List1, ?Elem, ?List2)
%
%	Is true when Lis1, with all occurences of Elem deleted results in
%	List2.

delete([], _, []) :- !.
delete([Elem|Tail], Elem, Result) :- !, 
	delete(Tail, Elem, Result).
delete([Head|Tail], Elem, [Head|Rest]) :-
	delete(Tail, Elem, Rest).

/*  nth0/3, nth1/3 are improved versions from
    Martin Jansche <martin@pc03.idf.uni-heidelberg.de>
*/

%%  nth0(?Index, ?List, ?Elem)
%%  is true when Elem is the Index'th element of List.  Counting starts
%%  at 0.  [This is a faster version of the original SWI-Prolog predicate.]

nth0(Index, List, Elem) :-
        integer(Index), !,
        Index >= 0,
        nth0_det(Index, List, Elem).    %% take nth deterministically
nth0(Index, List, Elem) :-
        var(Index), !,
        nth_gen(List, Elem, 0, Index).  %% match

nth0_det(0, [Elem|_], Elem) :- !.
nth0_det(1, [_,Elem|_], Elem) :- !.
nth0_det(2, [_,_,Elem|_], Elem) :- !.
nth0_det(3, [_,_,_,Elem|_], Elem) :- !.
nth0_det(4, [_,_,_,_,Elem|_], Elem) :- !.
nth0_det(5, [_,_,_,_,_,Elem|_], Elem) :- !.
nth0_det(N, [_,_,_,_,_,_   |Tail], Elem) :-
        M is N - 6,
        nth0_det(M, Tail, Elem).

nth_gen([Elem|_], Elem, Base, Base).
nth_gen([_|Tail], Elem, N, Base) :-
        succ(N, M),
        nth_gen(Tail, Elem, M, Base).


%%  nth1(?Index, ?List, ?Elem)
%%  Is true when Elem is the Index'th element of List.  Counting starts
%%  at 1.  [This is a faster version of the original SWI-Prolog predicate.]

nth1(Index1, List, Elem) :-
        integer(Index1), !,
        Index0 is Index1 - 1,
        nth0_det(Index0, List, Elem).   %% take nth deterministically
nth1(Index, List, Elem) :-
        var(Index), !,
        nth_gen(List, Elem, 1, Index).  %% match


%	last(?List, ?Elem)
%
%	Succeeds if `Last' unifies with the last element of `List'.

last([X|Xs], Last) :-
    last_(Xs, X, Last).

last_([], Last, Last).
last_([X|Xs], _, Last) :-
    last_(Xs, X, Last).


%	reverse(?List1, ?List2)
%
%	Is true when the elements of List2 are in reverse order compared to
%	List1.

reverse(Xs, Ys) :-
    reverse(Xs, [], Ys, Ys).

reverse([], Ys, Ys, []).
reverse([X|Xs], Rs, Ys, [_|Bound]) :-
    reverse(Xs, [X|Rs], Ys, Bound).


%	premutation(?Xs, ?Ys)
%	
%	permutation(Xs, Ys) is true when Xs is a permutation of Ys. This
%	can solve for Ys given Xs or Xs given Ys, or even enumerate Xs
%	and Ys together.

permutation(Xs, Ys) :-
	permutation(Xs, Ys, Ys).

permutation([], [], []).
permutation([X|Xs], Ys1, [_|Bound]) :-
	permutation(Xs, Ys, Bound),
	select(X, Ys1, Ys).

%	flatten(+List1, ?List2)
%
%	Is true when Lis2 is a non nested version of List1.

flatten(List, FlatList) :-
	flatten(List, [], FlatList0), !,
	FlatList = FlatList0.

flatten(Var, Tl, [Var|Tl]) :-
	var(Var), !.
flatten([], Tl, Tl) :- !.
flatten([Hd|Tl], Tail, List) :-
	flatten(Hd, FlatHeadTail, List), 
	flatten(Tl, Tail, FlatHeadTail).
flatten(Atom, Tl, [Atom|Tl]).

%	sumlist(+List, -Sum)
%	
%	Sum is the result of adding all numbers in List.

sumlist(Xs, Sum) :-
	sumlist(Xs, 0, Sum).

sumlist([], Sum, Sum).
sumlist([X|Xs], Sum0, Sum) :-
	Sum1 is Sum0 + X,
	sumlist(Xs, Sum1, Sum).


%	numlist(+Low, +High, -List)
%	
%	List is a list [Low, Low+1, ... High]

numlist(L, U, Ns) :-
    integer(L), integer(U), L =< U,
    numlist_(L, U, Ns).

numlist_(L, U, [L|Ns]) :-
    (   L =:= U
    ->  Ns = []
    ;   M is L + 1,
	numlist_(M, U, Ns)
    ).


		/********************************
		*       SET MANIPULATION        *
		*********************************/

%	is_set(+Set)
%	is True if Set is a proper list without duplicates.

is_set(0) :- !, fail.		% catch variables
is_set([]) :- !.
is_set([H|T]) :-
	memberchk(H, T), !, 
	fail.
is_set([_|T]) :-
	is_set(T).

%	list_to_set(+List, ?Set)
%
%	Is true when Set has the same element as List in the same order.
%	The left-most copy of the duplicate is retained.

list_to_set(List, Set) :-
	list_to_set_(List, Set0),
	Set = Set0.

list_to_set_([], R) :-
	close_list(R).
list_to_set_([H|T], R) :-
	memberchk(H, R), !, 
	list_to_set_(T, R).

close_list([]) :- !.
close_list([_|T]) :-
	close_list(T).


%	intersection(+Set1, +Set2, -Set3)
%
%	Succeeds if Set3 unifies with the intersection of Set1 and Set2

intersection([], _, []) :- !.
intersection([X|T], L, Intersect) :-
	memberchk(X, L), !, 
	Intersect = [X|R], 
	intersection(T, L, R).
intersection([_|T], L, R) :-
	intersection(T, L, R).


%	union(+Set1, +Set2, -Set3)
%	Succeeds if Set3 unifies with the union of Set1 and Set2

union([], L, L) :- !.
union([H|T], L, R) :-
	memberchk(H, L), !, 
	union(T, L, R).
union([H|T], L, [H|R]) :-
	union(T, L, R).


%	subset(+SubSet, +Set)
%	Succeeds if all elements of SubSet belong to Set as well.
%

subset([], _) :- !.
subset([E|R], Set) :-
	memberchk(E, Set), 
	subset(R, Set).


%	subtract(+Set, +Delete, -Result)
%
%	Delete all elements from `Set' that occur in `Delete' (a set) and
%	unify the result with `Result'.

subtract([], _, []) :- !.
subtract([E|T], D, R) :-
	memberchk(E, D), !,
	subtract(T, D, R).
subtract([H|T], D, [H|R]) :-
	subtract(T, D, R).
