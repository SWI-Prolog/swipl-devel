/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
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

:- module($sort,
	[ keysort/2
	, predsort/3
	, merge/3
	, merge_set/3
	]).

%   merge_set(+Set1, +Set2, -Set3)
%   Merge the ordered sets Set1 and Set2 into a new ordered set without
%   duplicates.

merge_set([], L, L) :- !.
merge_set(L, [], L) :- !.
merge_set([H1|T1], [H2|T2], [H1|R]) :- H1 @< H2, !, merge_set(T1, [H2|T2], R).
merge_set([H1|T1], [H2|T2], [H2|R]) :- H1 @> H2, !, merge_set([H1|T1], T2, R).
merge_set([H1|T1], [H2|T2], [H1|R]) :- H1 == H2,    merge_set(T1, T2, R).

%	merge(+List1, +List2, -List3)
%	Merge the ordered sets List1 and List2 into a new ordered  list.
%	Duplicates are not removed and their order is maintained.

merge([], L, L) :- !.
merge(L, [], L) :- !.
merge([H1|T1], [H2|T2], [H|R]) :-
	(   H1 @=< H2
	->  H = H1,
	    merge(T1, [H2|T2], R)
	;   H = H2,
	    merge([H1|T1], T2, R)
	).

%	keysort(+Random, ?Ordered)
%	Sorts a random list of Key-Value pairs, and does not remove duplicates.

keysort(List, Sorted) :-
	length(List, Length), 
	$keysort(Length, List, _, Result), 
	Sorted = Result.

$keysort(2, [X1, X2|L], L, R) :- !, 
	X1 = K1-_,
	X2 = K2-_,
	(   K1 @=< K2
	->  R = [X1, X2]
	;   R = [X2, X1]
	).
$keysort(1, [X|L], L, [X]) :- !.
$keysort(0, L, L, []) :- !.
$keysort(N, L1, L3, R) :-
	N1 is N // 2, 
	N2 is N - N1, 
	$keysort(N1, L1, L2, R1), 
	$keysort(N2, L2, L3, R2), 
	$keymerge(R1, R2, R).

$keymerge([], R, R) :- !.
$keymerge(R, [], R) :- !.
$keymerge(R1, R2, [X|R]) :-
	R1 = [X1|R1a], 
	R2 = [X2|R2a], 
	X1 = K1-_,
	X2 = K2-_,
	(   K1 @> K2
	->  X = X2, $keymerge(R1, R2a, R)
	;   X = X1, $keymerge(R1a, R2, R)
	).

:- module_transparent
	predsort/3, 
	$predsort/5, 
	$predmerge/4, 
	$predmerge/7.

/*  Predicate based sort. This one is not copied.

 ** Sun Jun  5 16:13:38 1988  jan@swivax.UUCP (Jan Wielemaker)  */

predsort(P, L, R) :-
	length(L, N), 
	$predsort(P, N, L, _, R1), !, 
	R = R1.

$predsort(P, 2, [X1, X2|L], L, R) :- !, 
	call(P, Delta, X1, X2),
	$sort2(Delta, X1, X2, R).
$predsort(_, 1, [X|L], L, [X]) :- !.
$predsort(_, 0, L, L, []) :- !.
$predsort(P, N, L1, L3, R) :-
	N1 is N // 2, 
	plus(N1, N2, N), 
	$predsort(P, N1, L1, L2, R1), 
	$predsort(P, N2, L2, L3, R2), 
	$predmerge(P, R1, R2, R).

$sort2(<, X1, X2, [X1, X2]).
$sort2(=, X1, _,  [X1]).
$sort2(>, X1, X2, [X2, X1]).

$predmerge(_, [], R, R) :- !.
$predmerge(_, R, [], R) :- !.
$predmerge(P, [H1|T1], [H2|T2], Result) :-
	call(P, Delta, H1, H2),
	$predmerge(Delta, P, H1, H2, T1, T2, Result).

$predmerge(>, P, H1, H2, T1, T2, [H2|R]) :-
	$predmerge(P, [H1|T1], T2, R).
$predmerge(=, P, H1, _, T1, T2, [H1|R]) :-
	$predmerge(P, T1, T2, R).
$predmerge(<, P, H1, H2, T1, T2, [H1|R]) :-
	$predmerge(P, T1, [H2|T2], R).
