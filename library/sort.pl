/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2012, University of Amsterdam
			      Vu University Amsterdam

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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(sort,
	[ predsort/3,			% :Compare, +List, -Sorted
	  locale_sort/2			% +ListOfAtoms, -Sorted
	]).

:- set_prolog_flag(generate_debug_info, false).

:- meta_predicate
	predsort(3, +, -).		% 3: Delta, Left, Right

%%	predsort(:Compare, +List, -Sorted) is det.
%
%	Sorts similar to sort/2, but determines   the order of two terms
%	by calling Compare(-Delta, +E1, +E2). This call must unify Delta
%	with one of <, > or =.  If built-in predicate compare/3 is used,
%	the result is the same as sort/2 (but sort/2 is built using more
%	low-level primitives and is considerably faster).
%
%	@see keysort/2 provides an more portable way to sort on
%	arbitrary keys that is usually faster.

predsort(P, L, R) :-
	'$skip_list'(N, L, Tail),
	(   Tail == []
	->  predsort(P, N, L, _, R1),
	    R = R1
	;   must_be(L, list)
	).

predsort(P, 2, [X1, X2|L], L, R) :- !,
	call(P, Delta, X1, X2),
	sort2(Delta, X1, X2, R).
predsort(_, 1, [X|L], L, [X]) :- !.
predsort(_, 0, L, L, []) :- !.
predsort(P, N, L1, L3, R) :-
	N1 is N // 2,
	plus(N1, N2, N),
	predsort(P, N1, L1, L2, R1),
	predsort(P, N2, L2, L3, R2),
	predmerge(P, R1, R2, R).

sort2(<, X1, X2, [X1, X2]).
sort2(=, X1, _,  [X1]).
sort2(>, X1, X2, [X2, X1]).

predmerge(_, [], R, R) :- !.
predmerge(_, R, [], R) :- !.
predmerge(P, [H1|T1], [H2|T2], Result) :-
	call(P, Delta, H1, H2), !,
	predmerge(Delta, P, H1, H2, T1, T2, Result).

predmerge(>, P, H1, H2, T1, T2, [H2|R]) :-
	predmerge(P, [H1|T1], T2, R).
predmerge(=, P, H1, _, T1, T2, [H1|R]) :-
	predmerge(P, T1, T2, R).
predmerge(<, P, H1, H2, T1, T2, [H1|R]) :-
	predmerge(P, T1, [H2|T2], R).

%%	locale_sort(+List, -Sorted) is det.
%
%	Sort a list of atoms using the current locale.
%
%	@param List	List of atoms
%	@param Sorted	Sorted atoms.

locale_sort(List, Sorted) :-
	collation_keys(List, Keyed),
	keysort(Keyed, KeySorted),
	unkey(KeySorted, Sorted).

collation_keys([], []).
collation_keys([H|T0], [K-H|T]) :-
	collation_key(H, K),
	collation_keys(T0, T).


unkey([], []).
unkey([_-H|T0], [H|T]) :-
	unkey(T0, T).
