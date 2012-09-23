/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2006, University of Amsterdam

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

:- module(pairs,
	  [ pairs_keys_values/3,
	    pairs_values/2,
	    pairs_keys/2,
	    group_pairs_by_key/2,
	    transpose_pairs/2,
	    map_list_to_pairs/3
	  ]).

/** <module> Operations on key-value lists

This module implements common operations on  Key-Value lists, also known
as  _Pairs_.  Pairs  have  great  practical  value,  especially  due  to
keysort/2 and the library assoc.pl.

This library is based  on  disussion   in  the  SWI-Prolog  mailinglist,
including specifications from Quintus and a  library proposal by Richard
O'Keefe.

@see	keysort/2, library(assoc)
@author Jan Wielemaker
*/

%%	pairs_keys_values(?Pairs, ?Keys, ?Values) is det.
%
%	True if Keys holds the keys of Pairs and Values the values.
%
%	Deterministic if any argument is instantiated   to a finite list
%	and the others are either free or  finite lists. All three lists
%	are in the same order.
%
%	@see pairs_values/2 and pairs_keys/2.

pairs_keys_values(Pairs, Keys, Values) :-
	(   nonvar(Pairs) ->
	    pairs_keys_values_(Pairs, Keys, Values)
	;   nonvar(Keys) ->
	    keys_values_pairs(Keys, Values, Pairs)
	;   values_keys_pairs(Values, Keys, Pairs)
	).

pairs_keys_values_([], [], []).
pairs_keys_values_([K-V|Pairs], [K|Keys], [V|Values]) :-
	pairs_keys_values_(Pairs, Keys, Values).

keys_values_pairs([], [], []).
keys_values_pairs([K|Ks], [V|Vs], [K-V|Pairs]) :-
	keys_values_pairs(Ks, Vs, Pairs).

values_keys_pairs([], [], []).
values_keys_pairs([V|Vs], [K|Ks], [K-V|Pairs]) :-
	values_keys_pairs(Vs, Ks, Pairs).

%%	pairs_values(+Pairs, -Values) is det.
%
%	Remove the keys  from  a  list   of  Key-Value  pairs.  Same  as
%	pairs_keys_values(Pairs, _, Values)

pairs_values([], []).
pairs_values([_-V|T0], [V|T]) :-
	pairs_values(T0, T).


%%	pairs_keys(+Pairs, -Keys) is det.
%
%	Remove the values  from  a  list   of  Key-Value  pairs.  Same  as
%	pairs_keys_values(Pairs, Keys, _)

pairs_keys([], []).
pairs_keys([K-_|T0], [K|T]) :-
	pairs_keys(T0, T).


%%	group_pairs_by_key(+Pairs, -Joined:list(Key-Values)) is det.
%
%	Group values with the same key. Pairs must be a key-sorted list.
%	For example:
%
%	==
%	?- group_pairs_by_key([a-2, a-1, b-4], X).
%
%	X = [a-[2,1], b-[4]]
%	==
%
%	@param	Pairs	Key-Value list, sorted to the standard order
%			of terms (as keysort/2 does)
%	@param  Joined	List of Key-Group, where Group is the
%			list of Values associated with Key.

group_pairs_by_key([], []).
group_pairs_by_key([M-N|T0], [M-[N|TN]|T]) :-
	same_key(M, T0, TN, T1),
	group_pairs_by_key(T1, T).

same_key(M, [M-N|T0], [N|TN], T) :- !,
	same_key(M, T0, TN, T).
same_key(_, L, [], L).


%%	transpose_pairs(+Pairs, -Transposed) is det.
%
%	Swap Key-Value to Value-Key. The resulting  list is sorted using
%	keysort/2 on the new key.

transpose_pairs(Pairs, Transposed) :-
	flip_pairs(Pairs, Flipped),
	keysort(Flipped, Transposed).

flip_pairs([], []).
flip_pairs([Key-Val|Pairs], [Val-Key|Flipped]) :-
	flip_pairs(Pairs, Flipped).


%%	map_list_to_pairs(:Function, +List, -Keyed)
%
%	Create a Key-Value list by mapping each element of List.
%	For example, if we have a list of lists we can create a
%	list of Length-List using
%
%	==
%		map_list_to_pairs(length, ListOfLists, Pairs),
%	==

:- meta_predicate
	map_list_to_pairs(2, +, -).

map_list_to_pairs(Function, List, Pairs) :-
	map_list_to_pairs2(List, Function, Pairs).

map_list_to_pairs2([], _, []).
map_list_to_pairs2([H|T0], Pred, [K-H|T]) :-
	call(Pred, H, K),
	map_list_to_pairs2(T0, Pred, T).

