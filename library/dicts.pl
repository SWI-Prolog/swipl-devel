/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2015, VU University Amsterdam

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

:- module(dicts,
	  [ dicts_same_tag/2,		% +List, -Tag
	    dict_keys/2,		% +Dict, -Keys
	    dicts_same_keys/2,		% +DictList, -Keys
	    dicts_to_same_keys/3,	% +DictsIn, :OnEmpty, -DictsOut
	    dict_fill/4,		% +Value, +Key, +Dict, -Value
	    dict_no_fill/3,		% +Key, +Dict, -Value
	    dicts_join/6,		% +Key, +Keys, :OnEmpty, +Ds1, +Ds2, -Ds
	    dicts_to_compounds/4	% ?Dicts, +Keys, :OnEmpty, ?Compounds
	  ]).
:- use_module(library(apply)).
:- use_module(library(pairs)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).

:- meta_predicate
	dicts_to_same_keys(+,3,-),
	dicts_join(+,+,3,+,+,-),
	dicts_to_compounds(?,+,3,?).

/** <module> Dict utilities

This library defines utilities that operate   on lists of dicts, notably
to make lists of dicts  consistent   by  adding missing keys, converting
between lists of compounds and  lists  of   dicts  and  joining lists of
dicts.
*/

%%	dicts_same_tag(+List, -Tag) is semidet.
%
%	True when List is a list of dicts that all have the tag Tag.

dicts_same_tag(List, Tag) :-
	maplist(keys_tag(Tag), List).

keys_tag(Tag, Dict) :-
	is_dict(Dict, Tag).

%%	dict_keys(+Dict, -Keys) is det.
%
%	True when Keys is an ordered set of the keys appearing in Dict.

dict_keys(Dict, Keys) :-
	dict_pairs(Dict, _Tag, Pairs),
	pairs_keys(Pairs, Keys).


%%	dicts_same_keys(+List, -Keys) is semidet.
%
%	True if List is a list of dicts  that all have the same keys and
%	Keys is an ordered set of these keys.

dicts_same_keys(List, Keys) :-
	maplist(keys_dict(Keys), List).

keys_dict(Keys, Dict) :-
	dict_keys(Dict, Keys).

%%	dicts_to_same_keys(+DictsIn, :OnEmpty, -DictsOut)
%
%	DictsOut is a copy of DictsIn, where each dict contains all keys
%	appearing in all dicts of  DictsIn.   Values  for  keys that are
%	added to a dict are produced by   calling  OnEmpty as below. The
%	predicate dict_fill/4 provides an implementation  that fills all
%	new cells with a predefined value.
%
%	  ==
%	  call(:OnEmpty, +Key, +Dict, -Value)
%	  ==

dicts_to_same_keys(Dicts, _, Table) :-
	dicts_same_keys(Dicts, _), !,
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

%%	dict_fill(+ValueIn, +Key, +Dict, -Value) is det.
%
%	Implementation for the dicts_to_same_keys/3   `OnEmpty`  closure
%	that  fills  new  cells  with  a  copy  of  ValueIn.  Note  that
%	copy_term/2 does not really copy  ground   terms.  Below are two
%	examples. Note that when filling empty   cells  with a variable,
%	each empty cell is bound to a new variable.
%
%	  ==
%	  ?- dicts_to_same_keys([r{x:1}, r{y:2}], dict_fill(null), L).
%	  L = [r{x:1, y:null}, r{x:null, y:2}].
%	  ?- dicts_to_same_keys([r{x:1}, r{y:2}], dict_fill(_), L).
%	  L = [r{x:1, y:_G2005}, r{x:_G2036, y:2}].
%	  ==
%
%	Use dict_no_fill/3 to raise an error if a dict is missing a key.

dict_fill(ValueIn, _, _, Value) :-
	copy_term(ValueIn, Value).

%%	dict_no_fill is det.
%
%	Can be used instead of dict_fill/4 to raise an exception if some
%	dict is missing a key.

dict_no_fill(Key, Dict, Value) :-
	Value = Dict.Key.

%%	dicts_join(+Key, +Keys, :OnEmpty,
%%		   +Dicts1, +Dicts2, -Dicts) is semidet.
%
%	Join two lists of dicts (Dicts1 and  Dicts2) on Key, copying the
%	values for Keys. Empty fields are filled with the result of
%
%	  ==
%	  call(:OnEmpty, +Key, +Dict, -Value)
%	  ==
%
%	Dicts from Dicts1 and Dicts2 that  have   no  value  for Key are
%	removed. The list Dicts is ordered on   the value for Key. Fails
%	if two dicts have conflicting values for   any of the keys Keys.
%	For example:
%
%	  ==
%	  ?- DL1 = [r{x:1,y:1},r{x:2,y:4}],
%	     DL2 = [r{x:1,z:2},r{x:3,z:4}],
%	     dicts_join(x, [x,y,z], dict_fill(null), DL1, DL2, DL).
%	  DL = [r{x:1, y:1, z:2}, r{x:2, y:4, z:null}, r{x:3, y:null, z:4}].
%	  ==

dicts_join(Join, Keys, OnEmpty, Dicts1, Dicts2, Dicts) :-
	include(has_key(Join), Dicts1, Dicts10),
	include(has_key(Join), Dicts2, Dicts20),
	sort(Join, @=<, Dicts10, Dicts11),
	sort(Join, @=<, Dicts20, Dicts21),
	join(Dicts11, Dicts21, Join, Keys, OnEmpty, Dicts).

has_key(Key, Dict) :-
	get_dict(Key, Dict, _).

join([], [], _, _, _, []) :- !.
join([D1|T1], [D2|T2], Join, Keys, OnEmpty, [DNew|MoreDicts]) :- !,
	get_dict(Join, D1, K1),
	get_dict(Join, D2, K2),
	compare(Diff, K1, K2),
	(   Diff == (=)
	->  consistent(Keys, D1, D2, Shared),
	    select_keys_dict(Keys, OnEmpty, Shared, DNew),
	    join(T1, T2, Join, Keys, OnEmpty, MoreDicts)
	;   Diff == (<)
	->  select_keys_dict(Keys, OnEmpty, D1, DNew),
	    join(T1, [D2|T2], Join, Keys, OnEmpty, MoreDicts)
	;   select_keys_dict(Keys, OnEmpty, D2, DNew),
	    join([D1|T1], T2, Join, Keys, OnEmpty, MoreDicts)
	).
join([], Dicts, _, Keys, OnEmpty, NewDicts) :- !,
	maplist(select_keys_dict(Keys, OnEmpty), Dicts, NewDicts).
join(Dicts, [], _, Keys, OnEmpty, NewDicts) :-
	maplist(select_keys_dict(Keys, OnEmpty), Dicts, NewDicts).

consistent(Keys, Dict1, Dict2, Shared) :-
	pairs_keys_values(Pairs, Keys, _),
	dict_pairs(Shared, _, Pairs),
	Shared >:< Dict1,
	Shared >:< Dict2.

select_keys_dict(Keys, OnEmpty, Dict0, Dict) :-
	is_dict(Dict0, Tag),
	select_key_value_pairs(Keys, OnEmpty, Dict0, Pairs),
	dict_pairs(Dict, Tag, Pairs).

select_key_value_pairs([], _, _, []).
select_key_value_pairs([H|T0], OnEmpty, Dict, [H-V|T]) :-
	key_value(Dict, OnEmpty, H, V),
	select_key_value_pairs(T0, OnEmpty, Dict, T).


%%	dicts_to_compounds(?Dicts, +Keys, :OnEmpty, ?Compounds) is semidet.
%
%	True when Dicts and Compounds are lists   of the same length and
%	each element of Compounds is  a   compound  term whose arguments
%	represent the values associated with   the corresponding keys in
%	Keys. When converting from  dict  to   row,  OnEmpty  is used to
%	compute missing values. The functor for the compound is the same
%	as the tag of the pair. When converting from dict to row and the
%	dict has no tag, the functor `row` is used. For example:
%
%	  ==
%	  ?- Dicts = [_{x:1}, _{x:2, y:3}],
%	     dicts_to_compounds(Dicts, [x], dict_fill(null), Compounds).
%	  Compounds = [row(1), row(2)].
%	  ?- Dicts = [_{x:1}, _{x:2, y:3}],
%	     dicts_to_compounds(Dicts, [x,y], dict_fill(null), Compounds).
%	  Compounds = [row(1, null), row(2, 3)].
%	  ?- Compounds = [point(1,1), point(2,4)],
%	     dicts_to_compounds(Dicts, [x,y], dict_fill(null), Compounds).
%	  Dicts = [point{x:1, y:1}, point{x:2, y:4}].
%	  ==
%
%	When converting from Dicts to  Compounds   Keys  may be computed by
%	dicts_same_keys/2.

dicts_to_compounds(Dicts, Keys, OnEmpty, Compounds) :-
	maplist(dict_to_compound(Keys, OnEmpty), Dicts, Compounds).

dict_to_compound(Keys, OnEmpty, Dict, Row) :-
	is_dict(Dict, Tag), !,
	default_tag(Tag, row),
	maplist(key_value(Dict, OnEmpty), Keys, Values),
	compound_name_arguments(Row, Tag, Values).
dict_to_compound(Keys, _, Dict, Row) :-
	compound(Row),
	compound_name_arguments(Row, Tag, Values),
	pairs_keys_values(Pairs, Keys, Values),
	dict_pairs(Dict, Tag, Pairs).

default_tag(Tag, Tag) :- !.
default_tag(_, _).

key_value(Dict, OnEmpty, Key, Value) :-
	(   get_dict(Key, Dict, Value0)
	->  Value = Value0
	;   call(OnEmpty, Key, Dict, Value)
	).
