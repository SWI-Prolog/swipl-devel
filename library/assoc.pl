%   File   : ASSOC.PL
%   Author : R.A.O'Keefe
%   Updated: 9 November 1983
%   Purpose: Binary tree implementation of "association lists".

%   Note   : the keys should be ground, the associated values need not be.

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Adapted for SWI-Prolog by Jan Wielemaker, January 2004.

To the best of my knowledge, this file   is in the public domain and can
therefore safely be distributed with SWI-Prolog and used in applications
without restrictions.

Various versions of this file exists. This   one  is copied from the YAP
library. The SICStus library contains  one   using  ALV  trees to ensure
proper balancing. Although based  on  this   library  they  changed  the
argument order of some of the predicates.

Richard O'Keefe has told me he  is  working   on  a  new version of this
library. This new version, as it becomes available, is likely to replace
this one.

If you wish to use this library  in   an  application, be aware that its
interface may change. If the new version   becomes  available it will be
documented in the SWI-Prolog Reference Manual.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- module(assoc,
	  [ assoc_to_list/2,		% +Assoc, -List
	    empty_assoc/1,		% -Assoc
	    gen_assoc/3,		% +Assoc, ?Key, ?Value
	    get_assoc/3,		% +Key, +Assoc, -Value
	    get_assoc/5,		% +Key, +Assoc, +Old, -NewAssoc, +New
	    list_to_assoc/2,		% +List, -Assoc
	    map_assoc/3,		% :Goal, +AssocIn, -AssocOut
	    ord_list_to_assoc/2,	% +List, -Assoc
	    put_assoc/4			% +Key, +Assoc, +Value, -NewAssoc
	  ]).

:- meta_predicate map_assoc(:, ?, ?).

empty_assoc(t).

assoc_to_list(Assoc, List) :-
	assoc_to_list(Assoc, List, []).


assoc_to_list(t(Key,Val,L,R), List, Rest) :-
	assoc_to_list(L, List, [Key-Val|More]),
	assoc_to_list(R, More, Rest).
assoc_to_list(t, List, List).


gen_assoc(t(_,_,L,_), Key, Val) :-
	gen_assoc(L, Key, Val).
gen_assoc(t(Key,Val,_,_), Key, Val).
gen_assoc(t(_,_,_,R), Key, Val) :-
	gen_assoc(R, Key, Val).


get_assoc(Key, t(K,V,L,R), Val) :-
	compare(Rel, Key, K),
	get_assoc(Rel, Key, V, L, R, Val).


get_assoc(=, _, Val, _, _, Val).
get_assoc(<, Key, _, Tree, _, Val) :-
	get_assoc(Key, Tree, Val).
get_assoc(>, Key, _, _, Tree, Val) :-
	get_assoc(Key, Tree, Val).


get_assoc(Key, t(K,V,L,R), Val, t(K,NV,NL,NR), NVal) :-
	compare(Rel, Key, K),
	get_assoc(Rel, Key, V, L, R, Val, NV, NL, NR, NVal).


get_assoc(=, _, Val, L, R, Val, NVal, L, R, NVal).
get_assoc(<, Key, V, L, R, Val, V, NL, R, NVal) :-
	get_assoc(Key, L, Val, NL, NVal).
get_assoc(>, Key, V, L, R, Val, V, L, NR, NVal) :-
	get_assoc(Key, R, Val, NR, NVal).


list_to_assoc(List, Assoc) :-
	list_to_assoc(List, t, Assoc).

list_to_assoc([], Assoc, Assoc).
list_to_assoc([Key-Val|List], Assoc0, Assoc) :-
	put_assoc(Key, Assoc0, Val, AssocI),
	list_to_assoc(List, AssocI, Assoc).

ord_list_to_assoc(Keys, Assoc) :-
	length(Keys, L),
	ord_list_to_assoc(L, Keys, Assoc, []).

ord_list_to_assoc(0, List, t, List) :- !.
ord_list_to_assoc(N, List, t(Key,Val,L,R), Rest) :-
	A is (N-1)//2,
	Z is (N-1)-A,
	ord_list_to_assoc(A, List, L, [Key-Val|More]),
	ord_list_to_assoc(Z, More, R, Rest).


map_assoc(Pred, t(Key,Val,L0,R0), t(Key,Ans,L1,R1)) :- !,
	map_assoc(Pred, L0, L1),
	call(Pred, Val, Ans),
	map_assoc(Pred, R0, R1).
map_assoc(_, t, t).


put_assoc(Key, t(K,V,L,R), Val, New) :- !,
	compare(Rel, Key, K),
	put_assoc(Rel, Key, K, V, L, R, Val, New).
put_assoc(Key, t, Val, t(Key,Val,t,t)).


put_assoc(=, Key, _, _, L, R, Val, t(Key,Val,L,R)).
put_assoc(<, Key, K, V, L, R, Val, t(K,V,Tree,R)) :-
	put_assoc(Key, L, Val, Tree).
put_assoc(>, Key, K, V, L, R, Val, t(K,V,L,Tree)) :-
	put_assoc(Key, R, Val, Tree).

