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
library. The SICStus library contains  one   using  AVL  trees to ensure
proper balancing. Although based  on  this   library  they  changed  the
argument order of some of the predicates.

Richard O'Keefe has told me he  is  working   on  a  new version of this
library. This new version, as it becomes available, is likely to replace
this one.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


/*
Balancing code merged from L.Damas, V.S.Costa, AVL trees in YAP.
Tree is either:

*) empty (t/0)  or
*) t(Key,Value,Balance,Left,Right)
   Left,Right: trees
   Balance: <, -, or > denoting |L|-|R| = 1, 0, or -1, respectively

TODO: get_next_assoc/4, get_prev_assoc/4 for SICStus compatibility

*/

/*
   Added del_assoc/4, del_min_assoc/4 and del_max_assoc/4

   Ported by Glenn Burgess from a language called Pure.
   Jiri Spitz ported the Pure AVL library from this SWI-Prolog library,
   but the deletion code was added by Jiri. Full circle.
   Also added is_assoc/1, which makes testing much easier.     */

:- module(assoc,
	  [ empty_assoc/1,		% -Assoc
	    is_assoc/1,                 % +Assoc
	    assoc_to_list/2,		% +Assoc, -Pairs
	    assoc_to_keys/2,		% +Assoc, -List
	    assoc_to_values/2,		% +Assoc, -List
	    gen_assoc/3,		% ?Key, +Assoc, ?Value
	    get_assoc/3,		% +Key, +Assoc, ?Value
	    get_assoc/5,		% +Key, +Assoc, ?Old, ?NewAssoc, +New
	    list_to_assoc/2,		% +List, ?Assoc
	    map_assoc/2,		% :Goal, +Assoc
	    map_assoc/3,		% :Goal, +AssocIn, ?AssocOut
	    max_assoc/3,		% +Assoc, ?Key, ?Value
	    min_assoc/3,		% +Assoc, ?Key, ?Value
	    ord_list_to_assoc/2,	% +List, ?Assoc
	    put_assoc/4,		% +Key, +Assoc, +Value, ?NewAssoc
	    del_assoc/4,                % +Key, +Assoc, +Value, ?NewAssoc
	    del_min_assoc/4,            % +Assoc, ?Key, ?Value, ?NewAssoc
	    del_max_assoc/4             % +Assoc, ?Key, ?Value, ?NewAssoc
	  ]).
:- use_module(library(error)).

/** <module> Binary associations

Assocs are Key-Value associations implemented as  a balanced binary tree
(AVL tree).

@see		library(pairs), library(rbtrees)
@author		R.A.O'Keefe, L.Damas, V.S.Costa and Jan Wielemaker
@license	Public domain
*/

:- meta_predicate
	map_assoc(1, ?),
	map_assoc(2, ?, ?).

%%	empty_assoc(-Assoc) is det.
%%	empty_assoc(+Assoc) is semidet.
%
%	Is true if Assoc is the empty assoc.

empty_assoc(t).

%%	assoc_to_list(+Assoc, -Pairs:list(Key-Value)) is semidet.
%
%	Translate Assoc to a list of pairs.  The keys in Pairs are
%	sorted in ascending order.

assoc_to_list(Assoc, List) :-
	assoc_to_list(Assoc, List, []).

assoc_to_list(t(Key,Val,_,L,R), List, Rest) :-
	assoc_to_list(L, List, [Key-Val|More]),
	assoc_to_list(R, More, Rest).
assoc_to_list(t, List, List).


%%	assoc_to_keys(+Assoc, -Keys:ord_set) is det.
%
%	True if Keys is the list of keys   in Assoc. The keys are sorted
%	in ascending order.

assoc_to_keys(Assoc, List) :-
	assoc_to_keys(Assoc, List, []).

assoc_to_keys(t(Key,_,_,L,R), List, Rest) :-
	assoc_to_keys(L, List, [Key|More]),
	assoc_to_keys(R, More, Rest).
assoc_to_keys(t, List, List).


%%	assoc_to_values(+Assoc, -Values:list) is det.
%
%	True if Values is the  list  of   values  in  Assoc.  Values are
%	ordered in ascending  order  of  the   key  to  which  they were
%	associated.  Values may contain duplicates.

assoc_to_values(Assoc, List) :-
	assoc_to_values(Assoc, List, []).

assoc_to_values(t(_,Value,_,L,R), List, Rest) :-
	assoc_to_values(L, List, [Value|More]),
	assoc_to_values(R, More, Rest).
assoc_to_values(t, List, List).

%%      is_assoc(+Assoc)
%
%	True if Assoc is an AVL-tree   association  list Checks that the
%	structure is valid, elements are in  order, and tree is balanced
%	to the extent guaranteed by AVL   trees.  I.e., branches of each
%	subtree differ in depth by at most 1.

is_assoc(Assoc) :-
	is_assoc(Assoc, _Min, _Max, _Depth).

is_assoc(t,X,X,0) :- !.
is_assoc(t(K,_,-,t,t),K,K,1) :- !, ground(K).
is_assoc(t(K,_,>,t,t(RK,_,-,t,t)),K,RK,2) :-
	% Ensure right side Key is 'greater' than K
	!, ground((K,RK)), K @< RK.

is_assoc(t(K,_,<,t(LK,_,-,t,t),t),LK,K,2) :-
	% Ensure left side Key is 'less' than K
	!, ground((LK,K)), LK @< K.

is_assoc(t(K,_,B,L,R),Min,Max,Depth) :-
	is_assoc(L,Min,LMax,LDepth),
	is_assoc(R,RMin,Max,RDepth),
	% Ensure Balance matches depth
	compare(Rel,RDepth,LDepth),
	balance(Rel,B),
	% Ensure ordering
	ground((LMax,K,RMin)),
	LMax @< K,
	K @< RMin,
	Depth is max(LDepth, RDepth)+1.

% Private lookup table matching comparison operators to Balance operators used in tree
balance(=,-).
balance(<,<).
balance(>,>).


%%	gen_assoc(?Key, +Assoc, ?Value) is nondet.
%
%	True if Key-Value is an association in Assoc. Enumerates keys in
%	ascending order.
%
%	@see get_assoc/3.

gen_assoc(Key, t(_,_,_,L,_), Val) :-
	gen_assoc(Key, L, Val).
gen_assoc(Key, t(Key,Val,_,_,_), Val).
gen_assoc(Key, t(_,_,_,_,R), Val) :-
	gen_assoc(Key, R, Val).


%%	get_assoc(+Key, +Assoc, -Value) is semidet.
%
%	True if Key-Value is an association in Assoc.
%
%	@error type_error(assoc, Assoc) if Assoc is not an assoc.

get_assoc(Key, Assoc, Val) :-
	must_be(assoc, Assoc),
	Assoc = t(K,V,_,L,R),
	compare(Rel, Key, K),
	get_assoc(Rel, Key, V, L, R, Val).

get_assoc(=, _, Val, _, _, Val).
get_assoc(<, Key, _, Tree, _, Val) :-
	get_assoc(Key, Tree, Val).
get_assoc(>, Key, _, _, Tree, Val) :-
	get_assoc(Key, Tree, Val).


%%	get_assoc(+Key, +AssocIn, +Val, -AssocOut, +NewVal) is semidet.
%
%	True if Key-Val is in AssocIn and Key-NewVal is in AssocOut.

get_assoc(Key, t(K,V,B,L,R), Val, t(K,NV,B,NL,NR), NVal) :-
	compare(Rel, Key, K),
	get_assoc(Rel, Key, V, L, R, Val, NV, NL, NR, NVal).

get_assoc(=, _, Val, L, R, Val, NVal, L, R, NVal).
get_assoc(<, Key, V, L, R, Val, V, NL, R, NVal) :-
	get_assoc(Key, L, Val, NL, NVal).
get_assoc(>, Key, V, L, R, Val, V, L, NR, NVal) :-
	get_assoc(Key, R, Val, NR, NVal).


%%	list_to_assoc(+List:list(Key-Value), -Assoc) is det.
%
%	Create an assoc from a pair-list.
%
%	@error domain_error(unique_key_pairs, List) if List contains duplicate keys

list_to_assoc(List, Assoc) :-
	(  List = [] -> Assoc = t
	;  keysort(List, Sorted),
	   (  ord_pairs(Sorted)
	   -> length(Sorted, N),
	      list_to_assoc(N, Sorted, [], _, Assoc)
	   ;  domain_error(unique_key_pairs, List)
	   )
	).

list_to_assoc(1, [K-V|More], More, 1, t(K,V,-,t,t)) :- !.
list_to_assoc(2, [K1-V1,K2-V2|More], More, 2, t(K2,V2,<,t(K1,V1,-,t,t),t)) :- !.
list_to_assoc(N, List, More, Depth, t(K,V,Balance,L,R)) :-
	N0 is N - 1,
	RN is N0 div 2,
	Rem is N0 mod 2,
	LN is RN + Rem,
	list_to_assoc(LN, List, [K-V|Upper], LDepth, L),
	list_to_assoc(RN, Upper, More, RDepth, R),
	Depth is LDepth + 1,
	compare(B, RDepth, LDepth), balance(B, Balance).

%%	ord_list_to_assoc(+List:list(Key-Value), -Assoc) is det.
%
%	Create an assoc from an ordered pair-list without duplicate keys.
%
%	@error domain_error(key_ordered_pairs, List) if pairs are not ordered.

ord_list_to_assoc(Sorted, Assoc) :-
	(  Sorted = [] -> Assoc = t
	;  (  ord_pairs(Sorted)
	   -> length(Sorted, N),
	      list_to_assoc(N, Sorted, [], _, Assoc)
	   ;  domain_error(key_ordered_pairs, Sorted)
	   )
	).

%%	ord_pairs(+List:list(Key-Value)) is semidet
%
%	True if Pairs is a list of Key-Val pairs strictly ordered by key.

ord_pairs([K-_V|Rest]) :-
	ord_pairs(Rest, K).
ord_pairs([], _K).
ord_pairs([K-_V|Rest], K0) :-
	K0 @< K,
	ord_pairs(Rest, K).

%%	map_assoc(:Pred, +Assoc) is semidet.
%
%	True if Pred(Value) is true for all values in Assoc.

map_assoc(Pred, T) :-
	map_assoc_(T, Pred).

map_assoc_(t, _).
map_assoc_(t(_,Val,_,L,R), Pred) :-
	map_assoc_(L, Pred),
	call(Pred, Val),
	map_assoc_(R, Pred).

%%	map_assoc(:Pred, ?AssocIn, ?AssocOut) is semidet.
%
%	True if for every Key, Pred(ValIn, ValOut) is true.

map_assoc(Pred, T0, T) :-
	map_assoc_(T0, Pred, T).

map_assoc_(t, _, t).
map_assoc_(t(Key,Val,B,L0,R0), Pred, t(Key,Ans,B,L1,R1)) :-
	map_assoc_(L0, Pred, L1),
	call(Pred, Val, Ans),
	map_assoc_(R0, Pred, R1).


%%	max_assoc(+Assoc, -Key, -Value) is semidet.
%
%	True if Key-Value is in assoc and Key is the largest.

max_assoc(t(K,V,_,_,R), Key, Val) :-
	max_assoc(R, K, V, Key, Val).

max_assoc(t, K, V, K, V).
max_assoc(t(K,V,_,_,R), _, _, Key, Val) :-
	max_assoc(R, K, V, Key, Val).


%%	min_assoc(+Assoc, -Key, -Value) is semidet.
%
%	True if Key-Value is in assoc and Key is the smallest.

min_assoc(t(K,V,_,L,_), Key, Val) :-
	min_assoc(L, K, V, Key, Val).

min_assoc(t, K, V, K, V).
min_assoc(t(K,V,_,L,_), _, _, Key, Val) :-
	min_assoc(L, K, V, Key, Val).


%%	put_assoc(+Key, +AssocIn, +Value, -AssocOut) is det.
%
%	Add Key-Value to AssocIn. If  Key   is  already  in AssocIn, the
%	associated value is replaced.

put_assoc(Key, A0, Value, A) :-
	insert(A0, Key, Value, A, _).

insert(t, Key, Val, t(Key,Val,-,t,t), yes).
insert(t(Key,Val,B,L,R), K, V, NewTree, WhatHasChanged) :-
	compare(Rel, K, Key),
	insert(Rel, t(Key,Val,B,L,R), K, V, NewTree, WhatHasChanged).

insert(=, t(Key,_,B,L,R), _, V, t(Key,V,B,L,R), no).
insert(<, t(Key,Val,B,L,R), K, V, NewTree, WhatHasChanged) :-
	insert(L, K, V, NewL, LeftHasChanged),
	adjust(LeftHasChanged, t(Key,Val,B,NewL,R), left, NewTree, WhatHasChanged).
insert(>, t(Key,Val,B,L,R), K, V, NewTree, WhatHasChanged) :-
	insert(R, K, V, NewR, RightHasChanged),
	adjust(RightHasChanged, t(Key,Val,B,L,NewR), right, NewTree, WhatHasChanged).

adjust(no, Oldree, _, Oldree, no).
adjust(yes, t(Key,Val,B0,L,R), LoR, NewTree, WhatHasChanged) :-
	table(B0, LoR, B1, WhatHasChanged, ToBeRebalanced),
	rebalance(ToBeRebalanced, t(Key,Val,B0,L,R), B1, NewTree, _, _).

%     balance  where     balance  whole tree  to be
%     before   inserted  after    increased   rebalanced
table(-      , left    , <      , yes       , no    ) :- !.
table(-      , right   , >      , yes       , no    ) :- !.
table(<      , left    , -      , no        , yes   ) :- !.
table(<      , right   , -      , no        , no    ) :- !.
table(>      , left    , -      , no        , no    ) :- !.
table(>      , right   , -      , no        , yes   ) :- !.

%%      del_min_assoc(+AssocIn, ?Key, ?Val, -AssocOut)
%
%	True if Key-Value  is  in  AssocIn   and  Key  is  the smallest.
%	AssocOut is AssocIn with Key-Value   removed. Warning: this will
%	succeed with no bindings for Key or Val if input Tree is t.

del_min_assoc(Tree, Key, Val, NewTree) :-
	del_min_assoc(Tree, Key, Val, NewTree, _DepthChanged).

del_min_assoc(t, _, _, t,no).
del_min_assoc(t(Key,Val,_B,t,R), Key, Val, R, yes) :- !.
del_min_assoc(t(K,V,B,L,R), Key, Val, NewTree, Changed) :-
	del_min_assoc(L, Key, Val, NewL, LeftChanged),
	deladjust(LeftChanged, t(K,V,B,NewL,R), left, NewTree, Changed).

%%      del_max_assoc(+AssocIn, ?Key, ?Val, -AssocOut)
%
%	True if Key-Value  is  in  AssocIn   and  Key  is  the greatest.
%	AssocOut is AssocIn with Key-Value   removed. Warning: this will
%	succeed with no bindings for Key or Val if input Tree is t.

del_max_assoc(Tree, Key, Val, NewTree) :-
	del_max_assoc(Tree, Key, Val, NewTree, _DepthChanged).

del_max_assoc(t, _, _, t,no).
del_max_assoc(t(Key,Val,_B,L,t), Key, Val, L, yes) :- !.
del_max_assoc(t(K,V,B,L,R), Key, Val, NewTree, Changed) :-
	del_max_assoc(R, Key, Val, NewR, RightChanged),
	deladjust(RightChanged, t(K,V,B,L,NewR), right, NewTree, Changed).

%%	del_assoc(+Key, +AssocIn, ?Value, -AssocOut)
%
%	True if Key-Value is  in  AssocIn.   AssocOut  is  AssocOut with
%	Key-Value removed.

del_assoc(Key, A0, Value, A) :-
	delete(A0, Key, Value, A, _).

% delete(+Subtree, +SearchedKey, ?SearchedValue, ?SubtreeOut, ?WhatHasChanged)
delete(t, _, _, t, no).          % deletion from empty tree succeeds with no bindings
delete(t(Key,Val,B,L,R), K, V, NewTree, WhatHasChanged) :-
	compare(Rel, K, Key),
	delete(Rel, t(Key,Val,B,L,R), K, V, NewTree, WhatHasChanged).

% delete(+KeySide, +Subtree, +SearchedKey, ?SearchedValue, ?SubtreeOut, ?WhatHasChanged)
% KeySide is an operator {<,=,>} indicating which branch should be searched for the key.
% WhatHasChanged {yes,no} indicates whether the NewTree has changed in depth.
delete(=, t(Key,Val,_B,t,R), Key, Val, R, yes) :- !.
delete(=, t(Key,Val,_B,L,t), Key, Val, L, yes) :- !.
delete(=, t(Key,Val,>,L,R), Key, Val, NewTree, WhatHasChanged) :-
	% Rh tree is deeper, so rotate from R to L
	del_min_assoc(R, K, V, NewR, RightHasChanged),
	deladjust(RightHasChanged, t(K,V,>,L,NewR), right, NewTree, WhatHasChanged), !.
delete(=, t(Key,Val,B,L,R), Key, Val, NewTree, WhatHasChanged) :-
	% Rh tree is not deeper, so rotate from L to R
	del_max_assoc(L, K, V, NewL, LeftHasChanged),
	deladjust(LeftHasChanged, t(K,V,B,NewL,R), left, NewTree, WhatHasChanged), !.

delete(<, t(Key,Val,B,L,R), K, V, NewTree, WhatHasChanged) :-
	delete(L, K, V, NewL, LeftHasChanged),
	deladjust(LeftHasChanged, t(Key,Val,B,NewL,R), left, NewTree, WhatHasChanged).
delete(>, t(Key,Val,B,L,R), K, V, NewTree, WhatHasChanged) :-
	delete(R, K, V, NewR, RightHasChanged),
	deladjust(RightHasChanged, t(Key,Val,B,L,NewR), right, NewTree, WhatHasChanged).

deladjust(no, OldTree, _, OldTree, no).
deladjust(yes, t(Key,Val,B0,L,R), LoR, NewTree, RealChange) :-
	deltable(B0, LoR, B1, WhatHasChanged, ToBeRebalanced),
	rebalance(ToBeRebalanced, t(Key,Val,B0,L,R), B1, NewTree, WhatHasChanged, RealChange).

%     balance  where     balance  whole tree  to be
%     before   deleted   after    changed   rebalanced
deltable(-      , right   , <      , no        , no    ) :- !.
deltable(-      , left    , >      , no        , no    ) :- !.
deltable(<      , right   , -      , yes       , yes   ) :- !.
deltable(<      , left    , -      , yes       , no    ) :- !.
deltable(>      , right   , -      , yes       , no    ) :- !.
deltable(>      , left    , -      , yes       , yes   ) :- !.
% It depends on the tree pattern in avl_geq whether it really decreases.

% Single and double tree rotations - these are common for insert and delete.
/* The patterns (>)-(>), (>)-( <), ( <)-( <) and ( <)-(>) on the LHS
   always change the tree height and these are the only patterns which can
   happen after an insertion. That's the reason why we can use a table only to
   decide the needed changes.

   The patterns (>)-( -) and ( <)-( -) do not change the tree height. After a
   deletion any pattern can occur and so we return yes or no as a flag of a
   height change.  */


rebalance(no, t(K,V,_,L,R), B, t(K,V,B,L,R), Changed, Changed).
rebalance(yes, OldTree, _, NewTree, _, RealChange) :-
	avl_geq(OldTree, NewTree, RealChange).

avl_geq(t(A,VA,>,Alpha,t(B,VB,>,Beta,Gamma)),
	t(B,VB,-,t(A,VA,-,Alpha,Beta),Gamma), yes) :- !.
avl_geq(t(A,VA,>,Alpha,t(B,VB,-,Beta,Gamma)),
	t(B,VB,<,t(A,VA,>,Alpha,Beta),Gamma), no) :- !.
avl_geq(t(B,VB,<,t(A,VA,<,Alpha,Beta),Gamma),
	t(A,VA,-,Alpha,t(B,VB,-,Beta,Gamma)), yes) :- !.
avl_geq(t(B,VB,<,t(A,VA,-,Alpha,Beta),Gamma),
	t(A,VA,>,Alpha,t(B,VB,<,Beta,Gamma)), no) :- !.
avl_geq(t(A,VA,>,Alpha,t(B,VB,<,t(X,VX,B1,Beta,Gamma),Delta)),
	t(X,VX,-,t(A,VA,B2,Alpha,Beta),t(B,VB,B3,Gamma,Delta)), yes) :- !,
	table2(B1, B2, B3).
avl_geq(t(B,VB,<,t(A,VA,>,Alpha,t(X,VX,B1,Beta,Gamma)),Delta),
	t(X,VX,-,t(A,VA,B2,Alpha,Beta),t(B,VB,B3,Gamma,Delta)), yes) :- !,
	table2(B1, B2, B3).

table2(< ,- ,> ).
table2(> ,< ,- ).
table2(- ,- ,- ).


		 /*******************************
		 *	      ERRORS		*
		 *******************************/

:- multifile
	error:has_type/2.

error:has_type(assoc, X) :-
	(   X == t
	->  true
	;   compound(X),
	    functor(X, t, 5)
	).
