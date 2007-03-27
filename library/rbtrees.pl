/* 

	This code implements Red-Black trees as described in:

	"Introduction to Algorithms", Second Edition
	Cormen, Leiserson, Rivest, and Stein,
	MIT Press

        Author: Vitor Santos Costa
*/


:- module(rbtrees,
	  [ rb_new/1,
	    rb_empty/1,			% ?T
	    rb_lookup/3,		% +Key, -Value, +T
	    rb_update/4,		% +T, +Key, +NewVal, -TN
	    rb_update/5,		% +T, +Key, ?OldVal, +NewVal, -TN
	    rb_apply/4,			% +T, +Key, :G, -TN
	    rb_lookupall/3,		% +Key, -Value, +T
	    rb_insert/4,		% +T0, +Key, ?Value, -TN
	    rb_delete/3,		% +T, +Key, -TN
	    rb_delete/4,		% +T, +Key, -Val, -TN
	    rb_visit/2,			% +T, -Pairs
%	    rb_visit/3,
	    rb_keys/2,			% +T, +Keys
%	    rb_keys/3,
	    rb_map/2,
	    rb_map/3,
	    rb_partial_map/4,
	    rb_clone/3,
	    rb_clone/4,
	    rb_min/3,
	    rb_max/3,
	    rb_del_min/4,
	    rb_del_max/4,
	    rb_next/4,
	    rb_previous/4,
	    list_to_rbtree/2,
	    ord_list_to_rbtree/2,
	    is_rbtree/1,
	    rb_size/2,
	    rb_in/3
	  ]).

/** <module> Red black trees

Red-Black trees are balanced search binary trees. They are named because
nodes can be classified as either red or   black. The code we include is
based on "Introduction  to  Algorithms",   second  edition,  by  Cormen,
Leiserson, Rivest and Stein. The library   includes  routines to insert,
lookup and delete elements in the tree.

A Red black tree is represented as a term t(Nil, Tree), where Nil is the
Nil-node, a node shared for each nil-node in  the tree. Any node has the
form colour(Left, Key, Value, Right), where _colour_  is one of =red= or
=black=.

@author Vitor Santos Costa
*/

%:- meta_predicate rb_map(+,:,-), rb_partial_map(+,+,:,-), rb_apply(+,+,:,-).
:- module_transparent
	rb_map/3,
	rb_partial_map/4,
	rb_apply/4.

%%	rb_new(-T) is det.
%
%	Create a new Red-Black tree.
%	
%	@deprecated	Use rb_empty/1.

rb_new(t(Nil,Nil)) :-
	Nil = black([],[],[],[]).

%%	rb_empty(?T) is semidet.
%
%	Succeeds if T is an empty Red-Black tree.

rb_empty(t(Nil,Nil)) :-
	Nil = black([],[],[],[]).

%%	rb_lookup(+Key, -Value, +T) is semidet.
%
%	Backtrack through all elements with  key   Key  in the Red-Black
%	tree T, returning for each the value Value.

rb_lookup(Key, Val, t(_,Tree)) :-
	lookup(Key, Val, Tree).

lookup(_, _, black([],_,_,[])) :- !, fail.
lookup(Key, Val, Tree) :-
	arg(2,Tree,KA),
	compare(Cmp,KA,Key),
	lookup(Cmp,Key,Val,Tree).

lookup(>, K, V, Tree) :-
	arg(1,Tree,NTree),
	lookup(K, V, NTree).
lookup(<, K, V, Tree) :-
	arg(4,Tree,NTree),
	lookup(K, V, NTree).
lookup(=, _, V, Tree) :-
	arg(3,Tree,V).

%%	rb_min(+T, -Key, -Value) is semidet.
%
%	Key is the minimum key in T, and is associated with Val.

rb_min(t(_,Tree), Key, Val) :-
	min(Tree, Key, Val).

min(red(black([],_,_,_),Key,Val,_), Key, Val) :- !.
min(black(black([],_,_,_),Key,Val,_), Key, Val) :- !.
min(red(Right,_,_,_), Key, Val) :-
	min(Right,Key,Val).
min(black(Right,_,_,_), Key, Val) :-
	min(Right,Key,Val).

%%	rb_max(+T, -Key, -Value) is semidet.
%
%	Key is the maximal key in T, and is associated with Val.

rb_max(t(_,Tree), Key, Val) :-
	max(Tree, Key, Val).

max(red(_,Key,Val,black([],_,_,_)), Key, Val) :- !.
max(black(_,Key,Val,black([],_,_,_)), Key, Val) :- !.
max(red(_,_,_,Left), Key, Val) :-
	max(Left,Key,Val).
max(black(_,_,_,Left), Key, Val) :-
	max(Left,Key,Val).

%%	rb_next(+T, +Key, -Next,-Value) is semidet.
%
%	Next is the next element after Key  in T, and is associated with
%	Val.

rb_next(t(_,Tree), Key, Next, Val) :-
	next(Tree, Key, Next, Val, []).

next(black([],_,_,[]), _, _, _, _) :- !, fail.
next(Tree, Key, Next, Val, Candidate) :-
	arg(2,Tree,KA),
	arg(3,Tree,VA),
	compare(Cmp,KA,Key),
	next(Cmp, Key, KA, VA, Next, Val, Tree, Candidate).

next(>, K, KA, VA, NK, V, Tree, _) :-
	arg(1,Tree,NTree),
	next(NTree,K,NK,V,KA-VA).
next(<, K, _, _, NK, V, Tree, Candidate) :-
	arg(4,Tree,NTree),
	next(NTree,K,NK,V,Candidate).
next(=, _, _, _, NK, Val, Tree, Candidate) :-
	arg(4,Tree,NTree),
	(   min(NTree, NK, Val)
	->  true
	;   Candidate = (NK-Val)
	).

%%	rb_previous(+T, +Key, -Previous, -Value) is semidet.
%
%	Previous is the  previous  element  after   Key  in  T,  and  is
%	associated with Val.

rb_previous(t(_,Tree), Key, Previous, Val) :-
	previous(Tree, Key, Previous, Val, []).

previous(black([],_,_,[]), _, _, _, _) :- !, fail.
previous(Tree, Key, Previous, Val, Candidate) :-
	arg(2,Tree,KA),
	arg(3,Tree,VA),
	compare(Cmp,KA,Key),
	previous(Cmp, Key, KA, VA, Previous, Val, Tree, Candidate).

previous(>, K, _, _, NK, V, Tree, Candidate) :-
	arg(1,Tree,NTree),
	previous(NTree,K,NK,V,Candidate).
previous(<, K, KA, VA, NK, V, Tree, _) :-
	arg(4,Tree,NTree),
	previous(NTree,K,NK,V,KA-VA).
previous(=, _, _, _, K, Val, Tree, Candidate) :-
	arg(1,Tree,NTree),
	(   max(NTree, K, Val)
	->  true
	;   Candidate = (K-Val)
	).

%%	rb_update(+T, +Key, +NewVal, -TN) is semidet.
%%	rb_update(+T, +Key, ?OldVal, +NewVal, -TN) is semidet.
%
%	Tree TN is tree T,  but  with   value  for  Key  associated with
%	NewVal.  Fails if it cannot find Key in T.

rb_update(t(Nil,OldTree), Key, Val, t(Nil,NewTree)) :-
	update(OldTree, Key, _, Val, NewTree).
rb_update(t(Nil,OldTree), Key, OldVal, Val, t(Nil,NewTree)) :-
	update(OldTree, Key, OldVal, Val, NewTree).

update(black(Left,Key0,Val0,Right), Key, OldVal, Val, NewTree) :-
	Left \= [],
	compare(Cmp,Key0,Key),
	(   Cmp == (=)
	->  OldVal = Val0,
	    NewTree = black(Left,Key0,Val,Right)
	;   Cmp == (>)
	->  NewTree = black(NewLeft,Key0,Val0,Right),
	    update(Left, Key, OldVal, Val, NewLeft)
	;   NewTree = black(Left,Key0,Val0,NewRight),
	    update(Right, Key, OldVal, Val, NewRight)
	).
update(red(Left,Key0,Val0,Right), Key, OldVal, Val, NewTree) :-
	compare(Cmp,Key0,Key),
	(   Cmp == (=)
	->  OldVal = Val0,
	    NewTree = red(Left,Key0,Val,Right)
	;   Cmp == (>)
	->  NewTree = red(NewLeft,Key0,Val0,Right),
	    update(Left, Key, OldVal, Val, NewLeft)
	;   NewTree = red(Left,Key0,Val0,NewRight),
	    update(Right, Key, OldVal, Val, NewRight)
	).

%%	rb_apply(+T, +Key, :G, -TN) is semidet.
%
%	If the value associated with  key  Key   is  Val0  in  T, and if
%	call(G,Val0,ValF) holds, then TN differs from T only in that Key
%	is associated with value ValF in  tree   TN.  Fails if it cannot
%	find Key in T, or if call(G,Val0,ValF) is not satisfiable.

rb_apply(t(Nil,OldTree), Key, Goal, t(Nil,NewTree)) :-
	apply(OldTree, Key, Goal, NewTree).

%apply(black([],_,_,[]), _, _, _) :- !, fail.
apply(black(Left,Key0,Val0,Right), Key, Goal,
      black(NewLeft,Key0,Val,NewRight)) :-
	Left \= [],
	compare(Cmp,Key0,Key),
	(   Cmp == (=)
	->  NewLeft = Left,
	    NewRight = Right,
	    call(Goal,Val0,Val)
	;   Cmp == (>)
	->  NewRight = Right,
	    Val = Val0,
	    apply(Left, Key, Goal, NewLeft)
	;   NewLeft = Left,
	    Val = Val0,
	    apply(Right, Key, Goal, NewRight)
	).
apply(red(Left,Key0,Val0,Right), Key, Goal,
      red(NewLeft,Key0,Val,NewRight)) :-
	compare(Cmp,Key0,Key),
	(   Cmp == (=)
	->  NewLeft = Left,
	    NewRight = Right,
	    call(Goal,Val0,Val)
	;   Cmp == (>)
	->  NewRight = Right,
	    Val = Val0,
	    apply(Left, Key, Goal, NewLeft)
	;   NewLeft = Left,
	    Val = Val0,
	    apply(Right, Key, Goal, NewRight)
	).

%%	rb_in(?Key, ?Val, +Tree) is nondet.
%
%	True if Key-Val appear in Tree. Uses indexing if Key is bound.

rb_in(Key, Val, t(_,T)) :-
	var(Key), !,
	enum(Key, Val, T).
rb_in(Key, Val, t(_,T)) :-
	lookup(Key, Val, T).


enum(Key, Val, black(L,K,V,R)) :-
	L \= [],
	enum_cases(Key, Val, L, K, V, R).
enum(Key, Val, red(L,K,V,R)) :-
	enum_cases(Key, Val, L, K, V, R).

enum_cases(Key, Val, L, _, _, _) :-
	enum(Key, Val, L).
enum_cases(Key, Val, _, Key, Val, _).
enum_cases(Key, Val, _, _, _, R) :-
	enum(Key, Val, R).


%%	rb_lookupall(+Key, -Value, +T)
%
%	Lookup all elements with  key  Key   in  the  red-black  tree T,
%	returning the value Value.

rb_lookupall(Key, Val, t(_,Tree)) :-
	lookupall(Key, Val, Tree).


lookupall(_, _, black([],_,_,[])) :- !, fail.
lookupall(Key, Val, Tree) :-
	arg(2,Tree,KA),
	compare(Cmp,KA,Key),
	lookupall(Cmp,Key,Val,Tree).

lookupall(>, K, V, Tree) :-
	arg(4,Tree,NTree),
	rb_lookupall(K, V, NTree).
lookupall(=, _, V, Tree) :-
	arg(3,Tree,V).
lookupall(=, K, V, Tree) :-
	arg(1,Tree,NTree),
	lookupall(K, V, NTree).
lookupall(<, K, V, Tree) :-
	arg(1,Tree,NTree),
	lookupall(K, V, NTree).


		 /*******************************
		 *	 TREE INSERTION		*
		 *******************************/

% We don't use parent nodes, so we may have to fix the root.

%%	rb_insert(+T0, +Key, ?Value, -TN)
%
%	Add an element with key Key and Value  to the tree T0 creating a
%	new red-black tree TN. Duplicated elements are not allowed.

rb_insert(t(Nil,Tree0),Key,Val,t(Nil,Tree)) :-
	insert(Tree0,Key,Val,Nil,Tree).


insert(Tree0,Key,Val,Nil,Tree) :-
	insert2(Tree0,Key,Val,Nil,TreeI,_),
	fix_root(TreeI,Tree).

%
% make sure the root is always black.
%
fix_root(black(L,K,V,R),black(L,K,V,R)).
fix_root(red(L,K,V,R),black(L,K,V,R)).


%
% Cormen et al present the algorithm as 
% (1) standard tree insertion;
% (2) from the viewpoint of the newly inserted node:
%     partially fix the tree;
%     move upwards
% until reaching the root.
%
% We do it a little bit different:
%
% (1) standard tree insertion;
% (2) move upwards:
%      when reaching a black node;
%        if the tree below may be broken, fix it.
% We take advantage of Prolog unification
% to do several operations in a single go.
%



%
% actual insertion
%
insert2(black([],[],[],[]), K, V, Nil, T, Status) :- !,
	T = red(Nil,K,V,Nil),
	Status = not_done.
insert2(red(L,K0,V0,R), K, V, Nil, red(NL,K0,V0,NR), Flag) :-
	(   K @< K0
	->  NR = R,
	    insert2(L, K, V, Nil, NL, Flag)
	;   NL = L,
	    insert2(R, K, V, Nil, NR, Flag)
	).
insert2(black(L,K0,V0,R), K, V, Nil, NT, Flag) :-
	(   K @< K0
	->  insert2(L, K, V, Nil, IL, Flag0),
	    fix_left(Flag0, black(IL,K0,V0,R), NT, Flag)
	;   insert2(R, K, V, Nil, IR, Flag0),
	    fix_right(Flag0, black(L,K0,V0,IR), NT, Flag)
	).

%
% How to fix if we have inserted on the left
%
fix_left(done,T,T,done) :- !.
fix_left(not_done,Tmp,Final,Done) :-
	fix_left(Tmp,Final,Done).

%
% case 1 of RB: just need to change colors.
%
fix_left(black(red(Al,AK,AV,red(Be,BK,BV,Ga)),KC,VC,red(De,KD,VD,Ep)),
	red(black(Al,AK,AV,red(Be,BK,BV,Ga)),KC,VC,black(De,KD,VD,Ep)),
	not_done) :- !.
fix_left(black(red(red(Al,KA,VA,Be),KB,VB,Ga),KC,VC,red(De,KD,VD,Ep)),
	red(black(red(Al,KA,VA,Be),KB,VB,Ga),KC,VC,black(De,KD,VD,Ep)),
	not_done) :- !.
%
% case 2 of RB: got a knee so need to do rotations
%
fix_left(black(red(Al,KA,VA,red(Be,KB,VB,Ga)),KC,VC,De),
	black(red(Al,KA,VA,Be),KB,VB,red(Ga,KC,VC,De)),
	done) :- !.
%
% case 3 of RB: got a line
%
fix_left(black(red(red(Al,KA,VA,Be),KB,VB,Ga),KC,VC,De),
	black(red(Al,KA,VA,Be),KB,VB,red(Ga,KC,VC,De)),
	done) :- !.
%
% case 4 of RB: nothig to do
%
fix_left(T,T,done).

%
% How to fix if we have inserted on the right
%
fix_right(done,T,T,done) :- !.
fix_right(not_done,Tmp,Final,Done) :-
	fix_right(Tmp,Final,Done).

%
% case 1 of RB: just need to change colors.
%
fix_right(black(red(Ep,KD,VD,De),KC,VC,red(red(Ga,KB,VB,Be),KA,VA,Al)),
	red(black(Ep,KD,VD,De),KC,VC,black(red(Ga,KB,VB,Be),KA,VA,Al)),
	not_done) :- !.
fix_right(black(red(Ep,KD,VD,De),KC,VC,red(Ga,Ka,Va,red(Be,KB,VB,Al))),
	red(black(Ep,KD,VD,De),KC,VC,black(Ga,Ka,Va,red(Be,KB,VB,Al))),
	not_done) :- !.
%
% case 2 of RB: got a knee so need to do rotations
%
fix_right(black(De,KC,VC,red(red(Ga,KB,VB,Be),KA,VA,Al)),
	black(red(De,KC,VC,Ga),KB,VB,red(Be,KA,VA,Al)),
	done) :- !.
%
% case 3 of RB: got a line
%
fix_right(black(De,KC,VC,red(Ga,KB,VB,red(Be,KA,VA,Al))),
	black(red(De,KC,VC,Ga),KB,VB,red(Be,KA,VA,Al)),
	done) :- !.
%
% case 4 of RB: nothing to do.
%
fix_right(T,T,done).

%%	rb_delete(+T, +Key, -TN).
%%	rb_delete(+T, +Key, -Val, -TN).
%
%	Delete element with key Key from the tree T, returning the value
%	Val associated with the key and a new tree TN.

rb_delete(t(Nil,T), K, t(Nil,NT)) :-
	delete(T, K, _, NT, _).

rb_delete(t(Nil,T), K, V, t(Nil,NT)) :-
	delete(T, K, V, NT, _).

%
% I am afraid our representation is not as nice for delete
%
delete(red(L,K0,V0,R), K, V, NT, Flag) :-
	K @< K0, !,
	delete(L, K, V, NL, Flag0),
	fixup_left(Flag0,red(NL,K0,V0,R),NT, Flag).
delete(red(L,K0,V0,R), K, V, NT, Flag) :-
	K @> K0, !,
	delete(R, K, V, NR, Flag0),
	fixup_right(Flag0,red(L,K0,V0,NR),NT, Flag).
delete(red(L,_,V,R), _, V, OUT, Flag) :-
%	K == K0,
	delete_red_node(L,R,OUT,Flag).
delete(black(L,K0,V0,R), K, V, NT, Flag) :-
	K @< K0, !,
	delete(L, K, V, NL, Flag0),
	fixup_left(Flag0,black(NL,K0,V0,R),NT, Flag).
delete(black(L,K0,V0,R), K, V, NT, Flag) :-
	K @> K0, !,
	delete(R, K, V, NR, Flag0),
	fixup_right(Flag0,black(L,K0,V0,NR),NT, Flag).
delete(black(L,_,V,R), _, V, OUT, Flag) :-
%	K == K0,
	delete_black_node(L,R,OUT,Flag).

%%	rb_del_min(+T, -Key, -Val, -TN)
%
%	Delete the least element from the tree T, returning the key Key,
%	the value Val associated with the key and a new tree TN.

rb_del_min(t(Nil,T), K, Val, t(Nil,NT)) :-
	del_min(T, K, Val, Nil, NT, _).

del_min(red(black([],_,_,_),K,V,R), K, V, Nil, OUT, Flag) :- !,
	delete_red_node(Nil,R,OUT,Flag).
del_min(red(L,K0,V0,R), K, V, Nil, NT, Flag) :-
	del_min(L, K, V, Nil, NL, Flag0),
	fixup_left(Flag0,red(NL,K0,V0,R), NT, Flag).
del_min(black(black([],_,_,_),K,V,R), K, V, Nil, OUT, Flag) :- !,
	delete_black_node(Nil,R,OUT,Flag).
del_min(black(L,K0,V0,R), K, V, Nil, NT, Flag) :-
	del_min(L, K, V, Nil, NL, Flag0),
	fixup_left(Flag0,black(NL,K0,V0,R),NT, Flag).


%%	rb_del_max(+T, -Key, -Val, -TN)
%
%	Delete the largest element from the   tree  T, returning the key
%	Key, the value Val associated with the key and a new tree TN.

rb_del_max(t(Nil,T), K, Val, t(Nil,NT)) :-
	del_max(T, K, Val, Nil, NT, _).

del_max(red(L,K,V,black([],_,_,_)), K, V, Nil, OUT, Flag) :- !,
	delete_red_node(L,Nil,OUT,Flag).
del_max(red(L,K0,V0,R), K, V, Nil, NT, Flag) :-
	del_max(R, K, V, Nil, NR, Flag0),
	fixup_right(Flag0,red(L,K0,V0,NR),NT, Flag).
del_max(black(L,K,V,black([],_,_,_)), K, V, Nil, OUT, Flag) :- !,
	delete_black_node(L,Nil,OUT,Flag).
del_max(black(L,K0,V0,R), K, V, Nil, NT, Flag) :-
	del_max(R, K, V, Nil, NR, Flag0),
	fixup_right(Flag0,black(L,K0,V0,NR), NT, Flag).

delete_red_node(L,L,L,done) :- !.
delete_red_node(black([],[],[],[]),R,R,done) :- !.
delete_red_node(L,black([],[],[],[]),L,done) :- !.
delete_red_node(L,R,OUT,Done) :-
	delete_next(R,NK,NV,NR,Done0),
	fixup_right(Done0,red(L,NK,NV,NR),OUT,Done).

delete_black_node(L,L,L,not_done) :- !.
delete_black_node(black([],[],[],[]),red(L,K,V,R),black(L,K,V,R),done) :- !.
delete_black_node(black([],[],[],[]),R,R,not_done) :- !.
delete_black_node(red(L,K,V,R),black([],[],[],[]),black(L,K,V,R),done) :- !.
delete_black_node(L,black([],[],[],[]),L,not_done) :- !.
delete_black_node(L,R,OUT,Done) :-
	delete_next(R,NK,NV,NR,Done0),
	fixup_right(Done0,black(L,NK,NV,NR),OUT,Done).

delete_next(red(black([],[],[],[]),K,V,R),K,V,R,done) :- !.
delete_next(black(black([],[],[],[]),K,V,red(L1,K1,V1,R1)),
	K,V,black(L1,K1,V1,R1),done) :- !.
delete_next(black(black([],[],[],[]),K,V,R),K,V,R,not_done) :- !.
delete_next(red(L,K,V,R),K0,V0,OUT,Done) :-
	delete_next(L,K0,V0,NL,Done0),
	fixup_left(Done0,red(NL,K,V,R),OUT,Done).
delete_next(black(L,K,V,R),K0,V0,OUT,Done) :-
	delete_next(L,K0,V0,NL,Done0),
	fixup_left(Done0,black(NL,K,V,R),OUT,Done).

fixup_left(done,T,T,done).
fixup_left(not_done,T,NT,Done) :-
	fixup2(T,NT,Done).


%
% case 1: x moves down, so we have to try to fix it again.
% case 1 -> 2,3,4 -> done
%
fixup2(black(black(Al,KA,VA,Be),KB,VB,red(black(Ga,KC,VC,De),KD,VD,black(Ep,KE,VE,Fi))),
	black(T1,KD,VD,black(Ep,KE,VE,Fi)),done) :- !,
	fixup2(red(black(Al,KA,VA,Be),KB,VB,black(Ga,KC,VC,De)),
		T1,
                _).
%
% case 2: x moves up, change one to red
%
fixup2(red(black(Al,KA,VA,Be),KB,VB,black(black(Ga,KC,VC,De),KD,VD,black(Ep,KE,VE,Fi))),
	black(black(Al,KA,VA,Be),KB,VB,red(black(Ga,KC,VC,De),KD,VD,black(Ep,KE,VE,Fi))),done) :- !.
fixup2(black(black(Al,KA,VA,Be),KB,VB,black(black(Ga,KC,VC,De),KD,VD,black(Ep,KE,VE,Fi))),
	black(black(Al,KA,VA,Be),KB,VB,red(black(Ga,KC,VC,De),KD,VD,black(Ep,KE,VE,Fi))),not_done) :- !.
%
% case 3: x stays put, shift left and do a 4
%
fixup2(red(black(Al,KA,VA,Be),KB,VB,black(red(Ga,KC,VC,De),KD,VD,black(Ep,KE,VE,Fi))),
	red(black(black(Al,KA,VA,Be),KB,VB,Ga),KC,VC,black(De,KD,VD,black(Ep,KE,VE,Fi))),
	done) :- !.
fixup2(black(black(Al,KA,VA,Be),KB,VB,black(red(Ga,KC,VC,De),KD,VD,black(Ep,KE,VE,Fi))),
	black(black(black(Al,KA,VA,Be),KB,VB,Ga),KC,VC,black(De,KD,VD,black(Ep,KE,VE,Fi))),
	done) :- !.
%
% case 4: rotate left, get rid of red
%
fixup2(red(black(Al,KA,VA,Be),KB,VB,black(C,KD,VD,red(Ep,KE,VE,Fi))),
	red(black(black(Al,KA,VA,Be),KB,VB,C),KD,VD,black(Ep,KE,VE,Fi)),
	done).
fixup2(black(black(Al,KA,VA,Be),KB,VB,black(C,KD,VD,red(Ep,KE,VE,Fi))),
	black(black(black(Al,KA,VA,Be),KB,VB,C),KD,VD,black(Ep,KE,VE,Fi)),
	done).


fixup_right(done,T,T,done).
fixup_right(not_done,T,NT,Done) :-
	fixup3(T,NT,Done).


%
% case 1: x moves down, so we have to try to fix it again.
% case 1 -> 2,3,4 -> done
%
fixup3(black(red(black(Fi,KE,VE,Ep),KD,VD,black(De,KC,VC,Ga)),KB,VB,black(Be,KA,VA,Al)),
	black(black(Fi,KE,VE,Ep),KD,VD,T1),done) :- !,
        fixup3(red(black(De,KC,VC,Ga),KB,VB,black(Be,KA,VA,Al)),T1,_).

%
% case 2: x moves up, change one to red
%
fixup3(red(black(black(Fi,KE,VE,Ep),KD,VD,black(De,KC,VC,Ga)),KB,VB,black(Be,KA,VA,Al)),
	black(red(black(Fi,KE,VE,Ep),KD,VD,black(De,KC,VC,Ga)),KB,VB,black(Be,KA,VA,Al)),
	done) :- !.
fixup3(black(black(black(Fi,KE,VE,Ep),KD,VD,black(De,KC,VC,Ga)),KB,VB,black(Be,KA,VA,Al)),
	black(red(black(Fi,KE,VE,Ep),KD,VD,black(De,KC,VC,Ga)),KB,VB,black(Be,KA,VA,Al)),
	not_done):- !.
%
% case 3: x stays put, shift left and do a 4
%
fixup3(red(black(black(Fi,KE,VE,Ep),KD,VD,red(De,KC,VC,Ga)),KB,VB,black(Be,KA,VA,Al)),
	red(black(black(Fi,KE,VE,Ep),KD,VD,De),KC,VC,black(Ga,KB,VB,black(Be,KA,VA,Al))),
	done) :- !.
fixup3(black(black(black(Fi,KE,VE,Ep),KD,VD,red(De,KC,VC,Ga)),KB,VB,black(Be,KA,VA,Al)),
	black(black(black(Fi,KE,VE,Ep),KD,VD,De),KC,VC,black(Ga,KB,VB,black(Be,KA,VA,Al))),
	done) :- !.
%
% case 4: rotate right, get rid of red
%
fixup3(red(black(red(Fi,KE,VE,Ep),KD,VD,C),KB,VB,black(Be,KA,VA,Al)),
	red(black(Fi,KE,VE,Ep),KD,VD,black(C,KB,VB,black(Be,KA,VA,Al))),
	done).
fixup3(black(black(red(Fi,KE,VE,Ep),KD,VD,C),KB,VB,black(Be,KA,VA,Al)),
	black(black(Fi,KE,VE,Ep),KD,VD,black(C,KB,VB,black(Be,KA,VA,Al))),
	done).


%
% whole list
%

%%	rb_visit(+T, -Pairs)
%
%	Pairs is an infix visit of tree   T, where each element of Pairs
%	is of the form K-Val.

rb_visit(t(_,T),Lf) :-
	visit(T,[],Lf).

%rb_visit(t(_,T),L0,Lf) :-
%	visit(T,L0,Lf).

visit(black([],_,_,_),L,L) :- !.
visit(red(L,K,V,R),L0,Lf) :-
	visit(L,[K-V|L1],Lf),
	visit(R,L0,L1).
visit(black(L,K,V,R),L0,Lf) :-
	visit(L,[K-V|L1],Lf),
	visit(R,L0,L1).

%%	rb_map(+T, :Goal) is semidet.
%
%	True if call(Goal, Value) is true for all nodes in T.

rb_map(t(_,Tree), Goal) :-
	map(Tree, Goal).

map(black([],[],[],[]),_) :- !.
map(red(L,_,V,R),Goal) :-
	call(Goal,V), !,
	map(L,Goal),
	map(R,Goal).
map(black(L,_,V,R),Goal) :-
	call(Goal,V), !,
	map(L,Goal),
	map(R,Goal).

%%	rb_map(+T, :G, -TN) is semidet.
%
%	For all nodes Key in the tree   T,  if the value associated with
%	key Key is Val0 in tree T,  and if call(G,Val0,ValF) holds, then
%	the  value  associated  with  Key  in   TN  is  ValF.  Fails  if
%	call(G,Val0,ValF) is not satisfiable for all Var0.

rb_map(t(Nil,Tree),Goal,t(Nil,NewTree)) :-
	map(Tree,Goal,NewTree).


map(black([],[],[],[]),_,black([],[],[],[])) :- !.
map(red(L,K,V,R),Goal,red(NL,K,NV,NR)) :-
	call(Goal,V,NV), !,
	map(L,Goal,NL),
	map(R,Goal,NR).
map(black(L,K,V,R),Goal,black(NL,K,NV,NR)) :-
	call(Goal,V,NV), !,
	map(L,Goal,NL),
	map(R,Goal,NR).

%%	rb_clone(+T, -NT, -Pairs)
%
%	"Clone" the red-back tree into a new  tree with the same keys as
%	the original but with all values set to unbound values. Nodes is
%	a list containing all new nodes as pairs K-V.

rb_clone(t(Nil,T),t(Nil,NT),Ns) :-
	clone(T,NT,Ns,[]).

clone(black([],[],[],[]),black([],[],[],[]),Ns,Ns) :- !.
clone(red(L,K,_,R),red(NL,K,NV,NR),NsF,Ns0) :-
	clone(L,NL,NsF,[K-NV|Ns1]),
	clone(R,NR,Ns1,Ns0).
clone(black(L,K,_,R),black(NL,K,NV,NR),NsF,Ns0) :-
	clone(L,NL,NsF,[K-NV|Ns1]),
	clone(R,NR,Ns1,Ns0).

%%	rb_clone(+T, -OldPairs, -NT, -UnboundPairs) is det.
%
%	OldPairs is a key-value list holding  the old association. NT is
%	a new tree where all values are   unbound  and UnboundPairs is a
%	key-value list matching NT.

rb_clone(t(Nil,T),ONs,t(Nil,NT),Ns) :-
	clone(T,ONs,[],NT,Ns,[]).

clone(black([],[],[],[]),ONs,ONs,black([],[],[],[]),Ns,Ns) :- !.
clone(red(L,K,V,R),ONsF,ONs0,red(NL,K,NV,NR),NsF,Ns0) :-
	clone(L,ONsF,[K-V|ONs1],NL,NsF,[K-NV|Ns1]),
	clone(R,ONs1,ONs0,NR,Ns1,Ns0).
clone(black(L,K,V,R),ONsF,ONs0,black(NL,K,NV,NR),NsF,Ns0) :-
	clone(L,ONsF,[K-V|ONs1],NL,NsF,[K-NV|Ns1]),
	clone(R,ONs1,ONs0,NR,Ns1,Ns0).

%%	rb_partial_map(+T, +Keys, :G, -TN)
%
%	For all nodes Key in Keys, if  the value associated with key Key
%	is Val0 in tree T,  and   if  call(G,Val0,ValF)  holds, then the
%	value associated with Key  in  TN  is   ValF.  Fails  if  or  if
%	call(G,Val0,ValF) is not satisfiable for  all Var0. Assumes keys
%	are not repeated.

rb_partial_map(t(Nil,T0), Map, Goal, t(Nil,TF)) :-
	partial_map(T0, Map, [], Nil, Goal, TF).

%rb_partial_map(t(Nil,T0), Map, Map0, Goal, t(Nil,TF)) :-
%	rb_partial_map(T0, Map, Map0, Nil, Goal, TF).

partial_map(T,[],[],_,_,T) :- !.
partial_map(black([],_,_,_),Map,Map,Nil,_,Nil) :- !.
partial_map(red(L,K,V,R),Map,MapF,Nil,Goal,red(NL,K,NV,NR)) :-
	partial_map(L,Map,MapI,Nil,Goal,NL),
	(   MapI == []
	->  NR = R, NV = V
	;   MapI = [K1|MapR],
	    (   K == K1
	    ->  once(call(Goal,V,NV)),
	        Map2 = MapR
	    ;   Map2 = MapI, NV = V
	    ),
	    partial_map(R,Map2,MapF,Nil,Goal,NR)
	).
partial_map(black(L,K,V,R),Map,MapF,Nil,Goal,black(NL,K,NV,NR)) :-
	partial_map(L,Map,MapI,Nil,Goal,NL),
	(   MapI == []
	->  NR = R, NV = V
	;   MapI = [K1|MapR],
	    (   K == K1
	    ->  once(call(Goal,V,NV)),
		Map2 = MapR
	    ;   Map2 = MapI, NV = V
	    ),
	    partial_map(R,Map2,MapF,Nil,Goal,NR)
	).


%
% whole keys
%

%%	rb_keys(+T, -Keys)
%
%	Keys is unified with  an  ordered  list   of  all  keys  in  the
%	Red-Black tree T.

rb_keys(t(_,T),Lf) :-
	keys(T,[],Lf).

%rb_keys(t(_,T),L0,Lf) :-
%	keys(T,L0,Lf).

keys(black([],[],[],[]),L,L) :- !.
keys(red(L,K,_,R),L0,Lf) :-
	keys(L,[K|L1],Lf),
	keys(R,L0,L1).
keys(black(L,K,_,R),L0,Lf) :-
	keys(L,[K|L1],Lf),
	keys(R,L0,L1).

%%	list_to_rbtree(+L, -T) is det.
%
%	T is the red-black tree corresponding to the mapping in list L.

list_to_rbtree(List, t(Nil,Tree)) :-
	Nil = black([], [], [], []),
	sort(List,Sorted),
	Ar =.. [seq|Sorted],
	functor(Ar,_,L),
	construct_rbtree(1, L, Ar, black, Nil, Tree).

%%	ord_list_to_rbtree(+L, -T) is det.
%
%	T is the red-black tree corresponding  to the mapping in ordered
%	list L.

ord_list_to_rbtree(List, t(Nil,Tree)) :-
	Nil = black([], [], [], []),
	Ar =.. [seq|List],
	functor(Ar,_,L),
	construct_rbtree(1, L, Ar, black, Nil, Tree).

construct_rbtree(L, M, _, _, Nil, Nil) :- M < L, !.
construct_rbtree(L, L, Ar, Color, Nil, Node) :- !,
	arg(L, Ar, K-Val),
	build_node(Color, Nil, K, Val, Nil, Node, _).
construct_rbtree(I0, Max, Ar, Color, Nil, Node) :-
	I is (I0+Max)//2,
	arg(I, Ar, K-Val),
	build_node(Color, Left, K, Val, Right, Node, NewColor),
	I1 is I-1,
	construct_rbtree(I0, I1, Ar, NewColor, Nil, Left),
	I2 is I+1,
	construct_rbtree(I2, Max, Ar, NewColor, Nil, Right).

build_node(black, Left, K, Val, Right, black(Left, K, Val, Right), red).
build_node(red, Left, K, Val, Right, red(Left, K, Val, Right), black).

%%	rb_size(+T, -Size) is det.
%
%	Size is the number of elements in T.

rb_size(t(_,T),Size) :-
	size(T,0,Size).

size(black([],_,_,_),Sz,Sz) :- !.
size(red(L,_,_,R),Sz0,Szf) :-
	Sz1 is Sz0+1,
	size(L,Sz1,Sz2),
	size(R,Sz2,Szf).
size(black(L,_,_,R),Sz0,Szf) :-
	Sz1 is Sz0+1,
	size(L,Sz1,Sz2),
	size(R,Sz2,Szf).

%%	is_rbtree(?Term) is semidet.
%
%	True if Term is a valide Red-Black tree.
%	
%	@tbd	Catch variables.

is_rbtree(X) :-
	var(X), !, fail.
is_rbtree(t(Nil,Nil)) :- !.
is_rbtree(t(_,T)) :-
	catch(rbtree1(T), msg(_,_), fail).

%
% This code checks if a tree is ordered and a rbtree
%
%

rbtree1(black(L,K,_,R)) :-
	find_path_blacks(L, 0, Bls),
	check_rbtree(L,-1000000,K,Bls),
	check_rbtree(R,K,1000000,Bls).
rbtree1(red(_,_,_,_)) :-
	throw(msg('root should be black',[])).
	

find_path_blacks(black([],[],[],[]), Bls, Bls) :- !.
find_path_blacks(black(L,_,_,_), Bls0, Bls) :-
	Bls1 is Bls0+1,
	find_path_blacks(L, Bls1, Bls).
find_path_blacks(red(L,_,_,_), Bls0, Bls) :-
	find_path_blacks(L, Bls0, Bls).

check_rbtree(black([],[],[],[]),Min,Max,Bls0) :- !,
	check_height(Bls0,Min,Max).
check_rbtree(red(L,K,_,R),Min,Max,Bls) :-
	check_val(K,Min,Max),
	check_red_child(L),
	check_red_child(R),
	check_rbtree(L,Min,K,Bls),
	check_rbtree(R,K,Max,Bls).
check_rbtree(black(L,K,_,R),Min,Max,Bls0) :-
	check_val(K,Min,Max),
	Bls is Bls0-1,
	check_rbtree(L,Min,K,Bls),
	check_rbtree(R,K,Max,Bls).

check_height(0,_,_) :- !.
check_height(Bls0,Min,Max) :-
	throw(msg('Unbalance ~d between ~w and ~w~n',[Bls0,Min,Max])).

check_val(K, Min, Max) :- K > Min, K < Max, !.
check_val(K, Min, Max) :- 
	throw(msg('not ordered: ~w not between ~w and ~w~n',[K,Min,Max])).

check_red_child(black(_,_,_,_)).
check_red_child(red(_,K,_,_)) :-
	throw(msg('must be red: ~w~n',[K])).


