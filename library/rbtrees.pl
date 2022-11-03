/*  Part of SWI-Prolog

    Author:        Vitor Santos Costa
    E-mail:        vscosta@gmail.com
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2007-2021, Vitor Santos Costa
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(rbtrees,
          [ rb_new/1,                   % -Tree
            rb_empty/1,                 % ?Tree
            rb_lookup/3,                % +Key, -Value, +Tree
            rb_update/4,                % +Tree, +Key,          ?NewVal, -NewTree
            rb_update/5,                % +Tree, +Key, -OldVal, ?NewVal, -NewTree
            rb_apply/4,                 % +Tree, +Key, :G, -NewTree
            rb_insert/4,                % +Tree, +Key, ?Value, -NewTree
            rb_insert_new/4,            % +Tree, +Key, ?Value, -NewTree
            rb_delete/3,                % +Tree, +Key,       -NewTree
            rb_delete/4,                % +Tree, +Key, -Val, -NewTree
            rb_visit/2,                 % +Tree, -Pairs
            rb_keys/2,                  % +Tree, +Keys
            rb_map/2,                   % +Tree, :Goal
            rb_map/3,                   % +Tree, :Goal, -MappedTree
            rb_partial_map/4,           % +Tree, +Keys, :Goal, -MappedTree
            rb_fold/4,                  % :Goal, +Tree, +State0, -State
            rb_clone/3,                 % +TreeIn, -TreeOut, -Pairs
            rb_min/3,                   % +Tree, -Key, -Value
            rb_max/3,                   % +Tree, -Key, -Value
            rb_del_min/4,               % +Tree, -Key, -Val, -TreeDel
            rb_del_max/4,               % +Tree, -Key, -Val, -TreeDel
            rb_next/4,                  % +Tree, +Key, -Next, -Value
            rb_previous/4,              % +Tree, +Key, -Next, -Value
            list_to_rbtree/2,           % +Pairs, -Tree
            ord_list_to_rbtree/2,       % +Pairs, -Tree
            is_rbtree/1,                % @Tree
            rb_size/2,                  % +Tree, -Size
            rb_in/3                     % ?Key, ?Value, +Tree
          ]).
:- autoload(library(error), [domain_error/2]).

/** <module> Red black trees

Red-Black trees are balanced search binary trees. They are named because
nodes can be classified as either red or   black. The code we include is
based on "Introduction  to  Algorithms",   second  edition,  by  Cormen,
Leiserson, Rivest and Stein. The library   includes  routines to insert,
lookup and delete elements in the tree.

A Red black tree is represented as a term t(Nil, Tree), where Nil is the
Nil-node, a node shared for each nil-node in  the tree. Any node has the
form colour(Left, Key, Value, Right), where _colour_  is one of `red` or
`black`.

__Warning: instantiation of keys__

Red-Black trees depend on  the  Prolog   _standard  order  of  terms_ to
organize the keys as a (balanced)  binary   tree.  This implies that any
term may be used as a key. The   tree may produce wrong results, such as
not being able to find a key, if  the ordering of keys changes after the
key has been inserted into the tree.   The user is responsible to ensure
that variables used as keys or appearing in  a term used as key that may
affect ordering are not  unified,  with   the  exception  of unification
against new fresh variables. For this   reason,  _ground_ terms are safe
keys. When using non-ground terms, either make sure the variables appear
in places that do not affect the   standard order relative to other keys
in the tree or make sure to not unify against these variables as long as
the tree is being used.

@see            library(pairs), library(assoc)
@author Vitor Santos Costa, Jan Wielemaker, Samer Abdallah,
        Peter Ludemann.
@see "Introduction to Algorithms", Second Edition Cormen, Leiserson,
     Rivest, and Stein, MIT Press
*/

% rbtrees.pl is derived from YAP's rbtrees.yap, with some minor editing.
% One difference is that the SWI-Prolog version  assumes that a key only
% appears once in the tree - the   YAP  code is somewhat inconsistent in
% that  (and  even  allows  rb_lookup/3  to    backtrack,  plus  it  has
% rb_lookupall/3, which isn't in the SWI-Prolog code).

% The code has also been modified to   use SWI-Prolog's '=>' operator to
% throw an existence_error(matching_rule, _)  exception   if  Tree isn't
% instantiated (if ':-' is used, an  uninstanted   Tree  gets  set to an
% empty tree, which probably isn't the desired result).

:- meta_predicate
    rb_map(+,2,-),
    rb_map(?,1),
    rb_partial_map(+,+,2,-),
    rb_apply(+,+,2,-),
    rb_fold(3,+,+,-).

/*
:- use_module(library(type_check)).

:- type rbtree(K,V) ---> t(tree(K,V),tree(K,V)).
:- type tree(K,V)   ---> black(tree(K,V),K,V,tree(K,V))
                       ; red(tree(K,V),K,V,tree(K,V))
                       ; ''.
:- type cmp ---> (=) ; (<) ; (>).


:- pred rb_new(rbtree(_K,_V)).
:- pred rb_empty(rbtree(_K,_V)).
:- pred rb_lookup(K,V,rbtree(K,V)).
:- pred lookup(K,V, tree(K,V)).
:- pred lookup(cmp, K, V, tree(K,V)).
:- pred rb_min(rbtree(K,V),K,V).
:- pred min(tree(K,V),K,V).
:- pred rb_max(rbtree(K,V),K,V).
:- pred max(tree(K,V),K,V).
:- pred rb_next(rbtree(K,V),K,pair(K,V),V).
:- pred next(tree(K,V),K,pair(K,V),V,tree(K,V)).
*/

%!  rb_new(-Tree) is det.
%
%   Create a new Red-Black tree Tree.
%
%   @deprecated     Use rb_empty/1.

:- det(rb_new/1).
rb_new(t(Nil,Nil)) :-
    Nil = black('',_,_,'').

%!  rb_empty(?Tree) is semidet.
%
%   Succeeds if Tree is an empty Red-Black tree.

rb_empty(t(Nil,Nil)) :-
    Nil = black('',_,_,'').

%!  rb_lookup(+Key, -Value, +Tree) is semidet.
%
%   True when Value is associated with Key   in the Red-Black tree Tree.
%   The given Key may include variables, in   which  case the RB tree is
%   searched for a key with equivalent   variables  (using (==)/2). Time
%   complexity is O(log N) in the number of elements in the tree.
%
%   @see rb_in/3 for backtracking over keys.

rb_lookup(Key, Val, t(_,Tree)) =>
    lookup(Key, Val, Tree).

lookup(_Key, _Val, black('',_,_,'')) => fail.
lookup(Key, Val, Tree) =>
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

%!  rb_min(+Tree, -Key, -Value) is semidet.
%
%   Key is the minimum key in Tree, and is associated with Val.

rb_min(t(_,Tree), Key, Val) =>
    min(Tree, Key, Val).

min(red(black('',_,_,_),Key0,Val0,_), Key, Val) => Key0=Key, Val0=Val.
min(black(black('',_,_,_),Key0,Val0,_), Key, Val) => Key0=Key, Val0=Val.
min(red(Right,_,_,_), Key, Val) =>
    min(Right,Key,Val).
min(black(Right,_,_,_), Key, Val) =>
    min(Right,Key,Val).
min('', _Key, _Val) => fail.

%!  rb_max(+Tree, -Key, -Value) is semidet.
%
%   Key is the maximal key in Tree, and is associated with Val.

rb_max(t(_,Tree), Key, Val) =>
    max(Tree, Key, Val).

max(red(_,Key0,Val0,black('',_,_,_)), Key, Val) => Key0=Key, Val0=Val.
max(black(_,Key0,Val0,black('',_,_,_)), Key, Val) =>Key0=Key, Val0=Val.
max(red(_,_,_,Left), Key, Val) =>
    max(Left,Key,Val).
max(black(_,_,_,Left), Key, Val) =>
    max(Left,Key,Val).
max('', _Key, _Val) => fail.

%!  rb_next(+Tree, +Key, -Next, -Value) is semidet.
%
%   Next is the next element after Key   in Tree, and is associated with
%   Val. Fails if Key isn't in Tree or if Key is the maximum key.

rb_next(t(_,Tree), Key, Next, Val) =>
    next(Tree, Key, Next, Val, []).

next(black('',_,_,''), _, _, _, _) => fail.
next(Tree, Key, Next, Val, Candidate) =>
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

%!  rb_previous(+Tree, +Key, -Previous, -Value) is semidet.
%
%   Previous  is  the  previous  element  after  Key  in  Tree,  and  is
%   associated with Val. Fails if Key isn't  in   Tree  or if Key is the
%   minimum key.

rb_previous(t(_,Tree), Key, Previous, Val) =>
    previous(Tree, Key, Previous, Val, []).

previous(black('',_,_,''), _, _, _, _) => fail.
previous(Tree, Key, Previous, Val, Candidate) =>
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

%!  rb_update(+Tree, +Key, ?NewVal, -NewTree) is semidet.
%
%   Tree NewTree is tree Tree, but with   value  for Key associated with
%   NewVal. Fails if Key is not in   Tree (using (==)/2). This predicate
%   may fail or give  unexpected  results   if  Key  is not sufficiently
%   instantiated.
%
%   @see rb_in/3 for backtracking over keys.

rb_update(t(Nil,OldTree), Key, OldVal, Val, NewTree2) =>
    NewTree2 = t(Nil,NewTree),
    update(OldTree, Key, OldVal, Val, NewTree).

%!  rb_update(+Tree, +Key, -OldVal, ?NewVal, -NewTree) is semidet.
%
% Same as =|rb_update(Tree, Key, NewVal, NewTree)|= but also unifies
% OldVal with the value associated with Key in Tree.

rb_update(t(Nil,OldTree), Key, Val, NewTree2) =>
    NewTree2 = t(Nil,NewTree),
    update(OldTree, Key, _, Val, NewTree).

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

%!  rb_apply(+Tree, +Key, :G, -NewTree) is semidet.
%
%   If the value associated  with  key  Key   is  Val0  in  Tree, and if
%   call(G,Val0,ValF) holds, then NewTree differs from Tree only in that
%   Key is associated with value  ValF  in   tree  NewTree.  Fails if it
%   cannot find Key in Tree, or if call(G,Val0,ValF) is not satisfiable.

rb_apply(t(Nil,OldTree), Key, Goal, NewTree2) =>
    NewTree2 = t(Nil,NewTree),
    apply(OldTree, Key, Goal, NewTree).

%apply(black('',_,_,''), _, _, _) :- !, fail.
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

%!  rb_in(?Key, ?Value, +Tree) is nondet.
%
%   True when Key-Value is a key-value pair in red-black tree Tree. Same
%   as below, but does not materialize the pairs.
%
%        rb_visit(Tree, Pairs), member(Key-Value, Pairs)
%
%   Leaves a choicepoint  even  if  Key   is  instantiated;  to  avoid a
%   choicepoint, use rb_lookup/3.

rb_in(Key, Val, t(_,T)) =>
    enum(Key, Val, T).

enum(Key, Val, black(L,K,V,R)) =>
    L \= '',
    enum_cases(Key, Val, L, K, V, R).
enum(Key, Val, red(L,K,V,R)) =>
    enum_cases(Key, Val, L, K, V, R).
enum(_Key, _Val, _Tree) => fail.

enum_cases(Key, Val, L, _, _, _) :-
    enum(Key, Val, L).
enum_cases(Key, Val, _, Key, Val, _).
enum_cases(Key, Val, _, _, _, R) :-
    enum(Key, Val, R).



                 /*******************************
                 *       TREE INSERTION         *
                 *******************************/

% We don't use parent nodes, so we may have to fix the root.

%!  rb_insert(+Tree, +Key, ?Value, -NewTree) is det.
%
%   Add an element with key Key and Value   to  the tree Tree creating a
%   new red-black tree NewTree. If Key is  a key in Tree, the associated
%   value is replaced by Value.  See   also  rb_insert_new/4. Does _not_
%   validate that Key is sufficiently instantiated   to  ensure the tree
%   remains valid if a key is further instantiated.

:- det(rb_insert/4).
rb_insert(t(Nil,Tree0),Key,Val,NewTree) =>
    NewTree = t(Nil,Tree),
    insert(Tree0,Key,Val,Nil,Tree).


insert(Tree0,Key,Val,Nil,Tree) :-
    insert2(Tree0,Key,Val,Nil,TreeI,_),
    fix_root(TreeI,Tree).

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
insert2(black('',_,_,''), K, V, Nil, T, Status) =>
    T = red(Nil,K,V,Nil),
    Status = not_done.
insert2(red(L,K0,V0,R), K, V, Nil, NT, Flag) =>
    (   K @< K0
    ->  NT = red(NL,K0,V0,R),
        insert2(L, K, V, Nil, NL, Flag)
    ;   K == K0
    ->  NT = red(L,K0,V,R),
        Flag = done
    ;   NT = red(L,K0,V0,NR),
        insert2(R, K, V, Nil, NR, Flag)
    ).
insert2(black(L,K0,V0,R), K, V, Nil, NT, Flag) =>
    (   K @< K0
    ->  insert2(L, K, V, Nil, IL, Flag0),
        fix_left(Flag0, black(IL,K0,V0,R), NT, Flag)
    ;   K == K0
    ->  NT = black(L,K0,V,R),
        Flag = done
    ;   insert2(R, K, V, Nil, IR, Flag0),
        fix_right(Flag0, black(L,K0,V0,IR), NT, Flag)
    ).

% We don't use parent nodes, so we may have to fix the root.

%!  rb_insert_new(+Tree, +Key, ?Value, -NewTree) is semidet.
%
%   Add a new element with key Key and Value to the tree Tree creating a
%   new red-black tree NewTree. Fails if  Key   is  a  key in Tree. Does
%   _not_ validate that Key is sufficiently   instantiated to ensure the
%   tree remains valid if a key is further instantiated.

rb_insert_new(t(Nil,Tree0),Key,Val,NewTree) =>
    NewTree = t(Nil,Tree),
    insert_new(Tree0,Key,Val,Nil,Tree).

insert_new(Tree0,Key,Val,Nil,Tree) :-
    insert_new_2(Tree0,Key,Val,Nil,TreeI,_),
    fix_root(TreeI,Tree).

%
% actual insertion, copied from insert2
%
insert_new_2(black('',_,_,''), K, V, Nil, T, Status) =>
    T = red(Nil,K,V,Nil),
    Status = not_done.
insert_new_2(red(L,K0,V0,R), K, V, Nil, NT, Flag) =>
    (   K @< K0
    ->  NT = red(NL,K0,V0,R),
        insert_new_2(L, K, V, Nil, NL, Flag)
    ;   K == K0
    ->  fail
    ;   NT = red(L,K0,V0,NR),
        insert_new_2(R, K, V, Nil, NR, Flag)
    ).
insert_new_2(black(L,K0,V0,R), K, V, Nil, NT, Flag) =>
    (   K @< K0
    ->  insert_new_2(L, K, V, Nil, IL, Flag0),
        fix_left(Flag0, black(IL,K0,V0,R), NT, Flag)
    ;   K == K0
    ->  fail
    ;   insert_new_2(R, K, V, Nil, IR, Flag0),
        fix_right(Flag0, black(L,K0,V0,IR), NT, Flag)
    ).

%
% make sure the root is always black.
%
:- det(fix_root/2).
fix_root(black(L,K,V,R), Root) => Root = black(L,K,V,R).
fix_root(red(L,K,V,R), Root) => Root = black(L,K,V,R).

%
% How to fix if we have inserted on the left
%
:- det(fix_left/4).
fix_left(done,T0,T,Done) => T = T0, Done = done.
fix_left(not_done,Tmp,Final,Done) =>
    fix_left(Tmp,Final,Done).

:- det(fix_left/3).
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
% case 4 of RB: nothing to do
%
fix_left(T,T,done).

%
% How to fix if we have inserted on the right
%
:- det(fix_right/4).
fix_right(done,T0,T,Done) => T0 = T, Done = done.
fix_right(not_done,Tmp,Final,Done) =>
    fix_right(Tmp,Final,Done).

:- det(fix_right/3).
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


%!  rb_delete(+Tree, +Key, -NewTree).
%
%   Delete element with key Key from the  tree Tree, returning the value
%   Val associated with the key and a new  tree NewTree. Fails if Key is
%   not in Tree  (using  (==)/2).
%
%   @see rb_in/3 for backtracking over keys.

rb_delete(t(Nil,T), K, NewTree) =>
    NewTree = t(Nil,NT),
    delete(T, K, _, NT, _).

%!  rb_delete(+Tree, +Key, -Val, -NewTree).
%
%   Same as rb_delete(Tree, Key, NewTree), but also unifies Val with the
%   value associated with Key in Tree.

rb_delete(t(Nil,T), K, V, NewTree) =>
    NewTree = t(Nil,NT),
    delete(T, K, V0, NT, _),
    V = V0.

%
% I am afraid our representation is not as nice for delete
%
delete(red(L,K0,V0,R), K, V, NT, Flag) =>
    delete_red(L,K0,V0,R, K, V, NT, Flag).
delete(black(L,K0,V0,R), K, V, NT, Flag) =>
    delete_black(L,K0,V0,R, K, V, NT, Flag).
delete('', _K, _V, _NT, _Flag) =>
    fail.

delete_red(L,K0,V0,R, K, V, NT, Flag), K @< K0 =>
    delete(L, K, V, NL, Flag0),
    fixup_left(Flag0,red(NL,K0,V0,R),NT, Flag).
delete_red(L,K0,V0,R, K, V, NT, Flag), K @> K0 =>
    delete(R, K, V, NR, Flag0),
    fixup_right(Flag0,red(L,K0,V0,NR),NT, Flag).
delete_red(L,_,V0,R, _, V, Out, Flag) => % K == K0,
    V0 = V,
    delete_red_node(L,R,Out,Flag).

delete_black(L,K0,V0,R, K, V, NT, Flag), K @< K0 =>
    delete(L, K, V, NL, Flag0),
    fixup_left(Flag0,black(NL,K0,V0,R),NT, Flag).
delete_black(L,K0,V0,R, K, V, NT, Flag), K @> K0 =>
    delete(R, K, V, NR, Flag0),
    fixup_right(Flag0,black(L,K0,V0,NR),NT, Flag).
delete_black(L,_,V0,R, _, V, Out, Flag) => % K == K0,
    V0 = V,
    delete_black_node(L,R,Out,Flag).

%!  rb_del_min(+Tree, -Key, -Val, -NewTree)
%
%   Delete the least element from the tree  Tree, returning the key Key,
%   the value Val associated with the key  and a new tree NewTree. Fails
%   if Tree is empty.

rb_del_min(t(Nil,T), K, Val, NewTree) =>
    NewTree = t(Nil,NT),
    del_min(T, K, Val, Nil, NT, _).

del_min(red(black('',_,_,_),K,V,R), K, V, Nil, Out, Flag) :-
    !,
    delete_red_node(Nil,R,Out,Flag).
del_min(red(L,K0,V0,R), K, V, Nil, NT, Flag) :-
    del_min(L, K, V, Nil, NL, Flag0),
    fixup_left(Flag0,red(NL,K0,V0,R), NT, Flag).
del_min(black(black('',_,_,_),K,V,R), K, V, Nil, Out, Flag) :-
    !,
    delete_black_node(Nil,R,Out,Flag).
del_min(black(L,K0,V0,R), K, V, Nil, NT, Flag) :-
    del_min(L, K, V, Nil, NL, Flag0),
    fixup_left(Flag0,black(NL,K0,V0,R),NT, Flag).


%!  rb_del_max(+Tree, -Key, -Val, -NewTree)
%
%   Delete the largest element from  the   tree  Tree, returning the key
%   Key, the value Val associated with the   key and a new tree NewTree.
%   Fails if Tree is empty.

rb_del_max(t(Nil,T), K, Val, NewTree) =>
    NewTree = t(Nil,NT),
    del_max(T, K, Val, Nil, NT, _).

del_max(red(L,K,V,black('',_,_,_)), K, V, Nil, Out, Flag) :-
    !,
    delete_red_node(L,Nil,Out,Flag).
del_max(red(L,K0,V0,R), K, V, Nil, NT, Flag) :-
    del_max(R, K, V, Nil, NR, Flag0),
    fixup_right(Flag0,red(L,K0,V0,NR),NT, Flag).
del_max(black(L,K,V,black('',_,_,_)), K, V, Nil, Out, Flag) :-
    !,
    delete_black_node(L,Nil,Out,Flag).
del_max(black(L,K0,V0,R), K, V, Nil, NT, Flag) :-
    del_max(R, K, V, Nil, NR, Flag0),
    fixup_right(Flag0,black(L,K0,V0,NR), NT, Flag).

delete_red_node(L1,L2,L1,done) :- L1 == L2, !.
delete_red_node(black('',_,_,''),R,R,done) :-  !.
delete_red_node(L,black('',_,_,''),L,done) :-  !.
delete_red_node(L,R,Out,Done) :-
    delete_next(R,NK,NV,NR,Done0),
    fixup_right(Done0,red(L,NK,NV,NR),Out,Done).

delete_black_node(L1,L2,L1,not_done) :-         L1 == L2, !.
delete_black_node(black('',_,_,''),red(L,K,V,R),black(L,K,V,R),done) :- !.
delete_black_node(black('',_,_,''),R,R,not_done) :- !.
delete_black_node(red(L,K,V,R),black('',_,_,''),black(L,K,V,R),done) :- !.
delete_black_node(L,black('',_,_,''),L,not_done) :- !.
delete_black_node(L,R,Out,Done) :-
    delete_next(R,NK,NV,NR,Done0),
    fixup_right(Done0,black(L,NK,NV,NR),Out,Done).

delete_next(red(black('',_,_,''),K,V,R),K,V,R,done) :-  !.
delete_next(black(black('',_,_,''),K,V,red(L1,K1,V1,R1)),
        K,V,black(L1,K1,V1,R1),done) :- !.
delete_next(black(black('',_,_,''),K,V,R),K,V,R,not_done) :- !.
delete_next(red(L,K,V,R),K0,V0,Out,Done) :-
    delete_next(L,K0,V0,NL,Done0),
    fixup_left(Done0,red(NL,K,V,R),Out,Done).
delete_next(black(L,K,V,R),K0,V0,Out,Done) :-
    delete_next(L,K0,V0,NL,Done0),
    fixup_left(Done0,black(NL,K,V,R),Out,Done).

fixup_left(done,T,T,done).
fixup_left(not_done,T,NT,Done) :-
    fixup2(T,NT,Done).

%
% case 1: x moves down, so we have to try to fix it again.
% case 1 -> 2,3,4 -> done
%
fixup2(black(black(Al,KA,VA,Be),KB,VB,
             red(black(Ga,KC,VC,De),KD,VD,
                 black(Ep,KE,VE,Fi))),
        black(T1,KD,VD,black(Ep,KE,VE,Fi)),done) :-
    !,
    fixup2(red(black(Al,KA,VA,Be),KB,VB,black(Ga,KC,VC,De)),
            T1,
            _).
%
% case 2: x moves up, change one to red
%
fixup2(red(black(Al,KA,VA,Be),KB,VB,
           black(black(Ga,KC,VC,De),KD,VD,
                 black(Ep,KE,VE,Fi))),
        black(black(Al,KA,VA,Be),KB,VB,
              red(black(Ga,KC,VC,De),KD,VD,
                  black(Ep,KE,VE,Fi))),done) :- !.
fixup2(black(black(Al,KA,VA,Be),KB,VB,
             black(black(Ga,KC,VC,De),KD,VD,
                   black(Ep,KE,VE,Fi))),
        black(black(Al,KA,VA,Be),KB,VB,
              red(black(Ga,KC,VC,De),KD,VD,
                  black(Ep,KE,VE,Fi))),not_done) :- !.
%
% case 3: x stays put, shift left and do a 4
%
fixup2(red(black(Al,KA,VA,Be),KB,VB,
           black(red(Ga,KC,VC,De),KD,VD,
                 black(Ep,KE,VE,Fi))),
        red(black(black(Al,KA,VA,Be),KB,VB,Ga),KC,VC,
            black(De,KD,VD,black(Ep,KE,VE,Fi))),
        done) :- !.
fixup2(black(black(Al,KA,VA,Be),KB,VB,
             black(red(Ga,KC,VC,De),KD,VD,
                   black(Ep,KE,VE,Fi))),
        black(black(black(Al,KA,VA,Be),KB,VB,Ga),KC,VC,
              black(De,KD,VD,black(Ep,KE,VE,Fi))),
        done) :- !.
%
% case 4: rotate left, get rid of red
%
fixup2(red(black(Al,KA,VA,Be),KB,VB,
           black(C,KD,VD,red(Ep,KE,VE,Fi))),
        red(black(black(Al,KA,VA,Be),KB,VB,C),KD,VD,
            black(Ep,KE,VE,Fi)),
        done).
fixup2(black(black(Al,KA,VA,Be),KB,VB,
             black(C,KD,VD,red(Ep,KE,VE,Fi))),
       black(black(black(Al,KA,VA,Be),KB,VB,C),KD,VD,
             black(Ep,KE,VE,Fi)),
       done).

fixup_right(done,T,T,done).
fixup_right(not_done,T,NT,Done) :-
    fixup3(T,NT,Done).

% case 1: x moves down, so we have to try to fix it again.
% case 1 -> 2,3,4 -> done
%
fixup3(black(red(black(Fi,KE,VE,Ep),KD,VD,
                 black(De,KC,VC,Ga)),KB,VB,
             black(Be,KA,VA,Al)),
        black(black(Fi,KE,VE,Ep),KD,VD,T1),done) :-
    !,
    fixup3(red(black(De,KC,VC,Ga),KB,VB,
               black(Be,KA,VA,Al)),T1,_).

%
% case 2: x moves up, change one to red
%
fixup3(red(black(black(Fi,KE,VE,Ep),KD,VD,
                 black(De,KC,VC,Ga)),KB,VB,
           black(Be,KA,VA,Al)),
       black(red(black(Fi,KE,VE,Ep),KD,VD,
                 black(De,KC,VC,Ga)),KB,VB,
             black(Be,KA,VA,Al)),
       done) :- !.
fixup3(black(black(black(Fi,KE,VE,Ep),KD,VD,
                   black(De,KC,VC,Ga)),KB,VB,
             black(Be,KA,VA,Al)),
       black(red(black(Fi,KE,VE,Ep),KD,VD,
                 black(De,KC,VC,Ga)),KB,VB,
             black(Be,KA,VA,Al)),
       not_done):- !.
%
% case 3: x stays put, shift left and do a 4
%
fixup3(red(black(black(Fi,KE,VE,Ep),KD,VD,
                 red(De,KC,VC,Ga)),KB,VB,
           black(Be,KA,VA,Al)),
       red(black(black(Fi,KE,VE,Ep),KD,VD,De),KC,VC,
           black(Ga,KB,VB,black(Be,KA,VA,Al))),
       done) :- !.
fixup3(black(black(black(Fi,KE,VE,Ep),KD,VD,
                   red(De,KC,VC,Ga)),KB,VB,
             black(Be,KA,VA,Al)),
       black(black(black(Fi,KE,VE,Ep),KD,VD,De),KC,VC,
             black(Ga,KB,VB,black(Be,KA,VA,Al))),
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

%!  rb_visit(+Tree, -Pairs) is det.
%
%   Pairs is an infix visit of tree Tree, where each element of Pairs is
%   of the form Key-Value.

:- det(rb_visit/2).
rb_visit(t(_,T),Lf) =>
    visit(T,[],Lf).

visit(black('',_,_,_),L0,L) => L0 = L.
visit(red(L,K,V,R),L0,Lf) =>
    visit(L,[K-V|L1],Lf),
    visit(R,L0,L1).
visit(black(L,K,V,R),L0,Lf) =>
    visit(L,[K-V|L1],Lf),
    visit(R,L0,L1).

:- meta_predicate map(?,2,?,?).  % this is required.

%!  rb_map(+T, :Goal) is semidet.
%
%   True if call(Goal, Value) is true for all nodes in T.

rb_map(t(Nil,Tree),Goal,NewTree2) =>
    NewTree2 = t(Nil,NewTree),
    map(Tree,Goal,NewTree,Nil).


map(black('',_,_,''),_,Nil0,Nil) => Nil0 = Nil.
map(red(L,K,V,R),Goal,NewTree,Nil) =>
    NewTree = red(NL,K,NV,NR),
    call(Goal,V,NV),
    map(L,Goal,NL,Nil),
    map(R,Goal,NR,Nil).
map(black(L,K,V,R),Goal,NewTree,Nil) =>
    NewTree = black(NL,K,NV,NR),
    call(Goal,V,NV),
    map(L,Goal,NL,Nil),
    map(R,Goal,NR,Nil).

:- meta_predicate map(?,1).  % this is required.

%!  rb_map(+Tree, :G, -NewTree) is semidet.
%
%   For all nodes Key in the tree Tree, if the value associated with key
%   Key is Val0 in tree Tree, and   if call(G,Val0,ValF) holds, then the
%   value  associated  with  Key  in   NewTree    is   ValF.   Fails  if
%   call(G,Val0,ValF)  is  not  satisfiable  for  all   Val0.  If  G  is
%   non-deterministic, rb_map/3 will backtrack over  all possible values
%   from call(G,Val0,ValF). You should not depend   on the order of tree
%   traversal (currently: key order).

rb_map(t(_,Tree),Goal) =>
    map(Tree,Goal).


map(black('',_,_,''),_) => true.
map(red(L,_,V,R),Goal) =>
    call(Goal,V),
    map(L,Goal),
    map(R,Goal).
map(black(L,_,V,R),Goal) =>
    call(Goal,V),
    map(L,Goal),
    map(R,Goal).

%!  rb_fold(:Goal, +Tree, +State0, -State).
%
%   Fold the given predicate  over  all   the  key-value  pairs in Tree,
%   starting with initial state State0  and   returning  the final state
%   State. Pred is called as
%
%       call(Pred, Key-Value, State1, State2)
%
%   Determinism depends on Goal.

rb_fold(Pred, t(_,T), S1, S2) =>
    fold(T, Pred, S1, S2).

fold(black(L,K,V,R), Pred) -->
    (   {L == ''}
    ->  []
    ;   fold_parts(Pred, L, K-V, R)
    ).
fold(red(L,K,V,R), Pred) -->
    fold_parts(Pred, L, K-V, R).

fold_parts(Pred, L, KV, R) -->
    fold(L, Pred),
    call(Pred, KV),
    fold(R, Pred).

%!  rb_clone(+TreeIn, -TreeOut, -Pairs) is det.
%
%   `Clone' the red-back tree TreeIn into a   new  tree TreeOut with the
%   same keys as the original but with all values set to unbound values.
%   Pairs is a list containing all new nodes as pairs K-V.

:- det(rb_clone/3).
rb_clone(t(Nil,T),TreeOut,Ns) =>
    TreeOut = t(Nil,NT),
    clone(T,Nil,NT,Ns,[]).

clone(black('',_,_,''),Nil0,Nil,Ns0,Ns) => Nil0=Nil, Ns0=Ns.
clone(red(L,K,_,R),Nil,TreeOut,NsF,Ns0) =>
    TreeOut = red(NL,K,NV,NR),
    clone(L,Nil,NL,NsF,[K-NV|Ns1]),
    clone(R,Nil,NR,Ns1,Ns0).
clone(black(L,K,_,R),Nil,TreeOut,NsF,Ns0) =>
    TreeOut = black(NL,K,NV,NR),
    clone(L,Nil,NL,NsF,[K-NV|Ns1]),
    clone(R,Nil,NR,Ns1,Ns0).

%!  rb_partial_map(+Tree, +Keys, :G, -NewTree)
%
%   For all nodes Key in Keys, if the   value associated with key Key is
%   Val0 in tree Tree, and if   call(G,Val0,ValF)  holds, then the value
%   associated with Key in NewTree is ValF,   otherwise  it is the value
%   associated with the key in Tree. Fails if   Key  isn't in Tree or if
%   call(G,Val0,ValF) is not satisfiable for all   Val0 in Keys. Assumes
%   keys are sorted and not repeated (fails if this is not true).

rb_partial_map(t(Nil,T0), Map, Goal, NewTree) =>
    NewTree = t(Nil,TF),
    partial_map(T0, Map, [], Nil, Goal, TF).

partial_map(T,[],[],_,_,T) :- !.
partial_map(black('',_,_,_),Map,Map,Nil,_,Nil) :- !.
partial_map(red(L,K,V,R),Map,MapF,Nil,Goal,red(NL,K,NV,NR)) :-
    partial_map(L,Map,MapI,Nil,Goal,NL),
    (   MapI == []
    ->  NR = R, NV = V, MapF = []
    ;   MapI = [K1|MapR],
        (   K == K1
        ->  (   call(Goal,V,NV)
            ->  true
            ;   NV = V
            ),
            MapN = MapR
        ;   NV = V,
            MapN = MapI
        ),
        partial_map(R,MapN,MapF,Nil,Goal,NR)
    ).
partial_map(black(L,K,V,R),Map,MapF,Nil,Goal,black(NL,K,NV,NR)) :-
    partial_map(L,Map,MapI,Nil,Goal,NL),
    (   MapI == []
    ->  NR = R, NV = V, MapF = []
    ;   MapI = [K1|MapR],
        (   K == K1
        ->  (   call(Goal,V,NV)
            ->  true
            ;   NV = V
            ),
            MapN = MapR
        ;   NV = V,
            MapN = MapI
        ),
        partial_map(R,MapN,MapF,Nil,Goal,NR)
    ).


%!  rb_keys(+Tree, -Keys) is det.
%
%   Keys is unified with an ordered list   of  all keys in the Red-Black
%   tree Tree.

:- det(rb_keys/2).
rb_keys(t(_,T),Lf) =>
    keys(T,[],Lf).

keys(black('',_,_,''),L0,L) => L0 = L.
keys(red(L,K,_,R),L0,Lf) =>
    keys(L,[K|L1],Lf),
    keys(R,L0,L1).
keys(black(L,K,_,R),L0,Lf) =>
    keys(L,[K|L1],Lf),
    keys(R,L0,L1).


%!  list_to_rbtree(+List, -Tree) is det.
%
%   Tree is the red-black tree  corresponding   to  the mapping in List,
%   which should be a list of Key-Value   pairs. List should not contain
%   more than one entry for each distinct key, but this is not validated
%   by list_to_rbtree/2.

:- det(list_to_rbtree/2).
list_to_rbtree(List, T) :-
    sort(List,Sorted),
    ord_list_to_rbtree(Sorted, T).

%!  ord_list_to_rbtree(+List, -Tree) is det.
%
%   Tree is the red-black tree  corresponding   to  the  mapping in list
%   List, which should be a list  of   Key-Value  pairs. List should not
%   contain more than one entry for each   distinct key, but this is not
%   validated by ord_list_to_rbtree/2. List is assumed
%   to be sorted according to the standard order of terms.

:- det(ord_list_to_rbtree/2).
ord_list_to_rbtree([], Tree) =>
    Tree = t(Nil,Nil),
    Nil = black('', _, _, '').
ord_list_to_rbtree([K-V], Tree) =>
    Tree = t(Nil,black(Nil,K,V,Nil)),
    Nil = black('', _, _, '').
ord_list_to_rbtree(List, Tree2) =>
    Tree2 = t(Nil,Tree),
    Nil = black('', _, _, ''),
    Ar =.. [seq|List],
    functor(Ar,_,L),
    Height is truncate(log(L)/log(2)),
    construct_rbtree(1, L, Ar, Height, Nil, Tree).

construct_rbtree(L, M, _, _, Nil, Nil) :- M < L, !.
construct_rbtree(L, L, Ar, Depth, Nil, Node) :-
    !,
    arg(L, Ar, K-Val),
    build_node(Depth, Nil, K, Val, Nil, Node).
construct_rbtree(I0, Max, Ar, Depth, Nil, Node) :-
    I is (I0+Max)//2,
    arg(I, Ar, K-Val),
    build_node(Depth, Left, K, Val, Right, Node),
    I1 is I-1,
    NewDepth is Depth-1,
    construct_rbtree(I0, I1, Ar, NewDepth, Nil, Left),
    I2 is I+1,
    construct_rbtree(I2, Max, Ar, NewDepth, Nil, Right).

build_node( 0, Left, K, Val, Right, red(Left, K, Val, Right)) :- !.
build_node( _, Left, K, Val, Right, black(Left, K, Val, Right)).


%!  rb_size(+Tree, -Size) is det.
%
%   Size is the number of elements in Tree.

:- det(rb_size/2).
rb_size(t(_,T),Size) =>
    size(T,0,Size).

size(black('',_,_,_),Sz,Sz) :- !.
size(red(L,_,_,R),Sz0,Szf) :-
    Sz1 is Sz0+1,
    size(L,Sz1,Sz2),
    size(R,Sz2,Szf).
size(black(L,_,_,R),Sz0,Szf) :-
    Sz1 is Sz0+1,
    size(L,Sz1,Sz2),
    size(R,Sz2,Szf).

%!  is_rbtree(@Term) is semidet.
%
%   True if Term is a valid Red-Black   tree. Processes the entire tree,
%   checking the coloring of the nodes, the  balance and the ordering of
%   keys.    Does _not_ validate that keys are sufficiently instantiated
%   to ensure the tree remains valid if a key is further instantiated.

is_rbtree(X), var(X) =>
    fail.
is_rbtree(t(Nil,Nil)) => true.
is_rbtree(t(_,T)) =>
    Err = error(_,_),
    catch(check_rbtree(T), Err, is_rbtree_error(Err)).
is_rbtree(_) =>
    fail.

is_rbtree_error(Err), Err = error(resource_error(_),_) => throw(Err).
is_rbtree_error(_) => fail.

% This code checks if a tree is ordered and a rbtree

check_rbtree(black(L,K,_,R)) =>
    find_path_blacks(L, 0, Bls),
    check_rbtree(L,-inf,K,Bls),
    check_rbtree(R,K,+inf,Bls).
check_rbtree(Node), Node = red(_,_,_,_) =>
    domain_error(rb_black, Node).


find_path_blacks(black('',_,_,''), Bls0, Bls) => Bls = Bls0.
find_path_blacks(black(L,_,_,_), Bls0, Bls) =>
    Bls1 is Bls0+1,
    find_path_blacks(L, Bls1, Bls).
find_path_blacks(red(L,_,_,_), Bls0, Bls) =>
    find_path_blacks(L, Bls0, Bls).

check_rbtree(black('',_,_,''),Min,Max,Bls0) =>
    check_height(Bls0,Min,Max).
check_rbtree(red(L,K,_,R),Min,Max,Bls) =>
    check_val(K,Min,Max),
    check_red_child(L),
    check_red_child(R),
    check_rbtree(L,Min,K,Bls),
    check_rbtree(R,K,Max,Bls).
check_rbtree(black(L,K,_,R),Min,Max,Bls0) =>
    check_val(K,Min,Max),
    Bls is Bls0-1,
    check_rbtree(L,Min,K,Bls),
    check_rbtree(R,K,Max,Bls).

check_height(0,_,_) => true.
check_height(Bls0,Min,Max) =>
    throw(error(rbtree(balance(Bls0, Min, Max)), _)).

check_val(K, Min, Max), (K @> Min ; Min == -inf), (K @< Max ; Max == +inf) =>
    true.
check_val(K, Min, Max) =>
    throw(error(rbtree(order(K, Min, Max)), _)).

check_red_child(black(_,_,_,_)) => true.
check_red_child(Node), Node = red(_,_,_,_) =>
    domain_error(rb_black, Node).


		 /*******************************
		 *            MESSAGES		*
		 *******************************/

:- multifile
    prolog:error_message//1.

prolog:error_message(rbtree(balance(Bls0, Min, Max))) -->
    [ 'Unbalance ~d between ~w and ~w'-[Bls0,Min,Max] ].
prolog:error_message(rbtree(order(K, Min, Max))) -->
    [ 'not ordered: ~w not between ~w and ~w'-[K,Min,Max] ].
