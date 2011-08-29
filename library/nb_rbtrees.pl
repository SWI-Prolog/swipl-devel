/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2007, University of Amsterdam

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

:- module(nb_rbtrees,
	  [ nb_rb_insert/3,		% !T0, +Key, +Value
	    nb_rb_get_node/3,		% +Tree, +Key, -Node
	    nb_rb_node_value/2,		% +Node, -Value
	    nb_rb_set_node_value/2	% +Node, +Value
	  ]).

/** <module> Non-backtrackable operations on red black trees

This library is  an  extension   to  rbtrees.pl,  implementing Red-black
trees. This library adds  non-backtrackable   destructive  update  to RB
trees which allows us to fill RB trees in a failure driven loop.

This module builds on top of the   rbtrees.pl  and used code copied from
library written by Vitor Santos Costa.

@author	Jan Wielemaker
*/

		 /*******************************
		 *	 TREE INSERTION		*
		 *******************************/

%%	nb_rb_insert(!RBTree, +Key, +Value)
%
%	Add  Key-Value  to  the  tree   RBTree  using  non-backtrackable
%	destructive assignment.

nb_rb_insert(Tree, Key0, Val0) :-
	duplicate_term(Key0, Key),
	duplicate_term(Val0, Val),
	Tree = t(Nil, T),
	insert(T, Key, Val, Nil, NT, Flag),
	(   Flag == shared
	->  true
	;   nb_linkarg(2, Tree, NT)
	).

insert(Tree0,Key,Val,Nil,Tree, Flag) :-
	insert2(Tree0,Key,Val,Nil,TreeI,Flag),
	(   Flag == shared
	->  Tree = Tree0
	;   fix_root(TreeI,Tree)
	).

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
insert2(black('',_,_,''), K, V, Nil, T, Status) :- !,
	T = red(Nil,K,V,Nil),
	Status = not_done.
insert2(In, K, V, Nil, NT, Flag) :-
	In = red(L,K0,V0,R), !,
	(   K @< K0
	->  insert2(L, K, V, Nil, NL, Flag),
	    (	Flag == shared
	    ->	NT = In
	    ;	NT = red(NL,K0,V0,R)
	    )
	;   insert2(R, K, V, Nil, NR, Flag),
	    (	Flag == shared
	    ->	NT = In
	    ;	NT = red(L,K0,V0,NR)
	    )
	).
insert2(In, K, V, Nil, NT, Flag) :-
	In = black(L,K0,V0,R),
	(   K @< K0
	->  insert2(L, K, V, Nil, IL, Flag0),
	    (	Flag0 == shared
	    ->	NT = In
	    ;   fix_left(Flag0, black(IL,K0,V0,R), NT0, Flag1),
		(   Flag1 == share
		->  nb_linkarg(1, In, IL),
		    Flag = shared,
		    NT = In
		;   NT = NT0,
		    Flag = Flag1
		)
	    )
	;   insert2(R, K, V, Nil, IR, Flag0),
	    (	Flag0 == shared
	    ->	NT = In
	    ;	fix_right(Flag0, black(L,K0,V0,IR), NT, Flag1),
		(   Flag1 == share
		->  nb_linkarg(4, In, IR),
		    Flag = shared,
		    NT = In
		;   NT = NT0,
		    Flag = Flag1
		)
	    )
	).

%
% How to fix if we have inserted on the left
%
fix_left(shared,T,T,shared) :- !.
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
fix_left(T,T,share).			% shared?

%
% How to fix if we have inserted on the right
%
fix_right(shared,T,T,shared) :- !.
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
fix_right(T,T,share).


		 /*******************************
		 *	      UPDATE		*
		 *******************************/

%%	nb_rb_get_node(+RBTree, +Key, -Node) is semidet.
%
%	True if Node is the node in   RBTree associated to Key. Fails if
%	Key is not in RBTree. This  predicate   is  intended  to be used
%	together with nb_rb_set_node_value/2 to   update  the associated
%	key destructively.

nb_rb_get_node(t(_Nil, Tree), Key, Node) :-
	find_node(Key, Tree, Node).

find_node(Key, Tree, Node) :-
	arg(2, Tree, K),
	compare(Diff, Key, K),
	find_node(Diff, Key, Tree, Node).

find_node(=, _, Node, Node).
find_node(<, Key, Tree, Node) :-
	arg(1, Tree, Left),
	find_node(Key, Left, Node).
find_node(>, Key, Tree, Node) :-
	arg(4, Tree, Right),
	find_node(Key, Right, Node).

%%	nb_rb_node_value(+Node, -Value) is det.
%
%	Value is the value associated to Node.

nb_rb_node_value(Node, Value) :-
	arg(3, Node, Value).

%%	nb_rb_set_node_value(!Node, +Value) is det.
%
%	Associate Value with Node.

nb_rb_set_node_value(Node, Value) :-
	nb_setarg(3, Node, Value).
