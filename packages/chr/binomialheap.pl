%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Binomial Heap imlementation based on
%	
%	Functional Binomial Queues
%	James F. King
%	University of Glasgow
%
% Author:	Tom Schrijvers
% Email:	Tom.Schrijvers@cs.kuleuven.ac.be
% Copyright:	K.U.Leuven 2004
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(binomialheap,
	[
		empty_q/1,
		insert_q/3,
		insert_list_q/3,
		delete_min_q/3,
		find_min_q/2
	]).	

:- use_module(library(lists)).

% data Tree a = Node a [Tree a]
% type BinQueue a = [Maybe (Tree a)]
% data Maybe a = Zero | One a
% type Item = (Entry,Key)

entry(Entry-_,Entry).
key(_-Key,Key).

empty_q([]).

meld_q(P,Q,R) :-
	meld_qc(P,Q,zero,R).

meld_qc([],Q,zero,Q) :- !.
meld_qc([],Q,C,R) :- !,
	meld_q(Q,[C],R).
meld_qc(P,[],C,R) :- !,
	meld_qc([],P,C,R).
meld_qc([zero|Ps],[zero|Qs],C,R) :- !,
	R = [C | Rs],
	meld_q(Ps,Qs,Rs).
meld_qc([one(node(X,Xs))|Ps],[one(node(Y,Ys))|Qs],C,R) :- !,
	key(X,KX),
	key(Y,KY),
	( KX < KY ->
		T = node(X,[node(Y,Ys)|Xs])
	;
		T = node(Y,[node(X,Xs)|Ys])
	),
	R = [C|Rs],
	meld_qc(Ps,Qs,one(T),Rs).
meld_qc([P|Ps],[Q|Qs],C,Rs) :-
	meld_qc([Q|Ps],[C|Qs],P,Rs).

insert_q(Q,I,NQ) :-
	meld_q([one(node(I,[]))],Q,NQ).

insert_list_q([],Q,Q).
insert_list_q([I|Is],Q,NQ) :-
	insert_q(Q,I,Q1),
	insert_list_q(Is,Q1,NQ).

min_tree([T|Ts],MT) :-
	min_tree_acc(Ts,T,MT).

min_tree_acc([],MT,MT).
min_tree_acc([T|Ts],Acc,MT) :-
	least(T,Acc,NAcc),
	min_tree_acc(Ts,NAcc,MT).

least(zero,T,T) :- !.
least(T,zero,T) :- !.
least(one(node(X,Xs)),one(node(Y,Ys)),T) :-
	key(X,KX),
	key(Y,KY),
	( KX < KY ->
		T = one(node(X,Xs))
	;
		T = one(node(Y,Ys))
	).		

remove_tree([],_,[]).
remove_tree([T|Ts],I,[NT|NTs]) :-
	( T == zero ->
		NT = T
	;
		T = one(node(X,_)),
		( X == I ->
			NT = zero
		;
			NT = T
		)
	),
	remove_tree(Ts,I,NTs).

delete_min_q(Q,NQ,Min) :-
	min_tree(Q,one(node(Min,Ts))),
	remove_tree(Q,Min,Q1),
	reverse(Ts,RTs),
	make_ones(RTs,Q2),
	meld_q(Q2,Q1,NQ).	

make_ones([],[]).
make_ones([N|Ns],[one(N)|RQ]) :-
	make_ones(Ns,RQ).

find_min_q(Q,I) :-
	min_tree(Q,one(node(I,_))).


