:- module(n_maplist,[n_maplist/4]).
:- meta_predicate n_maplist(+, :, ?, ?).

% n_maplist(+N,+FNC,?LX,?LY) will do a maplist N times.
n_maplist(0,_,L,L):-!.
n_maplist(N,Pred,LX,LZ):-
	ground(LX),!,
	maplist(Pred,LX,LY),
	NN is N-1,
      n_maplist(NN,Pred,LY,LZ).
n_maplist(N,Pred,LX,LZ):-
	maplist(Pred,LY,LZ),
	NN is N-1,
      n_maplist(NN,Pred,LX,LY).
