%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Author:	Tom Schrijvers
% Email:	Tom.Schrijvers@cs.kuleuven.ac.be
% Copyright:	K.U.Leuven 2004
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- module(builtins,
	[
		negate_b/2,
		entails_b/2,
		binds_b/2
	]).

:- use_module(hprolog).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
negate_b(true,fail).
negate_b(fail,true).
negate_b(X =< Y, Y < X).
negate_b(X > Y, Y >= X).
negate_b(X >= Y, Y > X).
negate_b(X < Y, Y =< X).
negate_b(var(X),nonvar(X)).
negate_b(nonvar(X),var(X)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
entails_b(A,B) :-
	( var(B) ->
		entails(A,B,[A])
	;
		once((
			entails(A,C,[A]),
			B == C
		))
	).

entails(A,A,_).
entails(A,C,History) :-
	entails_(A,B),
	\+ hprolog:memberchk_eq(B,History),
	entails(B,C,[B|History]).		

entails_(X > Y, X >= Y).
entails_(X > Y, Y < X).
entails_(X >= Y, Y =< X).
entails_(X < Y, Y > X).
entails_(X < Y, X =< Y).
entails_(X > Y, X \== Y).
entails_(X \== Y, Y \== X).
entails_(X == Y, Y == X).
entails_(ground(X),nonvar(X)).
entails_(compound(X),nonvar(X)).
entails_(atomic(X),nonvar(X)).
entails_(number(X),nonvar(X)).
entails_(atom(X),nonvar(X)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
binds_b(G,Vars) :-
	binds_(G,L,[]),
	sort(L,Vars).

binds_(var(_),L,L).
binds_(nonvar(_),L,L).
binds_(ground(_),L,L).
binds_(compound(_),L,L).
binds_(number(_),L,L).
binds_(atom(_),L,L).
binds_(atomic(_),L,L).
binds_(integer(_),L,L).
binds_(float(_),L,L).

binds_(_ > _ ,L,L).
binds_(_ < _ ,L,L).
binds_(_ =< _,L,L).
binds_(_ >= _,L,L).
binds_(_ =:= _,L,L).
binds_(_ =\= _,L,L).
binds_(_ == _,L,L).
binds_(_ \== _,L,L).

binds_(X is _,[X|L],L).
binds_((G1,G2),L,T) :-
	binds_(G1,L,R),
	binds_(G2,R,T).
binds_((G1;G2),L,T) :-
	binds_(G1,L,R),
	binds_(G2,R,T).
binds_((G1->G2),L,T) :-
	binds_(G1,L,R),
	binds_(G2,R,T).

binds_(\+ G,L,T) :-
	binds_(G,L,T).
