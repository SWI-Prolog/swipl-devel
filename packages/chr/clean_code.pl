%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Author:	Tom Schrijvers
% Email:	Tom.Schrijvers@cs.kuleuven.be
% Copyright:	K.U.Leuven 2004
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%   ____          _         ____ _                  _             
%%  / ___|___   __| | ___   / ___| | ___  __ _ _ __ (_)_ __   __ _ 
%% | |   / _ \ / _` |/ _ \ | |   | |/ _ \/ _` | '_ \| | '_ \ / _` |
%% | |__| (_) | (_| |  __/ | |___| |  __/ (_| | | | | | | | | (_| |
%%  \____\___/ \__,_|\___|  \____|_|\___|\__,_|_| |_|_|_| |_|\__, |
%%                                                           |___/ 
%%
%% removes redundant 'true's and other trivial but potentially non-free constructs

% TODO
%	Remove last clause with Body = fail

:- module(clean_code,
	[
		clean_clauses/2
	]).

:- use_module(hprolog).

clean_clauses([],[]).
clean_clauses([C|Cs],[NC|NCs]) :-
	clean_clause(C,NC),
	clean_clauses(Cs,NCs).

clean_clause(Clause,NClause) :-
	( Clause = (Head :- Body) ->
		clean_goal(Body,Body1),
		move_unification_into_head(Head,Body1,NHead,NBody),
		( NBody == true ->
			NClause = NHead
		;
			NClause = (NHead :- NBody)
		)
	;
		NClause = Clause
	).

clean_goal(Goal,NGoal) :-
	var(Goal), !,
	NGoal = Goal.
clean_goal((G1,G2),NGoal) :-
	!,
	clean_goal(G1,NG1),
	clean_goal(G2,NG2),
	( NG1 == true ->
		NGoal = NG2
	; NG2 == true ->
		NGoal = NG1
	;
		NGoal = (NG1,NG2)
	).
clean_goal((If -> Then ; Else),NGoal) :-
	!,
	clean_goal(If,NIf),
	( NIf == true ->
		clean_goal(Then,NThen),
		NGoal = NThen
	; NIf == fail ->
		clean_goal(Else,NElse),
		NGoal = NElse
	;
		clean_goal(Then,NThen),
		clean_goal(Else,NElse),
		NGoal = (NIf -> NThen; NElse)
	).
clean_goal((G1 ; G2),NGoal) :-
	!,
	clean_goal(G1,NG1),
	clean_goal(G2,NG2),
	( NG1 == fail ->
		NGoal = NG2
	; NG2 == fail ->
		NGoal = NG1
	;
		NGoal = (NG1 ; NG2)
	).
clean_goal(once(G),NGoal) :-
	!,
	clean_goal(G,NG),
	( NG == true ->
		NGoal = true
	; NG == fail ->
		NGoal = fail
	;
		NGoal = once(NG)
	).
clean_goal((G1 -> G2),NGoal) :-
	!,
	clean_goal(G1,NG1),
	( NG1 == true ->
		clean_goal(G2,NGoal)
	; NG1 == fail ->
		NGoal = fail
	;
		clean_goal(G2,NG2),
		NGoal = (NG1 -> NG2)
	).
clean_goal(Goal,Goal).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
move_unification_into_head(Head,Body,NHead,NBody) :-
	conj2list(Body,BodyList),
	move_unification_into_head_(BodyList,Head,NHead,NBody).

move_unification_into_head_([],Head,Head,true).
move_unification_into_head_([G|Gs],Head,NHead,NBody) :-
	( nonvar(G), G = (X = Y) ->
		term_variables(Gs,GsVars),
		( var(X), ( \+ memberchk_eq(X,GsVars) ; atomic(Y)) ->
			X = Y,
			move_unification_into_head_(Gs,Head,NHead,NBody)
		; var(Y), (\+ memberchk_eq(Y,GsVars) ; atomic(X)) ->
			X = Y,
			move_unification_into_head_(Gs,Head,NHead,NBody)
		;
			Head = NHead,
			list2conj([G|Gs],NBody)
		)	
	;
		Head = NHead,
		list2conj([G|Gs],NBody)
	).

% move_unification_into_head(Head,Body,NHead,NBody) :-
% 	( Body = (X = Y, More) ; Body = (X = Y), More = true), !,
% 	( var(X), term_variables(More,MoreVars), \+ memberchk_eq(X,MoreVars) ->
% 		X = Y,
% 		move_unification_into_head(Head,More,NHead,NBody)
% 	; var(Y) ->
% 		move_unification_into_head(Head,(Y = X,More),NHead,NBody)
% 	; 
% 		NHead = Head,
% 		NBody = Body
% 	).
% 
% move_unification_into_head(Head,Body,Head,Body).

		
conj2list(Conj,L) :-				%% transform conjunctions to list
  conj2list(Conj,L,[]).

conj2list(G,L,T) :-
	var(G), !,
	L = [G|T].
conj2list(true,L,L) :- !.
conj2list(Conj,L,T) :-
  Conj = (G1,G2), !,
  conj2list(G1,L,T1),
  conj2list(G2,T1,T).
conj2list(G,[G | T],T).
	
list2conj([],true).
list2conj([G],X) :- !, X = G.
list2conj([G|Gs],C) :-
	( G == true ->				%% remove some redundant trues
		list2conj(Gs,C)
	;
		C = (G,R),
		list2conj(Gs,R)
	).
