%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Author:	Tom Schrijvers
% Email:	Tom.Schrijvers@cs.kuleuven.ac.be
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


:- module(clean_code,
	[
		clean_clauses/2
	]).

clean_clauses([],[]).
clean_clauses([C|Cs],[NC|NCs]) :-
	clean_clause(C,NC),
	clean_clauses(Cs,NCs).

clean_clause(Clause,NClause) :-
	( Clause = (Head :- Body) ->
		clean_goal(Body,NBody),
		( NBody == true ->
			NClause = Head
		;
			NClause = (Head :- NBody)
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
