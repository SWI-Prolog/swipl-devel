%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Author:	Bart Demoen, Tom Schrijvers
% Email:	Bart.Demoen@cs.kuleuven.ac.be, Tom.Schrijvers@cs.kuleuven.ac.be
% Copyright:	K.U.Leuven 2004
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(find,
	[
		find_with_var_identity/4,
		forall/3,
		forsome/3
	]).

:- use_module(library(lists)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
find_with_var_identity(Template, IdVars, Goal, Answers) :-
        Key = foo(IdVars),
        findall(Key - Template, Goal, As),
        smash(As,Key,Answers).

smash([],_,[]).
smash([Key-T|R],Key,[T|NR]) :- smash(R,Key,NR).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
forall(X,L,G) :-
	\+ (member(X,L), \+ call(G)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
forsome(X,L,G) :-
	once((
		member(X,L),
		call(G)
	)).
