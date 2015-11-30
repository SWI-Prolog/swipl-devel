:- use_module(library(apply_macros)).

square_list(In, Out) :-
	maplist(square, In, In1),
	maplist(square, In1, Out).

square(V, V2) :-
	V2 is V*V.
