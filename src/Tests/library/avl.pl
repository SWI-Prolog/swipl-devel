:- module(avl, [avl/0]).

:- use_module(library(assoc)).

negative(N) :-
	N < 0.

positive(N) :-
	N > 0.

avl :-
	empty_assoc(A0),
	put_assoc(a, A0, -3, A1),
	put_assoc(b, A1, -2, A2),
	put_assoc(c, A2, -1, A3),
	min_assoc(A3, a, -3),
	max_assoc(A3, c, -1),
	map_assoc(negative, A3),
	map_assoc(plus(4), A3, A4),
	map_assoc(positive, A4),
	min_assoc(A4, a, 1),
	max_assoc(A4, c, 3),
	put_assoc(c, A4, 4, A5),
	get_assoc(c, A5, 4),
	get_assoc(c, A5, 4, A6, 5),
	get_assoc(c, A6, 5),
	assoc_to_list(A6, [a-1,b-2,c-5]).


