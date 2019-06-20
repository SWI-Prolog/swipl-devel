create_set([]).

add_elem(X, Set, BigSet) :-
	set_equal(BigSet, [X|Set]). % Note that all are multisets

difference(X, Y, Z) :-
	difference_bbf(X, Y, Z1), set_equal(Z, Z1).

difference_bbf([], _, []).
difference_bbf([X|Xs], Set2, Diff) :-
	remove(X, Set2, SmallSet)
	-> difference_bbf(Xs, SmallSet, Diff)
	;  difference_bbf(Xs, Set2, NDiff), Diff = [X|NDiff].

inter(X, Y, Z) :-
	inter_bbf(X, Y, Z1), set_equal(Z, Z1).

inter_bbf([], _, []).
inter_bbf([X|Xs], Set2, Inter) :-
	remove(X, Set2, SmallSet)
	 -> inter_bbf(Xs, SmallSet, Inter2),
	    Inter = [X|Inter2]
	 ;  inter_bbf(Xs, Set2, Inter).

set_equal(X , Y) :-
	((nonvar(X), nonvar(Y)) 
		-> set_equal_bb(X, Y)
		; X = Y
	).

set_equal_bb([], []).
set_equal_bb([X|Xs], Set2) :-
	remove(X, Set2, Ys),
	set_equal_bb(Xs, Ys).

remove(X, [X|Xs], Xs) :- !.
remove(X, [Y|Ys], [Y|Zs]) :- remove(X, Ys, Zs).
