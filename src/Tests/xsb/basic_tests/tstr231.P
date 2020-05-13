:- table a/2.

tw :- a(X,Y), write(X), write(' '), write(Y), nl, fail.
tw.

ts :- statistics,a(X,Y),fail.
ts :- statistics.

a(X,Y):-p(X,Y).
a(X,Y):-p(X,Z),a(Z,Y).

