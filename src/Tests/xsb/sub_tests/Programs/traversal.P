:- table anc_l/2, anc_r/2, anc_d/2, same_gen/2, genome_l/2.

anc_l(X,Y) :- p(X,Y).
anc_l(X,Y) :- anc_l(X,Z), p(Z,Y).

anc_r(X,Y) :- p(X,Y).
anc_r(X,Y) :- p(X,Z), anc_r(Z,Y).

anc_d(X,Y) :- p(X,Y).
anc_d(X,Y) :- anc_d(X,Z), anc_d(Z,Y).

same_gen(X,X).
same_gen(X,Y) :- p(W,X), same_gen(W,Z), p(Z,Y).

genome_l(X) :- anc_l(1,X), anc_l(2,X).
