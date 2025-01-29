:- table p/2.

p(A,A).
p(A,C) :- p(A,B), e(B, C).
e(a,b).
e(b,c).
%%%%%%%%%%%%%%%%
:- table p/2.

p(A,A).
p(A,C) :- p(A,B), e(B, C).
e(a,b).
e(b,c).

