query(p42,r(_684200,_684204),[p(a,b),r(a,b),r(a,c),r(a,d),r(a,e),r(a,f),r(a,g),r(a,h),r(a,i),r(a,j),r(a,k),r(a,l)],[p(a,b),r(a,b),r(a,c),r(a,d),r(a,e),r(a,f),r(a,g),r(a,h),r(a,i),r(a,j),r(a,k),r(a,l)],[]).
%-----------------------------------------------------------

:- table p / 2.
:- table r / 2.

r(A,B) :- p(A,B), tnot(p(B,A)).
p(A,B) :- q(A,B).
p(A,B) :- q(A,C), p(C,B).
q(a,b).
q(b,c).
q(c,d).
q(d,e).
q(e,f).
q(f,g).
q(g,h).
q(h,i).
q(i,j).
q(j,k).
q(k,l).
q(l,b).
