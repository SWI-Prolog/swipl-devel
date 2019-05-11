query(p19,r(_684208,_684212),[r(a,b),r(a,c),r(a,d),r(b,a),r(c,a),r(d,a),p(a,b),p(a,c),p(a,d),p(b,a),p(c,a),p(d,a)],[r(a,b),r(a,c),r(a,d),p(a,b),p(a,c),p(a,d)],[]).
%-----------------------------------------------------------

:- table p / 2.
:- table r / 2.

r(A,B) :- p(A,B), tnot(p(B,A)).
p(A,B) :- q(A,B).
p(A,B) :- q(A,C), p(C,B).
q(a,b).
q(b,c).
q(c,d).
q(d,b).
