query(p08, p, [p,q,r,s], [p,q,s], []).
%-------------------------------------

:- table p/0, q/0, r/0, s/0.

p :- q, tnot(r), s.
q.
r :- tnot(s).
s.

