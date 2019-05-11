query(p07, p, [p,q,r,s,t], [p,q,s], []).
%---------------------------------------

:- table p/0, q/0, r/0, s/0, t/0.

p :- q, tnot(r).
q.
r :- tnot(s).
s :- tnot(t).
t :- fail.

