query(p09, p, [p,q,r,s], [q,r], []).
%------------------------------------

:- table p/0, q/0, r/0, s/0.

p :- tnot(q).
q :- tnot(p).
q :- r.
r :- tnot(s).
s :- fail.

