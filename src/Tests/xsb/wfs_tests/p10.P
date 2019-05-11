query(p10,p,[p,q,r],[],[p,q,r]).
%-------------------------------

:- table p/0, q/0, r/0.

p :- q.
p :- r.
r :- tnot(q).
q :- tnot(r).

