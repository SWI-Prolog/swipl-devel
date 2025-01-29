query(p06,p,[p,q,r],[p,q],[]).
%-----------------------------

:- table p/0, q/0, r/0.

p :- q, tnot(r).
q.

r :- fail.
