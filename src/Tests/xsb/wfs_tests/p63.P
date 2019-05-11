query(p63,t,[p,q,s,r,t],[],[p,q,s,r,t]).
%-----------------------------------------------------------

:- table p / 0.
:- table q / 0.
:- table r / 0.
:- table s / 0.
:- table t / 0.

t :- tnot(s).
s :- tnot(p).
p :- q.
p :- r.
r :- tnot(q).
q :- tnot(r).
