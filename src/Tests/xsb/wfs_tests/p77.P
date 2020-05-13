query(p77,p,[p,q,r,s,t],[],[p,q,r,s,t]).
%-----------------------------------------------------------

:- table p / 0.
:- table q / 0.
:- table r / 0.
:- table s / 0.
:- table t / 0.

p :- q.
q :- r.
q :- tnot(t).
r :- s.
s :- p.
t :- r.
