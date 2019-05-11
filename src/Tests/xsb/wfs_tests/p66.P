query(p66,l,[l,p,q,r,s,t],[],[l,p,q,r,s,t]).
%-----------------------------------------------------------

:- table l / 0.
:- table p / 0.
:- table q / 0.
:- table r / 0.
:- table s / 0.
:- table t / 0.

l :- tnot(p).
p :- tnot(q).
p :- q.
p :- tnot(r).
q :- tnot(s).
s :- tnot(q).
r :- tnot(t).
t :- tnot(r).
