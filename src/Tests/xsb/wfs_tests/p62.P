query(p62,l,[l,p,q,s,r,t],[],[l,p,q,s,r,t]).
%-----------------------------------------------------------

:- table l / 0.
:- table p / 0.
:- table q / 0.
:- table r / 0.
:- table s / 0.
:- table t / 0.

l :- tnot(p).
p :- tnot(q), tnot(r).
q :- tnot(s).
s :- tnot(q).
r :- tnot(t).
t :- tnot(r).
