query(p26,l,[l,m,n,p,q,r,s],[l,n,s],[]).
%-----------------------------------------------------------

:- table l / 0.
:- table m / 0.
:- table n / 0.
:- table p / 0.
:- table q / 0.
:- table r / 0.
:- table s / 0.

l :- tnot(p), s.
m :- tnot(s).
n :- tnot(p).
p :- tnot(s), tnot(r), q.
q :- tnot(p), r.
r :- tnot(q), p.
s :- tnot(p), tnot(q), tnot(r).
