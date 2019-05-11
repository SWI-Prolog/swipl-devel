query(p15,s,[s,p,q,r],[s],[]).
%-----------------------------------------------------------

:- table p / 0.
:- table q / 0.
:- table r / 0.
:- table s / 0.

s :- tnot(p), tnot(q), tnot(r).
p :- tnot(s), tnot(r), q.
q :- tnot(p), r.
r :- tnot(q), p.
