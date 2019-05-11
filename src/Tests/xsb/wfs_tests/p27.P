query(p27,s,[p,q,r,s,ns],[s],[]).
%-----------------------------------------------------------

:- table ns / 0.
:- table p / 0.
:- table q / 0.
:- table r / 0.
:- table s / 0.

p :- tnot(r), ns, q.
ns :- tnot(s).
q :- r, tnot(p).
r :- p, tnot(q).
s :- tnot(p), tnot(q), tnot(r).
