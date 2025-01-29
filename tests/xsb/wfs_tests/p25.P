query(p25,s,[p,q,r,s],[s],[]).
%-----------------------------------------------------------

:- table p / 0.
:- table q / 0.
:- table r / 0.
:- table s / 0.

p :- q, tnot(s), tnot(r).
q :- r, tnot(p).
r :- p, tnot(q).
s :- tnot(p), tnot(q), tnot(r).
