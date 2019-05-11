query(p16,s,[s,p,q,r],[s],[]).
%-----------------------------------------------------------

:- table p / 0.
:- table q / 0.
:- table r / 0.
:- table s / 0.

s :- tnot(p), tnot(q), tnot(r).
p :- q, tnot(s), tnot(r).
q :- r, tnot(p).
r :- p, tnot(q).
