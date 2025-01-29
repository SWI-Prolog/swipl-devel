query(p78,s,[p,q,s],[s],[]).
%-----------------------------------------------------------

:- table p / 0.
:- table q / 0.
:- table s / 0.

s :- tnot(p), tnot(q).
p :- tnot(s), q.
q :- tnot(s), p.
