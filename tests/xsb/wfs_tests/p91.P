query(p91,p,[p,q,r],[p,r],[]).
%-----------------------------------------------------------

:- table p / 0.
:- table q / 0.
:- table r / 0.

p :- tnot(q), r.
q :- tnot(r), p.
r :- tnot(p), q.
r.
