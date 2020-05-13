query(p89,p,[p,q,r,s],[p,r],[]).
%-----------------------------------------------------------

:- table p / 0.
:- table q / 0.
:- table r / 0.
:- table s / 0.

p :- tnot(q), r.
q :- tnot(p), s.
r.
s :- fail.
