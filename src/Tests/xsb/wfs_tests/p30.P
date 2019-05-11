query(p30,p,[p,q,r],[],[p,q,r]).
%-----------------------------------------------------------

:- table p / 0.
:- table q / 0.
:- table r / 0.

p :- q.
p :- tnot(r).
r :- tnot(q).
q :- tnot(r).
