query(p48,p,[p,q,r,s],[p],[]).
%-----------------------------------------------------------

:- table p / 0.
:- table q / 0.
:- table r / 0.
:- table s / 0.

p :- tnot(q).
q :- tnot(p).
p :- tnot(r).
r :- tnot(p).
p :- tnot(s).
s :- fail.
