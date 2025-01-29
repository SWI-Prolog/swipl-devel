query(p32,(a  ','  p),[a,b,c,p,q,r,s],[c],[a,b]).
%-----------------------------------------------------------

:- table a / 0.
:- table b / 0.
:- table c / 0.
:- table p / 0.
:- table q / 0.
:- table r / 0.
:- table s / 0.

a :- c, tnot(b).
b :- tnot(a).
c.
p :- q, tnot(r).
p :- r, tnot(s).
q :- p.
r :- q.
s :- fail.
