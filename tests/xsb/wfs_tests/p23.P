query(p23,(d  ','  p),[p,q,r,s,t,a,b,c,d],[c],[a,b,d]).
%-----------------------------------------------------------

:- table a / 0.
:- table b / 0.
:- table c / 0.
:- table d / 0.
:- table p / 0.
:- table q / 0.
:- table r / 0.
:- table s / 0.
:- table t / 0.

a :- tnot(b), c.
b :- tnot(a).
c.
d :- a, c, a.
p :- tnot(r), q.
p :- tnot(s), r.
p :- t.
q :- p.
r :- q.
r :- tnot(c).
s :- fail.
t :- fail.
