query(p22,p,[p,q,a,b,c],[b],[p,q]).
%-----------------------------------------------------------

:- table a / 0.
:- table b / 0.
:- table c / 0.
:- table p / 0.
:- table q / 0.

a :- fail.
b :- tnot(a).
c :- tnot(b).
c :- a, tnot(p).
p :- tnot(q).
q :- tnot(p), b.
