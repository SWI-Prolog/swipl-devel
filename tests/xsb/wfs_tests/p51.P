query(p51,p0,[p0,p1,p2],[],[p0,p1,p2]).
%-----------------------------------------------------------

:- table p0 / 0.
:- table p1 / 0.
:- table p2 / 0.

p0 :- tnot(p0), tnot(p1), tnot(p2).
p1 :- tnot(p1), tnot(p2), tnot(p0).
p2 :- tnot(p2), tnot(p0), tnot(p1).
