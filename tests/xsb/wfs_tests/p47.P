query(p47,p,[p,q,l],[p],[]).
%-----------------------------------------------------------

:- table l / 0.
:- table p / 0.
:- table q / 0.

l :- tnot(p).
p :- tnot(q).
p.
q :- tnot(p).
