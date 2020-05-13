query(p46,q,[p(a),p(b),q,r],[p(b),r],[p(a)]).
%-----------------------------------------------------------

:- table p / 1.
:- table q / 0.
:- table r / 0.

q :- tnot(r).
r :- p(_A).
p(a) :- tnot(p(a)).
p(b).
