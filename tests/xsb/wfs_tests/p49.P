query(p49,q(a),[p(a),q(a),r(a)],[q(a),r(a)],[]).
%-----------------------------------------------------------

:- table p / 1.
:- table q / 1.
:- table r / 1.

q(A) :- tnot(p(A)).
p(A) :- tnot(p(A)), tnot(r(A)).
r(a).
