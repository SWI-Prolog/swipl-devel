query(p80,m,[m,p(a),p(b),p(c),q(a),q(b),q(c),r(a),r(b),r(c),s(a),s(b),s(c)],[m,p(b),q(a),q(b),r(a),s(a),s(c)],[]).
%-----------------------------------------------------------

:- table m / 0.
:- table p / 1.
:- table q / 1.
:- table r / 1.
:- table s / 1.

m :- q(A), p(A).
p(A) :- q(A), tnot(r(A)).
q(a).
q(b).
r(A) :- q(A), s(A).
s(a).
s(c).
