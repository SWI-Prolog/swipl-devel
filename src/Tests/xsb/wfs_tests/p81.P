query(p81,m(a),[m(a),m(b),m(c),p(a),p(b),p(c),q(a),q(b),q(c),r(a),r(b),r(c),s(a),s(b),s(c)],[m(a),m(b),p(a),p(b),q(a),q(b),r(c),s(a),s(c)],[]).
%-----------------------------------------------------------

:- table m / 1.
:- table p / 1.
:- table q / 1.
:- table r / 1.
:- table s / 1.

m(A) :- q(A), p(A).
p(A) :- q(A), tnot(r(A)).
q(a).
q(b).
r(A) :- s(A), tnot(q(A)).
s(a).
s(c).
