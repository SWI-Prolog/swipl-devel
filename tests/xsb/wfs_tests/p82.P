query(p82,m(a),[m(a),m(b),m(c),n(a),n(b),n(c),p(a),p(b),p(c),q(a),q(b),q(c),r(a),r(b),r(c),s(a),s(b),s(c),t(a),t(b),t(c)],[m(b),p(b),q(a),q(b),r(a),r(c),s(a),s(c)],[]).
%-----------------------------------------------------------

:- table m / 1.
:- table n / 1.
:- table p / 1.
:- table q / 1.
:- table r / 1.
:- table s / 1.
:- table t / 1.

m(A) :- n(A).
m(A) :- p(A).
n(A) :- q(A), t(A).
p(A) :- q(A), tnot(r(A)).
q(a).
q(b).
r(A) :- s(A), tnot(n(A)).
s(a).
s(c).
t(_A) :- fail.
