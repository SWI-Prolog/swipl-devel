query(p50,q(_684200),[q(a),q(b),p(a),p(b),r(a),r(b)],[q(b),p(a),r(a)],[]).
%-----------------------------------------------------------

:- table p / 1.
:- table q / 1.
:- table r / 1.

q(a) :- tnot(p(a)).
q(b) :- tnot(p(b)).
p(a) :- tnot(p(a)), tnot(r(a)).
p(a) :- tnot(p(b)), tnot(r(b)).
r(a).
