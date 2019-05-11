query(p17,s,[s,p,q,r,s1,p1,q1,r1,s2,p2,q2,r2,s3,p3,q3,r3,s4,p4,q4,r4,s5,p5,q5,r5],[s,s1,s2,s3,s4,s5],[]).
%-----------------------------------------------------------

:- table p / 0.
:- table p1 / 0.
:- table p2 / 0.
:- table p3 / 0.
:- table p4 / 0.
:- table p5 / 0.
:- table q / 0.
:- table q1 / 0.
:- table q2 / 0.
:- table q3 / 0.
:- table q4 / 0.
:- table q5 / 0.
:- table r / 0.
:- table r1 / 0.
:- table r2 / 0.
:- table r3 / 0.
:- table r4 / 0.
:- table r5 / 0.
:- table s / 0.
:- table s1 / 0.
:- table s2 / 0.
:- table s3 / 0.
:- table s4 / 0.
:- table s5 / 0.

s :- tnot(p), tnot(q), tnot(r), s1.
p :- tnot(r), tnot(s), s1, q.
q :- tnot(p), s1, r.
r :- tnot(q), s1, p.
s1 :- tnot(p1), tnot(q1), tnot(r1), s2.
p1 :- tnot(r1), tnot(s1), s2, q1.
q1 :- tnot(p1), s2, r1.
r1 :- tnot(q1), s2, p1.
s2 :- tnot(p2), tnot(q2), tnot(r2), s3.
p2 :- tnot(r2), tnot(s2), s3, q2.
q2 :- tnot(p2), s3, r2.
r2 :- tnot(q2), s3, p2.
s3 :- tnot(p3), tnot(q3), tnot(r3), s4.
p3 :- tnot(r3), tnot(s3), s4, q3.
q3 :- tnot(p3), s4, r3.
r3 :- tnot(q3), s4, p3.
s4 :- tnot(p4), tnot(q4), tnot(r4), s5.
p4 :- tnot(r4), tnot(s4), s5, q4.
q4 :- tnot(p4), s5, r4.
r4 :- tnot(q4), s5, p4.
s5 :- tnot(p5), tnot(q5), tnot(r5).
p5 :- tnot(s5), tnot(r5), q5.
q5 :- tnot(p5), r5.
r5 :- tnot(q5), p5.
