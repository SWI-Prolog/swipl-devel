query(p52a,p0,[p0,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10],[],[p0,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10]).
%-----------------------------------------------------------

:- table p0 / 0.
:- table p1 / 0.
:- table p10 / 0.
:- table p2 / 0.
:- table p3 / 0.
:- table p4 / 0.
:- table p5 / 0.
:- table p6 / 0.
:- table p7 / 0.
:- table p8 / 0.
:- table p9 / 0.

p0 :- tnot(p0), tnot(p1), tnot(p2), tnot(p3), tnot(p4), tnot(p5), tnot(p6), tnot(p7), tnot(p8), tnot(p9), tnot(p10).
p1 :- tnot(p1), tnot(p2), tnot(p3), tnot(p4), tnot(p5), tnot(p6), tnot(p7), tnot(p8), tnot(p9), tnot(p10), tnot(p0).
p2 :- tnot(p2), tnot(p3), tnot(p4), tnot(p5), tnot(p6), tnot(p7), tnot(p8), tnot(p9), tnot(p10), tnot(p0), tnot(p1).
p3 :- tnot(p3), tnot(p4), tnot(p5), tnot(p6), tnot(p7), tnot(p8), tnot(p9), tnot(p10), tnot(p0), tnot(p1), tnot(p2).
p4 :- tnot(p4), tnot(p5), tnot(p6), tnot(p7), tnot(p8), tnot(p9), tnot(p10), tnot(p0), tnot(p1), tnot(p2), tnot(p3).
p5 :- tnot(p5), tnot(p6), tnot(p7), tnot(p8), tnot(p9), tnot(p10), tnot(p0), tnot(p1), tnot(p2), tnot(p3), tnot(p4).
p6 :- tnot(p6), tnot(p7), tnot(p8), tnot(p9), tnot(p10), tnot(p0), tnot(p1), tnot(p2), tnot(p3), tnot(p4), tnot(p5).
p7 :- tnot(p7), tnot(p8), tnot(p9), tnot(p10), tnot(p0), tnot(p1), tnot(p2), tnot(p3), tnot(p4), tnot(p5), tnot(p6).
p8 :- tnot(p8), tnot(p9), tnot(p10), tnot(p0), tnot(p1), tnot(p2), tnot(p3), tnot(p4), tnot(p5), tnot(p6), tnot(p7).
p9 :- tnot(p9), tnot(p10), tnot(p0), tnot(p1), tnot(p2), tnot(p3), tnot(p4), tnot(p5), tnot(p6), tnot(p7), tnot(p8).
p10 :- tnot(p10), tnot(p0), tnot(p1), tnot(p2), tnot(p3), tnot(p4), tnot(p5), tnot(p6), tnot(p7), tnot(p8), tnot(p9).
