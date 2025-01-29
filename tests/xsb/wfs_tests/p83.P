query(p83,m,[m,a,b,c,d],[m,a,c],[]).
%-----------------------------------------------------------

:- table a / 0.
:- table b / 0.
:- table c / 0.
:- table d / 0.
:- table m / 0.

m :- c, a.
c :- b.
c.
b :- c, d.
a :- tnot(b).
d :- fail.
