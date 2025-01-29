query(p35,a,[a,b,c],[],[a,b,c]).
%-----------------------------------------------------------

:- table a / 0.
:- table b / 0.
:- table c / 0.

a :- tnot(b).
b :- tnot(a).
c :- a, b.
a :- tnot(c).
