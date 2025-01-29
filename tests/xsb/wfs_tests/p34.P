query(p34,p,[a,b,p],[],[a,b,p]).
%-----------------------------------------------------------

:- table a / 0.
:- table b / 0.
:- table p / 0.

a :- tnot(b).
b :- tnot(a).
p :- tnot(p).
p :- tnot(b).
