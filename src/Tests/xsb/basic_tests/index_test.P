:- table m/2.
:- index m/2-2.

m(S, a) :- t(S,_) ; t(S,T), m(T,a).
m(_, b) :- fail.
m(_,_) :- fail.

t(1,2). t(2,1).

s(1). s(2). s(3).

test :- s(A), m(A,a), writeln(A), fail; true.

