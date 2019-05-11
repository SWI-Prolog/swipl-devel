query(p12,win(_A),[win(a),win(b),win(c)],[win(b)],[]).
%-----------------------------------------------------------

:- table win / 1.

win(A) :- m(A,B), tnot(win(B)).
m(a,b).
m(b,a).
m(b,c).
