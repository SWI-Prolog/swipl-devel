query(p11,win(_A),[win(a),win(b),win(c),win(d)],[win(b),win(c)],[]).
%-----------------------------------------------------------

:- table win / 1.

win(A) :- m(A,B), tnot(win(B)).
m(a,b).
m(b,c).
m(c,d).
m(b,d).
