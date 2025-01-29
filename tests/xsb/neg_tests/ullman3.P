%
% The well-founded model is: {t(1), ~t(2), t(3)}
% Ground atoms t(4), t(5), and t(6) have ``unknown'' truth value.
%

:- table t/1.

t(Z) :- t0(Z).
t(Z) :- g(X,_,Z), t(X).
t(Z) :- g(_,Y,Z), tnot(t(Y)).

t0(1).

g(1,2,3).
g(2,5,4).
g(2,4,5).
g(5,3,6).

%------------------------------------------------------

test :- t(X), writeln(t(X)), fail.
test :- ( t(1), tnot(t(2)), t(3) -> writeln('Success') ; writeln('Failure') ).

