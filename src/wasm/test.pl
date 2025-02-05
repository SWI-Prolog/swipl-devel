% :- debug(js).
:- debug(js, 'Loading test.pl', []).
:- set_prolog_flag(prefer_rationals, true).

p(1).
p(3.14).
:- if(current_prolog_flag(bounded, false)).
p(Big) :- Big is 1<<200.
p(R) :- R is 1/3.
p(R) :- R is rational(pi).
:- endif.
p(aap).
p("noot").
p([]).
p([4.2,2,3]).
p([aap|_]).
p(point(3,4)).
p(term(1, aap, _)).
p(_{}).
p(_{name: "Jan", city: "Amsterdam"}).
p(user{name: "Jan", city: "Amsterdam"}).

%!  s(X)
%
%   Simple predicate for testing call performance

s(1).

u(X,X,S) :-
    format(string(S), '~q', [X]).

bigint(I, N, Neg) :-
    N is 2^I,
    Neg is -N.

q(10) :- writeln(q=10).
q(20) :- writeln(q=20).

fib(0,1) :- !.
fib(1,1) :- !.
fib(N,X) :- N>1, M is N-1, fib(M,Y), L is M-1, fib(L,Z), X is Y+Z.
