% :- debug(js).
:- debug(js, 'Loading test.pl', []).

p(1).
p(3.14).
p(1r3).		% Needs GMP
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

u(X,X) :-
    debug(js, 'u/2: Got ~q~n', [X]).

bigint(I, N, Neg) :-
    N is 2^I,
    Neg is -N.
