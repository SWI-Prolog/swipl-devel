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

u(X,X) :-
    debug(js, 'u/2: Got ~q~n', [X]).

bigint(I, N, Neg) :-
    N is 2^I,
    Neg is -N.
