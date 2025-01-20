:- use_module(library(debug)).
:- discontiguous bar__1/0.
:- discontiguous bar__2/0.

/* early defined clause */
goal_expansion(foo__1, _) :-
    assertion(predicate_property(bar__1, defined)),
    fail.
goal_expansion(foo__1, _) :-
    assertion(predicate_property(bar__1, number_of_clauses(1))),
    fail.
goal_expansion(foo__1, _) :-
    assertion(current_predicate(bar__1/0)),
    fail.
goal_expansion(foo__1, _) :-
    assertion(clause(bar__1, _)),
    fail.
goal_expansion(foo__1, _) :-
    assertion(call(bar__1)),
    fail.

/* late-defined clause */
goal_expansion(foo__2, _) :-
    assertion(predicate_property(bar__2, defined)),
    fail.
goal_expansion(foo__2, _) :-
    assertion(\+ predicate_property(bar__2, number_of_clauses(_))),
    fail.
goal_expansion(foo__2, _) :-
    assertion(current_predicate(bar__2/0)),
    fail.
goal_expansion(foo__2, _) :-
    assertion(\+ clause(bar__2, _)),
    fail.
goal_expansion(foo__2, _) :-
    assertion(\+ call(bar__2)),
    fail.

/* late-defined clause without a known predicate signature */
goal_expansion(foo__3, _) :-
    assertion(\+ predicate_property(bar__3, defined)),
    fail.
goal_expansion(foo__3, _) :-
    assertion(\+ predicate_property(bar__3, number_of_clauses(_))),
    fail.
goal_expansion(foo__3, _) :-
    assertion(\+ current_predicate(bar__3/0)),
    fail.
goal_expansion(foo__3, _) :-
    assertion(\+ clause(bar__3, _)),
    fail.
goal_expansion(foo__3, _) :-
    assertion(\+ predicate_property(bar__3, defined)),
    fail.

bar__1.

trigger :-
    foo__1,
    foo__2,
    foo__3.

bar__2.
bar__3.
