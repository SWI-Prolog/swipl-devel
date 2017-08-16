:-discontiguous bar__1/0.
:-discontiguous bar__2/0.

/* early defined clause */
goal_expansion(foo__1, _) :-
	(   predicate_property(bar__1, defined)
	->  true
	;   assertion(fail)
	),
	fail.
goal_expansion(foo__1, _) :-
	(   predicate_property(bar__1, number_of_clauses(1))
	->  true
	;   assertion(fail)
	),
	fail.
goal_expansion(foo__1, _) :-
	(   current_predicate(bar__1/0)
	->  true
	;   assertion(fail)
	),
	fail.
goal_expansion(foo__1, _) :-
	(   clause(bar__1, _)
	->  true
	;   assertion(fail)
	),
	fail.
goal_expansion(foo__1, _) :-
	(   call(bar__1)
	->  true
	;   assertion(fail)
	),
	fail.

/* late-defined clause */
goal_expansion(foo__2, _) :-
	(   predicate_property(bar__2, defined)
	->  true
	;   assertion(fail)
	),
	fail.
goal_expansion(foo__2, _) :-
	(   predicate_property(bar__2, number_of_clauses(_))
	->  assertion(fail)
	;   true
	),
	fail.
goal_expansion(foo__2, _) :-
	(   current_predicate(bar__2/0)
	->  true
	;   assertion(fail)
	),
	fail.
goal_expansion(foo__2, _) :-
	(   clause(bar__2, _)
	->  assertion(fail)
	;   true
	),
	fail.
goal_expansion(foo__2, _) :-
	(   call(bar__2)
	->  assertion(fail)
	;   true
	),
	fail.

/* late-defined clause without a known predicate signature */
goal_expansion(foo__3, _) :-
	(   predicate_property(bar__3, defined)
	->  assertion(fail)
	;   true
	),
	fail.
goal_expansion(foo__3, _) :-
	(   predicate_property(bar__3, number_of_clauses(_))
	->  assertion(fail)
	;   true
	),
	fail.
goal_expansion(foo__3, _) :-
	(   current_predicate(bar__3/0)
	->  assertion(fail)
	;   true
	),
	fail.
goal_expansion(foo__3, _) :-
	(   clause(bar__3, _)
	->  assertion(fail)
	;   true
	),
	fail.
goal_expansion(foo__3, _) :-
	(   predicate_property(bar__3, defined)
	->  assertion(fail)
	;   true
	),
	fail.

bar__1.

trigger :-
	foo__1,
	foo__2,
	foo__3.

bar__2.
bar__3.
