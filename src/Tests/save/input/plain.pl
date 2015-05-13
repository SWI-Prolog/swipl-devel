echo_true :-
	format('~q.~n', [true]),
	halt.

echo_argv :-
	current_prolog_flag(argv, Argv),
	format('~q.~n', [Argv]),
	halt.
