test :-
	rlimit(cpu, _, 2),
	on_signal(xcpu, _, cpu_exceeded),
	( repeat, fail ).

cpu_exceeded(_Sig) :-
	format(user_error, 'CPU time exceeded~n', []),
	halt(1).
