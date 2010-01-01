:- set_prolog_flag(optimise, true).
:- [test].

graph :-
	pce_dispatch([]),
	prolog_ide(thread_monitor).

test_loop :-
	set_prolog_stack(global, limit(2000000)),
	set_prolog_stack(local, limit(2000000)),
	set_prolog_flag(verbose, silent),
	forall(length(L,N),
	       ( format('RUN ~D~n', [N]),
		 test
	       )),
	is_list(L).			% avoid GC
