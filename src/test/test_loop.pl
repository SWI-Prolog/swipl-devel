:- set_prolog_flag(optimise, true).
:- set_prolog_flag(test_concurrent, true).
:- [test].

%%	test_loop
%
%	Run the normal test-suite as an   infinite loop. We use length/2
%	rather than repeat to shift  the  global   stack  a  bit in each
%	iteration and thus schedule GC/shifts differently.

test_loop :-
	set_prolog_flag(verbose, silent),
	forall(length(L,N),
	       ( format('RUN ~D~n', [N]),
		 test
	       )),
	is_list(L).			% avoid GC

%%	test_loops(+Threads)
%
%	Create a number of threads, each running test_loop/0

test_loops(N) :-
	forall(between(1, N, I),
	       (   atom_concat('tester_', I, Alias),
		   thread_create(test_loop, _, [alias(Alias)])
	       )).

%%	graph
%
%	Run  the  thread-monitor  in  separate    thread   to  see  nice
%	test-results.

graph :-
	pce_dispatch([]),
	prolog_ide(thread_monitor).

