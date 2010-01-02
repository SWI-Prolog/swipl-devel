:- module(test_agc_callback,
	  [ test_agc_callback/0
	  ]).

/** <module> Test AGC issues with callbacks

Doing a call-back from C to   Prolog  calls restore_after_query(), which
leaves  the  environment  for  a   short    while   in   a  state  where
markAtomsInEnvironments() (and also   mark_predicates_in_environments())
cannot deal with it.  This is now resolved using a lock on AGC.  As we
are talking queries, the impact on preformance should be acceptable.

Most likely it is possible to achieve the same effect by proper ordering
of the writes in restore_after_query()   -including  bariers- and taking
care in the marking functions.

Note that '$sig_atomic'/1 is used by setup_call_cleanup/3.
*/

test_agc_callback :-
	tagc(5000).

tagc(N) :-
	thread_create(call_loop, Id, []),
	forall(between(1, N, _),
	       garbage_collect_atoms),
	thread_signal(Id, throw(stop)),
	thread_join(Id, Status),
	Status == exception(stop).

call_loop :-
	'$sig_atomic'(member(x, [a,b,c,d,x])),
	call_loop.

graph :-
	pce_dispatch([]),
	prolog_ide(thread_monitor).
