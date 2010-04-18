:- module(test_signal,
	  [ test_signal/0
	  ]).

/** <module> Test reliability of signal processing

This test verifies that  signals  sent   to  a  thread  are (eventually)
processed.

@see Commit ca2af3256187464e305b6d2325ced0405cb713eb
*/

test_signal :-
	test_signal(true, 1000).

test_signal(true, Count) :- !,
	thread_create(agc_loop, Id, []),
	forall(between(1, Count, I), tsignal(I)),
	thread_signal(Id, throw(stop)),
	thread_join(Id, Status),
	Status == exception(stop).
test_signal(_, Count) :-
	forall(between(1, Count, _), tsignal).

tsignal(I) :-
	thread_self(From),
	thread_create(call_loop(From, I), Id, []),
	thread_get_message(running),
	thread_signal(Id, throw(stop)),
	thread_join(Id, Status),
	Status == exception(stop).

call_loop(From, I) :-
	thread_send_message(From, running),
	L is I mod 3 + 1,
	atom_concat(loop_, L, Loop),
	Loop.

loop_1 :- loop_1.
loop_2 :- repeat, fail.
loop_3 :-
	'$sig_atomic'(member(x, [a,b,c,d,x])),
	loop_3.


agc_loop :-
	garbage_collect_atoms,
	sleep(0.01),			% without a delay, this gets
	agc_loop.			% very slow
