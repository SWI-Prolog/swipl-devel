:- module(thread_agc_queue,
	  [ thread_agc_queue/0
	  ]).
:- use_module(library(debug)).

%%	thread_agc_queue
%
%	Test AGC using new message  queues   that  sweep atom references
%	rather then locking atoms from  records.   The  trick is to push
%	atoms XXX<N> over the queue and   validate them at the receiving
%	end. If atom-gc picks one of them   we will not receive a proper
%	sequence.
%	
%	For testing purposes we use a   rather  small number of messages
%	and frequent atom-gc.

thread_agc_queue :-
	current_prolog_flag(agc_margin, Old),
	set_prolog_flag(agc_margin, 1000),
	call_cleanup(test(100000),
		     set_prolog_flag(agc_margin, Old)).

test(N) :-
	message_queue_create(Q, [max_size(1000)]),
	thread_create(eat(Q, 1, N), Id, []),
	forall(between(1, N, I),
	       (   atom_concat('XXX', I, A),
		   thread_send_message(Q, A)
	       )),
	thread_join(Id, Exit),
	assertion(Exit == true).
	
eat(Q, I, N) :-
	thread_get_message(Q, A),
	atom_concat('XXX', NA, A),
	atom_number(NA, Num),
	assertion(Num==I),
	(   Num == N
	->  true
	;   I2 is I + 1,
	    eat(Q, I2, N)
	).
