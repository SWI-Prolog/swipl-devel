/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2008-2012, University of Amsterdam
			      VU University Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

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
	assertion(Exit == true),
	message_queue_destroy(Q).

eat(Q, I, N) :-
	thread_get_message(Q, A),
	assertion(atomic_concat('XXX', I, A)),
	(   I == N
	->  true
	;   I2 is I + 1,
	    eat(Q, I2, N)
	).
