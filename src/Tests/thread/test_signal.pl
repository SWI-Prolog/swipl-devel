/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2010, University of Amsterdam
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
