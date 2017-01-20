/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2013, University of Amsterdam
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

:- module(queue_timeout,
	  [ queue_timeout/0
	  ]).
:- use_module(library(debug)).
:- use_module(library(plunit)).

queue_timeout :-
	run_tests([ queue_timeout
		  ]).

:- begin_tests(queue_timeout).

test(relative,
     [ setup(message_queue_create(Q)),
       cleanup(message_queue_destroy(Q))
     ]) :-
	get_time(T0),
	\+ thread_get_message(Q, _, [timeout(0.01)]),
	get_time(T1),
	T1 > T0.
test(abs,
     [ setup(message_queue_create(Q)),
       cleanup(message_queue_destroy(Q))
     ]) :-
	get_time(T0),
	Deadline is T0+0.01,
	\+ thread_get_message(Q, _, [deadline(Deadline)]),
	get_time(T1),
	T1 >= Deadline.
test(abs_rel,
     [ setup(message_queue_create(Q)),
       cleanup(message_queue_destroy(Q))
     ]) :-
	get_time(T0),
	Deadline is T0+0.01,
	\+ thread_get_message(Q, _, [ deadline(Deadline),
				      timeout(1)
				    ]),
	get_time(T1),
	T1 >= Deadline,
	T1 < T0+1.
test(abs_rel,
     [ setup(message_queue_create(Q)),
       cleanup(message_queue_destroy(Q))
     ]) :-
	get_time(T0),
	Deadline is T0+1,
	\+ thread_get_message(Q, _, [ deadline(Deadline),
				      timeout(0.01)
				    ]),
	get_time(T1),
	T1 > T0,
	T1 < Deadline.

:- end_tests(queue_timeout).

