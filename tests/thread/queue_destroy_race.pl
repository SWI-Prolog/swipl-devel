/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2026, SWI-Prolog Solutions b.v.
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

:- module(queue_destroy_race,
	  [ queue_destroy_race/0
	  ]).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(apply)).

/** <module> Destroy named queues while other threads use them

Some threads repeatedly create and destroy a named message queue while
others send to it and ask for its properties.  Threads that found the
queue used to access it after it was freed.
*/

queue_destroy_race :-
	findall(T, ( between(1, 3, _),
		     thread_create(churn(2000), T, [])
		   ), Churners),
	findall(T, ( between(1, 3, _),
		     thread_create(use(2000), T, [])
		   ), Users),
	append(Churners, Users, Threads),
	maplist(join, Threads).

join(T) :-
	thread_join(T, Status),
	assertion(Status == true).

churn(N) :-
	forall(between(1, N, _),
	       ( catch(message_queue_create(_, [alias(qdr_queue)]), _, true),
		 catch(thread_send_message(qdr_queue, x), _, true),
		 catch(message_queue_destroy(qdr_queue), _, true)
	       )).

use(N) :-
	forall(between(1, N, _),
	       ( catch(thread_send_message(qdr_queue, y), _, true),
		 catch(forall(message_queue_property(_, size(_)), true),
		       _, true),
		 catch(forall(message_queue_property(qdr_queue, _), true),
		       _, true),
		 ignore(is_message_queue(qdr_queue))
	       )).
