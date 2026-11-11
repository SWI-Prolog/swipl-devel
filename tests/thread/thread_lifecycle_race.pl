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

:- module(thread_lifecycle_race,
	  [ thread_lifecycle_race/0
	  ]).
:- use_module(library(debug)).

/** <module> Races between using and destroying threads and engines

- engine_destroy/1 while another thread calls engine_next/2 used to
  crash on the destroyed engine mutex.
- thread_create/3 with inherit_from(T) for a thread T that completed
  but was not joined used to crash.
- thread_detach/1 on a thread that completed or was completing did not
  reclaim the thread.
*/

thread_lifecycle_race :-
	forall(between(1, 500, _), engine_round),
	inherit_from_completed,
	detach_completed.

engine_round :-
	engine_create(X, between(1, inf, X), E),
	thread_create(nexter(E, 20), T, []),
	catch(engine_destroy(E), _, true),
	thread_join(T, Status),
	assertion(Status == true).

nexter(E, N) :-
	forall(between(1, N, _),
	       catch(ignore(engine_next(E, _)), _, true)).

inherit_from_completed :-
	thread_create(true, T, []),
	thread_property(T, status(_)),	% make sure T exists
	wait_completed(T),
	catch(thread_create(true, T2, [inherit_from(T)]), E, true),
	assertion(E = error(existence_error(thread, _), _)),
	assertion(var(T2)),
	thread_join(T).

wait_completed(T) :-
	thread_property(T, status(running)),
	!,
	sleep(0.01),
	wait_completed(T).
wait_completed(_).

detach_completed :-
	findall(T, thread_property(T, status(_)), Before),
	forall(between(1, 500, _),
	       ( thread_create(true, T, []),
		 thread_detach(T)
	       )),
	thread_create(true, T1, []),
	wait_completed(T1),
	thread_detach(T1),
	wait_threads(Before, 100).

wait_threads(Before, N) :-
	findall(T, thread_property(T, status(_)), Now),
	length(Before, LB),
	length(Now, LN),
	(   LN =< LB
	->  true
	;   N > 0
	->  sleep(0.01),
	    N1 is N - 1,
	    wait_threads(Before, N1)
	;   assertion(LN =< LB)
	).
