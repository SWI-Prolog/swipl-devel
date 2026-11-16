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
:- module(assert_retract_race,
	  [ assert_retract_race/0
	  ]).
:- use_module(library(debug)).

/** <module> Retract clauses while they are being asserted

Worker threads retract clauses as soon as they become visible, while
clause garbage collection runs.  assert/2 accessed the new clause after
adding it to the predicate, at which point it may have been retracted
and reclaimed.  Detected using AddressSanitizer.
*/

:- dynamic job/1.

assert_retract_race :-
	thread_create(worker, T1, []),
	thread_create(worker, T2, []),
	forall(between(1, 50000, I),
	       (   I mod 2 =:= 0
	       ->  assertz(job(I))
	       ;   assertz(job(I), Ref),
		   assertion(blob(Ref, clause))
	       )),
	assertz(job(done)),
	assertz(job(done)),
	thread_join(T1, S1),
	thread_join(T2, S2),
	assertion(S1-S2 == true-true).

worker :-
	(   retract(job(X))
	->  (   X == done
	    ->  true
	    ;   worker
	    )
	;   worker
	).
