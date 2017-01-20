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
