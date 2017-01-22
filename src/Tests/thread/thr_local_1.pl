/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2002-2011, University of Amsterdam
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

:- module(thr_local_1,
	  [ thr_local_1/0
	  ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This test validates the operation of thread-local dynamic predicates. It
creates 5 threads asserting the 1000 facts 1...1000 and checks they can
be retracted in the proper order.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- thread_local
	foo/1.

thr_local_1 :-
	thr_local_1(5, 1000).

thr_local_1(Threads, Asserts) :-
	thread_create(join(Threads), Id, []),
	forall(between(1, Threads, _),
	       thread_create(test_foo(Asserts), _,
			     [ at_exit(done(Id))
			     ])),
	join_ok(Id).

join(Times) :-
	forall(between(1, Times, _),
	       (   thread_get_message(done(Done)),
		   join_ok(Done)
	       )).

join_ok(Id) :-
	thread_join(Id, Return),
	(   Return == true
	->  true
	;   format('~N~p returned ~p~n', [Id, Return]),
	    fail
	).


test_foo(N) :-
	forall(between(0, N, X),
	       assert(foo(X))),
	predicate_property(foo(_), number_of_clauses(Count)),
	(   Count =:= N+1
	->  true
	;   format(user_error, '~D clauses!?~n', [Count])
	),
	findall(X, retract(foo(X)), List),
	(   check(0, N, List)
	->  true
	;   numlist(0, N, OkList),
	    ord_subtract(OkList, List, Missing),
	    compact(Missing, Ranges),
	    thread_self(TID),
	    format(user_error, '~N[~w] MISSING: ~q~n', [TID, Ranges]),
	    fail
	).

check(I, N, []) :-
	I > N, !.
check(I, N, [I|T]) :-
	NI is I + 1,
	check(NI, N, T).

compact([], []).
compact([H|T0], [Range|T]) :-
	subsequent(T0, H, E, T1),
	(   H == E
	->  Range = H
	;   Range = (H-E)
	),
	compact(T1, T).

subsequent([H|T], X, E, R) :-
	H =:= X+1, !,
	subsequent(T, H, E, R).
subsequent(L, X, X, L).


done(Report) :-
	thread_self(Me),
	thread_send_message(Report, done(Me)).
