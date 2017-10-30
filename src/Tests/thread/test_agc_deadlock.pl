/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2017, University of Amsterdam
                         VU University Amsterdam
		         CWI, Amsterdam
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

:- module(test_agc_deadlock,
          [ test_agc_deadlock/0,
            test_agc_deadlock/1
          ]).

%!  test_agc_deadlock is det.
%!  test_agc_deadlock(+Threads) is det.
%
%   Tests a deadlock situation that occurred between queues, GC and AGC.

test_agc_deadlock :-
    current_prolog_flag(cpu_count, Count),
    Threads is min(4, Count),
    test_agc_deadlock(Threads).
test_agc_deadlock(N) :-
    thread_create(receiver, Receiver, [alias(receiver)]),
    thread_create(sender(Receiver), Sender, [alias(sender)]),
    length(AGC, N),
    maplist(thread_create(agc), AGC),
    thread_join(Receiver),
    thread_join(Sender),
    maplist(abort, AGC),
    maplist(join, AGC).

abort(Thread) :-
    thread_signal(Thread, abort).

join(Thread) :-
    thread_join(Thread, _).

sender(Client) :-
    numlist(1, 1000, L),
    maplist(atom_concat(x), L, Message),
    forall(between(1, 50000, _), thread_send_message(Client, Message)),
    thread_send_message(Client, done).

receiver :-
    receive(_).

receive([H|T]) :-
    thread_get_message(H),
    (   H == done
    ->  true
    ;   receive(T)
    ).

agc :-
    thread_self(Me),
    thread_property(Me, id(Id)),
    atom_concat(a, Id, Prefix),
    forall(between(1, infinite, X),
           atom_concat(Prefix, X, _)).


