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
          [ test_agc_deadlock/0
          ]).

%!  test_agc_deadlock
%
%   Tests a deadlock situation that occurred between queues, GC and AGC.

test_agc_deadlock :-
    thread_create(receiver, Receiver, [alias(receiver)]),
    thread_create(sender(Receiver), Sender, [alias(sender)]),
    thread_create(agc, AGC, [alias(agc)]),
    thread_join(Receiver),
    thread_join(Sender),
    thread_signal(AGC, abort),
    thread_join(AGC, _).

sender(Client) :-
    numlist(1, 1000, L),
    forall(between(1, 10000, _), thread_send_message(Client, L)),
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
    forall(between(1, infinite, X),
           atom_concat(a, X, _)).


