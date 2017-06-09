/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2011, University of Amsterdam
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

:- module(test_shared_dynamic,
	  [ test_shared_dynamic/0,
	    test_shared_dynamic/1
	  ]).

:- dynamic(foo/1).
:- dynamic(failed/1).

test_shared_dynamic :-
	test_shared_dynamic(50000).

test_shared_dynamic(N) :-
	message_queue_create(Queue),
        thread_create(producer(Queue, N), Producer, []),
        thread_create(consumer(Queue), Consumer, []),
	thread_join(Producer, PStat),
	thread_join(Consumer, CStat),
	message_queue_destroy(Queue),
	PStat == true,
	CStat == true,
	\+ failed(_).

producer(Queue, N) :-
	(   between(1, N, X),
	    assert(foo(X)),
	    thread_send_message(Queue, X),
	    fail
        ;   thread_send_message(Queue, done)
	).

consumer(Queue) :-
        repeat,
        thread_get_message(Queue, X),
	(   X == done
	->  !
	;   (   retract(foo(X))
	    ->  true
	    ;   writeln(failed(X)),
		assert(failed(X))
	    ),
	    fail
	).

