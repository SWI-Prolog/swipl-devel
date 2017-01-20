/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2005-2008, University of Amsterdam
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

:- module(th_resource,
	  [ th_resource/0
	  ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Test to see what happens if we create threads exhausting virtual memory.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

th_resource :-
	length(L, 20),
	create_threads(L),
	kiss_threads(L),
	join_threads(L).

create_threads([]).
create_threads([H|T]) :-
	(   catch(thread_create(thread_get_message(_), H,
				[ local(infinite),
				  global(infinite),
				  trail(infinite)
				]),
		  error(resource_error(_), _), fail)
	->  true
	;   H = (-)
	),
	create_threads(T).

kiss_threads([]).
kiss_threads([H|T]) :-
	(   H == (-)
	->  true
	;   catch(thread_send_message(H, done), _, true)
	),
	kiss_threads(T).

join_threads([]).
join_threads([H|T]) :-
	thread_join(H, Status),
	check_status(Status),
	join_threads(T).

check_status(true) :- !.
check_status(exception(error(resource_error(memory), _))).
