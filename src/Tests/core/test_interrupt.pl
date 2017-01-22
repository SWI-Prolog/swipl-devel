/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2008-2015, University of Amsterdam
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

:- module(test_interrupt, [test_interrupt/0]).
:- use_module(library(plunit)).

/** <module> Test Prolog core interrupt manipulation primitives

This module is a Unit test for  Prolog built-ins that process interrupts,
such as numbervars, univ, etc.

@author	Jan Wielemaker
*/

test_interrupt :-
	run_tests([ interrupt
		  ]).

:- meta_predicate
	test_interrupt(0).

%%	test_interrupt(:Goal) is semidet.
%
%	True if we can interrupt Goal, which also means we can use it in
%	call_with_time_limit/2. Goal is  must  run   long  enough  to be
%	interrupted.
%
%	@bug	If we cannot interrupt, the thread remains hanging.
%	@bug	There is no good way to know the code to be tested has
%		reached the critical predicate or is still at the
%		startup phase.

test_interrupt(Goal) :-
	thread_self(Me),
	thread_create(run(Me, Goal), Id, []),
	thread_get_message(running),
	sleep(0.1),
	thread_signal(Id, throw(stop)),
	(   between(1, 40, _),
	    thread_property(Id, status(Status)),
	    (	Status == running
	    ->	sleep(0.05),
		fail
	    ;	true
	    )
	->  thread_join(Id, _),
	    (   Status == exception(stop)
	    ->	true
	    ;	throw(error(unexpected_status(Status), _))
	    )
	).

run(Parent, Goal) :-
	thread_send_message(Parent, running),
	Goal.

cp_zero_null :-
	open('/dev/zero', read, In, [type(binary)]),
	open('/dev/null', write, Out, [type(binary)]),
	call_cleanup(copy_stream_data(In, Out),
		     (	 close(In),
			 close(Out)
		     )).


:- begin_tests(interrupt, [condition(current_prolog_flag(threads, true))]).

test(copy_stream_data, [ sto(rational_trees),
			 condition(access_file('/dev/zero', exist))]) :-
	test_interrupt(cp_zero_null).

:- end_tests(interrupt).
