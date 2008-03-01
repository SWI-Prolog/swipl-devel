/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2007, University of Amsterdam

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

:- module(test_interrupt, [test_interrupt/0]).
:- use_module(library(plunit)).

/** <module> Test Prolog core interrupt manipulation primitives

This module is a Unit test for  Prolog built-ins that process interrupts,
suchj as numbervars, univ, etc.

@author	Jan Wielemaker
*/

test_interrupt :-
	run_tests([ interrupt
		  ]).

:- module_transparent
	test_interrupt/1.

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
	strip_module(Goal, M, G),
	thread_self(Me),
	thread_create(run(Me, M:G), Id, []),
	thread_get_message(running),
	sleep(0.1),
	thread_signal(Id, throw(stop)),
	(   between(1, 10, _),
	    thread_property(Id, status(Status)),
	    (	Status == running
	    ->	sleep(0.05),
		fail
	    ;	true
	    )
	->  (   Status == exception(stop)
	    ->	true
	    ;	throw(error(unexpected_status(Status), _))
	    )
	).

run(Parent, Goal) :-
	thread_send_message(Parent, running),
	Goal.

cp_zero_null :-
	open('/dev/zero', read, In),
	open('/dev/null', write, Out),
	call_cleanup(copy_stream_data(In, Out),
		     (	 close(In),
			 close(Out)
		     )).


:- begin_tests(interrupt).

test(copy_stream_data, [ sto(rational_trees),
			 condition(access_file('/dev/zero', exist))]) :-
	test_interrupt(cp_zero_null).

:- end_tests(interrupt).
