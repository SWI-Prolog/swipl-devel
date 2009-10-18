/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2008, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/


:- module(test_thread_pool,
	  [ test_thread_pool/0,
	    t/0, d/0
	  ]).
:- use_module(library(plunit)).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(thread_pool)).

d :-
	debug(thread_pool),
	debug(thread_pool(_)).

t :-
	test_thread_pool.

test_thread_pool :-
	run_tests([ thread_pool
		  ]).

start :-
	start([detached(true)]).
start(Options) :-
	thread_pool_create(test, 3, Options).
stop :-
	thread_pool_destroy(test).

join_all([]).
join_all([H|T]) :-
	thread_join(H, Status),
	assertion(Status==true),
	join_all(T).

:- begin_tests(thread_pool, [sto(rational_trees),
			     condition(current_prolog_flag(threads, true))]).

:- dynamic
	v/1.

test(current, [setup(start),cleanup(stop)]) :-
	current_thread_pool(test).
test(free, [setup(start),cleanup(stop),X==3]) :-
	thread_pool_property(test, free(X)).
test(free, [setup(start),cleanup(stop),X==0]) :-
	thread_pool_property(test, backlog(X)).
test(loop, [setup(start([])),cleanup(stop), L == Vs]) :-
	numlist(0, 10, L),
	findall(Id,
		(   between(0, 10, I),
		    thread_create_in_pool(test, run(I), Id, [])
		), Ids),
	join_all(Ids),
	setof(V, retract(v(V)), Vs).

run(I) :-
	sleep(0.05),
	assert(v(I)).

:- end_tests(thread_pool).
