/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2002, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- use_module(library('http/http_client')).
:- use_module(library(debug)).
:- use_module(library('http/http_sgml_plugin')).


%	stress(+Times, +Threads, +URLOrAlias)
%	
%	Typical use: stress(1000, 3, 1): run the test 1000 times with
%	3 client threads on the /xml test from demo_body.pl and verify
%	the parsed result.

stress(Times, Parallel, Alias) :-
	answer(Alias, URL, _), !,
	stress(Times, Parallel, URL).
stress(Times, Parallel, URL) :-
	(   pool(pool, _)
	->  delete_pool(pool)
	;   true
	),
	create_pool(pool, Parallel),
	stress(Times, URL),
	wait_done(Times),
	delete_pool(pool).

wait_done(0) :- !.
wait_done(N) :-
	thread_get_message(done, Result),
	put(Result), flush,
	N1 is N - 1,
	wait_done(N1).
	
stress(0, _) :- !.
stress(N, URL) :-
	thread_send_message(pool, stress_url(URL)),
	NN is N - 1,
	stress(NN, URL).

stress_url(URL) :-
	thread_self(Me),
	atom_number(N, Me),
	(   catch(http_get(URL, X, [connection(close)]), E, true)
	->  (   var(E)
	    ->	(   answer(_, URL, Correct)
		->  (   X == Correct
		    ->  thread_send_message(done, N)
		    ;   thread_send_message(done, !)
		    )
		;   thread_send_message(done, ?)
		)
	    ;	print_message(error, E),
		thread_send_message(done, 'E')
	    )
	;   thread_send_message(done, -)
	).

:- dynamic
	pool/2.				% name, threads

create_pool(Name, N) :-
	message_queue_create(Name),
	findall(Id, (between(1, N, _),
		     thread_create(worker(Name), Id, [])), Threads),
	assert(pool(Name, Threads)).
	

delete_pool(Name) :-
	pool(Name, Threads),
	forall(member(_, Threads), thread_send_message(Name, thread_exit(ok))),
	forall(member(Id, Threads), thread_join(Id, _)),
	message_queue_destroy(Name),
	retract(pool(Name, Threads)).





worker(Queue) :-
	repeat,
	  thread_get_message(Queue, Goal),
	  (   catch(Goal, E, true)
	  ->  (   var(E)
	      ->  true
	      ;   print_message(error, E)
	      )
	  ;   print_message(error, goal_failed(Goal))
	  ),
	fail.


		 /*******************************
		 *	  CORRECT ANSWERS	*
		 *******************************/


answer(1, 'http://localhost:3000/xml',
       [ element(message,
	  [],
	  [ '\n  ',
	    element(head,
		    [],
		    [ '\n  ',
		      element(from,
			      [],
			      [ 'Jan Wielemaker'
			      ]),
		      '\n  ',
		      element(to,
			      [],
			      [ 'Prolog users'
			      ]),
		      '\n  ',
		      element(subject,
			      [],
			      [ 'The SWI-Prolog web-server'
			      ]),
		      '\n  '
		    ]),
	    '\n  ',
	    element(body,
		    [],
		    [ '\n',
		      element(p,
			      [],
			      [ '\nThis is the first demo of the web-server serving an XML message\n'
			      ]),
		      '\n  '
		    ]),
	    '\n'
	  ])
       ]).
