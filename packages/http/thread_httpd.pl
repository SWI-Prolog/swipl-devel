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

:- module(thread_httpd,
	  [ http_current_server/2,	% ?:Goal, ?Port
	    http_server/2,		% :Goal, +Options
	    http_workers/2		% +Port, ?WorkerCount
	  ]).
:- use_module(http_wrapper).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This library provides a multi-threaded Prolog-based HTTP server based on
the same wrapper as xpce_httpd and   inetd_httpd. This server can handle
multiple clients in Prolog threads and doesn't need XPCE.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- meta_predicate
	http_server(:, +).

:- dynamic
	current_server/3,		% Port, Goal, Queue
	queue_worker/2,			% Queue, ThreadID
	queue_options/2.		% Queue, Options

%	http_server(:Goal, ?Port, [+Options])
%	
%	Create a server at  Port  that   calls  Goal  for  eached parsed
%	request. Options provide a list of options. Defined options are
%	
%	  workers(N)	[2]		Define the number of worker threads
%	  timeout(S)	[infinite]	Drop connections after inactivity
%	  local(KBytes)	
%	  global(KBytes)
%	  trail(KBytes) [<CommandLine>] Stack-sizes of worker threads
%	  after(:Goal)  		Run Goal on request after finishing
%					the HTTP reply.

http_server(Goal, Options) :-
	select(port(Port), Options, Options1), !,
	http_server(Goal, Port, Options1).
http_server(_Goal, _Options) :-
	throw(error(existence_error(option, port), _)).


http_server(Goal, Port, Options0) :-
	strip_module(Goal, M, G),
	tcp_socket(Socket),
	tcp_bind(Socket, Port),
	tcp_listen(Socket, 5),
	(   select(after(After), Options0, Options1)
	->  strip_module(After, MA, A),
	    Options2 = [after(MA:A)|Options1]
	;   Options2 = Options0
	),
	atom_concat('httpd@', Port, Queue),
	Options = [queue(Queue)|Options2],
	create_pool(Options),
	create_server(Socket, M:G, Port, Queue, Options).

create_server(Socket, Goal, Port, Queue, Options) :-
	atom_concat('http@', Port, Alias),
	thread_create(accept_server(Socket, Goal, Options), _,
		      [ detached(true),
			local(128),
			global(128),
			trail(128),
			alias(Alias)
		      ]),
	assert(current_server(Port, Goal, Queue)).


%	http_current_server(:?Goal, ?Port)
%	
%	Enumerate the created servers.

http_current_server(Goal, Port) :-
	current_server(Port, Goal, _).


%	http_workers(+Port, ?Workers)
%	
%	Query or set the number of workers for the server at this port

http_workers(Port, Workers) :-
	current_server(Port, _, Queue),
	(   integer(Workers)
	->  resize_pool(Queue, Workers)
	;   findall(W, queue_worker(Queue, W), WorkerIDs),
	    length(WorkerIDs, Workers)
	).


%	accept_server(+Socket, :Goal, +Options)
%
%	The goal of a small server-thread accepting new requests and
%	posting them to the queue of workers.

accept_server(Socket, Goal, Options) :-
	option(queue(Queue), Options, http_client),
	repeat,
	  tcp_accept(Socket, Client, Peer),
	  thread_send_message(Queue, client(Client, Goal, Peer)),
	fail.


		 /*******************************
		 *    WORKER QUEUE OPERATIONS	*
		 *******************************/

%	create_pool(+Options)
%	
%	Create the pool of HTTP worker-threads. Each worker has the
%	alias http_worker_N.

create_pool(Options) :-
	option(workers(N), Options, 2),
	option(queue(Queue), Options, http_client),
	catch(message_queue_create(Queue), _, true),
	atom_concat(Queue, '_', AliasBase),
	create_workers(1, N, Queue, AliasBase, Options),
	assert(queue_options(Queue, Options)).

create_workers(I, N, _, _, _) :-
	I > N, !.
create_workers(I, N, Queue, AliasBase, Options) :-
	gensym(AliasBase, Alias),
	thread_create(http_worker(Options), Id,
		      [ alias(Alias)
		      | Options
		      ]),
	assert(queue_worker(Queue, Id)),
	I2 is I + 1,
	create_workers(I2, N, Queue, AliasBase, Options).


resize_pool(Queue, Size) :-
	findall(W, queue_worker(Queue, W), Workers),
	length(Workers, Now),
	(   Now < Size
	->  queue_options(Queue, Options),
	    atom_concat(Queue, '_', AliasBase),
	    I0 is Now+1,
	    create_workers(I0, Size, Queue, AliasBase, Options)
	;   Now == Size
	->  true
	;   Now > Size
	->  Excess is Now - Size,
	    forall(between(1, Excess, _), thread_send_message(Queue, quit))
	).


%	http_worker(+Options)
%	
%	A worker.  Workers simply wait until they are passes an accepted
%	socket to process a client.

http_worker(Options) :-
	option(timeout(Timeout), Options, infinite),
	option(after(After), Options, []),
	option(queue(Queue), Options, http_client),
	thread_at_exit(done_worker),
	repeat,
	  thread_get_message(Queue, Message),
	  (   Message == quit
	  ->  thread_self(Self),
	      thread_detach(Self)
	  ;   Message = client(Socket, Goal, Peer),
	      tcp_open_socket(Socket, In, Out),
	      set_stream(In, timeout(Timeout)),
	      (	  server_loop(Goal, In, Out, Socket, Peer, After)
	      ->  true
	      ;	  format(user_error, 'FAILED~n', [])
	      ),
	      fail
	  ),
	!.


done_worker :-
	thread_self(Self),
	retract(queue_worker(_Queue, Self)),
	print_message(informational,
		      httpd_stopped_worker(Self)).


%	server_loop(:Goal, +In, +Out, +Socket, +Peer, :After)
%	
%	Handle a client on the given stream. It will keep the connection
%	open as long as the client wants this

server_loop(_Goal, In, _Out, Socket, _, _) :-
	at_end_of_stream(In), !,
	tcp_close_socket(Socket).
server_loop(Goal, In, Out, Socket, Peer, After) :-
	http_wrapper(Goal, In, Out, Connection, [request(Request)]),
	(   downcase_atom(Connection, 'keep-alive')
	->  after(After, Request),
	    server_loop(Goal, In, Out, Socket, Peer, After)
	;   tcp_close_socket(Socket),
	    after(After, Request)
	).

after([], _) :- !.
after(Goal, Request) :-
	call(Goal, Request).

%	option(+Term, +Options, +Default)
%	
%	Fetch option from the list.

option(Opt, Options, _) :-
	memberchk(Opt, Options), !.
option(Opt, _, Def) :-
	arg(1, Opt, Def).


		 /*******************************
		 *	      MESSAGES		*
		 *******************************/

:- multifile
	prolog:message/3.

prolog:message(httpd_stopped_worker(Self)) -->
	[ 'Stopped worker ~p'-[Self] ].
