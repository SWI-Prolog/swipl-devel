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
	    http_workers/2,		% +Port, ?WorkerCount
	    http_current_worker/2	% ?Port, ?ThreadID
	  ]).
:- use_module(library(debug)).
:- use_module(http_wrapper).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This library provides a multi-threaded Prolog-based HTTP server based on
the same wrapper as xpce_httpd and   inetd_httpd. This server can handle
multiple clients in Prolog threads and doesn't need XPCE.

In addition to the other two frontends   (XPCE and inetd), this frontend
provides code to deal with the SSL library for creating an HTTPS server.
It is activated using the option  ssl(+SSLOptions), where SSLOptions are
options required by ssl_init/3. See package ssl for details.

BUGS: currently the library depends on library(socket) and library(ssl),
both of which are accessed through  autoloading. Although this works, it
is not elegant. 
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- meta_predicate
	http_server(:, +),
	http_current_server(:, ?).

:- dynamic
	current_server/3,		% Port, Goal, Queue
	queue_worker/2,			% Queue, ThreadID
	queue_options/2.		% Queue, Options

%	http_server(:Goal, ?Port, [+Options])
%	
%	Create a server at Port that calls Goal for each parsed request.
%	Options provide a list of options. Defined options are
%	
%	  workers(N)	[2]		Define the number of worker threads
%	  timeout(S)	[infinite]	Drop connections after inactivity
%	  keep_alive_timeout [10]	Drop Keep-Alive connection timeout
%	  local(KBytes)	
%	  global(KBytes)
%	  trail(KBytes) [<CommandLine>] Stack-sizes of worker threads
%	  after(:Goal)  		Run Goal on request after finishing
%					the HTTP reply.

http_server(Goal, Options) :-
	strip_module(Goal, Module, G),
	select(port(Port), Options, Options1), !,
	http_server(G, Module, Port, Options1).
http_server(_Goal, _Options) :-
	throw(error(existence_error(option, port), _)).


http_server(Goal, Module, Port, Options0) :-
	select(ssl(SSLOptions), Options0, Options1), !,
	ssl_init(SSL, server, [port(Port)|SSLOptions]),
	atom_concat('httpsd@', Port, Queue),
	Options = [ queue(Queue),
		    ssl_instance(SSL)
		  | Options1
		  ],
	create_pool(Options),
	create_server(SSL, Module:Goal, Port, Queue, Options).
http_server(Goal, Module, Port, Options0) :-
	tcp_socket(Socket),
	tcp_setopt(Socket, reuseaddr),
	tcp_bind(Socket, Port),
	tcp_listen(Socket, 5),
	after_option(Options0, Module, Options1),
	atom_concat('httpd@', Port, Queue),
	Options = [queue(Queue)|Options1],
	create_pool(Options),
	create_server(Socket, Module:Goal, Port, Queue, Options).

%	after_option(+Options0, +Module, -Options)
%	
%	Add the module qualifier to the goal for the after(Goal) option

after_option(Options0, Module, Options) :-
	select(after(After), Options0, Options1), !,
	strip_module(Module:After, MA, A),
	Options = [after(MA:A) | Options1].
after_option(Options, _, Options).


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
	strip_module(Goal, Module, G),
	current_server(Port, Module:G, _).


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


%	http_current_worker(?Port, ?ThreadID)
%	
%	True if ThreadID is the identifier   of  a Prolog thread serving
%	Port. This predicate is  motivated  to   allow  for  the  use of
%	arbitrary interaction with the worker thread for development and
%	statistics.

http_current_worker(Port, ThreadID) :-
	current_server(Port, _, Queue),
	queue_worker(Queue, ThreadID).


%	accept_server(+Socket, :Goal, +Options)
%
%	The goal of a small server-thread accepting new requests and
%	posting them to the queue of workers.

accept_server(SSL, Goal, Options) :-
	memberchk(ssl_instance(SSL), Options), !,
	option(queue(Queue), Options, http_client),
	repeat,
	  ssl_accept(SSL, Client, Peer),
	  debug(connection, 'New HTTPS connection from ~p', [Peer]),
	  thread_send_message(Queue, ssl_client(SSL, Client, Goal, Peer)),
	fail.
accept_server(Socket, Goal, Options) :-
	option(queue(Queue), Options, http_client),
	repeat,
	  tcp_accept(Socket, Client, Peer),
	  debug(connection, 'New HTTP connection from ~p', [Peer]),
	  thread_send_message(Queue, tcp_client(Client, Goal, Peer)),
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
	option(queue(Queue), Options, http_client),
	thread_at_exit(done_worker),
	repeat,
	  thread_get_message(Queue, Message),
	  (   Message == quit
	  ->  thread_self(Self),
	      thread_detach(Self)
	  ;   (   Message = tcp_client(Client, Goal, Peer)
	      ->  tcp_open_socket(Client, In, Out)
	      ;	  Message = ssl_client(SSL, Client, Goal, Peer),
		  ssl_open(SSL, Client, In, Out)
	      ),
	      set_stream(In, timeout(Timeout)),
	      debug(server, 'Running server goal ~p on ~p -> ~p',
		    [Goal, In, Out]),
	      (	  catch(server_loop(Goal, In, Out, Peer, Options), E, true)
	      ->  (   var(E)
		  ->  true
		  ;   (   message_level(E, Level)
		      ->  true
		      ;	  Level = error
		      ),
		      debug(server, 'Caught exception ~q (level ~q)',
			    [E, Level]),
		      print_message(Level, E),
		      close_connection(In, Out)
		  )
	      ;	  print_message(error,
				goal_failed(server_loop(Goal, In, Out,
							Peer, Options)))
	      ),
	      fail
	  ),
	!.


done_worker :-
	thread_self(Self),
	retract(queue_worker(_Queue, Self)),
	print_message(informational,
		      httpd_stopped_worker(Self)).


%	thread_httpd:message_level(+Exception, -Level)
%	
%	Determine the message stream used for  exceptions that may occur
%	during server_loop/5. Being multifile, clauses   can be added by
%	the   application   to   refine   error   handling.   See   also
%	message_hook/3 for further programming error handling.

:- multifile
	message_level/2.

message_level(error(io_error(read, _), _),	silent).
message_level(error(timeout_error(read, _), _),	informational).
message_level(keep_alive_timeout,		silent).

%	server_loop(:Goal, +In, +Out, +Socket, +Peer, +Options)
%	
%	Handle a client on the given stream. It will keep the connection
%	open as long as the client wants this

server_loop(_Goal, In, Out, _, _) :-
	at_end_of_stream(In), !,
	close_connection(In, Out).
server_loop(Goal, In, Out, Peer, Options) :-
	http_wrapper(Goal, In, Out, Connection, [request(Request)]),
	(   downcase_atom(Connection, 'keep-alive')
	->  after(Request, Options),
	    option(timeout(TimeOut), Options, infinite),
	    option(keep_alive_timeout(KeepAliveTMO), Options, 5),
	    set_stream(In, timeout(KeepAliveTMO)),
	    catch(peek_code(In, _), _, throw(keep_alive_timeout)),
	    set_stream(In, timeout(TimeOut)),
	    server_loop(Goal, In, Out, Peer, Options)
	;   close_connection(In, Out),
	    after(Request, Options)
	).

%	run `after' hook each time after processing a request.

after(Request, Options) :-
	(   option(after(After), Options, []),
	    After \== []
	->  call(After, Request)
	;   true
	).

%	close_connection(+In, +Out)
%	
%	Closes the connection from the server to the client.  Errors are
%	currently silently ignored.

close_connection(In, Out) :-
	catch(close(In, [force(true)]), _, true),
	catch(close(Out, [force(true)]), _, true).

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
