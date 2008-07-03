/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2007, University of Amsterdam

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
	    http_current_worker/2,	% ?Port, ?ThreadID
	    http_stop_server/2,		% +Port, +Options
	    http_spawn/2,		% :Goal, +Options
	    
	    http_requeue/1,		% +Request
	    http_close_connection/1	% +Request
	  ]).
:- use_module(library(debug)).
:- use_module(library(error)).
:- use_module(library(option)).
:- use_module(library(lists)).
:- use_module(library(socket)).
:- use_module(library(thread_pool)).
:- use_module(http_wrapper).
:- use_module(http_stream).


/** <module> Threaded HTTP server

This library provides a multi-threaded Prolog-based HTTP server based on
the same wrapper as xpce_httpd and   inetd_httpd. This server can handle
multiple clients in Prolog threads and doesn't need XPCE.

In addition to the other two frontends   (XPCE and inetd), this frontend
provides hooks for http_ssl_plugin.pl for creating   an HTTPS server. It
is activated using the  option   ssl(+SSLOptions),  where SSLOptions are
options required by ssl_init/3. See   http_ssl_plugin.pl and package ssl
for details.
*/

:- meta_predicate
	http_server(:, +),
	http_current_server(:, ?),
	http_spawn(:, +).

:- dynamic
	port_option/2,			% Port, Option
	current_server/4,		% Port, Goal, Thread, Queue
	queue_worker/2,			% Queue, ThreadID
	queue_options/2.		% Queue, Options

:- multifile
	make_socket_hook/3,
	accept_hook/2,
	close_hook/1,
	open_client_hook/5.

%%	http_server(:Goal, +Options) is det.
%	
%	Create a server at Port that calls Goal for each parsed request.
%	Options provide a list of options. Defined options are
%	
%	| port(?Port)	     | - 	| Port to listen to |
%	| workers(N)	     | 2 	| Define the number of worker threads |
%	| timeout(S)	     | infinite	| Drop connections after inactivity   |
%	| keep_alive_timeout | 10	| Drop Keep-Alive connection timeout  |
%	| local(KBytes)	     | <CommandLine> |				    |
%	| global(KBytes)     | <CommandLine> |				    |
%	| trail(KBytes)      | <CommandLine> | Stack-sizes of worker threads  |

http_server(Goal, Options) :-
	strip_module(Goal, Module, G),
	select_option(port(Port), Options, Options1), !,
	make_socket(Port, Options1, Options2),
	set_port_options(Port, Options2),
	create_workers(Options2),
	create_server(Module:G, Port, Options2).
http_server(_Goal, _Options) :-
	throw(error(existence_error(option, port), _)).


%%	make_socket(?Port, +OptionsIn, -OptionsOut) is det.
%
%	Create the HTTP server socket and  worker pool queue. OptionsOut
%	is quaranteed to hold the option queue(QueueId).

make_socket(Port, Options0, Options) :-
	make_socket_hook(Port, Options0, Options), !.
make_socket(Port, Options0, Options) :-
	tcp_socket(Socket),
	tcp_setopt(Socket, reuseaddr),
	tcp_bind(Socket, Port),
	tcp_listen(Socket, 5),
	atom_concat('httpd@', Port, Queue),
	Options = [ queue(Queue),
		    tcp_socket(Socket)
		  | Options0
		  ].

create_server(Goal, Port, Options) :-
	memberchk(queue(Queue), Options),
	atom_concat('http@', Port, Alias),
	thread_create(accept_server(Goal, Options), _,
		      [ local(128),
			global(128),
			trail(128),
			alias(Alias)
		      ]),
	assert(current_server(Port, Goal, Alias, Queue)).


%%	set_port_options(+Port, +Options) is det.
%
%	Register Options for the HTTP server at Port.

set_port_options(Port, Options) :-
	retractall(port_option(Port, _)),
	assert_port_options(Options, Port).

assert_port_options([], _).
assert_port_options([Name=Value|T], Port) :- !,
	Opt =.. [Name,Value],
	assert(port_option(Port, Opt)),
	assert_port_options(T, Port).
assert_port_options([Opt|T], Port) :- !,
	assert(port_option(Port, Opt)),
	assert_port_options(T, Port).


%%	http_current_server(:Goal, ?Port) is nondet.
%	
%	True if Goal is the goal of a server at Port.

http_current_server(Goal, Port) :-
	(   var(Goal)
	->  current_server(Port, Goal, _, _)
	;   strip_module(Goal, Module, G),
	    current_server(Port, Module:G, _, _)
	).


%%	http_workers(+Port, -Workers) is det.
%%	http_workers(+Port, +Workers:int) is det.
%	
%	Query or set the number of workers  for the server at this port.
%	The number of workers is dynamically   modified. Setting it to 1
%	(one) can be used to profile the worker using tprofile/1.

http_workers(Port, Workers) :-
	must_be(integer, Port),
	current_server(Port, _, _, Queue), !,
	(   integer(Workers)
	->  resize_pool(Queue, Workers)
	;   findall(W, queue_worker(Queue, W), WorkerIDs),
	    length(WorkerIDs, Workers)
	).
http_workers(Port, _) :-
	existence_error(http_server, Port).


%%	http_current_worker(?Port, ?ThreadID) is nondet.
%	
%	True if ThreadID is the identifier   of  a Prolog thread serving
%	Port. This predicate is  motivated  to   allow  for  the  use of
%	arbitrary interaction with the worker thread for development and
%	statistics.

http_current_worker(Port, ThreadID) :-
	current_server(Port, _, _, Queue),
	queue_worker(Queue, ThreadID).


%%	accept_server(:Goal, +Options)
%
%	The goal of a small server-thread accepting new requests and
%	posting them to the queue of workers.

accept_server(Goal, Options) :-
	catch(accept_server2(Goal, Options), http_stop, true),
	thread_self(Thread),
	retract(current_server(_Port, _, Thread, _Queue)),
	close_server_socket(Options).

accept_server2(Goal, Options) :-
	accept_hook(Goal, Options), !.
accept_server2(Goal, Options) :-
	memberchk(tcp_socket(Socket), Options), !,
	memberchk(queue(Queue), Options),
	repeat,
	  (   catch(tcp_accept(Socket, Client, Peer), E, true)
	  ->  (   var(E)
	      ->  debug(http(connection), 'New HTTP connection from ~p', [Peer]),
		  thread_send_message(Queue, tcp_client(Client, Goal, Peer)),
		  fail
	      ;   E == http_stop
	      ->  throw(http_stop)
	      ;   print_message(error, E),
		  fail
	      )
	  ;   print_message(error, goal_failed(tcp_accept(Socket, Client, Peer)))
	  ).


%%	close_server_socket(+Options)
%
%	Close the server socket.

close_server_socket(Options) :-
	close_hook(Options), !.
close_server_socket(Options) :-
	memberchk(tcp_socket(Socket), Options), !,
	tcp_close_socket(Socket).


%%	http_stop_server(+Port, +Options)
%
%	Stop the indicated  HTTP  server   gracefully.  First  stops all
%	workers, then stops the server.
%	
%	@tbd	Realise non-graceful stop

http_stop_server(Port, _Options) :-
	http_workers(Port, 0),
	current_server(Port, _, Thread, Queue),
	retractall(queue_options(Queue, _)),
	thread_signal(Thread, throw(http_stop)),
	catch(connect(localhost:Port), _, true),
	thread_join(Thread, _),
	message_queue_destroy(Queue).

connect(Address) :-
        tcp_socket(Socket),
        tcp_connect(Socket, Address),
	tcp_close_socket(Socket).


		 /*******************************
		 *    WORKER QUEUE OPERATIONS	*
		 *******************************/

%%	create_workers(+Options)
%	
%	Create the pool of HTTP worker-threads. Each worker has the
%	alias http_worker_N.

create_workers(Options) :-
	option(workers(N), Options, 2),
	option(queue(Queue), Options),
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


%%	resize_pool(+Queue, +Workers) is det.
%
%	Create or destroy workers. If workers   are  destroyed, the call
%	waits until the desired number of waiters is reached.

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
	    thread_self(Me),
	    forall(between(1, Excess, _), thread_send_message(Queue, quit(Me))),
	    forall(between(1, Excess, _), thread_get_message(quitted(_)))
	).


%%	http_worker(+Options)
%	
%	Run HTTP worker main loop. Workers   simply  wait until they are
%	passed an accepted socket to process  a client. 
%	
%	If the message quit(Sender) is read   from the queue, the worker
%	stops.

http_worker(Options) :-
	thread_at_exit(done_worker),
	option(queue(Queue), Options),
	repeat,
	  garbage_collect,
	  debug(http(worker), 'Waiting for a job ...', []),
	  thread_get_message(Queue, Message),
	  debug(http(worker), 'Got job ~p', [Message]),
	  (   Message = quit(Sender)
	  ->  !,
	      thread_self(Self),
	      thread_detach(Self),
	      thread_send_message(Sender, quitted(Self))
	  ;   open_client(Message, Queue, Goal, In, Out,
			  Options, ClientOptions),
	      (	  catch(http_process(Goal, In, Out, ClientOptions),
			Error, true)
	      ->  true
	      ;	  Error = goal_failed(http_process/4)
	      ),
	      (	  var(Error)
	      ->  fail
	      ;	  current_message_level(Error, Level),
		  print_message(Level, Error),
		  memberchk(peer(Peer), ClientOptions),
		  close_connection(Peer, In, Out),
		  fail
	      )
	  ).
	

%%	open_client(+Message, +Queue, -Goal, -In, -Out,
%%		    +Options, -ClientOptions) is semidet.
%
%	Opens the connection to the client in a worker from the message
%	sent to the queue by accept_server/2.

open_client(requeue(In, Out, Goal, ClOpts), _, Goal, In, Out, Opts, ClOpts) :- !,
	memberchk(peer(Peer), ClOpts),
	option(keep_alive_timeout(KeepAliveTMO), Opts, 5),
	check_keep_alife_connection(In, KeepAliveTMO, Peer, In, Out).
open_client(Message, Queue, Goal, In, Out, _Opts,
	    [ pool(Queue, Goal, In, Out)
	    | Options
	    ]) :-
	catch(open_client(Message, Goal, In, Out, Options),
	      E, report_error(E)),
	memberchk(peer(Peer), Options),
	debug(http(connection), 'Opened connection from ~p', [Peer]).


open_client(Message, Goal, In, Out, Options) :-
	open_client_hook(Message, Goal, In, Out, Options), !.
open_client(tcp_client(Socket, Goal, Peer), Goal, In, Out,
	    [ peer(Peer),
	      protocol(http)
	    ]) :-
	tcp_open_socket(Socket, In, Out).

report_error(E) :-
	print_message(error, E),
	fail.


%%	check_keep_alife_connection(+In, +TimeOut, +Peer, +In, +Out) is semidet.
%
%	Wait for the client for at most  TimeOut seconds. Succeed if the
%	client starts a new request within   this  time. Otherwise close
%	the connection and fail.

check_keep_alife_connection(In, TMO, Peer, In, Out) :-
	stream_property(In, timeout(Old)),
	set_stream(In, timeout(TMO)),
	debug(http(keep_alife), 'Waiting for keep-alife ...', []),
	catch(peek_code(In, Code), E, true),
	(   var(E),			% no exception
	    Code \== -1			% no end-of-file
	->  set_stream(In, timeout(Old)),
	    debug(http(keep_alife), '\tre-using keep-alife connection', [])
	;   (   Code == -1
	    ->	debug(http(keep_alife), '\tRemote closed keep-alife connection', [])
	    ;	debug(http(keep_alife), '\tTimeout on keep-alife connection', [])
	    ),
	    close_connection(Peer, In, Out),
	    fail
	).


%%	done_worker
%
%	Called when worker is terminated due to http_workers/2.

done_worker :-
	thread_self(Self),
	thread_property(Self, status(Status)),
	retract(queue_worker(_Queue, Self)),
	print_message(informational,
		      httpd_stopped_worker(Self, Status)).


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

current_message_level(Term, Level) :-
	(   message_level(Term, Level)
	->  true
	;   Level = error
	).


%%	http_requeue(+Header)
%
%	Re-queue a connection to  the  worker   pool.  This  deals  with
%	processing additional requests on keep-alife connections.

http_requeue(Header) :-
	requeue_header(Header, ClientOptions),
	memberchk(pool(Queue, Goal, In, Out), ClientOptions),
	thread_send_message(Queue, requeue(In, Out, Goal, ClientOptions)).

requeue_header([], []).
requeue_header([H|T0], [H|T]) :-
	requeue_keep(H), !,
	requeue_header(T0, T).
requeue_header([_|T0], T) :-
	requeue_header(T0, T).

requeue_keep(pool(_,_,_,_)).
requeue_keep(peer(_)).
requeue_keep(protocol(_)).


%%	http_process(Message, Queue, +Options)
%	
%	Handle a single client message on the given stream.

http_process(Goal, In, Out, Options) :-
	debug(http(server), 'Running server goal ~p on ~p -> ~p',
	      [Goal, In, Out]),
	http_wrapper(Goal, In, Out, Connection,
		     [ request(Request)
		     | Options
		     ]),
	next(Connection, Request).

next(spawned, _) :- !,
	debug(http(spawn), 'Handler spawned', []).
next(Connection, Request) :-
	downcase_atom(Connection, 'keep-alive'), !,
	http_requeue(Request).
next(_, Request) :-
	http_close_connection(Request).


%%	http_close_connection(+Request)
%
%	Close connection associated to Request.  See also http_requeue/1.

http_close_connection(Request) :-
	memberchk(pool(_Queue, _Goal, In, Out), Request),
	memberchk(peer(Peer), Request),
	close_connection(Peer, In, Out).

%%	close_connection(+Peer, +In, +Out)
%	
%	Closes the connection from the server to the client.  Errors are
%	currently silently ignored.

close_connection(Peer, In, Out) :-
	debug(http(connection), 'Closing connection from ~p', [Peer]),
	catch(close(In, [force(true)]), _, true),
	catch(close(Out, [force(true)]), _, true).

%%	http_spawn(:Goal, +Options) is det.
%
%	Continue this connection on a  new   thread.  A handler may call
%	http_spawn/2 to start a new thread that continues processing the
%	current request using Goal. The original   thread returns to the
%	worker pool for processing new requests.   Options are passed to
%	thread_create/3, except for:
%	
%	    * pool(+Pool)
%	    Interfaces to library(thread_pool), starting the thread
%	    on the given pool.
%	    * backlog(+MaxBacklog)
%	    Reply using a 503 (service unavailable) if too many requests
%	    are waiting in this pool.

http_spawn(Goal, Options) :-
	strip_module(Goal, M, G),
	spawn(M:G, Options) .

spawn(Goal, Options) :-
	select_option(pool(Pool), Options, Options1), !,
	select_option(backlog(BackLog), Options1, ThreadOptions, infinite),
	check_backlog(BackLog, Pool),
	current_output(CGI),
	thread_create_in_pool(Pool,
			      wrap_spawned(CGI, Goal), Id,
			      [ detached(true)
			      | ThreadOptions
			      ]),
	http_spawned(Id).
spawn(Goal, Options) :-
	current_output(CGI),
	thread_create(wrap_spawned(CGI, Goal), Id,
		      [ detached(true)
		      | Options
		      ]),
	http_spawned(Id).

wrap_spawned(CGI, Goal) :-
	set_output(CGI),
	http_wrap_spawned(Goal, Request, Connection),
	next(Connection, Request).

%%	check_backlog(+MaxBackLog, +Pool)
%
%	Check whether the backlog in the pool  has been exceeded. If so,
%	reply as =busy=, which causes an HTTP 503 response.

check_backlog(BackLog, Pool) :-
	integer(BackLog),
	thread_pool_property(Pool, backlog(Waiting)),
	Waiting > BackLog, !,
	throw(http_reply(busy)).
check_backlog(_, _).


		 /*******************************
		 *	      MESSAGES		*
		 *******************************/

:- multifile
	prolog:message/3.

prolog:message(httpd_stopped_worker(Self, Status)) -->
	[ 'Stopped worker ~p: ~p'-[Self, Status] ].


		 /*******************************
		 *	      XREF		*
		 *******************************/

:- multifile
	prolog:meta_goal/2.
:- dynamic
	prolog:meta_goal/2.

prolog:meta_goal(http_server(G, _), [G+1]).
prolog:meta_goal(http_current_server(G, _), [G+1]).
