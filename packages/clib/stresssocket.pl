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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Stress-testing sockets, message queues and   threads. These elements are
the most important and complicated parts   of multi-threaded servers and
require intensive testing. This program performs the following steps for
each test:

	* Create a server consisting of an accepting thread and 5 workers.
	* Run the tests from test_def.
	* Destroy the server

For simple tests, get yourself a free machine and run

	?- forever.

It creates a logfile named <host>-forever.txt
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- use_module(library(socket)). :- use_module(library(debug)).

:- dynamic
	on_thread/2,
	port/1,
	worker/1.

:- discontiguous
	test_impl/1.

		 /*******************************
		 *	       FOREVER		*
		 *******************************/

forever :-
	forever(_).

forever(Test) :-
	gethostname(Host),
	concat_atom([Host, -, 'forever.txt'], Log),
	protocol(Log),
	between(1, 10000000, N),
	  get_time(T),
	  convert_time(T, S),
	  format('***** TEST RUN ~w ***** [~w]~n', [N, S]),
	  test(Test),
	fail.
forever(_).				% takes very long

		 /*******************************
		 *	       TOPLEVEL		*
		 *******************************/

test :-
	test(_).

test(T) :-
	create_server(5),
	(   test_def(T, Test),
	    (	Test = concurrent(Times, Threads, CTest)
	    ->	run(concurrent(Times, Threads, test_impl(CTest)))
	    ;	run(test_impl(Test))
	    ),
	    fail
	;   true
	),
	stop_server.

test_def(s1, echo(100)).
test_def(s2, big(100000)).
test_def(s3, big(10000, 10)).
test_def(s5, timeout).
test_def(s6, nohost).
test_def(s7, noport).
test_def(c1, concurrent(10, 3, echo(100))).
test_def(c1, concurrent(10, 3, big(10000))).


		 /*******************************
		 *		SERVER		*
		 *******************************/

create_server :-
	create_server(5).

create_server(Workers) :-
	thread_create(server(Workers), _, [alias(server)]),
	thread_get_message(ready),
	debug(tcp, 'Server started', []).
 
stop_server :-
	forall(worker(_), client(exit, _)),
	debug(tcp, 'Workers exited, prepare to join server', []),
	connect,			% make accept return
	thread_join(server).

server(Workers) :-
	create_pool(Workers),
	tcp_socket(Socket),
	tcp_bind(Socket, Port),
	assert(port(Port)),
	tcp_listen(Socket, 5),
	debug(tcp, 'Server ready on port ~w', [Port]),
	thread_send_message(main, ready),
	repeat,
	  tcp_accept(Socket, Client, _Peer),
	  debug(connect, 'Connection from ~w', [_Peer]),
	  (   worker(_)
	  ->  thread_send_message(queue, Client),
	      fail
	  ;   !,
	      tcp_close_socket(Client)
	  ),
	debug(tcp, 'All workers have gone, stopping', []),
	stop_pool,
	tcp_close_socket(Socket),
	retract(port(Port)).
 
create_pool(Number) :-
	message_queue_create(queue),   
	forall(between(1, Number, _),  
	       (   thread_create(work(queue), Id, []),
		   assert(worker(Id))
	       )).
				 
 
stop_pool :-
	forall(worker(_),
	       thread_send_message(queue, stop)),
	forall(retract(worker(Id)),
	       thread_join(Id)),
	message_queue_destroy(queue),
	debug(tcp, 'Pool stopped', []).

work(Queue) :-
	repeat,
	thread_get_message(Queue, Socket),
	handle(Socket, Data),
	Data == exit, !.
 
handle(Client, Data) :-
	tcp_open_socket(Client, In, Out),
	read(In, Data),
%	open_null_stream(Null),
%	copy_stream_data(In, Null),
%	close(Null),
	close(In),
	thread_self(Me),
	debug(data, '~p: Received ~W', [Me, Data, [max_depth(10)]]),
	(   debugging(thread)
	->  assert(on_thread(Me, Data))
	;   true
	),
	do_work(Data, Answer),
	writeq(Out, Answer),
	write(Out, '.\n'),
	debug(data, 'Sent ~W', [Answer, [max_depth(10)]]),
	flush_output(Out),
	close(Out).

thread_join(Id) :-
	thread_join(Id, Status),
	(   Status == true
	->  true
	;   format('WRONG join-status for ~w: ~w~n', [Id, Status])
	).


do_work(exit, true) :- !,
	thread_self(Me),
	debug(tcp, 'Exit worker ~w', [Me]),
	thread_detach(Me),
	retract(worker(Me)).
do_work(echo(Data), Data) :- !.
do_work(sleep(Time), true) :- !,
	sleep(Time).


		 /*******************************
		 *	      CLIENT		*
		 *******************************/

host(localhost).
 
client(Term, Reply) :-
	client(Term, Reply, 0).
client(Term, Reply, TimeOut) :-
	host(Host),
	port(Port),
	client(Host, Port, TimeOut, Term, Reply).
client(Host, Port, Timeout, Term, Reply) :-
	tcp_socket(Socket),
	tcp_connect(Socket, Host:Port),
	tcp_open_socket(Socket, In, Out),
	send_output(Term, Out),
	close(Out),
	(   Timeout == 0
	->  true
	;   set_stream(In, timeout(Timeout))
	),
	call_cleanup(read(In, Reply), close(In)),
	debug(data, 'Reply = ~W', [Reply, [max_depth(10)]]).
 
send_output(List, Out) :-
	is_list(List), !,
	send_list_output(List, Out).
send_output(Term, Out) :-
	send_term(Term, Out).

send_term(Term, Out) :-
	writeq(Out, Term),
	write(Out, '.\n').

send_list_output([], _).
send_list_output([H|T], Out) :-
	send_term(H, Out),
	send_list_output(T, Out).

echo(Term) :-
	client(echo(Term), Reply),
	assume(Term =@= Reply).

connect :-
	host(Host),
	port(Port),
	tcp_socket(Socket),
	tcp_connect(Socket, Host:Port),
	tcp_close_socket(Socket).


		 /*******************************
		 *	       TESTS		*
		 *******************************/

%	echo(Times)
%	
%	Send a lot of short messages to the server

test_impl(echo(Times)) :- !,
	forall(between(1, Times, N),
	       echo(N)).

%	big(Size)
%	
%	Send a list of 1...Size to the server

test_impl(big(Size)) :-
	findall(X, between(1, Size, X), L),
	echo(L).

%	big(Size, Times)
%	
%	Send Times lists of 1...Size to the server

test_impl(big(Size, Times)) :-
	findall(X, between(1, Size, X), L),
	forall(between(1, Times, _),
	       echo(L)).

test_impl(timeout) :-
	catch(client(sleep(2), _Reply, 1), E, true),
	assume(E = error(timeout_error(read, '$stream'(_)), _G285)).


		 /*******************************
		 *	       RUN		*
		 *******************************/

:- meta_predicate
	run(:).

run(Goal) :-
	debug(run, ' ** Run ~p ...', [Goal]),
	call_cleanup(Goal, E, cleanup(Goal, E)).

cleanup(Goal, E) :-
	debug(run, ' ** Finished ~p: ~p', [Goal, E]).


		 /*******************************
		 *	  SPECIAL TESTS		*
		 *******************************/

test_impl(nohost) :-
	tcp_socket(S),
	catch(tcp_connect(S, 'foo.bar':80), E, true),
	tcp_close_socket(S),
	assume(E =@= error(socket_error('Host not found'), _)).

test_impl(noport) :-
	tcp_socket(S),
	catch(call_with_time_limit(5, tcp_connect(S, localhost:4321)),
	      E, true),
	tcp_close_socket(S),
	assume(E = error(socket_error(_), _)).


		 /*******************************
		 *	   DISTRIBUTION		*
		 *******************************/

on_threads :-
	(   bagof(D, on_thread(Id, D), Ds),
	    length(Ds, L),
	    format('Thread ~w handled ~w~n', [Id, L]),
	    fail
	;   true
	).

clean :-
	retractall(on_thread(_, _)).


		 /*******************************
		 *	       CONCUR		*
		 *******************************/

:- meta_predicate
	concurrent(+, +, :),
	ok(:).

%	concurrent(+Times, +Threads, :Goal)
%	
%	Run Goal Times times concurrent in Threads

concurrent(Times, 1, Goal) :- !,
	forall(between(1, Times, _),
	       ok(Goal)).
concurrent(Times, Threads, Goal) :-
	strip_module(Goal, M, G),
	message_queue_create(Done),
	message_queue_create(Queue),
	create_workers(Threads, M, Queue, Done),
	forall(between(1, Times, _),
	       thread_send_message(Queue, goal(G))),
	debug(concurrent, 'Waiting for ~w replies', [Times]),
	wait_n(Times, Done),
	forall(between(1, Threads, _),
	       thread_send_message(Queue, done)),
	debug(concurrent, 'Waiting for ~w threads', [Threads]),
	wait_n(Threads, Done),
	message_queue_destroy(Queue),
	message_queue_destroy(Done),
	debug(concurrent, 'All done', []).

wait_n(0, _) :- !.
wait_n(N, Queue) :-
	thread_get_message(Queue, done),
	N2 is N - 1,
	wait_n(N2, Queue).

create_workers(N, Module, Queue, Done) :-
	N > 0, !,
	thread_create(worker(Module, Queue, Done),
		      _,
		      [ detached(true)
		      ]),
	N2 is N - 1,
	create_workers(N2, Module, Queue, Done).
create_workers(_, _, _, _).


worker(Module, Queue, Done) :-
	thread_get_message(Queue, Message),
	(   Message = goal(Goal)
	->  ok(Module:Goal),
	    thread_send_message(Done, done),
	    worker(Module, Queue, Done)
	;   thread_send_message(Done, done)
	).

ok(Goal) :-
	(   catch(Goal, E, true)
	->  (   var(E)
	    ->  true
	    ;   print_message(error, E)
	    )
	;   print_message(warning, failed(Goal))
	).

		 /*******************************
		 *	       DEBUG		*
		 *******************************/

%:- interactor.
:- debug(tcp).
%:- debug(data).
:- debug(run).
:- debug(concurrent).
