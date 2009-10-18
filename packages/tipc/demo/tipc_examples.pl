:- use_module(library(tipc/tipc)).
:- use_module(library(unix)).


/*
*   An example of a connectionless transaction oriented service
*/

ping :-
	tipc_socket(S, dgram),
	tipc_bind(S, name_seq(18888,17,17), scope(zone)),
	ping_loop(S).

ping_loop(S) :-
	tipc_receive(S, Data, From, [as(codes)]),
%	format('from: ~w data: ~s~n', [From, Data]),
	tipc_send(S, Data, From, []),
	ping_loop(S).


test :-
	tipc_service_exists(name_seq(18888,17,17)),
	format('sending 1 million pings...~n', []),
	tipc_socket(S, dgram),
	tipc_send(S, "now is the time for all  good men...aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", name(18888,17,0), []),
	tipc_receive(S, Data, From, [as(codes)]),   % From, will be his port-id. Use it from now on
	forall(between(1,1000000, _X),
	       (
    	       tipc_send(S, Data, From, []),
	       tipc_receive(S, Data, From, [as(codes)])
	       )),

	tipc_close_socket(S).

test :-
      format('you must do a "start_ping", first~n'),
      !, fail.

start_ping :-
	thread_create(ping, _, [alias(ping_server), detached(true)]).

/*
*   An example of a connection oriented byte stream stream service
*/

server(S, In, Out) :-
	tipc_socket(S, stream),
	tipc_bind(S, name(20000,21,0), scope(zone)),
	tipc_listen(S, 5),
	tipc_open_socket(S, Accept, _),
	call_cleanup(dispatch(Accept, In, Out), tipc_close_socket(S)).


dispatch(Accept, In, Out) :-
	tipc_accept(Accept, Socket, Peer),
	format('peer: ~w~n', [Peer]),
	tipc_open_socket(Socket, In, Out),
	(   loopback(In, Out);
	writeln('dispatch exiting...')).

loopback(In, Out) :-
	catch(\+at_end_of_stream(In), Error, (writeln(Error), fail)),
	read_pending_input(In, Codes, []),
	format(Out, '~s', [Codes]),
	flush_output(Out),
	loopback(In, Out).

/*
* everything that you write on Out will appear as input on In
*/

client(S, In,Out) :-
	tipc_service_exists(name(20000,21,0)),
	format('everything that you write on Out will appear on In~n', []),
	tipc_socket(S, stream),
	tipc_connect(S, name(20000,21,0)),
	tipc_open_socket(S, In, Out).

client(_S, _In, _Out) :-
	format('you must do a "start_server/0", first~n'),
	fail.

start_server :-
	thread_create(server(_S, _In, _Out), _, [alias(loopback_server), detached(true)]).
/*
* An example of a connectionless datagram service using a multi-cast
* service topology
*/

server_mcast(Lower, Upper) :-
	tipc_socket(S, rdm),
	tipc_bind(S,name_seq(20001, Lower, Upper), scope(zone)),

	call_cleanup(forall(repeat,
	      (
	      tipc_receive(S, Data, From, [as(codes)]),
	      thread_self(Self),
	      format('thread: ~w rcvd: ~s from: ~w~n', [Self, Data, From])
	      )), tipc_close_socket(S)).

start_mcast :-
	thread_create(server_mcast(0,99), _, [alias(first), detached(true)]),
	thread_create(server_mcast(100,199), _, [alias(second), detached(true)]),
	thread_create(server_mcast(200,299), _, [alias(third), detached(true)]),
	thread_create(server_mcast(300,399), _, [alias(fourth), detached(true)]).

/*
* Specify which servers are to receive the message by varying Lower
* and Upper.
*/

client_mcast(Lower, Upper) :-
	tipc_service_exists(name_seq(20001, 0, 400)),
	tipc_socket(S, rdm),
	format(codes(Msg), 'message to {~d, ~d, ~d}', [18888, Lower, Upper]),
	tipc_send(S, Msg, mcast(20001, Lower, Upper), []),
	tipc_close_socket(S), !.

client_mcast( _Lower, _Upper) :-
	format('you must start the servers via "start_mcast/0", first.~n'),
	fail.


/*
* Here's a more complex usage of the subscription service
* A collection of connection-oriented byte-stream services is started,
* then a client uses the topology server to talk to each one, in turn.
*/

bind_server(name_seq(Service, Lower, Upper), _Timeout) :-
	tipc_socket(S, seqpacket),
	tipc_bind(S, name_seq(Service, Lower, Upper), scope(node)),
	tipc_get_name(S, PortId),
	thread_self(Myself),
	format('~w: ~w~n', [Myself, PortId]),

	tipc_listen(S, 5),
	tipc_open_socket(S, AcceptFd, _),
	tipc_accept(AcceptFd, S1, From),
	tipc_open_socket(S1, In, Out),
	format('~w: connected and waiting for data~n', [Myself]),
	\+at_end_of_stream(In),
	read_pending_input(In, Data, []),

	format('~w: received: "~s" from: ~w~n', [Myself, Data, From]),
	format(Out, 'goodbye from ~w', [Myself]),

	close(In),
	close(Out),
	tipc_close_socket(S).

new_server(_Alias, NameSeq, Timeout) :-
	thread_create(bind_server(NameSeq, Timeout), _, [ detached(true)]).

start_subscription_servers :-
	new_server(server1, name_seq(20002, 6, 53), 20),
	new_server(server2, name_seq(20002, 3, 5), 25),
	new_server(server3, name_seq(20002, 54, 55), 30),
 	new_server(server4, name_seq(20002, 56,60), 35),

	format('now use "tipc-config -nt=20002" to see the name table~n').


send_greetings(PortId) :-
       tipc_socket(S1, seqpacket),
       tipc_connect(S1, PortId),
       tipc_open_socket(S1, In1, Out1),

       format(Out1, "hello", []),
       flush_output(Out1),

       \+at_end_of_stream(In1),
       read_pending_input(In1, Codes, []),
       format('client rvcd: ~s~n', [Codes]),

       close(In1),
       close(Out1).

%
%

greetEm(published(_NameSeq, PortId)) :-
	send_greetings(PortId).

greetEm(X) :-
	writeln(X).

subscribe_demo :-       % new hotness
	tipc_service_exists(name_seq(20002, 0, 100)),
	tipc_service_port_monitor([name_seq(20002, 0, 100)], greetEm, 0.5), !.

subscribe_demo :-
	format('you must start the servers using "start_subscription_servers/0", first.~n'),
	fail.
/*
*/

% TIPC quicksort by Rosenwald, from Keysey, from Bratko, (with
% apologies to Hoare!)
%
% A really good example of how NOT to do this sort of thing. It does
% however, have the advantage of creating lots of worker
% processes, allowing us to observe TIPC's distribution of client
% requests among a collection of servers.

lesser(X, Y) :- Y @< X.
/*
*  The original algorithm
*/
quicksort([], []).

quicksort([Pivot | More], Sorted) :-
	partition(lesser(Pivot), More, Smalls, Bigs),
	quicksort(Smalls, SortedSmalls),
	quicksort(Bigs, SortedBigs),
	append(SortedSmalls, [Pivot | SortedBigs], Sorted).
/*
* This is the same algorithm, decomposed into a multi-server
* parallel form.
*
* CAUTION: This is a toy application that can be broken quite easily.
*/
portray(term(X)) :- writeq(X), writeln(.).

tipc_quicksort([], []) :- !.

tipc_quicksort(Data, Sorted) :-
%	tipc_service_exists(name(20003,23,0)),
	tipc_socket(S, stream),
	catch(tipc_connect(S, name(20003,23,0)), Err,
	      (writeln(Err), tipc_close_socket(S), !, fail)),
	tipc_open_socket(S, In, Out),
	print(Out, term(Data)),    % output a readable term
	close(Out),
	read(In, Sorted),
	close(In), !.

tipc_quicksort(_, _) :-
	format('you must start the servers using "start_quicksort_server/0", first~n'),
	fail.
/*
*/


start_quicksort_server :-
	tipc_service_exists(name(20003,23,0)),
	format('service already exists~n'), !.

start_quicksort_server :-
	forall(between(1,16, _),
	       thread_create(tipc_quicksort_server, _, [detached(true)])),
	format('do a "tipc-config -nt=20003", and you will see 16 servers bound to name(20003,23,0)~n').

tipc_quicksort_server :-
	tipc_socket(S, stream),
	tipc_bind(S, name(20003,23,0), scope(node)),
	tipc_listen(S, 5),
	tipc_open_socket(S, AcceptFd, _),
	call_cleanup(dispatch(AcceptFd),    % doesn't exit, except on error
		tipc_close_socket(S)).

dispatch(AcceptFd) :-
       tipc_accept(AcceptFd, S1, _Peer),
       tipc_qs_daemon(S1),
       dispatch(AcceptFd).


tipc_qs_daemon(S1) :-
	tipc_open_socket(S1, In, Out),
	read(In, [Pivot | Data]),
	close(In),

	partition(lesser(Pivot), Data, Smalls, Bigs),

	tipc_quicksort(Smalls, SortedSmalls),
	tipc_quicksort(Bigs, SortedBigs),

	append(SortedSmalls, [Pivot | SortedBigs], Sorted),
	print(Out, term(Sorted)),
	close(Out).
/*
*
*/
server_half(S) :-
	tipc_receive(S, Data, From, [as(codes)]),
	format('server received: ~s from: ~w~n', [Data, From]),
	sleep(1),
	tipc_send(S, "goodbye!", From, []),
	tipc_close_socket(S).

child_half(S1) :-
	tipc_get_name(S1, PortId),
	tipc_close_socket(S1),

	tipc_socket(S, rdm),
	tipc_send(S, "hello", PortId, []),
	tipc_receive(S, Data, From, [as(codes)]),
	format('child_received: ~s from: ~w~n', [Data, From]),
        tipc_close_socket(S),
	halt.

two_way_fork_demo(ExitStatus) :-
	tipc_socket(S, rdm),
	fork(Pid),
	(   (Pid == child) -> child_half(S); (server_half(S), wait(Pid, ExitStatus))).

server_half1(S) :-
	repeat,
	   tipc_receive(S, Data, _From, [as(codes)]),
	   format('server receives: ~s~n', [Data]),
	Data == "end_of_file",
	tipc_close_socket(S).


child_half1(S) :-
	tipc_get_name(S, PortId),
	forall(between(1,2000, X),
	       (
	       format(codes(Data), 'message ~d', [X]),
	       tipc_send(S, Data, PortId, [])
	       )),
	tipc_send(S, "end_of_file", PortId, []),
	tipc_close_socket(S),
	halt.


one_way_fork_demo(ExitStatus) :-
	tipc_socket(S, dgram),
	fork(Pid),
	(   (Pid == child) -> child_half1(S);
	      (process_wait(Pid, ExitStatus), server_half1(S))), !.











