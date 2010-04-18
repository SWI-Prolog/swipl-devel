:- use_module(library(tipc/tipc)).
:- use_module(library(time)).

:- op(950, xfy, ~>).

:- meta_predicate ~>(0,0).

~>(P, Q) :-
	setup_call_cleanup(P, (true; fail), assertion(Q)).

show_publication(Port) :-
	format('~p~n', [Port]).

start_tipc_port_monitor_thread :-
	tipc_service_port_monitor([name(1000,0,0)], show_publication, detached(_Id)).

tipc_listener :-
	tipc_socket(S, rdm) ~>
	    tipc_close_socket(S),

	tipc_bind(S, name(1000,0,0), scope(node)),

	thread_self(Self),

	tipc_get_name(S, Name),

	format('thread ~p  using ~p (~w)~n', [Self, Name, Name]),

	tipc_receive(S, Data, From, [as(atom)]),

	term_to_atom(thread(Self, replied(Data)), Atom),

	tipc_send(S, Atom, From, []), !.

%
%

send_message(S, Msg, Port) :-
	tipc_socket(S, rdm) ~>
	   tipc_close_socket(S),

	format('~nsent: ~p (~w)~n', [Port, Port]),

	tipc_send(S, Msg, Port, []).

send_timeout(S1) :-
	tipc_socket(S, rdm) ~>
	   tipc_close_socket(S),

	tipc_get_name(S1, Port),

	tipc_setopt(S, importance(critical)),

	tipc_send(S, '$timeout', Port, []), !.


get_messages(S, Data, From, Timeout) :-
	alarm(Timeout, send_timeout(S), Id) ~>
	   remove_alarm(Id),

	repeat,
	tipc_receive(S, Data, From, [as(atom)]),

	(   (Data == '$timeout') -> (!, fail); true).

%
%

make_listeners(0).

make_listeners(N) :-
	succ(N1, N),
	make_listeners(N1),
	thread_create(tipc_listener, Id, []) ~>
	   thread_join(Id, true).

test_port(Port) :-
	send_message(S, 'hello world', Port),
	forall(get_messages(S, Reply, FromPort, 0.250),
	       format('rcvd: ~p from ~p~n', [Reply, FromPort])), !.

tipc_port_test :-
	make_listeners(8),

	sleep(1.0),

	forall(tipc_service_probe(name(1000,0,0), Port),
	       test_port(Port)), !.

tipc_unicast_test :-
	make_listeners(8),

	sleep(1.0),

	forall(between(1,8, _),
	       test_port(name(1000,0,0))), !.

tipc_broadcast_test :-
	make_listeners(8),

	sleep(1.0),

	test_port(name_seq(1000,0,0)), !.


tipc_test :-
	protocol('tipc_test.txt') ~>
	   noprotocol,

	format('~n********* tipc_port_test ********~n'),

	tipc_port_test,

	threads,

	format('~n********* tipc_unicast_test ***********~n'),

	tipc_unicast_test,

	threads,

	format('~n********* tipc_broadcast_test *********~n'),

	tipc_broadcast_test,

	threads, !.

:- initialization
      tipc_initialize.



