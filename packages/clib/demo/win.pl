/*  $Id$

    Part of SWI-Prolog
    Designed and implemented by Jan Wielemaker

    Copyright (C) 1999 SWI, University of Amseterdam. All rights reserved.
*/

:- asserta(library_directory('.')).
:- asserta(user:file_search_path(foreign, '.')).
:- use_module(library(socket)).

		 /*******************************
		 *	  SOCKET (CLIENT)	*
		 *******************************/

get_http_data(Host, URL) :-
	tcp_socket(Socket),
	tcp_connect(Socket, Host:80),
	tcp_open_socket(Socket, Read, Write),
	format(Write, 'GET ~w~n~n', URL),
	flush_output(Write),
	copy_stream(Read, user_output),
	close(Read),
	close(Write).

copy_stream(In, Out) :-
	get0(In, C0),
	copy_stream(C0, In, Out).

copy_stream(-1, _, _) :- !.
copy_stream(C, In, Out) :-
	put(Out, C),
	get0(In, C2),
	copy_stream(C2, In, Out).


		 /*******************************
		 *	   SOCKET (SERVER)	*
		 *******************************/

:- dynamic
	server/1,
	client/2.

mkserver :-
	mkserver(3000).
mkserver(Port) :-
	tcp_socket(Socket),
	tcp_bind(Socket, Port),
	tcp_listen(Socket, 5),
	tcp_open_socket(Socket, Read, _),
	asserta(server(Read)).

dispatch :-
	repeat,
	\+ dispatch1, !.

dispatch1 :-
	findall(C, client(C, _), Clients),
	server(Server),
	wait_for_input([Server|Clients], Ready, 0),
	dispatch(Ready).

dispatch([]).
dispatch([H|T]) :-
	format('Dispatching ~w ... ', [H]), flush,
	dispatch_fd(H),
	format('ok~n'),
	dispatch(T).

dispatch_fd(Server) :-
	server(Server), !,
	tcp_accept(Server, ClientSocket, Peer),
	format('Connected from ~w~n', [Peer]),
	tcp_fcntl(ClientSocket, setfl, nonblock),
	tcp_open_socket(ClientSocket, Read, Write),
	format(Write, 'Please to meet you!~n', []),
	flush_output(Write),
	assert(client(Read, Write)).
dispatch_fd(Client) :-
	client(Client, Write),
	format(Write, 'You typed: ', []),
	copy_stream(Client, Write),
	(   at_end_of_stream(Client)
	->  format('Closing client ~w~n', [Client]),
	    close(Client),
	    close(Write),
	    retractall(client(Client, _))
	;   flush_output(Write)
	).
	
		 /*******************************
		 *		CLIENT		*
		 *******************************/
	
client :-
	client(3000).
client(Port) :-
	tcp_socket(Socket),
	tcp_connect(Socket, localhost:Port),
	tcp_open_socket(Socket, In, Out),
	format(Out, 'Hello World~n', []),
	flush_output(Out),
	copy_line(In, user_output),
	close(In),
	close(Out).

copy_line(In, Out) :-
	get0(In, C0),
	copy_line(C0, In, Out).

copy_line(10, _, _) :- !.
copy_line(C, In, Out) :-
	put(Out, C),
	get0(In, C2),
	copy_line(C2, In, Out).

