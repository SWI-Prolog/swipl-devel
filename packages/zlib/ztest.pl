:- module(ztest,
	  [
	  ]).
:- asserta(user:file_search_path(foreign, '.')).
:- asserta(user:file_search_path(foreign, '../clib')).
:- asserta(user:file_search_path(library, '.')).
:- asserta(user:file_search_path(library, '../plunit')).
:- asserta(user:file_search_path(library, '../clib')).

:- use_module(user:library(zlib)).
:- use_module(user:library(plunit)).
:- use_module(user:library(readutil)).
:- use_module(user:library(socket)).
:- use_module(library(debug)).

read_file_to_codes(File, Codes) :-
	open(File, read, In),
	call_cleanup(read_stream_to_codes(In, Codes), close(In)).

:- begin_tests(zlib).

%	gunzip: can we read a file compressed with gzip

test(gunzip,
     [ setup(shell('gzip < ztest.pl > plunit-tmp.gz')),
       cleanup(delete_file('plunit-tmp.gz'))
     ]) :-
	gzopen('plunit-tmp.gz', read, ZIn),
	call_cleanup(read_stream_to_codes(ZIn, Codes0), close(ZIn)),
	read_file_to_codes('ztest.pl', Codes1),
	Codes0 == Codes1.
	
%	gzip: Can gunzip read our compressed file

test(gzip,
     [ cleanup(delete_file('plunit-tmp.gz'))
     ]) :-
	read_file_to_codes('ztest.pl', Codes),
	gzopen('plunit-tmp.gz', write, ZOut),
	format(ZOut, '~s', [Codes]),
	close(ZOut),
	read_file_to_codes(pipe('gunzip < plunit-tmp.gz'), Codes1),
	Codes == Codes1.

%	deflate: test read/write of deflate format

test(deflate,
     [ cleanup(delete_file('plunit-tmp.z'))
     ]) :-
	read_file_to_codes('ztest.pl', Codes),
	open('plunit-tmp.z', write, Out),
	zopen(Out, ZOut, []),
	format(ZOut, '~s', [Codes]),
	close(ZOut),
	open('plunit-tmp.z', read, In),
	zopen(In, ZIn, []),
	read_stream_to_codes(ZIn, Codes1),
	close(ZIn),
	Codes == Codes1.

%	zstream: test compressed stream flushing and processing

test(zstream) :-
	server(Port),
	debug(server, 'Server at ~w~n', [Port]),
	client(Port),
	thread_join(server, Exit),
	Exit == true.

server(Port) :-
	tcp_socket(S),
	tcp_bind(S, Port),
	tcp_listen(S, 5),
	tcp_open_socket(S, AcceptFd, _),
	thread_create(process(AcceptFd), _, [alias(server)]).

process(AcceptFd) :-
	tcp_accept(AcceptFd, S2, _Peer),
	tcp_open_socket(S2, ZIn, ZOut),
	zopen(ZIn, In, []),
	zopen(ZOut, Out, []),
	loop(In, Out),
	close(Out), close(In).

loop(In, Out) :-
	read(In, Term),
	debug(server, 'Read ~w', [Term]),
	format(Out, '~q.~n', [Term]),
	flush_output(Out),
	debug(server, 'Replied', [Term]),
	(   Term == quit
	->  true
	;   loop(In, Out)
	).

client(Port) :-
	integer(Port), !,
	client(localhost:Port).
client(Address) :-
	tcp_socket(S),
	tcp_connect(S, Address),
	tcp_open_socket(S, ZIn, ZOut),
	zopen(ZIn, In, []),
	zopen(ZOut, Out, []),
	process_client(In, Out),
	close(Out),
	close(In).

process_client(In, Out) :-
	forall(between(0, 50, X),
	       (   format(Out, '~q.~n', [X]),
		   flush_output(Out),
		   read(In, Term),
		   %put(user_error, .),
		   (   X == Term
		   ->  true
		   ;   format('Wrong reply~n'),
		       fail
		   )
	       )),
	format(Out, 'quit.~n', []).


		 /*******************************
		 *	      BIG DATA		*
		 *******************************/

test(big) :-
	forall(between(1, 5, I),
	       (   Max is 10**I,
		   big(_, Max))).

big(Port, N):- 
	tcp_socket(SockFd),
	tcp_setopt(SockFd, reuseaddr),
	tcp_bind(SockFd, Port),
	tcp_listen(SockFd, 5),
	thread_create(client_test(Port, N), Client, []),
	tcp_accept(SockFd, ClientFd, _Peer),
	tcp_open_socket(ClientFd, InStream, OutStream),
	zopen(OutStream, ZOut, [close_parent(false), format(deflate)]),
	send_data(1, N, ZOut),
	close(InStream),
	character_count(ZOut, RawCnt),
	close(ZOut),
	character_count(OutStream, CompressedCnt),
	debug(zlib, 'compressed ~d into ~d bytes~n',
	      [RawCnt, CompressedCnt]),
	close(OutStream),
	tcp_close_socket(SockFd),
	thread_join(Client, Status),
	assertion(Status == true).
	
send_data(I, N, ZOut) :-
	I =< N, !,
	format(ZOut, '~d.~n', [I]),
	I2 is I + 1,
	send_data(I2, N, ZOut).
send_data(_, _, _).


client_test(Port, N) :-
	tcp_socket(SockFd),
	tcp_connect(SockFd, localhost:Port),
	tcp_open_socket(SockFd, In, Out),
	zopen(In, ZIn, [format(deflate)]),
	get_data(ZIn, N),
	close(ZIn),
	close(Out).
	
get_data(ZIn, _) :-
	debugging(data), !,
	between(0, inf, X),
	get_byte(ZIn, C),
	(   C == -1
	->  !,
	    format('EOF at ~w~n', [X])
	;   put_byte(C),
	    fail
	).
get_data(ZIn, N) :-
	between(1, inf, X),
	read(ZIn, Term),
	(   Term == end_of_file
	->  !,
	    assertion(X =:= N + 1)
	;   assertion(Term == X),
	    fail
	).

:- end_tests(zlib).
