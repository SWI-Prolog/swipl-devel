:- module(ciao_sockets,
	  [ connect_to_socket/3,	% Host, +Port, -Stream
	    socket_recv/2,		% +Stream, ?String
	    hostname_address/2,		% +HostName, -Address
	    socket_shutdown/2,		% +Stream, +How
	    socket_recv_code/3,		% +Stream, ?String, ?Length
	    socket_send/2,		% +Stream, +String
%	    select_socket/5,
	    socket_accept/2,		% +Sock, -Stream
	    bind_socket/3		% ?Port, +Length, -Socket
%	    connect_to_socket_type/4
	  ]).
:- use_module(library(socket)).

/** <module> CIAO Compatible socket interface

This library emulates library(sockets) from  CIAO   Prolog.  One  of the
problems we are faced with  here  is   that  CIAO  streams  appear to be
read/write, while SWI-Prolog streams are  either   input  or  output. We
solve this problem by wrapping both streams in rw_stream(Read, Write).

This implies that socket streams can only be used with the socket-stream
operations.
*/

%%	connect_to_socket(+Host, +Port, -Stream) is det,
%
%	Calls connect_to_socket_type/4 with SOCK_STREAM connection type.
%	This is the connection type you want in order to use the write/2
%	and read/2 predicates (and other stream IO related predicates).

connect_to_socket(Host, Port, rw_stream(Read, Write)) :-
	tcp_socket(Socket),
	tcp_connect(Socket, Host:Port),
	tcp_open_socket(Socket, Read, Write).


%%	hostname_address(+HostName, -Address) is det.
%
%	Translate between HostName and Address.  Address is an atom
%	of the form XX.XX.XX.XX.

hostname_address(HostName, Address) :-
	tcp_host_to_address(HostName, Addr0),
	address_to_atom(Addr0, Address).

address_to_atom(ip(A,B,C,D), Address) :-
	atomic_list_concat([A,B,C,D], '.', Address).

%%	socket_shutdown(+Stream, +How)
%
%	Shut down a duplex communication  socket   with  which Stream is
%	associated. All or part of the   communication  can be shutdown,
%	depending on the  value  of  How.   The  atoms  read,  write, or
%	read_write  should  be  used  to  denote  the  type  of  closing
%	required.

socket_shutdown(rw_stream(Read, Write), How) :-
	must_be(oneof([read,write,read_write]), How),
	(   How == read
	->  close(Read)
	;   How == write
	->  close(Write)
	;   catch(close(Write), E, true),
	    close(Read),
	    (	nonvar(E)
	    ->	throw(E)
	    ;	true
	    )
	).


%%	socket_recv_code(+Stream, ?String)
%
%	As socket_recv_code/3, but the return code is ignored.

socket_recv(Stream, String) :-
	socket_recv_code(Stream, String, _).

%%	socket_recv_code(+Stream, ?String, ?Length)
%
%	Receives a String from the  socket   associated  to  Stream, and
%	returns its Length. If Length is -1, no more data is available.

socket_recv_code(rw_stream(Read, _Write), String, Length) :-
	(   at_end_of_stream(Read)
	->  String = "",
	    Length = -1
	;   read_pending_input(Read, String, []),
	    length(String, Length)
	).


%%	socket_send(+Stream, +String)
%
%	Sends String to the socket associated  to Stream. The socket has
%	to be in connected state. String  is   not  supposed  to be NULL
%	terminated, since it is a Prolog   string.  If a NULL terminated
%	string is needed at the other  side,   it  has  to be explicitly
%	created in Prolog.

socket_send(rw_stream(_Read, Write), String) :-
	format(Write, '~s', [String]),
	flush_output(Write).

%%	bind_socket(?Port, +Length, -Socket) is det.
%
%	Returs an AF_INET Socket bound to Port (which may be assigned by
%	the OS or defined by the caller),   and  listens to it (hence no
%	listen call in this set  of   primitives).  Length specifies the
%	maximum number of pending connections.

bind_socket(Port, Length, Socket) :-
	tcp_socket(Socket),
	tcp_setopt(Socket, reuseaddr),
	tcp_bind(Socket, Port),
	tcp_listen(Socket, Length).

%%	socket_accept(+Sock, -Stream) is det.
%
%	Creates a new Stream connected to Sock.

socket_accept(Socket, rw_stream(Read, Write)) :-
	tcp_accept(Socket, Client, _Peer),
	tcp_open_socket(Client, Read, Write).
