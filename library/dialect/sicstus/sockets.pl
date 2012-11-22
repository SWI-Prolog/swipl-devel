/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2010-2012, University of Amsterdam
			      VU University Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(sicstus_sockets,
	  [ socket/2,			% +Domain, -Socket
	    socket_close/1,		% +Socket
	    socket_bind/2,		% +Socket, 'AF_INET'(+Host,+Port)
	    socket_connect/3,		% +Socket, 'AF_INET'(+Host,+Port), -Stream
	    socket_listen/2,		% +Socket, +Length
	    socket_accept/2,		% +Socket, -Stream
	    socket_accept/3,		% +Socket, -Client, -Stream
	    socket_select/5,		% +TermsSockets, -NewTermsStreams,
					% +TimeOut, +Streams, -ReadStreams
	    current_host/1,		% ?HostName
	    hostname_address/2		% ?HostName, ?HostAddress
	  ]).
:- use_module(library(socket)).
:- use_module(library(error)).
:- use_module(library(apply)).
:- use_module(library(pairs)).
:- use_module(library(lists)).

:- multifile sicstus:rename_module/2.

sicstus:rename_module(sockets, sicstus_sockets).

/** <module> SICStus compatible socket library

@tbd Our implementation does not support AF_UNIX sockets.
@see http://www.sics.se/sicstus/docs/3.7.1/html/sicstus_28.html
*/

socket(Domain, Socket) :-
	must_be(oneof(['AF_INET']), Domain),
	tcp_socket(Socket).

socket_close(Socket) :-
	tcp_close_socket(Socket).

socket_bind(Socket, Address) :-
	(   Address = 'AF_INET'(Host, Port)
	->  true
	;   type_error(socket_address, Address)
	),
	(   var(Host)
	->  gethostname(Host)
	;   true			% Warning?
	),
	tcp_bind(Socket, Port).

socket_connect(Socket, Address, StreamPair) :-
	(   Address = 'AF_INET'(Host, Port)
	->  true
	;   type_error(socket_address, Address)
	),
	tcp_connect(Socket, Host:Port),
	tcp_open_socket(Socket, Read, Write),
	stream_pair(StreamPair, Read, Write).

socket_listen(Socket, Length) :-
	tcp_listen(Socket, Length).

socket_accept(Socket, Client, StreamPair) :-
	tcp_accept(Socket, Socket2, Peer),
	peer_to_client(Peer, Client),
	tcp_open_socket(Socket2, Read, Write),
	stream_pair(StreamPair, Read, Write).

socket_accept(Socket, Stream) :-
	socket_accept(Socket, _Client, Stream).


peer_to_client(ip(A,B,C,D), Client) :-
	Parts = [A,B,C,D],
	ground(Parts), !,
	atomic_list_concat(Parts, '.', Client).
peer_to_client(ip(A,B,C,D), Client) :-
	atomic_list_concat(Parts, '.', Client),
	maplist(atom_number, Parts, Numbers),
	length(Numbers, 4), !,
	Numbers = [A,B,C,D].
peer_to_client(_, Client) :-
	domain_error(ip_address, Client).


%%	socket_select(+TermsSockets, -NewTermsStreams,
%%		      +TimeOut, +Streams, -ReadStreams) is det.
%
%	The  list  of  streams  in  Streams   is  checked  for  readable
%	characters. A stream can be any   stream  associated with an I/O
%	descriptor.  The  list  ReadStreams  returns  the  streams  with
%	readable data. socket_select/5 also waits for connections to the
%	sockets specified by TermsSockets.  This   argument  should be a
%	list of Term-Socket pairs, where Term, which can be any term, is
%	used  as  an  identifier.   NewTermsStreams    is   a   list  of
%	Term-connection(Client,Stream) pairs, where  Stream   is  a  new
%	stream open for communicating with a   process connecting to the
%	socket identified with Term, Client is   the client host address
%	(see socket_accept/3). If TimeOut is   instantiated  to off, the
%	predicate waits until something is available.  If TimeOut is S:U
%	the predicate waits at most S seconds and U microseconds. Both S
%	and U must be integers >=0. If   there is a timeout, ReadStreams
%	and NewTermsStreams are [].

socket_select(TermsSockets, NewTermsStreams, SicsTimeOut, Streams, ReadStreams) :-
	pairs_values(TermsSockets, Sockets),
	append(Sockets, Streams, AllStream),
	map_timeout(SicsTimeOut, TimeOut),
	wait_for_input(AllStream, ReadyStream, TimeOut),
	process_ready(ReadyStream, TermsSockets, NewTermsStreams, ReadStreams).

map_timeout(off, infinite) :- !.
map_timeout(S:U, Seconds) :- !,
	Seconds is S+U/1000000.
map_timeout(SicsTimeOut, _) :-
	type_error(sicstus_timeout, SicsTimeOut).

process_ready([], _, [], []).
process_ready([H|T], TermsSockets, NewTermsStreams, ReadStreams) :-
	memberchk(Term-H, TermsSockets), !,
	socket_accept(H, Client, Stream),
	NewTermsStreams = [Term-connection(Client,Stream)|NewTSTail],
	process_ready(T, TermsSockets, NewTSTail, ReadStreams).
process_ready([H|T], TermsSockets, NewTermsStreams, [H|ReadStreams]) :-
	process_ready(T, TermsSockets, NewTermsStreams, ReadStreams).


%%	current_host(-Host) is det.
%
%	True when Host is an atom that denotes the name of the host.
%
current_host(Host) :-
	gethostname(Host).

%%	hostname_address(+Host:atom, -Address:atom) is det.
%
%	True when Address is the IP address of Host.

hostname_address(Host, Address) :-
	nonvar(Host), !,
	tcp_host_to_address(Host, IP),
	peer_to_client(IP, Address).
