/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2010-2012, University of Amsterdam
                              VU University Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
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
