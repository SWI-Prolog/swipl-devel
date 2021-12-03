/*  Part of SWI-Prolog

    WWW:           http://www.swi-prolog.org
    Copyright (c)  2021, SWI-Prolog Solutions b.v.
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

:- module(sicstus4_sockets,
	  [ socket_client_open/3,
	    socket_server_open/2,
	    socket_server_open/3,
	    socket_server_accept/4,
	    socket_server_close/1,
	    socket_select/7,
	    current_host/1
	  ]).

:- reexport('../sicstus/sockets',
	    [ current_host/1
	    ]).

:- use_module(library(lists)).
:- use_module(library(socket), [tcp_connect/3]).

:- multifile sicstus4:rename_module/2.

sicstus4:rename_module(sockets, sicstus4_sockets).

/** <module> SICStus 4-compatible library(sockets).

@tbd	This library is incomplete.
	Some predicates don't fully support all options available on SICStus.
	See the documentation for individual predicates for details.

@see	https://sicstus.sics.se/sicstus/docs/4.6.0/html/sicstus.html/lib_002dsockets.html
*/

sicstus_address_to_swi(Address, Address) :- var(Address), !.
sicstus_address_to_swi(inet(Nodename, Servname), SwiAddress) :- !,
	sicstus_address_to_swi(Nodename:Servname, SwiAddress).
sicstus_address_to_swi('':Servname, Servname) :- !.
sicstus_address_to_swi(Address, Address).

% The following options are not supported yet:
% * eof_action(Action)
% * eol(Eol)
sicstus_parse_stream_options(Options, [type(Type), encoding(Encoding)]) :-
	(   selectchk(type(Type), Options, Options1)
	->  true
	;   Type = text,
	    Options1 = Options
	),
	(   selectchk(encoding(Encoding), Options1, Options2)
	->  true
	;   Encoding = iso_latin_1,
	    Options2 = Options1
	),
	% Check that no unsupported options were passed
	Options2 == [].

sicstus_apply_stream_options(Stream, ParsedOptions) :-
	maplist(set_stream(Stream), ParsedOptions).

socket_client_open(Addr, Stream, Options) :-
	sicstus_address_to_swi(Addr, SwiAddr),
	sicstus_parse_stream_options(Options, ParsedOptions),
	tcp_connect(SwiAddr, Stream, []),
	sicstus_apply_stream_options(Stream, ParsedOptions).

sicstus_address_handle_loopback(Address, false, Address) :- !.
sicstus_address_handle_loopback(Servname, true, localhost:Servname) :- var(Servname), !.
sicstus_address_handle_loopback(_Nodename:Servname, true, localhost:Servname) :- !.
sicstus_address_handle_loopback(Servname, true, localhost:Servname) :- !.

sicstus_server_address_to_swi(Address, Loopback, SwiAddress) :-
	sicstus_address_to_swi(Address, TempAddress),
	sicstus_address_handle_loopback(TempAddress, Loopback, SwiAddress).

% The following options are not supported yet:
% * numeric_nodename(Bool)
% * numeric_servname(Bool)
socket_server_open(Addr, ServerSocket) :- socket_server_open(Addr, ServerSocket, []).
socket_server_open(Addr, ServerSocket, Options) :-
	(   selectchk(reuseaddr(ReuseAddr), Options, Options1)
	->  (ReuseAddr == true ; ReuseAddr == false)
	;   ReuseAddr = false,
	    Options1 = Options
	),
	(   selectchk(loopback(Loopback), Options1, Options2)
	->  (Loopback == true ; Loopback == false)
	;   Loopback = false,
	    Options2 = Options1
	),
	sicstus_server_address_to_swi(Addr, Loopback, SwiAddr),
	% Check that no unsupported options were passed
	Options2 == [],
	tcp_socket(SocketId),
	(   ReuseAddr == true
	->  tcp_setopt(SocketId, reuseaddr)
	;   true
	),
	tcp_bind(SocketId, SwiAddr),
	tcp_listen(SocketId, 5),
	tcp_open_socket(SocketId, ServerSocket).

socket_server_accept(ServerSocket, Client, Stream, StreamOptions) :-
	sicstus_parse_stream_options(StreamOptions, ParsedStreamOptions),
	tcp_accept(ServerSocket, ClientSocket, Client),
	tcp_open_socket(ClientSocket, Stream),
	sicstus_apply_stream_options(Stream, ParsedStreamOptions).

socket_server_close(ServerSocket) :- close(ServerSocket).

sicstus_timeout_to_swi(off, infinite).
sicstus_timeout_to_swi(Seconds:Microseconds, N) :-
	number(Seconds),
	number(Microseconds),
	N is Seconds + Microseconds / 1000000,
	N > 0.
sicstus_timeout_to_swi(N, N) :-
	number(N),
	N > 0.

:- use_module(library(lists), [list_to_set/2, intersection/3]).
% On SICStus, the input lists can contain not just streams/sockets,
% but also pairs of the form Identifier-Stream, where Identifier may be
% an arbitrary term. If a stream from such a pair is ready, the entire
% pair is returned in the corresponding ready list(s). This allows the
% calling code to tell apart different ready streams more easily.
% This emulation currently does *not* support this feature
% and only accepts and returns bare streams.
socket_select(ServerSockets, SReady, ReadStreams, RReady, WriteStreams, WReady, Timeout) :-
	sicstus_timeout_to_swi(Timeout, SwiTimeout),
	append([ServerSockets, ReadStreams, WriteStreams], AllStreamsDup),
	list_to_set(AllStreamsDup, AllStreams),
	wait_for_input(AllStreams, ReadyList, SwiTimeout),
	intersection(ServerSockets, ReadyList, SReady),
	intersection(ReadStreams, ReadyList, RReady),
	intersection(WriteStreams, ReadyList, WReady).
