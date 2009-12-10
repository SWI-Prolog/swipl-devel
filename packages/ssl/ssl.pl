/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2004, SWI-Prolog Foundation

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

:- module(ssl,
	  [ ssl_context/3,		% +Role, +Options, -Config
            ssl_init/3,                 % -Config, +Role, +Options
            ssl_accept/3,               % +Config, -Socket, -Peer
            ssl_open/3,                 % +Config, -Read, -Write
            ssl_open/4,                 % +Config, +Socket, -Read, -Write
            ssl_negotiate/5,            % +Config, +PlainRead, +PlainWrite, -SSLRead, -SSLWrite
	    ssl_exit/1			% +Config
	  ]).

:- use_foreign_library(foreign(ssl4pl)).

/*
  These predicates are here to support backward compatability with the previous
  incarnation of the SSL library. No changes should be required for legacy code.
*/

ssl_init(SSL, server, Options):-
        memberchk(port(Port), Options),
        tcp_socket(Socket),
        tcp_bind(Socket, Port),
        tcp_listen(Socket, 5),
        ssl_context(server, Options, SSL),
        Socket = '$socket'(S),
        ssl_put_socket(SSL, S).

ssl_init(SSL, client, Options):-
        memberchk(port(Port), Options),
        memberchk(host(Host), Options),
        writeln(Host:Port),
        tcp_socket(Socket),
        tcp_connect(Socket, Host:Port),
        ssl_context(client, Options, SSL),
        Socket = '$socket'(S),
        ssl_put_socket(SSL, S).


ssl_accept(SSL, Socket, Peer):-
        ssl_get_socket(SSL, S),
        tcp_accept('$socket'(S), Socket, Peer).

ssl_open(SSL, Socket, In, Out):-
        tcp_open_socket(Socket, Read, Write),
        ssl_negotiate(SSL, Read, Write, In, Out).

ssl_open(SSL, In, Out):-
        ssl_get_socket(SSL, S),
        tcp_open_socket('$socket'(S), Read, Write),
        ssl_negotiate(SSL, Read, Write, In, Out).