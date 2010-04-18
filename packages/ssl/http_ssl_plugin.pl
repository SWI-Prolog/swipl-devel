/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2007, University of Amsterdam

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
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(http_ssl_plugin, []).
:- use_module(library(ssl)).
:- use_module(thread_httpd).

:- multifile
	thread_httpd:make_socket_hook/3,
	thread_httpd:accept_hook/2,
	thread_httpd:open_client_hook/5,
        http:http_protocol_hook/7.


thread_httpd:make_socket_hook(Port, Options0, Options) :-
	memberchk(ssl(SSLOptions), Options0), !,
	ssl_init(SSL, server, [port(Port)|SSLOptions]),
	atom_concat('httpsd@', Port, Queue),
	Options = [ queue(Queue),
		    ssl_instance(SSL)
		  | Options0
		  ].

thread_httpd:accept_hook(Goal, Options) :-
	memberchk(ssl_instance(SSL), Options), !,
	memberchk(queue(Queue), Options),
	repeat,
	  catch(ssl_accept(SSL, Client, Peer), E,
		(   print_message(error, E),
		    fail
		)),
	  debug(http(connection), 'New HTTPS connection from ~p', [Peer]),
	  thread_send_message(Queue, ssl_client(SSL, Client, Goal, Peer)),
	fail.

thread_httpd:open_client_hook(ssl_client(SSL, Client, Goal, Peer),
			      Goal, In, Out,
			      [peer(Peer), protocol(https)]) :-
	ssl_open(SSL, Client, In, Out).

http:http_protocol_hook(https, Parts, PlainIn, PlainOut, In, Out, Options):-
        memberchk(host(Host), Parts),
        option(port(Port), Parts, 443),
        ssl_context(client, SSL, [host(Host), port(Port), close_parent(true)|Options]),
        catch(ssl_negotiate(SSL, PlainIn, PlainOut, In, Out),
              Exception,
              ( ssl_exit(SSL), throw(Exception)) ).
