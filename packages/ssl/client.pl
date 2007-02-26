/*  $Id$

    Part of SWI-Prolog

    Author:        Jan van der Steen and Jan Wielemaker
    E-mail:        J.van.der.Steen@diff.nl and jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2002, SWI-Prolog Foundation

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


:- module(client,
	  [ client/0
	  ]).

:- use_module(library(ssl)).

client :-
	ssl_init(SSL, client,
		 [ host('localhost'),
                   port(1111),
                   cert(true),
                   peer_cert(true),
		   cacert_file('etc/demoCA/cacert.pem'),
		   certificate_file('etc/client/client-cert.pem'),
		   key_file('etc/client/client-key.pem'),
%		   password('apenoot2'),
		   pem_password_hook(get_client_pwd)
		 ]),
	client_loop(SSL),
        ssl_exit(SSL).

client_loop(SSL) :-
	ssl_open(SSL, In, Out),
	write_server(In, Out),
	write_server(In, Out),
	write_server(In, Out),
	close(In),
	close(Out).

write_server(In, Out) :-
        format(Out, 'Hello~n', ''),
        flush_output(Out),
	read_line_to_codes(In, Line),
	(   Line == end_of_file
	->  true
	;   format('Got ~s~n', [Line])
	).

user:get_client_pwd(_SSL, apenoot2) :-
	format('Returning password from client passwd hook~n').

