/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2007, University of Amsterdam

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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Demo using the threaded HTTP library with SSL.  

URL:	https://localhost:1443/env

For further details on supported paths,   see demo_body.pl from the http
examples.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- load_files([ '../http/demo_body',
		library('http/thread_httpd'),
		library('http/http_ssl_plugin')
	      ],
	      [ silent(true)
	      ]).

server :-
	server(1443, []).

server(Port, Options) :-
	http_server(reply,
		    [ port(Port),
		      timeout(60),
		      ssl([ host('localhost'),
%			    cert(true),
%			    peer_cert(true),
			    cacert_file('etc/demoCA/cacert.pem'),
			    certificate_file('etc/server/server-cert.pem'),
			    key_file('etc/server/server-key.pem'),
			    cert_verify_hook(get_cert_verify),
			    password('apenoot1')
			  ])
		    | Options
		    ]).

get_cert_verify(_SSL, Certificate, Error) :-
	format('Handling detailed certificate verification~n'),
	format('Certificate: ~w, error: ~w~n', [Certificate, Error]),
	format('Server accepts the client certificate~n').
