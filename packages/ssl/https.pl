/*  $Id$

    Part of SWI-Prolog

    Author:  Jan Wielemaker
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/SWI-Prolog/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2004 SWI, University of Amsterdam. All rights
    reserved.
*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Demo using the threaded HTTP library with SSL.  

URL:	https://localhost:1443/env
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- load_files([ '../http/demo_body',
		library('http/thread_httpd')
	      ],
	      [ silent(true)
	      ]).

server :-
	server(1443, []).

server(Port, Options) :-
	http_server(reply,
		    [ port(Port),
		      timeout(20),
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

tm :-
	prolog_ide(thread_monitor).

get_cert_verify(_SSL, Certificate, Error) :-
	format('Handling detailed certificate verification~n'),
	format('Certificate: ~w, error: ~w~n', [Certificate, Error]),
	format('Server accepts the client certificate~n').
