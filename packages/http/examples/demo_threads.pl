/*  $Id$

    Part of SWI-Prolog

    Author:  Jan Wielemaker
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/SWI-Prolog/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

:- load_files([ demo_body,
		library('http/thread_httpd')
	      ],
	      [ silent(true)
	      ]).

server :-
	server(3000, []).

server(Port, Options) :-
	http_server(reply,
		    [ port(Port),
		      timeout(20)
		    | Options
		    ]).

tm :-
	prolog_ide(thread_monitor).
