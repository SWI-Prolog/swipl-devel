/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2002, University of Amsterdam

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

:- module(http_test,
	  [ http_server/2		% :Goal, +Options
	  ]).
:- use_module(http_wrapper).

:- meta_predicate
	http_server(:, +),
	server_loop(:, +).

%	http_server(:Goal, +Options)
%	
%	Start the server from inetd. This is really easy as user_input
%	is connected to the HTTP input and user_output is the place to
%	write our reply to.  Options:
%	
%	    after(:Goal)	% Run Goal on the request after reply 

http_server(Goal, Options) :-
	prompt(_, ''),
	set_stream(user_output, buffer(full)),
	set_stream(user_input, buffer(full)),
	server_loop(Goal, Options).

server_loop(_, _) :-
	at_end_of_stream(user_input), !,
	halt.
server_loop(Goal, Options) :-
	http_wrapper(Goal, user_input, user_output, Connection,
		     request(Request)), !,
	(   memberchk(after(After), Options)
	->  call(After, Request)
	;   true
	),
	(   downcase_atom(Connection, 'keep-alive')
	->  server_loop(Goal, Options)
	;   halt
	).
server_loop(_, _) :-			% wrapper failed
	halt.
