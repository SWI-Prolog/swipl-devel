/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker & Steve Prior
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

:- module(prolog_server,
	  [ prolog_server/2		% +Port, +Options
	  ]).

:- use_module(library(socket)).
 
%	prolog_server(?Port, +Options)
%	
%	Create a TCP/IP based server  on  the   given  Port,  so you can
%	telnet into Prolog and run an  interactive session. This library
%	is intended to provide access for   debugging  and management of
%	embedded servers.
%	
%	Currently defined options are:
%	
%		# allow(IP)
%		Allow access from IP, a term of the format ip(A,B,C,D).
%		Multiple of such terms can exist and access is granted
%		if the peer IP address unifies to one of them.  If no
%		allow option is provided access is only granted from
%		ip(127,0,0,1) (localhost).
%	
%	For example:
%	
%		?- prolog_server(4000, []).
%
%		% telnet localhost 4000
%		Welcome to the SWI-Prolog server on thread 3
%		
%		1 ?-
%
%	BUGS:		
%
%	As the connection does not involve a terminal, command history
%	and completion are not provided. Neither are interrupts
%	(Control-C).  To terminate the Prolog shell one must enter the
%	command "end_of_file."


prolog_server(Port, Options) :-
	tcp_socket(ServerSocket),
	tcp_setopt(ServerSocket, reuseaddr),
	tcp_bind(ServerSocket, Port),
	tcp_listen(ServerSocket, 5),
	thread_create(server_loop(ServerSocket, Options), _,
		      [ alias(prolog_server)
		      ]).
 
server_loop(ServerSocket, Options) :-
	tcp_accept(ServerSocket, Slave, Peer),
	tcp_open_socket(Slave, InStream, OutStream),
	tcp_host_to_address(Host, Peer),
	atom_concat('client@', Host, Alias),
	thread_create(service_client(InStream, OutStream, Peer, Options),
		      _,
		      [ alias(Alias)
		      ]),
	server_loop(ServerSocket, Options).
 
service_client(InStream, OutStream, Peer, Options) :-
	allow(Peer, Options), !,
	thread_self(Id),
	set_prolog_IO(InStream, OutStream, OutStream),
	format(user_error,
	       'Welcome to the SWI-Prolog server on thread ~w~n~n',
	       [Id]),
	run_prolog,
	close(InStream),
	close(OutStream),
	thread_detach(Id).
service_client(InStream, OutStream, _, _):-
	thread_self(Id),
	format(OutStream, 'Go away!!~n', []),
	close(InStream),
	close(OutStream),
	thread_detach(Id).


run_prolog :-
	catch(prolog, E,
	      ( print_message(error, E),
%		E = error(_, _),
		run_prolog)).


allow(Peer, Options) :-
	(   member(allow(Allow), Options)
	*-> Peer = Allow,
	    !
	;   Peer = ip(127,0,0,1)
	).
