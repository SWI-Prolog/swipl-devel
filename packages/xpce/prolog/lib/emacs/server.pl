/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
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

:- module(emacs_server,
	  [ 
	  ]).
:- use_module(library(pce)).
:- require([ file_base_name/2
	   , file_directory_name/2
	   , term_to_atom/2
	   ]).

:- pce_global(@emacs_server, make_emacs_server).
:- pce_global(@emacs_server_address, make_emacs_server_address).
:- pce_global(@emacs_server_method,
	      new(chain(send_method(unlink_to, new(vector),
				    and(message(@receiver?from, free),
					message(@receiver, free))),
			send_method(unlink_from, new(vector),
				    and(message(@receiver?to, free),
					message(@receiver, free)))))).

make_emacs_server_address(F) :-
	get(@pce, hostname, Host),
	atom_concat('~/.xpce_emacs_server.', Host, Server),
	new(F, file(Server)).


make_emacs_server(Socket) :-
	new(Socket, socket(@emacs_server_address)),
	send(Socket, input_message,
	     message(@prolog, server_action_atom, @receiver, @arg1)),
	send(Socket, send_method,
	     send_method(end_of_file, new(vector),
			 message(@receiver, free))).


server_action_atom(Socket, Action) :-
	get(Action, value, Atom),
	term_to_atom(Term, Atom),
	(   server_action(Term, Socket)
	->  true
	;   send(Socket, format, 'Request failed: %s\n', Action),
	    send(Socket, free)
	).


server_action((A,B), Socket) :- !,
	server_action(A, Socket),
	server_action(B, Socket).
server_action(edit(File), Socket) :- !,
	server_action(edit(File, []), Socket).
server_action(edit(File, Line), Socket) :- !,
	new(B, emacs_buffer(File)),
	new(W, emacs_frame(B)),
	send(W, sticky_window),
	get(W, editor, Editor),
	new(H, hyper(Socket, Editor, editor, server)),
	send(H, send_method, @emacs_server_method),
	send(B, check_modified_file),
	(   Line == []
	->  true
	;   send(W?editor, goto_line, Line)
	).
server_action(gdb(File, Pid), Socket) :- !,
	file_directory_name(File, Dir),
	file_base_name(File, Exe),
	new(X, emacs_gdb_buffer(Exe, Pid)),
	get(X, process, Process),
	(   Process \== @nil,
	    get(Process, status, inactive)
	->  send(Process, directory, Dir),
	    send(X, directory, Dir)
	;   true
	),
	new(W, emacs_frame(X)),
	get(W, editor, Editor),
	new(H, hyper(Socket, Editor, editor, server)),
	send(H, send_method, @emacs_server_method),
	send(X, start_process),
	send(X, open).
server_action(gdb(File), Socket) :- !,
	server_action(gdb(File, @default), Socket).

server_action(Cmd, Socket) :-
	Cmd =.. [Sel|Args],
	Msg =.. [send, Socket, hyper_send, editor, Sel | Args],
	Msg.
