/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

:- module(emacs_server,
	  [ 
	  ]).
:- use_module(library(pce)).
:- require([ term_to_atom/2
	   ]).

:- pce_global(@emacs_server, make_emacs_server).
:- pce_global(@emacs_server_address, new(file('~/.xpce_emacs_server'))).
:- pce_global(@emacs_server_method,
	      new(chain(send_method(unlink_to, new(vector),
				    and(message(@receiver?from, free),
					message(@receiver, free))),
			send_method(unlink_from, new(vector),
				    and(message(@receiver?to, free),
					message(@receiver, free)))))).


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
	new(B, emacs_buffer(File)),
	new(W, emacs_window(B)),
	send(W, sticky_window),
	get(W, editor, Editor),
	new(H, hyper(Socket, Editor, editor, server)),
	send(H, send_method, @emacs_server_method).
server_action(Cmd, Socket) :-
	Cmd =.. [Sel|Args],
	Msg =.. [send, Socket, hyper_send, editor, Sel | Args],
	Msg.
