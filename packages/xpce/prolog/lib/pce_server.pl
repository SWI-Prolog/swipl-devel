/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

:- module(pce_server,
	  [ pce_server/1
	  ]).

:- meta_predicate
	pce_server(:).

:- use_module(library(pce)).
:- require([ atom_to_term/3
	   , strip_module/3
	   , term_to_atom/2
	   ]).

%	pce_server(+Atom|Int)
%	Create a PCE socket and interpret incomming lines as Prolog goals.
%	The argument is:
%	
%		# Atom
%		Unix-domain socket.  Atom is used as filename
%		# Integer
%		Internet socket.  Integer is the port number (> 5000)

pce_server(Address) :-
	strip_module(Address, Module, Addr),
	new(S, socket(Addr)),
	send(S, attribute, attribute(module, Module)),
	send(S, attribute, attribute(prompt, '(pce) ')),
	attach_messages(S),
	send(S, listen).


attach_messages(S) :-
	send(S, input_message,
	     message(@prolog, call_atom, @receiver, @arg1)),
	send(S, accept_message,
	     message(@arg1, format, S?prompt)).


call_atom(Socket, Command) :-
	get(Socket, module, Module),
	get(Socket, prompt, Prompt),
	get(Command, value, CommandAtom),
	(   CommandAtom == ''
	->  send(Socket, format, '\n%s', Prompt)
	;   (   atom_to_term(CommandAtom, Term, Bindings)
	    ->  (   Term == exit
		->  send(Socket, close)
		;   (   call(Module:Term)
		    ->  write_bindings(Bindings, Socket),
			send(Socket, format, 'yes\n%s', Prompt)
		    ;   send(Socket, format, 'no\n%s', Prompt)
		    )
		)
	    ;   send(Socket, format, 'Syntax error\n%s', Prompt)
	    )
	).


write_bindings([], _).
write_bindings([Name = Value|Rest], Socket) :-
	term_to_atom(Value, Atom),
	send(Socket, format, '%s = %s\n', Name, Atom),
	write_bindings(Rest, Socket).
