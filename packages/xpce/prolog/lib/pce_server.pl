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

:- module(pce_server,
	  [ pce_server/1
	  ]).

:- meta_predicate(pce_server(:)).

:- use_module(library(pce)).
:- require([ atom_to_term/3
	   , strip_module/3
	   , term_to_atom/2
	   ]).

%	pce_server(+Atom|Int)
%
%	Create a PCE socket and interpret incomming lines as Prolog goals.
%	The argument is:
%	
%		# Atom
%		Unix-domain socket.  Atom is used as filename
%		# Integer
%		Internet socket.  Integer is the port number (> 5000)
%
%	Output send to current_output is send to the client, as are error
%	messages

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
	;   (   catch(atom_to_term(CommandAtom, Term, Bindings), _, fail)
	    ->  (   Term == exit
		->  send(Socket, close)
		;   current_output(Old),
		    pce_open(Socket, append, SockStream),
		    set_output(SockStream),
		    (   catch(call(Module:Term), E, true)
		    ->  flush_output,
		        (   var(E)
			->  write_bindings(Bindings, Socket),
			    send(Socket, format, 'yes\n%s', Prompt)
			;   message_to_string(E, Message),
			    send(Socket, format, 'ERROR: %s\n%s',
				 Message, Prompt)
			)
		    ;   flush_output,
		        send(Socket, format, 'no\n%s', Prompt)
		    ),
		    set_output(Old),
		    close(SockStream)
		)
	    ;   send(Socket, format, 'Syntax error\n%s', Prompt)
	    )
	).


write_bindings([], _) :-
	flush_output.
write_bindings([Name = Value|Rest], Socket) :-
	format('    ~w = ~p~n', [Name, Value]),
	write_bindings(Rest, Socket).
