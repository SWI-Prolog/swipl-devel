/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1996 University of Amsterdam. All rights reserved.
*/

:- module(pce_shell,
	  [ pce_shell_command/1
	  ]).
:- use_module(library(pce)).
:- require([ concat_atom/3
	   ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
pce_shell_command(+Command(..+Arg..)
    Run an external command that is no supposed to produce output and
    wait for it to terminate.  The output of the command is captured
    in a text_buffer.  If the command fails, a view is created to show
    the output and this predicate will fail.

    This predicate is generally better then using Prolog's system/1,
    shell/1 or unix/1 as it ensures event-handling during the execution
    of the external command and presentation of possible output in a
    window rather then to the Prolog window.

    Example:

	    ...
	    pce_shell_command(lpr('-PPostscript', File)),
	    ...

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

pce_shell_command(Cmd) :-
	Cmd =.. List,
	ProcTerm =.. [process | List],
	new(P, ProcTerm),
	new(TB, text_buffer),
	send(P, input_message,
	     and(message(@arg1, translate, 13, @nil),
		 message(TB, append, @arg1))),
	send(P, record_separator, @nil),
	concat_atom(List, ' ', CmdAtom),
	send(P, report, progress, 'running %s ...', CmdAtom),
	send(P, open),
	send(P, wait),
	(   get(P, code, 0)
	->  send(P, report, done),
	    free(TB)
	;   get(P, code, Code),
	    (   atom(Code)
	    ->  send(P, report, error, 'Caught signal %s', Code)
	    ;   send(P, report, error, 'Exit status %s', Code)
	    ),
	    new(V, view(string('Output of %s', CmdAtom))),
	    send(V, text_buffer, TB),
	    send(new(D, dialog), below, V),
	    send(D, append, button(quit, message(V, destroy))),
	    send(V?frame, confirm_done, @off),
	    send(V, open),
	    fail
	).
