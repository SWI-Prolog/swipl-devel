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
