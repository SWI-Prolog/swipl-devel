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

:- module(emacs_gdb_mode, []).

:- use_module(library(pce)).
:- set_prolog_flag(character_escapes, false).

:- pce_begin_class(emacs_gdb_buffer, emacs_process_buffer).

variable(gdb_command,	string*,	get,	"Collected gdb command").

initialise(B, Target:file, Pid:[int]) :->
	"Create GBD buffer for name"::
	(   Pid == @default
	->  new(P, process(gdb, '-fullname', Target?name))
	;   new(P, process(gdb, '-fullname', Target?name, Pid))
	),
	send(B, send_super, initialise, P, string('*gdb-%s*', Target?name)),
	send(B, pool, gdb),
	send(B, prompt_regex, '(gdb) ').

:- pce_global(@gdb_fullname_regex,	% 26 == Ctrl-Z!
	      new(regex(string('%c%c\\([^:]+\\):\\(\\sd+\\):.*', 26, 26)))).
:- pce_global(@gdb_at_regex,
	      new(regex('at \([^:]\):\(\sd+\)'))).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
->insert_process_input is a little complicated.  As both the application
and gdb can issue various  prompts,  we   cannot  break  the  input into
records, but we have to  collect   the  gdb source-referencing commands.
Therefore, if the input contains ^Z^Z,   we start building <-gdb_command
until that string contains the newline. We   then handle the command and
send the remainder of the input to the buffer.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

insert_process_input(B, Input:string) :->
	"Trap input from gdb"::
	get(B, gdb_command, CmdString),
	(   CmdString \== @nil
	->  send(CmdString, append, Input),
	    (	get(CmdString, index, 10, EOL)
	    ->	get(CmdString, sub, 0, EOL, CmdLine),
		get(CmdString, sub, EOL, RestInput),
		send(B, gdb_command, CmdLine),
		send(B, slot, gdb_command, @nil),
		send(B, insert_process_input, RestInput)
	    ;	true
	    )
	;   get(Input, index, 26, SOC)
	->  get(Input, sub, 0, SOC, PreInput),
	    send(B, send_super, insert_process_input, PreInput),
	    get(Input, sub, SOC, CmdLine),
	    send(B, slot, gdb_command, CmdLine),
	    send(B, insert_process_input, string(''))
	;   send(B, send_super, insert_process_input, Input)
	).
	    
gdb_command(B, CmdLine:string) :->
	(   send(@gdb_fullname_regex, match, CmdLine),
	    send(B, show_match, @gdb_fullname_regex, CmdLine)
	->  true
	;   true
	).


show_match(B, Re:regex, Input) :->
	"Show position of match"::
	get(Re, register_value, Input, 1, P0),
	get(Re, register_value, Input, 2, L0),
	(   send(P0, prefix, /)
	->  get(@pce, convert, P0, file, Path)
	;   new(Path, file(string('%s/%s', B?directory?path, P0)))
	),
	get(@pce, convert, L0, int, Line),
	new(B2, emacs_buffer(Path)),
	send(B2, open),
	send(B2?editors?head, select_line, Line).

:- pce_end_class.
