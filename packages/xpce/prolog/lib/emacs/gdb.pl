/*  $Id$ $

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

:- module(emacs_gdb_mode, []).

:- use_module(library(pce)).

:- pce_begin_class(emacs_gdb_buffer, emacs_process_buffer).

initialise(B, Target:file) :->
	"Create GBD buffer for name"::
	new(P, process(gdb, '-fullname', Target?name)),
	send(B, send_super, initialise, P, string('*gdb-%s*', Target?name)),
	send(B, pool, gdb),
	send(B, prompt_regex, '(gdb) ').

:- pce_global(@gdb_fullname_regex,	% 26 == Ctrl-Z!
	      new(regex(string('%c%c\\([^:]+\\):\\(\\sd+\\):', 26, 26)))).
:- pce_global(@gdb_at_regex,
	      new(regex('at \([^:]\):\(\sd+\)'))).

insert_process_input(B, Input:string) :->
	"Trap input from gdb"::
	(   get(Input, index, 26, I0),
	    send(@gdb_fullname_regex, match, Input, I0),
	    send(B, show_match, @gdb_fullname_regex, Input)
	->  send(Input, delete, I0)
%	;   send(@gdb_at_regex, search, Input),
%	    send(B, show_match, @gdb_at_regex, Input)
	;   true
	),
	send(B, send_super, insert_process_input, Input).


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
