/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    M-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

:- module(emacs_c_mode, []).
:- use_module(library(pce)).
:- require([ between/3
	   , default/3
	   , forall/2
	   , memberchk/2
	   ]).

:- emacs_begin_mode(c, language,
		    "Mode for editing C programs",
		    [ insert_c_begin	= key('{'),
		      prototype_mark	= key('\C-cRET'),
		      add_prototype	= key('\C-c\C-p'),
		      insert_NAME_	= key('\C-c\C-n'),
		      run_gdb		= button(gdb),
		      break_at_line	= button(gdb),
		      break_at_function = button(gdb),
		      gdb_go		= button(gdb) + key('\C-c\C-g'),
		      gdb_step		= button(gdb) + key('\C-c\C-s'),
		      gdb_next		= button(gdb) + key('\C-c\C-n') +
		      			  key('\C-z'),
		      gdb_print		= button(gdb) + key('\C-c\C-p'),
		      prolog_manual     = button(prolog)
		    ],
		    [ '"'  = string_quote(\),
		      '''' = string_quote(\),
		      (/)  + comment_start(*),
		      (*)  + comment_end(/),

		      paragraph_end(regex('\s *$\|/\*\|.*\*/'))
		    ]).

:- initialization
	send(@class, attribute, outline_regex_list,
	     chain(regex(string('^\\(\\w+([^)]*).*\n\\)\\({\\([^}].*\n\\)+}\\(\\s *\n\\)*\\)')),
		   regex(string('^\\(\\w+.*\n\\)\\({\\([^}].*\n\\)+};\\(\\s *\n\\)*\\)')),
		   regex(string('^\\(#\\s *define.*\\\\\\)\n\\(\\(.*\\\\\n\\)+.*\n\\)')))).

:- pce_global(@c_indent, new(number(2))).
:- pce_global(@c_undent_regex, new(regex('{\|else\|\w+:'))).

indent_line(E, Times:[int]) :->
	"Indent according to C-mode"::
	default(Times, 1, Tms),
	(   between(1, Tms, N),
	    send(E, beginning_of_text_on_line),
	    (	(   send(E, indent_close_bracket_line)
		;   send(E, indent_expression_line, ')]')
		;   send(E, indent_statement)
		;   send(E, align_with_previous_line, '\s *\({\s *\)*')
		)
	    ->	true
	    ),
	    (	N == Tms
	    ->	true
	    ;	send(E, next_line)
	    ),
	    fail
	;   true
	).


backward_skip_statement(TB, Here, Start) :-
	get(TB, skip_comment, Here, 0, H1),
	(   (	H1 == 0   
	    ;	get(TB, character, H1, C1),
		memberchk(C1, "{;}")
	    ),
	    get(TB, skip_comment, H1+1, H2),
	    \+ send(regex(else), match, TB, H2)
	->  Start = H2
	;   get(TB, scan, H1, term, -1, start, H2),
	    H3 = H2 - 1,
	    backward_skip_statement(TB, H3, Start)
	).


backward_statement(E, Here:[int], There:int) :<-
	"Find start of C-statement"::
	default(Here, E?caret, Caret),
	get(E, text_buffer, TB),
	get(TB, skip_comment, Caret, 0, H1),
	(   get(TB, character, H1, Chr),
	    memberchk(Chr, "};")
	->  get(TB, scan, H1+1, term, -1, H2a),
	    H2 is H2a - 1
	;   H2 = H1
	),
	backward_skip_statement(TB, H2, There).


backward_statement(E) :->
	"Go back one statement"::
	get(E, backward_statement, Start),
	send(E, caret, Start).


indent_statement(E) :->
	"Indent statement in { ... } context"::
	get(E, text_buffer, TB),
	get(E, caret, Caret),
	get(E, matching_bracket, Caret, '}', _), % in C-body
	get(TB, skip_comment, Caret-1, 0, P0),
	get(TB, character, P0, Chr),
	(   memberchk(Chr, ";}")	% new statement
	->  get(E, backward_statement, P0+1, P1),
	    back_prefixes(E, P1, P2),
	    get(E, column, P2, Col),
	    send(E, align_line, Col)
	;   memberchk(Chr, "{")		% first in compound block
	->  get(E, column, P0, Col),
	    send(E, align, Col + @c_indent)
	;   \+ memberchk(Chr, ";,"),	% for, while, if, ...
	    (   get(TB, matching_bracket, P0, P1)
	    ->  true
	    ;   P1 = P0
	    ),
	    get(TB, scan, P1, word, 0, start, P2),
	    back_prefixes(E, P2, P3),
	    get(E, column, P3, Column),
	    (   send(@c_undent_regex, match, TB, Caret)
	    ->  send(E, align, Column)
	    ;   send(E, align, Column + @c_indent)
	    )
	).


back_prefixes(E, P0, P) :-
	get(E, text_buffer, TB),
	get(TB, scan, P0, line, 0, start, SOL),
	(   get(regex('\s(\|:'), search, TB, P0, SOL, P1)
	->  P2 is P1 + 1
	;   P2 = SOL
	),
	get(TB, skip_comment, P2, P0, P).


insert_c_begin(E, Times:[int], Id:[event_id]) :->
	"Insert and adjust the inserted '{'"::
	send(E, insert_self, Times, Id),
	get(E, caret, Caret),
	get(E, text_buffer, TB),
	get(TB, scan, Caret, line, 0, start, SOL),
	(   send(regex(string('\\s *%c', Id)), match, TB, SOL, Caret)
	->  new(F, fragment(TB, Caret, 0)),
	    send(E, indent_line),
	    send(E, caret, F?start),
	    free(F)
	;   true
	).


		 /*******************************
		 *	   PROTOPTYPES		*
		 *******************************/

:- pce_global(@emacs_makeproto, make_makeproto).

make_makeproto(P) :-
	new(P, process(makeproto)),
	send(P, open).


prototype_mark(E) :->
	"Make a mark for local prototype definitions"::
	get(E, caret, Caret),
	get(E, text_buffer, TB),
	send(TB, attribute, attribute(prototype_mark_location,
				      fragment(TB, Caret, 0))),
	send(E, report, status, 'Prototype mark set').

add_prototype(E) :->
	"Add prototype for pointed function"::
	get(E, caret, Caret),
	get(E, text_buffer, TB),
	get(TB, scan, Caret, paragraph, 0, start, SOH),
	get(TB, find, SOH, '{', 1, end, EOH),
	get(TB, contents, SOH, EOH-SOH, Header),
	send(@emacs_makeproto, format, '%s\n}\n', Header),
	send(@pce, format, '%s\n', Header),
	get(TB, prototype_mark_location, Fragment),
	get(Fragment, end, End),
	forall(get(@emacs_makeproto, read_line, 100, Prototype),
	       send(Fragment, insert, @default, Prototype)),
	send(E, caret, End).
	
	
		 /*******************************
		 *	    GDB SUPPORT		*
		 *******************************/

run_gdb(M, Cmd:file) :->
	"Run GDB on program (same as ->gdb)"::
	send(M, gdb, Cmd).

tell_gdb(M, Fmt:char_array, Args:any ...) :->
	"Send a command to gdb"::
	get(@emacs?buffers, find_all,
	    and(message(@arg1, instance_of, emacs_gdb_buffer),
		@arg1?process?status == running),
	    ActiveGdbBuffers),
	(   get(ActiveGdbBuffers, size, 1)
	->  get(ActiveGdbBuffers, head, Buffer),
	    Msg =.. [format_data, Fmt|Args],
	    send(Buffer, Msg)
	;   send(M, report, warning, 'No or multiple GDB buffers')
	).

break_at_line(M) :->
	"Set GDB break point at line of caret"::
	get(M, line_number, M?caret, CaretLine),
	get(M?text_buffer, file, File),
	get(File, base_name, Base),
	send(M, tell_gdb, 'break %s:%d\n', Base, CaretLine).

break_at_function(M, Function:[name]) :->
	"Set GDB breakpoint at function"::
	get(M, expand_tag, Function, TheFunction),
	send(M, tell_gdb, 'break %s\n', TheFunction).

gdb_go(M) :->
	"Send `run' to GDB"::
	send(M, tell_gdb, 'run\n').

gdb_step(M, Times:[int]) :->
	"Send `run' to GDB"::
	(   Times == @default
	->  send(M, tell_gdb, 'step\n')
	;   send(M, tell_gdb, 'step %d\n', Times)
	).

gdb_next(M, Times:[int]) :->
	"Send `run' to GDB"::
	(   Times == @default
	->  send(M, tell_gdb, 'next\n')
	;   send(M, tell_gdb, 'next %d\n', Times)
	).

gdb_print(M, Expression:expresion=string) :->
	"Print value of expression"::
	send(M, tell_gdb, 'print %s\n', Expression).


		 /*******************************
		 *         XPCE THINGS		*
		 *******************************/

insert_NAME_(M) :->
	"Insert NAME_"::
	send(M, format, 'NAME_').

:- emacs_end_mode.

