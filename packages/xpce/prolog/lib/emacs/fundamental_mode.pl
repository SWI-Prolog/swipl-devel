/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

:- module(emacs_fundamental_mode, []).
:- use_module(library(pce)).
:- require([ append/3
	   , auto_call/1
	   , between/3
	   , chain_list/2
	   , default/3
	   , ignore/1
	   ]).
:- set_prolog_flag(character_escapes, false).

:- emacs_begin_mode(fundamental, [],	% []: root of the mode hierarchy
		    "Generic PceEmacs editing mode",
	[ prefix		   = key('\C-h'),
	  show_key_bindings	   = key('\C-hb'),
	  insert_file		   = key('\C-xi'),
	  write_file		   = key('\C-x\C-w'),
	  split_window		   = key('\C-x2'),
	  only_window              = key('\C-x1'),
	  save_and_kill            = key('\C-x#'),
	  what_cursor_position     = key('\C-x='),
	  count_lines_region       = key('\e='),
	  query_replace_regex      = key('\e%'),
	  grab_region              = key('\ew'),
	  justify_paragraph        = key('\eQ'),
	  bookmark_line            = key('\e@'),
	  execute_extended_command = key('\ex'),
	  sticky_window		   = key('\es'),
	  write_region		   = key('\e\C-w'),
	  compile		   = key('\C-xRET'),
	  
					% FILE menu
	  show_buffer_menu	   = key('\C-x\C-b') + button(file),
	  switch_to_buffer	   = key('\C-xb') +
				     button(file, @emacs_mode?buffers),
	  find_file		   = key('\C-x\C-f') + button(file),
	  save_buffer		   = key('\C-x\C-s') + button(file),
	  save_as		   = button(file),
	  revert		   = button(file),
	  kill_buffer		   = key('\C-xk')    + button(file),
	  ispell		   = button(file),
	  shell			   = button(file),
	  (mode)		   = key('\em') +
	  			     button(file, @emacs_mode?modes),
	  identify		   = button(file),
	  quit			   = key('\C-x\C-c') + button(file),

					% EDIT menu
	  undo			   = button(edit),
	  copy			   = button(edit),
	  cut			   = button(edit),
	  paste			   = button(edit),

					% BROWSER menu
	  bookmark_line		   = button(browse),
	  show_bookmarks	   = button(browse),
	  grep			   = button(browse),

					% COMPILE menu
	  compile		   = button(compile),

					% HELP menu
	  help			   = button(help),
	  customise		   = button(help),
	  show_key_bindings	   = button(help),
	  manpce		   = button(help),
	  manual_entry		   = button(help)
	],
	[
	]).

class_variable(grep_command,	string, 'grep -n %s /dev/null',
	       "Command of M-x grep").
class_variable(shell_command,	chain*,
	       when(@pce?window_system == windows,
		    @nil,
		    chain('/bin/sh', '-c')),
	       "Command for running grep, make, etc.").

		 /*******************************
		 *	GLOBAL UTILITIES	*
		 *******************************/

keyboard_quit(M) :->
	"Quit current operation"::
	send(M?editor, keyboard_quit),
	send(M, selection, 0, 0).


show_key_bindings(M) :->
	"Display window with key-bindings"::
	auto_call(show_key_bindings(M)).


quit(M) :->
	"Destroy the editor"::
	ignore(send(M?text_buffer, save_if_modified)),
	send(M?frame, destroy).


		 /*******************************
		 *		REGION		*
		 *******************************/

region(M, Tuple:tuple) :<-
	"Get region start and end (sorted)"::
	get(M, mark, Mark),
	get(M, caret, Caret),
	(   Caret > Mark
	->  new(Tuple, tuple(Mark, Caret))
	;   Mark > Caret
	->  new(Tuple, tuple(Caret, Mark))
	).

line_region(M, F0:[int], T0:[int], Tuple:tuple) :<-
	"Start and end-of region for line-oriented commands"::
	(   F0 == @default,
	    T0 == @default
	->  get(M, region, tuple(From, To))
	;   From = F0,
	    To = T0
	),
	get(M, text_buffer, TB),
	get(TB, scan, From, line, 0, start, S),
	get(TB, scan, To,   line, 0, start, E),
	new(Tuple, tuple(S, E)).

for_lines_in_region(M, From:[int], To:[int], Code:code) :->
	"Run code with @arg1, @arg2 at the start/end of each line"::
	get(M, line_region, From, To, tuple(S, E)),
	for_lines_in_region(M, S, E, Code).

for_lines_in_region(_M, S, E, _Code) :-
	S >= E, !.
for_lines_in_region(M, S, E, Code) :-
	get(M, scan, S, line, 0, EOL),
	send(Code, forward, S, EOL),
	SOL is EOL + 1,
	for_lines_in_region(M, SOL, E, Code).

warn_big_region(M, Action:name, N:int) :->
	"Warn if region exceeds size"::
	get(M, region, tuple(Start, End)),
	(   End - Start > N
	->  send(@display, confirm,
		 'Region is > %d characters; %s?', N, Action)
	;   true
	).

warn_big_paragraph(M, Action:name, N:int) :->
	"Warn if paragraph exceeds size"::
	get(M, caret, Caret),
	get(M, text_buffer, TB),
	get(TB, scan, Caret+1, paragraph, 0, start, SP),
	get(TB, scan, Caret-1, paragraph, 0, end, EP),
	(   EP - SP > N
	->  send(@display, confirm,
		 'Paragraph is > %d characters; %s?', N, Action)
	;   true
	).


		 /*******************************
		 *      DANGEROUS ACTIONS	*
		 *******************************/

upcase_region(M) :->
	"Upcase region, but warn if dangerous"::
	send(M, warn_big_region, upcase, 100),
	send(M, send_super, upcase_region).

downcase_region(M) :->
	"Downcase region, but warn if dangerous"::
	send(M, warn_big_region, downcase, 100),
	send(M, send_super, downcase_region).

fill_region(M) :->
	"Fill region, but warn if dangerous"::
	send(M, warn_big_region, fill, 1000),
	send(M, send_super, fill_region).

justify_paragraph(M) :->
	"Justify paragraph, but warn if dangerous"::
	send(M, warn_big_paragraph, justify, 1000),
	send(M, send_super, justify_paragraph).

fill_paragraph(M) :->
	"Fill paragraph, but warn if dangerous"::
	send(M, warn_big_paragraph, justify, 1000),
	send(M, send_super, fill_paragraph).


		 /*******************************
		 *		EDIT		*
		 *******************************/

grab_region(M) :->
	"Copy the current region into the cut-buffer"::
	send(M, kill_or_grab_region, 1).


sort_lines_in_region(M) :->
	"Sort lines in mark ... caret alphabetically"::
	get(M, region, tuple(Start, End)),
	send(M, sort, Start, End).


delete_matching_lines(M, Re:regex) :->
	"Delete lines in (point,end) matching regex"::
	get(M, caret, Caret),
	get(M, text_buffer, TB),
	get(TB, scan, Caret, line, 0, start, SOL),
	send(Re, compile, @on),
	new(Here, number(SOL)),
	repeat,
	    (	get(TB, size, Size),
		get(Here, value, PlHere),
		PlHere >= Size
	    ->	!
	    ;   get(TB, scan, Here, line, 0, end, EOL),
		(   send(Re, search, TB, Here, EOL)
		->  send(TB, delete, Here, EOL+1-Here)
		;   send(Here, value, EOL+1)
		),
		fail
	    ).
		

/*
delete_rectangle(M) :->
	"Delete rectangular area (mark,caret)"::
	get(M, mark, Mark),
	get(M, column, Mark, MarkColumn),
	get(M, caret, Caret),
	get(M, column, Caret, CaretColumn),
	min(MarkColumn, CaretColumn, FromColumn),
	max(MarkColumn, CaretColumn, ToColumn),
	
*/

		 /*******************************
		 *	      LOAD/SAVE		*
		 *******************************/

buffers(_M, Buffers:chain) :<-
	"Chain with existing buffers"::
	get(@emacs, buffers, Buffers).

modes(_M, Modes:chain) :<-
	"Chain with defined modes"::
	get(@emacs, modes, Modes).


save_text(M) :->
	"Save buffer as ASCII if it is modified"::
	get(M, text_buffer, TB),
	(   get(TB, modified, @on)
	->  send(M?text_buffer, save),
	    send(M, report, status,
		 'Buffer saved in file `%s''', TB?file?base_name)
	;   send(M, report, status, 'No changes need saving')
	).


save_buffer(M) :->
	"Save buffer if it is modified"::
	send(M, save_text).


save_as(M, File:file) :->
	"Equivalent to write_file"::
	send(M, write_file, File).


save_some_buffers(_M, Arg:[int]) :->
	"Save modified buffers. Arg: donot confirm"::
	(   Arg == @default
	->  send(@emacs, save_some_buffers)
	;   send(@emacs, save_some_buffers, @on)
	).


find_file(_M, File:file) :->
	"Find file (other window)"::
	get(File, name, Name),
	(   send(Name, suffix, '.pd')
	->  auto_call(pcedraw(Name))
	;   new(B, emacs_buffer(File)),
	    send(B, open)
	).


insert_file(M, File:file) :->
	"Insert file at point"::
	send(M?text_buffer, insert_file, M?caret, File).


write_file(M, File:file) :->
	"Write buffer to named file"::
	send(M?text_buffer, save, File).


write_region(M, File:file) :->
	"Write current region to file"::
	get(M, region, tuple(Start, End)),
	Length is End - Start,
	send(M?text_buffer, write_region, File, Start, Length).


show_buffer_menu(_M) :->
	"Expose the buffer-menu window"::
	send(@emacs, show_buffer_menu).

switch_to_buffer(_M, Buffer:emacs_buffer) :->
	"Switch to named buffer"::
	send(Buffer, open).


kill_buffer(M) :->
	"Kill the current buffer"::
	get(M, text_buffer, Buffer),
	send(Buffer, kill).


		 /*******************************
		 *	       PRINT		*
		 *******************************/

/*
print(M) :->
	"Print on emacs_fundamental_mode.print_command"::
	get(M, print_command, Cmd),
	send(M, save_if_modified),
	get(M, file, File),
	get(File, name, Name).
*/
	

		 /*******************************
		 *	      REPLACE		*
		 *******************************/

query_replace_regex(M,
		    From:'replace=regex',
		    To:'into=string') :->
	"Query replace regular expression"::
	send(M, set_mark),
	send(M, attribute, attribute(replace_search_pattern, From)),
	send(M, attribute, attribute(replace_replace_pattern, To)),
	replace_find_and_mark(M),
	send(M, focus_function, '_query_replace_regex').


replace_find_and_mark(M) :-
	get(M, replace_search_pattern, Regex),
	(   send(Regex, search, M?text_buffer, M?caret)
	->  get(Regex, register_start, Start),
	    get(Regex, register_end, End),
	    send(M, selection, Start, End),
	    send(M?text_cursor, displayed, @off),
	    send(M, caret, End)
	;   send(M, report, status, 'Done.'),
	    send(M, focus_function, @nil),
	    send(M, selection, 0, 0),
	    send(M?text_cursor, displayed, @on),
	    fail
	).


replace_match(M) :-
	get(M, replace_search_pattern, Regex),
	get(M, replace_replace_pattern, Replace),
	send(Regex, replace, M?text_buffer, Replace),
	get(Regex, register_end, End),
	get(Regex, register_start, Start),
	(   End == Start		% 0-lenght search string!
	->  send(M, caret, End+1)
	;   send(M, caret, End)
	).


replace_end(M) :-
	send(M, focus_function, @nil),
	send(M, selection, 0, 0),
	send(M?text_cursor, displayed, @on).


'_query_replace_regex'(M, Id:event_id) :->
	"Focus function for query_replace_regex"::
	(   Id == 0'y				% replace-and-comtinue
 	->  replace_match(M),
	    ignore(replace_find_and_mark(M))
	;   Id == 0'.				% replace and done
	->  replace_match(M),
	    replace_end(M),
	    send(M, report, status, 'Done.')
	;   Id == 0'n				% donot replace and continue
	->  (   replace_find_and_mark(M)
	    ->	true
	    ;	get(M, focus_function, @nil)
	    )
	;   Id == 27				% exit
	->  replace_end(M)
	;   Id == 0'!				% replace no-query
	->  repeat,
		send(M, '_query_replace_regex', 0'y),
		get(M, focus_function, @nil),
	    !
	;   send(M, selection, 0, 0),		% anything else: cancel
	    send(M?text_cursor, displayed, @on),
	    fail
	).


bookmark_line(M) :->
	"Create a mark in the mark-list"::
	get(M, caret, Caret),
	get(M, scan, Caret, line, 0, start, SOL),
	send(@emacs_mark_list, append_hit, M?text_buffer, SOL).


show_bookmarks(_) :->
	"Show PceEmacs bookmarks"::
	send(@emacs_mark_list, open).


		 /*******************************
		 *	   PROCESS STUFF	*
		 *******************************/

has_processes(M) :->
	"Test if this XPCE version supports processes"::
	(   send(@pce, has_feature, process)
	->  true
	;   send(M, report, error, 'No inferior processes in this OS'),
	    fail
	).

		 /*******************************
		 *	     SPELLING		*
		 *******************************/

:- pce_autoload(ispell, demo(ispell)).

ispell(M) :->
	"Start ispell on this text_buffer"::
	send(M, has_processes),
	new(Ispell, ispell),
	get(M, text_buffer, TB),
	send(Ispell, buffer, TB),
	send(Ispell, label, string('Ispell Emacs buffer %s', TB?name)),
	send(Ispell, open),
	send(Ispell, spell).


		 /*******************************
		 *	  M-x PROCESSING	*
		 *******************************/

command_names(M, Names:chain) :<-
	"Find all send_methods on this mode"::
	get(M, find_all_send_methods, Chain),
	get(Chain, map, @arg1?name, Names).


:- initialization
   (   object(@emacs_mode_command)
   ->  true
   ;   new(@emacs_mode_command,
	   type(emacs_mode_command, value_set,
		quote_function(@current_emacs_mode?command_names),
		@nil))
   ).

execute_extended_command(M,
			 CmdName:command=emacs_mode_command,
			 Times:[int]) :->
	"Prompt for interactive command"::
	get(M, send_method, CmdName, tuple(_, Impl)),
	send(M, open_history, Impl, @on),
	new(Argv, vector),
	between(1, 100, ArgN),
	    (   get(Impl, argument_type, ArgN, ArgType)
	    ->  (   send(ArgType, includes, int),
		    Times \== @default
		->  send(Argv, element, ArgN, Times)
		;   send(ArgType, includes, default)
		->  send(Argv, element, ArgN, @default)
		;   get(M, interactive_argument, Impl, ArgN, Arg),
		    get(ArgType, check, Arg, CheckedArg)
		->  send(Argv, element, ArgN, CheckedArg)
		;   !, fail
		),
		fail			% force backtracking
	    ;   !
	    ),
	send(M, report, status, ''),
	result(send(M, send_vector, CmdName, Argv), YesNo),
	(   object(M)			% may be ->free'd!
	->  send(M, close_history, Argv),
	    (	YesNo == fail
	    ->  send(M, report, status, no)
	    ;	true
	    )
	;   true
	).

result(Goal, true) :-
	Goal, !.
result(_, fail).


		 /*******************************
		 *	     COMPILE		*
		 *******************************/

split_command_string(String, List) :-
	new(Ch, chain),
	new(Re, regex('[^ ]+')),
	send(Re, for_all, String,
	     message(Ch, append, ?(@arg1, register_value, @arg2, 0, name))),
	chain_list(Ch, List).

compile(M, Command:shell_command=string, Label:[name], Pool:[name]) :->
	"Run Unix (compilation) process in buffer"::
	send(M, save_some_buffers),
	send(M, has_processes),
	default(Label, Command, Lbl),
	get(M, directory, Dir),
	new(B, emacs_process_buffer(@default, string('*%s*', Lbl))),
	(   get(B, process, OldP), OldP \== @nil,
	    get(OldP, status, running)
	->  new(D, dialog('Running Process')),
	    send(D, append, label(reporter,
				  'Kill running process and start new one?')),
	    send(D, append, button(kill,
				   and(message(B, process, @nil),
				       message(D, return, ok)))),
	    send(D, append, button(cancel,
				   message(D, return, cancel))),
	    get(D, confirm_centered, Status),
	    send(D, destroy),
	    Status == ok
	;   true
	),
	get(M, shell_command, ShellCommandChain),
	(   ShellCommandChain \== @nil
	->  chain_list(ShellCommandChain, List),
	    append(List, [Command], ArgList),
	    Process =.. [process|ArgList]
	;   split_command_string(Command, Args),
	    Process =.. [process|Args]
	),
	new(P, Process),
	default(Pool, compile, ThePool),
	send(B, pool, ThePool),
	send(P, directory, Dir),
	send(B, clear),
	send(B, format, 'cd %s\n%s\n', Dir?path, Command),
	send(B, directory, Dir),
	send(B, process, P),
	send(B, start_process),
	send(B, open),
	send(B?editors, for_some,
	     and(message(@arg1, report, status, 'Running ...'),
		 message(@arg1, scroll_to, 0),
		 message(@arg1, caret, @default))).


grep(M, GrepArgs:grep_arguments=string) :->
	"Run Unix grep in compilation buffer"::
	get(M, grep_command, GrepCommad),
	send(M, compile,
	     string(GrepCommad, GrepArgs),
	     string('grep %s', GrepArgs),
	     grep).


shell(M) :->
	"Start interactive shell"::
	send(M, has_processes),
	(   get(@emacs, buffer, '*shell*', Buffer)
	->  send(Buffer, open)
	;   (	get(@pce, environment_variable, 'SHELL', Shell)
	    ->  better_shell(Shell, Shell2)
	    ;   Shell2 = sh
	    ),
	    new(P, process(Shell2, '-i')),
	    get(M, directory, Dir),
	    send(P, directory, Dir),
	    new(B, emacs_process_buffer(P, '*shell*')),
	    send(B, pool, shell),
	    send(B, directory, Dir),
	    send(B, start_process),
	    send(B, open)
	).

better_shell('/bin/tcsh', '/bin/csh') :- !.
better_shell(Shell, Shell).

manual_entry(M, Spec:unix_manual_entry_for=name) :->
	"Lookup Unix manual entry"::
	send(M, has_processes),
	new(B, emacs_buffer(@nil, Spec)),
	send(B, pool, manual_entry),
	send(B, (mode), man),
	send(B, open),
	send(B?editors?head, man, Spec).

gdb(M, Cmd:file) :->
	"Run Unix GDB on command"::
	send(M, has_processes),
	new(X, emacs_gdb_buffer(Cmd)),
	get(X, process, Process),
	(   Process \== @nil,
	    get(Process, status, inactive)
	->  get(M, directory, Dir),
	    send(Process, directory, Dir),
	    send(X, directory, Dir)
	;   true
	),
	send(X, start_process),
	send(X, open).

		 /*******************************
		 *	 WYSIWYG (ANNOTATE)	*
		 *******************************/

annotate(M) :->
	"Start annotation buffer from current buffer"::
	get(M, text_buffer, OB),
	get(OB, file, OFile),
	(   OFile == @nil
	->  NFile = @nil,
	    get(OB?name, append, '.ann', BufName)
	;   get(OFile?absolute_path, append, '.ann', NewName),
	    new(NFile, file(NewName)),
	    BufName = @default
	),
	(   send(NFile, exists)
	->  new(B, emacs_annotate_buffer(NFile, BufName))
	;   pce_catch_error(open_file,
			    new(B, emacs_annotate_buffer(NFile, BufName))),
	    send(B, contents, OB?contents)
	),
	send(B, (mode), annotate),
	send(B, pool, wysiwyg),
	send(B, open).


		 /*******************************
		 *	  MISCELLENEOUS		*
		 *******************************/

split_window(M) :->
	"Create another window for this buffer"::
	get(M, text_buffer, Buffer),
	new(W2, emacs_window(Buffer)),
	send(W2?editor, caret, M?caret).


only_window(M) :->
	"Quit other windows on this buffer"::
	get(M, text_buffer, Buffer),
	get(M, editor, Editor),
	send(Buffer?editors, for_all,
	     if(@arg1 \== Editor,
		message(@arg1, quit))).


save_and_kill(M) :->
	"Save, kill window and buffer (return server)"::
	get(M, editor, Editor),
	get(M, text_buffer, TB),
	send(TB, save),
	(   get(Editor, hypered, server, _Server)
	->  send(TB, kill)
	;   send(M, quit)
	).


insert_date(M) :->
	"Insert current time"::
	send(M, insert, @pce?date).


what_cursor_position(M) :->
	"Inform user on current position"::
	get(M, caret, Caret),
	get(M, length, Size),
	Proc is (100 * Caret) / Size,
	get(M, column, Caret, Col),
	send(M, report, inform, 'point=%d of %d(%d%%); column=%d',
	     Caret, Size, Proc, Col).


what_line(M) :->
	"Inform user on current line"::
	get(M, line_number, M?caret, CaretLine),
	send(M, report, inform, 'line %d', CaretLine).


goto_line(M, LineNo:int) :->
	"Goto given line-number"::
	send(M, point_to_top_of_file, LineNo).


count_lines_region(M) :->
	"Inform user on # lines in region"::
	get(M, region, tuple(Start, End)),
	get(M, line_number, Start, CaretLine),
	get(M, line_number, End, MarkLine),
	Lines is MarkLine - CaretLine,
	send(M, report, inform, 'Region has %d lines', Lines).

help(_) :->
	"Display general help"::
	send(@emacs, help).

customise(_) :->
	"Display customisation help"::
	send(@emacs, customise).


		 /*******************************
		 *	       PCE		*
		 *******************************/

manpce(_M, Class:'behaviour|class*') :->
	"Start XPCE manual on behaviour or class"::
	(   Class == @nil
	->  auto_call(manpce)
	;   auto_call(manpce(Class))
	).


editpce(M, Class:'behaviour|class*') :->
	"Edit XPCE class or class <->method"::
	(   Class == @nil
	->  send(M, report, warning,
		 'Enter "class" or "class->|<-|-selector"')
	;   auto_call(editpce(Class))
	).


tracepce(M, Method:behaviour*) :->
	"Put trace-point on XPCE class <->method"::
	(   Method == @nil
	->  send(M, report, warning,
		 'Enter "class->|<-|-selector"')
	;   auto_call(tracepce(Method))
	).


breakpce(M, Method:behaviour*) :->
	"Put break-point on XPCE class <->method"::
	(   Method == @nil
	->  send(M, report, warning,
		 'Enter "class->|<-|-selector"')
	;   auto_call(breakpce(Method))
	).


spypce(M, Method:behaviour*) :->
	"Put spy-point on XPCE class <->method"::
	(   Method == @nil
	->  send(M, report, warning,
		 'Enter "class->|<-|-selector"')
	;   auto_call(spypce(Method))
	).


		 /*******************************
		 *	       DROP		*
		 *******************************/
	
preview_drop(M, Obj:object*) :->
	"Preview the upcomming drop action"::
	(   Obj == @nil
	->  send(M, report, status, '')
	;   send(Obj, instance_of, file)
	->  send(M, report, status,
		 'Please drop to switch to %s', Obj?absolute_path)
	).


drop_files(M, Files:chain) :->
	"Drop chain of files"::
	send(Files, for_all, message(M, drop, create(file, @arg1))).


drop(M, Obj:object) :->
	"Import source-code from object"::
	(   send(Obj, instance_of, file)
	->  send(M, find_file, Obj)
	;   send(Obj, instance_of, chain)
	->  send(Obj, for_all,
		 if(message(@arg1, instance_of, object),
		    message(M, drop, @arg1)))
	).

:- emacs_end_mode.
