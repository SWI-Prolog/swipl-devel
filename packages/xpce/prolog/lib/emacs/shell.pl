/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

:- module(emacs_shell_mode,
	  []).
:- use_module(library(pce)).
:- require([ between/3
	   , forall/2
	   , ignore/1
	   ]).


:- pce_begin_class(emacs_process_buffer, emacs_buffer).

variable(process,	   process*,	get,	"Associated process").
variable(process_fragment, fragment,	get,	"Process' insertion point").
variable(prompt_regex,     regex*,	both,	"Regex for prompt").
variable(history,	   chain,	get,	"Chain of commands send").

delegate_to(process).

resource(prompt_regex,	   regex*,	'@nil',	"Default prompt").

initialise(B, Process:[process]*, Name:[name]) :->
	"Create from process and name"::
	(   Name == @default
	->  new(BufName, string('*%s*', Process?name))
	;   BufName = Name
	),

	send(B, send_super, initialise, @nil, BufName),
	send(B, slot, process_fragment, fragment(B, 0, 0)),
	send(B, slot, prompt_regex, @default),
	send(B, slot, history, new(chain)),
	send(B, mode, shell),
	(   Process \== @default
	->  send(B, process, Process)
	;   true
	),
	send(B, obtain_resources).


unlink(B) :->
	"Kill the process"::
	get(B, process, P),
	(   P \== @nil
	->  send(P, free)
	;   true
	),
	send(B, send_super, unlink).


lookup(_Ctx, Process:[process]*, Name:name, Buffer:emacs_buffer) :<-
	"Reuse named buffer"::
	get(@emacs_buffers, member, Name, DictItem),
	get(DictItem, object, Buffer),
	send(Buffer, instance_of, emacs_process_buffer),
	(   Process \== @default
	->  send(Buffer, clear),
	    send(Buffer, process, Process)
	;   true
	).



		 /*******************************
		 *	       LABEL		*
		 *******************************/

modified(_B, _Val:bool) :->
	"Just succeed"::
	true.


update_label(B) :->
	"Update label in the buffer-menu"::
	get(B, name, Name),
	get(B, process, Process),
	(   Name \== @nil,
	    Process \== @nil
	->  get(Process, status, Status),
	    map_status(Status, Indicator),
	    get(@emacs_buffers, member, Name, DictItem),
	    send(DictItem, label, string('%s\t%s', Name, Indicator)),
	    new(FrameLabel, string('%s [%s]', Name, Status)),
	    send(B?editors, for_all,
		 message(@arg1?frame, label, FrameLabel))
	;   true
	).

map_status(running, 'R').
map_status(stopped, 'Z').
map_status(killed,  'K').
map_status(exited,  'Done').
map_status(_,       '??').


		 /*******************************
		 *          THE PROCESS		*
		 *******************************/

process(B, Process:process*) :->
	"Associate process with buffer and start it"::
	(   get(B, process, OldProcess),
	    OldProcess \== @nil
	->  send(OldProcess, free)
	;   true
	),
	send(B, slot, process, Process),
	get(B, process_fragment, Fragment),
	send(Fragment, start, B?size),
	send(Fragment, length, 0),
	(   Process \== @nil
	->  send(Process, record_separator, @nil),
	    send(Process, input_message,
		 message(B, insert_process_input, @arg1)),
	    send(Process, terminate_message,
		 message(B, terminated, @arg1)),
	    send(Process, environment, 'TERM', emacs),
	    ignore(send(Process?environment, delete, 'TERMCAP')),
	    send(Process, environment, 'INFERIOR', yes),
	    send(B, update_label)
	;   true
	).


insert_process_input(B, Input:string) :->
	send(Input, translate, 13, @nil),     % delete \r's (fixed in process)
	get(B, process_fragment, Fragment),
	send(Fragment, insert, @default, Input).


terminated(B, Code:'name|int') :->
	"Indicate proces terminated"::
	(   atom(Code)
	->  send(B, report, warning, 'Terminated on signal %s at %s',
		 Code, @pce?date)
	;   Code == 0
	->  send(B, report, status, 'Finished at %s', @pce?date)
	;   send(B, report, warning, 'Exited status %s at %s',
		 Code, @pce?date)
	),
	send(B, update_label).



		 /*******************************
		 *        SENDING INPUT		*
		 *******************************/

:- pce_global(@newline, new(string('\n'))).

send_input(B, Caret:int) :->
	"Send data from the process input"::
	get(B, process_fragment, Fragment),
	get(Fragment, end, End),
	(   Caret >= End
	->  get(B, contents, End, Caret-End, Data),
	    send(B, insert, Caret, @newline)
 	;   get(B, scan, Caret, line, 0, start, SOL),
	    get(B, scan, Caret, line, 0, end, EOL),
	    get(B, contents, SOL, EOL-SOL, Data),
	    (   get(B, prompt_regex, Prompt), Prompt \== @nil,
		get(Prompt, match, Data, Size)
	    ->  send(Data, delete, 0, Size)
 	    ;   true
 	    ),
	    send(B, append, Data),
	    send(B, append, @newline),
	    send(B?editors, for_all, message(@arg1, caret))
	),
	get(B, size, Size),
	send(Fragment, start, Size),
	send(Fragment, length, 0),
	(   get(Data, size, 0)
	->  send(Data, newline)
	;   send(Data, ensure_nl)
	),
	new(H, string('%s', Data)),
	send(H, strip),
	(   \+ send(H, equal, '')
	->  send(B?history, prepend, H)
	;   true
	),
	(   pce_catch_error(not_open,
			    send(B?process, append, Data))
	->  true
	;   send(B, report, warning, 'No process')
	).


format_data(B, Fmt:char_array, Args:any...) :->
	"Send formatted data to process"::
	new(S, string),
	send(S, send_vector, format, Fmt, Args),
	send(B, append, S),
	send(B?process, append, S).


		 /*******************************
		 *        EDIT OPERATIONS	*
		 *******************************/

clear(B) :->
	"->clear, but reinsert ->process_fragment"::
	send(B, send_super, clear),
	send(B?history, clear),
	send(B, slot, process_fragment, fragment(B, 0, 0)).


kill(B) :->
	"Kill buffer and process"::
	get(B, process, Process),
	(   get(Process, pid, Pid), Pid \== @nil
	->  new(D, dialog('Kill running process buffer?')),
	    send(D, append, new(L, label(reporter))),
	    send(L, format, 'Process is running'),
	    send(D, append,
		 button(kill, message(D, return, kill))),
	    send(D, append,
		 button(cancel, message(D, return, cancel))),
	    get(D, confirm_centered, Rval),
	    send(D, destroy),
	    (	Rval == kill
	    ->	send(B, free)
	    ;	fail
	    )
	;   send(B, free)
	).


identify(B) :->
	get(B, identify, V),
	get(B, process, Process),
	send(V, appendf, 'Command:\t%s\n', Process?name),
	send(V, appendf, 'Arguments:\t'),
	get(Process, arguments, Argv),
	get(Argv, size, Size),
	(   Size == 0
	->  send(V, appendf, '<none>')
	;   get(Argv, element, 1, A1),
	    send(V, appendf, '%s', A1),
	    forall(between(2, Size, AN),
		   send(V, appendf, ' %s', ?(Argv, element, AN)))
	),
	send(V, appendf, '\n'),
	send(V, appendf, 'Status:\t%s\n', Process?status).
	

		 /*******************************
		 *      WINDOW INTERFACE	*
		 *******************************/

start_process(B) :->
	"Start process if not running"::
	(   get(B, process, Process), Process \== @nil
	->  send(Process, open),
	    send(B, update_label)
	;   send(B, report, warning, 'No process')
	).
	

open(B, New:[bool]) :->
	"Create window for buffer"::
	(   New == @on
	->  send(new(Window, emacs_window(B)), open)
	;   (	\+ send(B?editors, empty)
	    ->	send(B?editors?head?frame, expose)
	    ;	get(@emacs, free_window, B?pool, Window)
	    ->	send(Window, buffer, B)
	    ;	send(emacs_window(B), open)
	    )
	),
	(   nonvar(Window)
	->  send(Window?editor, caret, @default)
	;   true
	),
	send(B, update_label).
	

:- pce_end_class.


		 /*******************************
		 *         SHELL MODE		*
		 *******************************/

:- initialization
	new(KB, key_binding(shell, fundamental)),
	send(KB, function, 'RET', send_input),
	send(KB, function, '\C-c\C-c', interrupt_subjob),
	send(KB, function, '\C-c\C-d', end_of_file),
	send(KB, function, '\C-c\C-h', show_history),
	send(KB, function, '\ep',      backward_history),
	send(KB, function, '\en',      forward_history),
	send(KB, function, '\C-cRET',  mark_errors),
	send(KB, function, '\C-c\C-f', visit_file),
	send(KB, function, '\C-c\C-e', goto_error),
	send(KB, function, '\C-c\C-\', quit_subjob),
	send(KB, function, '\C-c\C-k', kill_subjob).

:- initialization
	new(T, syntax_table(shell)),
	send(T, syntax, '.', symbol),
	send(T, syntax, '-', symbol),
	send(T, syntax, '"', string_quote, '\'),
	send(T, syntax, '''', string_quote, '\').

:- initialization
	new(MM, emacs_mode_menu(shell, fundamental)),
	send(MM, append, shell, start_process),
	send(MM, append, shell, goto_error),
	send(MM, append, shell, mark_errors),
	send(MM, append, shell, visit_file),

	send(MM, append, shell, end_of_file),
	send(MM, append, shell, interrupt),
	send(MM, append, shell, kill_subjob).
	

:- pce_begin_class(emacs_shell_mode, emacs_fundamental_mode).

variable(current_history, number*, both, "Current history number").

:- pce_global(@event_mode, new(@event?receiver?window?editor?mode)).

process(E, P:process) :<-
	"Process object associated with editor"::
	get(E, text_buffer, TB),
	send(TB, instance_of, emacs_process_buffer),
	get(TB, process, P),
	(   get(P, pid, Pid), Pid \== @nil
	->  true
	;   send(E, report, error, 'No process'),
	    fail
	).


		 /*******************************
		 *        SENDING INPUT		*
		 *******************************/

send_input(E) :->
	"Send data to process"::
	send(E, end_of_line),
	send(E?text_buffer, send_input, E?caret).


		 /*******************************
		 *	       EDIT		*
		 *******************************/

beginning_of_line(E, Lines:[int]) :->
	"Move to start of input"::
	(   Lines == @default,
	    get(E, caret, Caret),
	    get(E, text_buffer, B),
	    get(B, scan, Caret, line, 0, start, SOL),
	    get(B, process_fragment, Fragment),
	    get(Fragment, end, End),
	    Caret >= End,
	    SOL =< Caret
	->  send(E, caret, End)
	;   send(E?editor, beginning_of_line, Lines)
	).


		 /*******************************
		 *	     HISTORY		*
		 *******************************/

show_history(E, N:[int]) :->
	"Show last N commands"::
	default(N, 10, Cmds),
	get(E, text_buffer, TB),
	get(E, caret, Caret),
	get(TB, scan, Caret, line, 0, start, SOL),
	get(TB, contents, SOL, Caret-SOL, Prompt),
	get(TB, history, Chain),
	insert_history(Cmds, Chain, E),
	send(E, format, '\n%s', Prompt),
	get(TB, process_fragment, Frag),
	send(Frag, end, E?caret).

insert_history(0, _, _) :- !.
insert_history(N, Chain, E) :-
	(   get(Chain, nth1, N, Data)
	->  send(E, format, '\n\t%s', Data)
	;   true
	),
	NN is N - 1,
	insert_history(NN, Chain, E).
	

backward_history(E, Id:event_id) :->
	"Insert last command"::
	send(E, current_history, 0),
	send(E, focus_function, '_history'),
	send(E, '_history', Id).


'_history'(E, Id:event_id) :->
	get(E?bindings, function, Id, Func),
	get(E, text_buffer, B),
	get(B, history, Chain),

	get(E, current_history, Current),
	(   Func == backward_history
	->  send(Current, plus, 1)
	;   Func == forward_history
	->  send(Current, minus, 1)
	),

	get(B, process_fragment, Frag),
	get(Frag, end, End),
	send(E, caret, End),
	get(B, size, Size),
	send(B, delete, End, Size),

	(   get(Chain, nth1, Current, Data)
	->  send(B, insert, End, Data)
	;   send(E, report, status, 'No (more) history')
	).

		 /*******************************
		 *           SIGNALS		*
		 *******************************/

kill_subjob(E, Signal:'[int|name]') :->
	"Send signal to shell"::
	send(E?process, kill, Signal).

interrupt_subjob(E) :->
	"Send control-C to process"::
	send(E?process, append, string('%c', 3)).	% ^C

quit_subjob(E) :->
	"Send control-\ to process"::
	send(E?process, append, string('%c', 28)).	% ^\

suspend_subjob(E) :->
	"Send control-Z to process"::
	send(E?process, append, string('%c', 26)).	% ^Z

end_of_file(E) :->
	"Send control-D to process"::
	send(E?process, append, string('%c', 4)).	% ^D


		 /*******************************
		 *	  FINDING ERRORS	*
		 *******************************/

:- pce_global(@emacs_error_regexs,
	      new(chain(regex('\(\S +\):\s *\(\sd+\):'),        % gcc, grep
			regex('"\(\S +\)", line \(\sd+\):')))). % SUN cc
:- pce_global(@emacs_cd_regexs,
	      new(chain(regex('\bcd\s +\(\(\w\|[_/+-.]\)+\)'),
			regex('Entering directory `\([^'']+\)''')))).
:- pce_global(@emacs_canonise_dir_regex,
	      new(regex('[^/]+/\.\./'))).

canonise_path(Path) :-
	send(regex('[^/]+/\.\./'), for_all, Path,
	     message(@arg1, replace, Path, '')),
	send(regex('/\./\|//'), for_all, Path,
	     message(@arg1, replace, Path, '/')).

directory_name(M, Pos:[int]*, DirName:string) :<-
	"New string with Directory at position"::
	get(M, text_buffer, TB),
	(   Pos == @nil
	->  get(TB, directory, Dir),
	    new(DirName, string('%s', Dir?path))
	;   (	Pos == @default
	    ->	get(M, caret, P0)
	    ;	P0 = Pos
	    ),
	    (	match_cd_regex(TB, P0, Dir0, Here)
	    ->  (   (	send(Dir0, prefix, /)
		    ;	send(Dir0, prefix, ~)
		    )
		->  DirName = Dir0
		;   get(M, directory_name, Here, DirName),
		    send(DirName, ensure_suffix, /),
		    send(DirName, append, Dir0)
		)
	    ;	get(M?text_buffer, directory, Dir),
		new(DirName, string('%s', Dir?path))
	    )
	),
	send(DirName, ensure_suffix, /),
	canonise_path(DirName).


match_cd_regex(TB, P0, Dir, Here) :-
	get(@emacs_cd_regexs, find_all,
	    message(@arg1, search, TB, P0, 0), Hits),
	send(Hits, sort,
	     ?(@arg2?register_start, compare, @arg1?register_start)),
	get(Hits, head, Regex),
	get(Regex, register_start, Here),
	get(Regex, register_value, TB, 1, Dir),
	send(Hits, done).


directory(M, Pos:[int]*, Dir:directory) :<-
	"Current working directory"::
	get(M, directory_name, Pos, DirName),
	new(Dir, directory(DirName)).


error(M, Pos:int, Tuple:tuple) :<-
	"FileName and line number from error"::
	get(M, text_buffer, TB),
	get(TB, scan, Pos, line, 0, start, SOL),
	get(TB, scan, Pos, line, 0, end, EOL),
	get(@emacs_error_regexs, find,
	    message(@arg1, search, TB, SOL, EOL), Regex),
	get(Regex, register_value, TB, 1, R1),
	get(Regex, register_value, TB, 2, R2),
	(   get(@pce, convert, R1, int, LineNo)
	->  get(R2, value, File)
	;   get(@pce, convert, R2, int, LineNo)
	->  get(R1, value, File)
	;   send(M, report, warning, 'No line number in error'),
	    fail
	),
	(   send(File, prefix, /)
	->  new(Tuple, tuple(File, LineNo))
	;   get(M, directory_name, SOL, DirNameStr),
	    send(DirNameStr, append, File),
	    canonise_path(DirNameStr),
	    get(DirNameStr, value, DirName),
	    new(Tuple, tuple(DirName, LineNo))
	).


mark_errors(M, From:[int], To:[int]) :->
	"Mark errors in range using fragments"::
	send(M, report, status, 'Marking errors ...'),
	get(M, text_buffer, TB),
	default(From, 0, F0),
	default(To, TB?size, T),
	get(TB, scan, F0, line, 0, start, F1),
	new(Here, number(F1)),
	new(Errors, number(0)),
	repeat,
	     (	 get(M, error, Here, tuple(File, Line))
	     ->	 new(B, emacs_buffer(File)),
		 get(B, scan, 0, line, Line-1, start, SOF),
		 get(B, scan, SOF, line, 0, end, EOF),
		 new(FThere, emacs_link_fragment(B, SOF, EOF-SOF)),
		 get(TB, scan, Here, line, 0, end, EOL),
		 new(FHere, emacs_link_fragment(TB, Here, EOL-Here)),
		 send(FHere, link, FThere),
		 send(Errors, plus, 1)
	     ;	 true
	     ),
	     get(TB, scan, Here, line, 1, start, NewHere),
	     (	 send(T, larger, NewHere)
	     ->	 send(Here, value, NewHere),
		 fail
	     ;	 !,
	         (   send(Errors, larger, 0)
		 ->  send(M, report, status, 'Marked %d errors', Errors)
		 ;   send(M, report, status, 'No errors')
		 )
	     ).
	     

goto_error(M) :->
	"Goto error from current_line"::
	get(M, caret, Caret),
	(   get(M, find_fragment,
		and(message(@arg1, instance_of, emacs_link_fragment),
		    message(@arg1, overlap, Caret)),
		Fragment)
	->  send(Fragment, follow)
	;   get(M, error, Caret, tuple(File, Line))
	->  new(B, emacs_buffer(File)),
	    send(B, open),
	    send(B?editors?head, line_number, Line)
	;   send(M, report, warning, 'No error on this line')
	).

visit_file(M) :->
	"Visit file from the caret"::
	get(M, word, FileName),
	get(M, directory_name, M?caret, Dir),
	send(Dir, append, FileName),
	new(F, file(Dir)),
	(   send(F, exists)
	->  new(B, emacs_buffer(F)),
	    send(B, open)
	;   send(M, report, error, '%s: no such file', Dir),
	    fail
	).

:- pce_end_class.


		 /*******************************
		 *	 LINKED FRAGMENTS	*
		 *******************************/

:- pce_begin_class(emacs_link_fragment, fragment,
		   "Pair of linked fragments").

variable(link,		fragment,	get,	"Fragment I'm linked to").

unlink(F) :->
	"Remove fragment and 'linked-to'"::
	get(F, link, F2),
	(   F2 \== @nil
	->  send(F, slot, link, @nil),
	    free(F2)
	;   true
	),
	send(F, send_super, unlink).
	

link(F, F2:emacs_link_fragment) :->
	send(F, slot, link, F2),
	send(F2, slot, link, F).


follow(F) :->
	"Goto other end of link"::
	get(F, link, F2),
	get(F2, text_buffer, B),
	send(B, open),
	send(B?editors?head, caret, F2?start).

:- pce_end_class.
