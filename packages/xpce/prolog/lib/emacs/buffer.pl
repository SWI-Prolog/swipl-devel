/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

:- module(emacs_buffer, []).
:- use_module(library(pce)).
:- require([ between/3
	   , ignore/1
	   ]).

:- pce_begin_class(emacs_buffer(file, name), text_buffer).

variable(name,		  name,		get,  "Name of this buffer").
variable(directory,	  directory,	both, "Associated CWD").
variable(file,		  file*,	get,  "Associated file").
variable(mode,		  name,		get,  "Major mode of operation").
variable(time_stamp,	  date*,	get,  "Time-stamp for file").
variable(ensure_newline,  bool,		both, "Add newline when done").
variable(auto_save_mode,  bool,		both, "Auto-save?").
variable(auto_save_count, number,	get,  "Auto-save at expiration").
variable(saved_caret,	  int,		both, "Saved caret on last quit").
variable(saved_fill,	  bool,		both, "Saved fill_mode on quit").
variable(pool,		  [name],	both, "Window pool I belong too").

initialise(B, File:file*, Name:[name]) :->
	"Create from file and name"::
	send(B, send_super, initialise),

	(   File == @nil
	->  send(B, undo_buffer_size, 0),
	    send(B, auto_save_mode, @off),
	    default(Name, '*scratch*', BufBaseName),
	    send(B, slot, mode, fundamental),
	    (	BufBaseName == '*scratch*'
	    ->	send(B, pool, @default)
	    ;	send(B, pool, other)
	    ),
	    send(B, directory, directory('.'))
	;   send(File, absolute_path),
	    get(File, base_name, FileBaseName),
	    default(Name, FileBaseName, BufBaseName),
	    send(B, file, File),
	    send(B, pool, file),
	    send(B, auto_save_mode, @on),
	    send(@emacs_base_names, append, FileBaseName, B),
	    (   get(@emacs_mode_list?members, find,
		    message(@arg1?name, match, FileBaseName), Att)
	    ->  send(B, slot, mode, Att?value)
	    ;   send(B, slot, mode, @emacs_default_mode)
	    )
	),

	send(B, slot, auto_save_count, number(300)),
	send(B, saved_caret, 0),
	send(B, saved_fill, @off),
	send(B, ensure_newline, @on),
	send(B, name, BufBaseName).


unlink(B) :->
	"Remove from buffer-list and base_name table"::
	send(@emacs_buffers, delete, B?name),
	(   get(B, file, File), File \== @nil
	->  send(@emacs_base_names, delete, File?base_name, B)
	;   true
	),
	send(B, send_super, unlink).


attach(B, E:editor) :->
	"A new editor is attached.  Prepare it"::
	get(B, editors, Editors),
	(   send(Editors, empty)
	->  get(B, saved_caret, Caret),
	    get(B, saved_fill, Fill)
	;   get(Editors?head, caret, Caret),
	    get(Editors?head, fill_mode, Fill)
	),
	send(B, send_super, attach, E),
	send(E, caret, Caret),
	send(E, fill_mode, Fill).


detach(B, E:editor) :->
	"An editor is detached"::
	get(B, editors, Editors),
	(   get(Editors, size, 1)
	->  send(B, saved_caret, E?caret),
	    send(B, saved_fill, E?fill_mode)
	;   true
	),
	send(B, send_super, detach, E).


name(B, Name:name) :->
	"Rename buffer to name"::
	get(B, name, OldName),
	(   Name == OldName
	->  true
	;   (   get(@emacs_buffers, member, Name, _)
	    ->  between(2, 1000000, N),
		get(Name, append, string('<%d>', N), BufName),
	        \+ get(@emacs_buffers, member, BufName, _),
		!
 	    ;   BufName = Name
	    ),
	    send(B, slot, name, BufName),
	    (	OldName \== @nil,
	        get(@emacs_buffers, member, OldName, DictItem)
	    ->  send(DictItem, key, BufName)
	    ;	send(@emacs_buffers, append, dict_item(BufName, @default, B))
	    ),
	    send(B, update_label),
	    send(B?editors, for_some, message(@arg1?frame, label, BufName))
	).


lookup(_Ctx, File:file*, Name:[name], Buffer:emacs_buffer) :<-
	"Lookup in name and file-table"::
	(   Name \== @default,
	    get(@emacs_buffers, member, Name, DictItem),
	    get(DictItem, object, Buffer)
	->  true
	;   File \== @nil,
	    get(@emacs_base_names, member, File?base_name, Chain),
	    get(Chain, find, message(@arg1?file, same, File), Buffer)
	;   File \== @nil,
	    send(File, exists),
	    send(File, check_object),
	    get(File, object, Buffer),
	    send(Buffer, instance_of, emacs_buffer),
	    get(Buffer, name, BufName),
	    send(Buffer, slot, name, ''),
	    send(Buffer, name, BufName),
	    send(Buffer, slot, file, File),
	    send(@emacs_base_names, append, File?base_name, Buffer),
	    send(Buffer, reset_undo),
	    send(Buffer, modified, @off),
	    send(Buffer, slot, time_stamp, File?time),
	    send(Buffer, loaded)
	).


		 /*******************************
		 *           LOAD/SAVE		*
		 *******************************/

file(B, File:file) :->
	"Switch to indicated file"::
	send(B, clear),
	(   send(directory(File?name), exists)
	->  send(File, error, open_file, read, 'is a directory')
	;   send(File, exists)
	->  ignore(send(B, insert_file, 0, File)),
	    send(B, reset_undo),
	    send(B, modified, @off),
	    send(B, slot, time_stamp, File?time)
	;   send(B, reset_undo),
	    send(B, modified, @off)
	),
	send(B, slot, file, File),
	send(B, directory, File?directory_name).


save(B, File:[file]) :->
	"->do_save and update time_stamp"::
	(   File == @default
	->  get(B, file, SaveFile),
	    (	SaveFile == @nil
	    ->	send(B, report, error, 'No file associated to this buffer'),
		fail
	    ;	true
	    )
	;   SaveFile = File,
	    (	get(B, file, OldFile), OldFile \== @nil
	    ->	send(@emacs_base_names, delete, OldFile?base_name, B)
	    ;	true
	    ),
	    send(File, absolute_path),
	    get(File, base_name, BaseName),
	    send(B, slot, file, File),
	    send(B, directory, File?directory_name),
	    send(B, name, BaseName),
	    send(@emacs_base_names, append, File?base_name, B)
	),
	(   get(B, ensure_newline, @on)
	->  get(B, size, Size),
	    (	(   Size == 0
		;   get(B, character, Size-1, 10)
		)
	    ->	true
	    ;	send(B, append, string('\n'))
	    )
	;   true
	),
	ignore(send(SaveFile, backup)),
	send(B, do_save, SaveFile),
	send(B, slot, time_stamp, SaveFile?time).


do_save(B, SaveFile:file) :->
	"Do the actual saving"::
	send(B, send_super, save, SaveFile).


write_region(B, File:file, Start:int, Length:int) :->
	"Wrote region to file (start, length)"::
	send(B, send_super, save, File, Start, Length).


save_if_modified(B, Confirm:[bool]) :->
	"Save if associated with a file and modified"::
	(   get(B, modified, @on),
	    get(B, file, File), File \== @nil
	->  (	(   Confirm == @off
		;   send(@display, confirm,
			 '%s is modified.  Save?', File?name)
		)
	    ->	send(B, save)
	    ;	fail
	    )
	;   true
	).


		 /*******************************
		 *	     AUTO-SAVE		*
		 *******************************/

check_auto_save(B) :->
	"Check whether to auto_save"::
	(   get(B, modified, @on),
	    get(B, auto_save_count, C),
	    send(C, minus, 1),
	    send(C, equal, 0),
	    get(B, auto_save_mode, @on)
	->  send(B, auto_save)
	;   true
	).
	

auto_save_file(B, F:file) :<-
	get(B, file, File), File \== @nil,
	get(File, backup_file_name, '#', Name),
	new(F, file(Name)).


auto_save(B) :->
	"Auto-save the buffer (when file)"::
	(   get(B, auto_save_file, File)
	->  send(B, report, status, 'Auto saving ...'),
	    send(@display, flush),
	    ignore(send(B, send_super, save, File, 0, B?size)),
	    send(B?auto_save_count, value, 300),
	    send(B, report, status, 'Auto saving ... done')
	;   true
	).


delete_auto_save_file(B) :->
	"Delete the autosave-file if present"::
	(   get(B, auto_save_file, File)
	->  ignore(send(File, remove))
	;   true
	).


		 /*******************************
		 *	      KILL		*
		 *******************************/

kill(B) :->
	"->save_if_modified and ->free"::
	(   get(B, modified, @off)
	->  send(B, free)
	;   get(B, file, File), File \== @nil, \+ get(B, size, 0)
	->  new(D, dialog('Kill modified buffer?')),
	    send(D, append, new(L, label(reporter))),
	    send(L, format, 'Buffer %s is modified', B?name),
	    send(D, append,
		 button('save & kill', message(D, return, save_and_kill))),
	    send(D, append,
		 button(kill, message(D, return, kill))),
	    send(D, append,
		 button(cancel, message(D, return, cancel))),
	    get(D, confirm_centered, Rval),
	    send(D, destroy),
	    (	Rval == save_and_kill
	    ->	send(B, save),
		send(B, free)
	    ;	Rval == kill
	    ->	send(B, free)
	    ;	fail
	    )
	;   send(B, free)
	).


revert(B) :->
	"Reload associated file"::
	get(B, file, File),
	(   File == @nil
	->  send(B, report, warning, 'No file'),
	    fail
	;   new(Carets, chain),
	    send(B?editors, for_all, message(Carets, append, @arg1?caret)),
	    new(@emacs_reverting, object), % avoid trap
	    send(B, file, File),
	    send(B?editors, for_all,
		 and(message(@arg1, caret, Carets?head),
		     message(Carets, delete_head))),
	    free(@emacs_reverting),
	    send(B, report, status, 'Reloaded %s', File?absolute_path)
	).


		 /*******************************
		 *          NAME/LABEL		*
		 *******************************/

update_label(B) :->
	"Update label in the buffer-menu"::
	get(B, name, Name),
	(   Name \== @nil
	->  get(@emacs_buffers, member, Name, DictItem),
	    (   get(B, modified, @on)
	    ->  send(DictItem, label, string('%s\t**', Name)),
		new(EditorLabel, string('%s [modified]', Name))
	    ;   send(DictItem, label, Name),
		EditorLabel = Name
	    ),
	    send(B?editors, for_all,
		 message(@arg1?frame, label, EditorLabel))
	;   true
	).



		 /*******************************
		 *           MODIFIED		*
		 *******************************/

modified(B, Val:bool) :->
	"Check the file; mark buffer-menu"::
	send(B, send_super, modified, Val),
	(   Val == @on,
	    get(B, file, File), File \== @nil
	->  (	\+ send(File, exists)
	    ->	true
	    ;   get(B, time_stamp, Stamp),
	        get(File, time, FileStamp),
		send(Stamp, equal, FileStamp)
	    ->	true
	    ;	object(@emacs_reverting)
	    ->	true
	    ;	new(D, dialog('Modified file')),
		send(D, append,
		     label(title,  string('File %N was modified', File))),
		send(D, append,
		     button(reload_file, message(D, return, reload_file))),
		send(D, append,
		     button(edit_buffer, message(D, return, edit_buffer))),
		get(D, confirm_centered, RVal),
		send(D, destroy),
		(   RVal == reload_file
		->  send(B, revert)
		;   true
		)
	    )
	;   send(B, delete_auto_save_file)
	),
	send(B, update_label).


		 /*******************************
		 *          OPEN WINDOW		*
		 *******************************/

open(B, New:[bool], Window:emacs_window) :<-
	"Create window for buffer"::
	(   New == @on
	->  send(new(Window, emacs_window(B)), open)
	;   (	\+ send(B?editors, empty)
	    ->	get(B?editors?head, frame, Window),
		send(Window, expose)
	    ;	get(@emacs, free_window, B?pool, Window)
	    ->	send(Window, buffer, B)
	    ;	send(new(Window, emacs_window(B)), open)
	    )
	).

open(B, New:[bool]) :->
	"Create window for buffer"::
	get(B, open, New, _).


identify(Buffer) :->
	"Display information-window on buffer"::
	get(Buffer, identify, _).

identify(Buffer, V:view) :<-
	"Display information-window on buffer"::
	get(Buffer, name, Name),
	get(Buffer, modified, Modified),
	get(Buffer, size, Size),
	get(Buffer, line_number, Lines),
	get(Buffer, mode, Mode),
	get(Buffer, pool, Pool),
	new(V, view(string('Buffer %s', Name), size(60, 8))),
	send(V, confirm_done, @off),
	send(V, tab_stops, vector(200)),
	send(V, appendf, 'Buffer Name:\t%s\n', Name),
	send(V, appendf, 'Mode:\t%s\n', Mode),
	send(V, appendf, 'Window Pool:\t%s\n', Pool),
	send(V, appendf, 'Modified:\t%s\n', Modified?name),
	send(V, appendf, 'Size:\t%d characters; %d lines\n', Size, Lines-1),
	get(Buffer, file, File),
	(   Modified == @on,
	    File \== @nil
	->  get(File, size, FileSize),
	    send(V, appendf, 'File Size:\t%d characters\n', FileSize)
	;   true
	),
	(   File \== @nil
	->  get(File, absolute_path, Path),
	    send(V, appendf, 'Path:\t%s\n', Path)
	;   send(V, appendf, 'Path:\t<No file>\n')
	),
	send(V, caret, 0),
	send(new(D, dialog), below, V),
	send(D, append, button(quit, message(V, destroy))),
	send(V, open).


		 /*******************************
		 *	      MODE		*
		 *******************************/
	
mode(B, Mode:name) :->
	"Switch to named mode"::
	(   get(B, mode, Mode)
	->  true
	;   send(B, slot, mode, Mode),
	    send(B?editors, for_some, message(@arg1, mode, Mode))
	).


		 /*******************************
		 *       LANGUAGE SUPPORT	*
		 *******************************/

name_and_arity(TB, Pos:int, Tuple:tuple) :<-
	"Find name and arity of term at position"::
	get(TB, scan, Pos, word, 0, start, P1),
	get(TB, scan, P1, word, 0, end, P2),
	get(TB, contents, P1, P2-P1, NameString),
	(   send(regex('('), match, TB, P2)
	->  P4 is P2 + 1,
	    count_args(TB, P4, 0, Arity)
	;   Arity = 0
	),
	new(Tuple, tuple(NameString?value, Arity)).


count_args(TB, Here, A0, A) :-
	get(TB, scan, Here, term, 1, EndTerm),
	(   get(TB, character, EndTerm, 0'))
	->  A is A0 + 1
	;   get(TB, character, EndTerm, 0',)
	->  A1 is A0 + 1,
	    count_args(TB, EndTerm, A1, A)
	;   count_args(TB, EndTerm, A0, A)
	).

:- pce_end_class.
	
	
