/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1996 University of Amsterdam. All rights reserved.
*/

:- module(emacs_application, []).
:- use_module(library(pce)).
:- require([ ignore/1
	   , pce_help_file/2
	   ]).

:- pce_global(@finder, new(finder)).
:- pce_autoload(finder, library(find_file)).


:- pce_begin_class(emacs, application,
		   "PceEmacs main object").

variable(buffer_list,	dict,	get, "List of buffers maintained").
variable(exit_message,	message,get, "Registered exit message").

		 /*******************************
		 *	      CREATE		*
		 *******************************/

initialise(Emacs, Buffers:dict) :->
	send(Emacs, send_super, initialise, emacs),
	send(Emacs, kind, service),
	send(Emacs, slot, buffer_list, Buffers),
	new(Msg, message(Emacs, check_saved_at_exit)),
	send(@pce, exit_message, Msg),
	send(Emacs, slot, exit_message, Msg),
	new(@emacs_mark_list, emacs_bookmark_editor),
	ignore(send(Emacs, server_start)),
	ignore(send(Emacs, load_user_init_file)).
	
unlink(Emacs) :->
	(   get(Emacs, exit_message, Msg),
	    send(@pce?exit_messages, delete, Msg)
	;   true
	),
	send(Emacs, send_super, unlink).

start(_Emacs) :->
	true.

		 /*******************************
		 *	   BUFFER MENU		*
		 *******************************/

:- pce_group(buffer).

show_buffer_menu(Emacs) :->
	"Show the buffer menu"::
	(   get(Emacs, member, buffer_menu, Menu)
	->  send(Menu, expose)
	;   send(emacs_buffer_menu(Emacs), open)
	).
	    

selection(Emacs, B:emacs_buffer*) :->
	"Select emacs buffer"::
	(   get(Emacs, member, buffer_menu, Menu)
	->  send(Menu, selection, B)
	;   true
	).


		 /*******************************
		 *      BUFFERS AND FILES	*
		 *******************************/

:- pce_group(file).

buffer(Emacs, Name:name, B:emacs_buffer) :<-
	"Find named buffer"::
	get(Emacs, buffer_list, Dict),
	get(Dict, member, Name, DI),
	get(DI, object, B).

file_buffer(_, File:file, Buffer:emacs_buffer) :<-
	"Find existing buffer holding file"::
	get(File, base_name, Base),
	get(@emacs_base_names, member, Base, Chain),
	get(Chain, find, message(@arg1?file, same, File), Buffer).

buffers(Emacs, Buffers:chain) :<-
	"Chain with all emacs-buffers"::
	get(Emacs?buffer_list?members, map, @arg1?object, Buffers).


open_file(_Emacs, File:file, NewWindow:[bool]) :->
	"Open a file"::
	new(B, emacs_buffer(File)),
	send(B, open, NewWindow).


find_file(Emacs, Dir:[directory]) :->
	"Find and edit file"::
	get(@finder, file, @off, @default, Dir, FileName),
	send(Emacs, open_file, FileName).

goto_source_location(_Emacs, Location:source_location) :->
	"Visit the indicated source-location"::
	get(Location, file_name, File),
	new(B, emacs_buffer(File)),
	send(B, open),
	send(B, check_modified_file),
	(   get(Location, line_no, Line),
	    Line \== @nil
	->  get(B?editors, head, E),
	    send(E?mode, select_line, Line),
	    get(E, slot, selection_start, Caret), % without slot, fails
	    send(E, caret, Caret)		  % on empty line!
	;   true
	).


edit(Emacs, Location:source_location) :->
	"Equivalent to ->goto_source_location"::
	send(Emacs, goto_source_location, Location).


existing_file(_Emacs, Dir:[directory], File:file) :<-
	"Find existing file in directory"::
	get(@finder, file, @on, @default, Dir, FileName),
	new(File, file(FileName)).


		 /*******************************
		 *	       SAVE		*
		 *******************************/
:- pce_group(save).

save_some_buffers(BM, Confirm:[bool]) :->
	"Save all modified buffers"::
	new(ModifiedItem,
	    and(@arg1?object?file \== @nil,
		@arg1?object?modified == @on)),
	(   get(BM?buffer_list, find, ModifiedItem, _)
	->  send(BM?buffer_list, for_some,
		 and(ModifiedItem,
		     or(Confirm == @off,
			message(@display, confirm, 'Save %s?',
				@arg1?object?file?name)),
		     message(@arg1?object, save, @arg1?object?file)))
	;   send(@pce, report, status, 'No buffers need saving')
	).

	
check_saved_at_exit(BM) :->
	"Check for unsaved buffers when called from exit"::
	send(BM, save_some_buffers, @on),
	new(ModifiedItem,
	    and(@arg1?object?file \== @nil,
		@arg1?object?modified == @on)),
	(   get(BM?buffer_list, find, ModifiedItem, _)
	->  (   send(@display, confirm, 'Discard modified buffers?')
	    ->	true
	    ;	repeat,
			send(@display, dispatch),
			format('Dispatch running; discarding input~n', []),
			get0(_),
			fail
	    )
	;   true
	).

		 /*******************************
		 *	      WINDOWS		*
		 *******************************/
:- pce_group(window).

free_window(BM, Pool:[name], Frame:emacs_window) :<-
	"Return the first non-sticky window"::
	(   send(@event, instance_of, event),
	    get(@event?receiver, frame, Frame),
	    send(Frame, instance_of, emacs_window),
	    (Pool == @default ; get(Frame, pool, Pool)),
	    get(Frame, sticky_window, @off)
	->  true
	;   get(BM, buffer_list, Dict),
	    new(Ed, var),
	    get(Dict, find,
		and(assign(new(B, var), @arg1?object),
		    assign(Ed, ?(B?editors, find,
				 and(message(@arg1?frame, instance_of,
					     emacs_window),
				     or(@arg1?frame?pool == Pool,
					@arg1?frame?pool == @default),
				     @arg1?frame?sticky_window == @off)),
			   global)),
		_),
	    get(Ed, frame, Frame),
	    send(Frame, expose)
	).


		 /*******************************
		 *	       MODE		*
		 *******************************/

:- pce_group(mode).

modes(_Emacs, ModeNames:chain) :<-
	"Return chain with known modes"::
	get(@mode_name_type, context, ModeNames).


		 /*******************************
		 *	       HELP		*
		 *******************************/

:- pce_group(help).

:- dynamic
	help_file/1.

:- pce_help_file(emacs, pce_help('emacs.hlp')).
:- pce_help_file(emacs_customise, pce_help('customise.hlp')).

help(_Emacs) :->
	"Display general help"::
	send(@helper, give_help, emacs, main).

customise(_Emacs) :->
	"Display customisation help"::
	send(@helper, give_help, emacs_customise, main).


		 /*******************************
		 *		SERVER		*
		 *******************************/

:- pce_group(server).

server_start(Emacs, Force:[bool]) :->
	"Start server-mode (xpce-client interface)"::
	(   (	\+ get(class(socket), send_method, listen, _)
	    ;	\+ send(class(socket), has_feature, unix_domain)
	    ;	get(@emacs_server, status, listen)
	    )
	->  true
	;   (	send(@emacs_server_address, exists, @off)
	    ->  (   Force \== @on,
		    pce_catch_error(socket, send(@emacs_server, connect))
		->  free(@emacs_server),
		    send(Emacs, report, status, 'Server on other PceEmacs'),
		    fail
		;   free(@emacs_server), % will recreate!
		    ignore(send(Emacs, report, status, 'Restarted server')),
		    send(@emacs_server_address, remove)
		)
	    ;	true
	    ),
	    ignore(send(@emacs_server, listen))
	).
	

:- pce_group(customise).

		 /*******************************
		 *	 USER EXTENSIONS	*
		 *******************************/

load_user_extension(_Emacs, Base:name) :->
	"Load Prolog user file with this base-name"::
	new(Name, string('~/lib/xpce/emacs/%s', Base)),
	send(Name, ensure_suffix, '.pl'),
	new(F, file(Name)),
	(   send(file(Name), exists)
	->  get(F, absolute_path, Path),
	    ensure_loaded(Path)
	;   true
	).


load_user_init_file(_Emacs) :->
	"Load ~/.pceemacsrc: user extensions"::
	new(F, file('~/.pceemacsrc')),
	(   send(F, access, read)
	->  get(F, absolute_path, Path),
            ignore(consult(user:Path))
	;   true
	).

:- pce_end_class.
	  
