/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

:- module(emacs_buffer_menu, []).
:- use_module(library(pce)).
:- require([ ignore/1
	   , send_list/3
	   ]).


:- pce_global(@finder, new(finder)).
:- pce_autoload(finder, library(find_file)).
:- pce_global(@emacs, new(emacs_buffer_menu(@emacs_buffers))).

:- pce_begin_class(emacs_buffer_menu, frame).

resource(geometry,	geometry,	'211x190+0+125').

variable(buffer_list,	dict,	get,	"Dict holding all buffers").

initialise(BM, BufferList:dict) :->
	"Create menu for buffer-list"::
	send(BM, send_super, initialise, 'PCE Emacs Buffers'),
	send(BM, append, new(B, browser)),
	send(B, tab_stops, vector(150)),
	send(new(D, dialog), below, B),
	send(D, gap, size(10, 0)),
	send(D, pen, 0),
	send(D, append, label(reporter)),
	send(BM, slot, buffer_list, BufferList),
	send(BM, done_message, message(BM, show, @off)),

	send(B, dict, BufferList),
	send(B, open_message, message(@arg1?object, open)),
 	send(B, popup, new(P, popup)),

	new(Buffer, @arg1?object),
	send_list(P, append,
		  [ menu_item(help,
			      message(BM, help),
			      end_group := @on)
		  , menu_item(open_buffer,
			      message(Buffer, open))
		  , menu_item(open_other_window,
			      message(Buffer, open, @on),
			      @default, @on)
		  , menu_item(find_file,
			      message(BM, find_file),
			      @default, @on)
		  , menu_item(identify,
			      message(BM, identify_buffer, Buffer),
			      @default, @on)
		  , menu_item(save_some_buffers,
			      message(BM, save_some_buffers),
			      @default, @on)
		  , menu_item(kill_buffer,
			      message(Buffer, kill))
		  ]),
	send(@pce, exit_message, message(BM, save_some_buffers)),
	send(BM, create),
	ignore(send(BM, server_start)).


buffer(_BM, Name:name, B:emacs_buffer) :<-
	"Find named buffer"::
	get(@emacs_buffers, member, Name, DI),
	get(DI, object, B).


buffers(_BM, Buffers:chain) :<-
	"Chain with all emacs-buffers"::
	get(@emacs_buffers?members, map, @arg1?object, Buffers).


:- dynamic
	help_file/1.

:- absolute_file_name('emacs.hlp', File),
   asserta(help_file(File)).

help(_BM) :->
	"Display general help"::
	help_file(HelpFile),
	new(B, emacs_buffer(HelpFile, '*help*')),
	send(B, open, @on).		% new window!


selection(BM, B:emacs_buffer*) :->
	"Select emacs buffer"::
	get(BM, member, browser, Browser),
	(   B == @nil
	->  send(Browser, selection, @nil)
	;   get(B, name, Name),
	    get(Browser, member, Name, DictItem),
	    send(Browser, insert_after, DictItem, @nil), % move to top
	    send(Browser, selection, DictItem)
	).


find_file(_BM, Dir:[directory]) :->
	"Find and edit file"::
	get(@finder, file, @off, @default, Dir, FileName),
	new(B, emacs_buffer(FileName)),
	send(B, open).


goto_source_location(_BM, Location:source_location) :->
	"Visit the indicated source-location"::
	get(Location, file_name, File),
	new(B, emacs_buffer(File)),
	send(B, open),
	(   get(Location, line_no, Line),
	    Line \== @nil
	->  get(B?editors, head, E),
	    send(E, select_line, Line),
	    send(E, caret, E?selection?x) % dubious!
	;   true
	).


edit(BM, Location:source_location) :->
	"Equivalent to ->goto_source_location"::
	send(BM, goto_source_location, Location).


existing_file(_BM, Dir:[directory], File:file) :<-
	"Find existing file in directory"::
	get(@finder, file, @on, @default, Dir, FileName),
	new(File, file(FileName)).


identify_buffer(_BM, Buffer:emacs_buffer) :->
	"Show description of buffer"::
	send(Buffer, identify).


save_some_buffers(BM, Confirm:[bool]) :->
	"Save all modified buffers"::
	new(ModifiedItem,
	    and(@arg1?object?file \== @nil,
		@arg1?object?modified == @on)),
	(   get(BM?buffer_list, find, ModifiedItem, _)
	->  send(BM?buffer_list, for_some,
		 and(ModifiedItem,
		     or(Confirm == @off,
			message(BM?display, confirm, 'Save %s?',
				@arg1?object?file?name)),
		     message(@arg1?object, save, @arg1?object?file)))
	;   send(@pce, report, status, 'No buffers need saving')
	).

	
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
				 and(or(@arg1?frame?pool == Pool,
					@arg1?frame?pool == @default),
				     @arg1?frame?sticky_window == @off)),
			   global)),
		_),
	    get(Ed, frame, Frame),
	    send(Frame, expose)
	).


server_start(BM, Force:[bool]) :->
	"Start server-mode (xpce-client interface)"::
	(   (	\+ get(class(socket), send_method, listen, _)
	    ;	get(@emacs_server, status, listen)
	    )
	->  true
	;   (	send(@emacs_server_address, exists, @off)
	    ->  (   Force \== @on,
		    pce_catch_error(socket, send(@emacs_server, connect))
		->  free(@emacs_server),
		    send(BM, report, status, 'Server on other PceEmacs'),
		    fail
		;   send(BM, report, status, 'Restarted server'),
		    send(@emacs_server_address, remove)
		)
	    ;	true
	    ),
	    ignore(send(@emacs_server, listen))
	).
	

		 /*******************************
		 *	 USER EXTENSIONS	*
		 *******************************/

load_user_extension(_BM, Base:name) :->
	"Load Prolog user file with this base-name"::
	new(Name, string('~/lib/xpce/emacs/%s', Base)),
	send(Name, ensure_suffix, '.pl'),
	new(F, file(Name)),
	(   send(file(Name), exists)
	->  get(F, absolute_path, Path),
	    consult(Path)
	;   true
	).


:- pce_end_class.
