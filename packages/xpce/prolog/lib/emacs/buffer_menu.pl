/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

:- module(emacs_buffer_menu, []).
:- use_module(library(pce)).
:- require([ send_list/3
	   ]).


:- pce_begin_class(emacs_buffer_menu, frame,
		   "List showing all PceEmacs buffers").

class_variable(geometry,	geometry,	'211x190+0+125').

initialise(BM, Emacs:emacs) :->
	"Create menu for buffer-list"::
	send(BM, send_super, initialise,
	     'PCE Emacs Buffers', application := Emacs),
	send(BM, name, buffer_menu),
	send(BM, append, new(B, browser)),
	send(B, send_method,
	     send_method(drop_files, vector(chain, point),
			 message(@arg1, for_all,
				 message(Emacs, open_file, @arg1)))),
			 
	send(B, tab_stops, vector(150)),
	send(new(D, dialog), below, B),
	send(D, gap, size(10, 0)),
	send(D, pen, 0),
	send(D, append, label(reporter)),

	send(B, dict, Emacs?buffer_list),
	send(B, open_message, message(@arg1?object, open)),
 	send(B, popup, new(P, popup)),

	new(Buffer, @arg1?object),
	send_list(P, append,
		  [ menu_item(help,
			      message(Emacs, help),
			      end_group := @on)
		  , menu_item(open_buffer,
			      message(Buffer, open))
		  , menu_item(open_other_window,
			      message(Buffer, open, @on),
			      @default, @on)
		  , menu_item(find_file,
			      message(Emacs, find_file),
			      @default, @on)
		  , menu_item(identify,
			      message(BM, identify_buffer, Buffer),
			      @default, @on)
		  , menu_item(save_some_buffers,
			      message(Emacs, save_some_buffers),
			      @default, @on)
		  , menu_item(kill_buffer,
			      message(Buffer, kill))
		  ]).


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


identify_buffer(_BM, Buffer:emacs_buffer) :->
	"Show description of buffer"::
	send(Buffer, identify).

:- pce_end_class.
