/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

:- module(emacs_buffer_menu, []).
:- use_module(library(pce)).
:- require([ send_list/3
	   ]).

:- pce_autoload(tool_bar, library(toolbar)).

resource(open,	  image, image('16x16/open.xpm')).
resource(saveall, image, image('16x16/saveall.xpm')).
resource(help,    image, image('16x16/help.xpm')).
resource(buffers, image, image('32x32/buffers.xpm')).

:- pce_begin_class(emacs_buffer_menu, frame,
		   "List showing all PceEmacs buffers").

class_variable(geometry,	geometry,	'211x190+0+125').

initialise(BM, Emacs:emacs) :->
	"Create menu for buffer-list"::
	send(BM, send_super, initialise,
	     'PCE Emacs Buffers', application := Emacs),
	send(BM, icon, resource(buffers)),
	send(BM, name, buffer_menu),
	send(BM, append, new(D, dialog)),
	send(D, pen, 0),
	send(D, gap, size(0, 3)),
	send(D, append, new(TB, tool_bar(Emacs))),
	send_list(TB, append,
		  [ tool_button(find_file,
				resource(open),
				'Open file for editing'),
		    tool_button(save_some_buffers,
				resource(saveall),
				'Save all modified buffers'),
		    tool_button(help,
				resource(help),
				'Help on PceEmacs')
		  ]),

	send(new(B, emacs_buffer_browser(Emacs)), below, D),
	send(new(report_dialog), below, B).

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

:- pce_end_class(emacs_buffer_menu).

:- pce_begin_class(emacs_buffer_browser, browser,
		   "Browse the emacs buffers").

initialise(B, Emacs:emacs) :->
	"Create for Emacs"::
	send_super(B, initialise, 'Emacs buffers'),
	send(B, name, browser),
	send(B, open_message, message(@arg1?object, open)),
	send(B, tab_stops, vector(150)),
	send(B, attach_popup),
	send(B, dict, Emacs?buffer_list).

attach_popup(B) :->
	"Attach the popup menu"::
	send(B, popup, new(P, popup)),

	new(Buffer, @arg1?object),
	send(P, update_message,
	     message(B, selection, @arg1)),
	send_list(P, append,
		  [ menu_item(open_buffer,
			      message(Buffer, open)),
		    menu_item(open_new_window,
			      message(Buffer, open, @on),
			      @default, @on),
		    menu_item(identify,
			      message(Buffer, identify),
			      @default, @on),
		    menu_item(kill_buffer,
			      message(Buffer, kill))
		  ]).


drop_files(B, Files:chain, _At:point) :->
	"Drag-and-drop interface"::
	get(B, application, Emacs),
	send(Files, for_all,
	     message(Emacs, open_file, @arg1)).

:- pce_end_class(emacs_buffer_browser).




