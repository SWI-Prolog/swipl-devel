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

:- module(emacs_buffer_menu, []).
:- use_module(library(pce)).
:- use_module(library(persistent_frame)).
:- require([ send_list/3
	   ]).

:- pce_autoload(tool_bar, library(toolbar)).

resource(open,	  image, image('16x16/open.xpm')).
resource(saveall, image, image('16x16/saveall.xpm')).
resource(help,    image, image('16x16/help.xpm')).
resource(buffers, image, image('32x32/buffers.xpm')).

:- pce_begin_class(emacs_buffer_menu, persistent_frame,
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




