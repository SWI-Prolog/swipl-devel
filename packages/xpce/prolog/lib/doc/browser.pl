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

:- module(doc_browser, []).
:- use_module(library(pce)).
:- use_module(library(toolbar)).
:- use_module(library(pce_report)).
:- use_module(library(url)).
:- use_module(doc(window)).

:- pce_image_directory(icons).

resource(backward, image, image('back.xpm')).
resource(forward,  image, image('forward.xpm')).
resource(reload,   image, image('reload.xpm')).
resource(source,   image, image('source.xpm')).


		 /*******************************
		 *	    THE BROWSER		*
		 *******************************/

:- pce_begin_class(doc_browser, frame, "Document browser").

initialise(TB) :->
	send_super(TB, initialise),
	send(TB, append, new(D, dialog)),
	send(D, name, top_dialog),
        send(D, pen, 0),
        send(D, gap, size(0, 5)),
        send(D, append, tool_bar(TB)),
	send(D, append, graphical(0,0,20,1), right),
	send(D, append,
	     new(TI, text_item(url, '', message(TB, url, @arg1))),
	     right),
	send(TI, hor_stretch, 100),
	send(D, resize_message, message(D, layout, @arg2)),
	get(TI, height, H),
	send(TI, reference, point(0, H+5)),
	send(TI, value_set, new(chain)),
	send(TB, fill_toolbar),
	send(new(DW, doc_window), below, D),
	send(new(report_dialog), below, DW).

:- pce_group(parts).

document_window(TB, DW:doc_window) :<-
	"Window representing current document"::
	get(TB, member, doc_window, DW).

tool_bar(TB, Toolbar:tool_bar) :<-
	"Find the toolbar"::
	get(TB, member, top_dialog, D),
	get(D, member, tool_bar, Toolbar).

:- pce_group(menu).

fill_toolbar(F) :->
	get(F, tool_bar, TB),
	send_list(TB, append,
		  [ tool_button(backward,
				resource(backward),
				backward),
		    tool_button(forward,
				resource(forward),
				forward),
		    gap,
		    tool_button(reload,
				resource(reload),
				reload),
		    gap,
		    tool_button(view_source,
				resource(source),
				view_source)
		  ]).

reload(TB) :->
	"Reload current page"::
	get(TB, document_window, DW),
	get(DW, url, URL),
	send(DW, url, @nil),
	send(DW, url, URL).


backward(TB) :->
	"Go back to previous location"::
	get(TB, document_window, DW),
	send(DW, backward).


forward(TB) :->
	"Go forwards in history"::
	get(TB, document_window, DW),
	send(DW, forward).


view_source(TB) :->
	"Go forwards in history"::
	get(TB, document_window, DW),
	get(DW, page_source, File),
	start_emacs,
	send(@emacs, open_file, File).


add_history(TB, URL:name) :->
	"Add URL to history"::
	get(TB, member, top_dialog, D),
	get(D, member, url, TI),
	send(TI, selection, URL),
	get(TI, value_set, Chain),
	send(Chain, delete_all, URL),
	send(Chain, prepend, URL),
	(   get(Chain, size, Size),
	    Size > 20
	->  send(Chain, delete_tail)
	;   true
	).

:- pce_group(navigate).

url(TB, URL:name*) :->
	"Jump to indicated URL"::
	get(TB, document_window, DW),
	parse_url(URL, Parts),
	parse_url(Cannonical, Parts),
	send(DW, url, Cannonical).

:- pce_end_class.
