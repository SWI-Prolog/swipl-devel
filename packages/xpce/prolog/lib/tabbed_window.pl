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

:- module(tabbed_window, []).
:- use_module(library(pce)).
:- use_module(library(hyper)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This class creates a tabbed window:  a   window  displaying  a number of
tabs, each displaying a window.   Here is some simple code using it:

test :-
	new(TW, tabbed_window('Nice tabs')),
	send(TW, append, new(P, picture)),
	send(P, display, box(200, 200), point(50,50)),
	send(TW, append, new(view)),
	send(TW, append, new(D, dialog)),
	send(D, append, text_item(name)),
	send(D, append, button(quit, message(TW, destroy))),
	send(TW, open).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


:- pce_begin_class(tabbed_window, dialog,
		   "Resizeable window holding set of tabs").

variable(label_popup,	popup*, both, "Popup shown on labels").

initialise(W, Label:label=[name], Size:size=[size],
	   Display:display=[display]) :->
	send_super(W, initialise, Label, Size, Display),
	send(W, hor_stretch, 100),
	send(W, ver_stretch, 100),
	send(W, pen, 0),
	send(W, border, size(0,0)),
	send_super(W, append, new(tab_stack)).

resize(W, Tab:[tab]) :->
	"Resize member tabs to fit the dialog"::
	get_super(W, member, tab_stack, TS),
	get(W, area, area(_,_,Width, Height)),
	new(LabelH, number(0)),
	send(TS?graphicals, for_all,
	     message(LabelH, maximum, @arg1?label_size?height)),
	get(LabelH, value, LH),
	TabH is Height - LH,
	(   Tab == @default
	->  send(TS?graphicals, for_all,
		 message(@arg1, size, size(Width,TabH)))
	;   send(Tab, size, size(Width,TabH))
	).
    
:- pce_group(stack).

on_top(W, Name:name) :->
	"Put the named tab on top"::
	get_super(W, member, tab_stack, TS),
	send(TS, on_top, Name).

:- pce_group(members).

append(W, Window:window=window, Label:name=[name]) :->
	"Append a window to the tabs"::
	send(W, tab, window_tab(Window, Label)).

member(W, Name:name, Window:window) :<-
	"Get named window from tabbed window"::
	get_super(W, member, tab_stack, TS),
	get(TS, member, Name, Tab),
	get(Tab, window, Window).

members(W, Windows:chain) :<-
	"New chain with member windows"::
	new(Windows, chain),
	get_super(W, member, tab_stack, TS),
	send(TS?graphicals, for_all,
	     message(Windows, append, @arg1?window)).
	
clear(W) :->
	"Remove all member tabs"::
	get_super(W, member, tab_stack, TS),
	send(TS, clear).

tab(W, Tab:tab) :->
	"Add normal tab"::
	get_super(W, member, tab_stack, TS),
	send(TS, append, Tab),
	send(W, resize, Tab).

tab(W, Name:name, Tab:tab) :<-
	"Find named tab"::
	get_super(W, member, tab_stack, TS),
	get(TS, member, Name, Tab).

:- pce_end_class(tabbed_window).


		 /*******************************
		 *	     WINDOW TAB		*
		 *******************************/


:- pce_begin_class(window_tab, tab,
		   "Tab displaying a window").

variable(window,	window*, get, "Displayed window").
delegate_to(window).

initialise(T, Window:window=[window], Name:name=[name]) :->
	"Create from window and name"::
	(   Window == @default
	->  new(P, picture)
	;   P = Window
	),
	(   Name == @default
	->  get(P, name, TheName)
	;   TheName = Name
	),
	(   get(Window, slot, frame, Frame),
	    Frame \== @nil
	->  send(Window, lock_object, @on),
	    send(Frame, delete, Window),
	    get(Window, unlock, _)
	;   true
	),
	send_super(T, initialise, TheName),
	send(T, border, size(0,0)),
	send_super(T, display, P),
	send(T, slot, window, P),
	new(_, mutual_dependency_hyper(T, P, window, tab)).


:- pce_group(resize).

size(T, Size:size) :->
	"Adjust size of tab and window"::
	get(T, window, Window),
	get(Size, width, W),
	get(Size, height, H),
	(   get(Window, decoration, Decor),
	    Decor \== @nil
	->  send(Decor, do_set, 0,0,W,H)
	;   send(Window, do_set, 0,0,W,H)
	),
	send_super(T, size, Size).

:- pce_group(event).

status(T, Status:{on_top,hidden}) :->
	send_super(T, status, Status),
	(   Status == on_top,
	    get(T, frame, Frame)
	->  get(T, window, Window),
	    send(Window, resize),
	    send(Frame, keyboard_focus, Window)
	;   true
	).

:- pce_group(delegate).

display(T, Gr:graphical, Pos:[point]) :->
	"Delegate to window"::
	get(T, window, Window),
	send(Window, display, Gr, Pos).

append(T, Item:graphical, RelPos:[{below,right,next_row}]) :->
	"Delegate to window"::
	get(T, window, Window),
	send(Window, append, Item, RelPos).

:- pce_group(event).

label_popup(Tab, Popup:popup) :<-
	"Get popup for label"::
	get_super(Tab, window, TabbedWindow),
	get(TabbedWindow, label_popup, Popup),
	Popup \== @nil.

:- pce_global(@window_tab_label_recogniser,
	      new(popup_gesture(@receiver?label_popup))).

label_event(G, Ev:event) :->
	"Show popup on label of tab"::
	(   send_super(G, label_event, Ev)
	->  true
	;   send(@window_tab_label_recogniser, event, Ev)
	).

:- pce_group(frame).

untab(Tab, W:window) :<-
	"Remove a tab from the tabbed window and return the window"::
	get(Tab, window, W),
	send(W, lock_object, @on),
	send(Tab, delete_hypers, window),
	free(Tab),
	get(W, unlock, _).
	
untab(Tab) :->
	"Turn the window into a toplevel window"::
	get(Tab, name, Name),
	get(Tab, untab, Window),
	send(Window?frame, label, Name?label_name),
	send(Window, open).

:- pce_end_class(window_tab).
