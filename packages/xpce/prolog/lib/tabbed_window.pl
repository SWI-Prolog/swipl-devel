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
    
layout_dialog(W, _Gap:[size], _Size:[size], _Border:[size]) :->
	"Overrule to deal with nested tabbed windows"::
	new(S0, size(0,0)),
	send_super(W, layout_dialog, S0, S0, S0).

:- pce_group(stack).

on_top(W, Name:name) :->
	"Put the named tab on top"::
	get_super(W, member, tab_stack, TS),
	(   get(TS, member, Name, Tab)
	->  send(TS, on_top, Tab)
	;   get(W, hypered, tab, @arg3?name == Name, Window)
	->  send(Window, expose)
	).

:- pce_group(members).

%	->append: Window, Label, [Expose]
%	
%	Append a new tab using Window with the given tab label.
%
%	The call to ->'_compute_desired_size' should be properly delayed
%	until the tabbed window is actually   created,  but this doesn't
%	appear to work properly. If Expose == @on the tab is immediately
%	brought to the top.

append(W, Window:window=window, Label:name=[name], Expose:expose=[bool]) :->
	"Append a window to the tabs"::
	send(Window, '_compute_desired_size'),
	send(W, tab, new(Tab, window_tab(Window, Label))),
	(   Expose == @on
	->  get_super(W, member, tab_stack, TS),
	    send(TS, on_top, Tab)
	;   true
	).

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
	     message(Windows, append, @arg1?window)),
	(   get(W, all_hypers, Hypers)
	->  send(Hypers, for_all,
		 if(@arg1?forward_name == toplevel,
		    message(Windows, append, @arg1?to)))
	;   true
	).
	
clear(W) :->
	"Remove all member tabs"::
	get_super(W, member, tab_stack, TS),
	send(TS, clear).

tab(W, Tab:tab) :->
	"Add normal tab"::
	get_super(W, member, tab_stack, TS),
	send(TS, append, Tab),
	(   get(W, is_displayed, @on)
	->  send(W, resize, Tab)
	;   true
	).

tab(W, Name:name, Tab:tab) :<-
	"Find named tab"::
	get_super(W, member, tab_stack, TS),
	get(TS, member, Name, Tab).

:- pce_end_class(tabbed_window).


		 /*******************************
		 *	     WINDOW TAB		*
		 *******************************/


:- pce_begin_class(window_tab(name), tab,
		   "Tab displaying a window").

variable(window,	window*,      get, "Displayed window").
variable(closing,	bool := @off, get, "We are about to close").
delegate_to(window).

initialise(T, Window:window=[window], Name:name=[name]) :->
	"Create from window and name"::
	(   Window == @default
	->  new(W, picture)
	;   W = Window
	),
	(   Name == @default
	->  get(W, name, TheName)
	;   TheName = Name
	),
	(   get(W, decoration, Decor),
	    Decor \== @nil
	->  true
	;   Decor = Window
	),
	send(Decor, lock_object, @on),
	(   get(Decor, slot, frame, Frame),
	    Frame \== @nil
	->  send(Frame, delete, Decor)
	;   true
	),
	send(Decor, slot, tile, @nil),
	send_super(T, initialise, TheName),
	send(T, border, size(0,0)),
	send_super(T, display, Decor),
	get(Decor, unlock, _),
	send(T, slot, window, W),
	new(_, mutual_dependency_hyper(T, W, window, tab)).


:- pce_group(resize).

%	->size
%	
%	This method must update the size of  the window. For some, to me
%	unknown,  reason  this  does  not    work  correctly  when  done
%	immediately.  Possibly  this  has  something   to  do  with  X11
%	synchronisation. We use the hack   in_pce_thread/1 to reschedule
%	the window resize in the event loop.

size(T, Size:size) :->
	"Adjust size of tab and window"::
	(   get(T, closing, @on)
	->  true
	;   in_pce_thread(send(T, resize_window)),
	    send_super(T, size, Size)
	).

resize_window(T) :->
	get(T, size, size(W, H)),
	get(T, window, Window),
	(   get(Window, decoration, Decor),
	    Decor \== @nil
	->  Resize = Decor
	;   Resize = Window
	),
	send(Resize, do_set, 0,0,W,H).

:- pce_group(event).

status(T, Status:{on_top,hidden}) :->
	send_super(T, status, Status),
	(   Status == on_top,
	    get(T, is_displayed, @on),
	    get(T, container, tabbed_window, TabbedWindow)
	->  send(TabbedWindow, resize, T)
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

rank(Tab, Rank:'1..') :<-
	"Get position number of the tab"::
	get(Tab, device, Stack),
	get(Stack?graphicals, index, Tab, Rank).

rank(Tab, Rank:'1..') :->
	"Move tab in rank"::
	get(Tab, device, Stack),
	get(Stack?graphicals, index, Tab, Rank0),
	(   Rank == Rank0
	->  true
	;   (   Rank > Rank0
	    ->  Rank1 is Rank+1
	    ;   Rank1 = Rank
	    ),
	    (   Rank1 == 1
	    ->  send(Tab, hide)
	    ;   Before is Rank1 - 1,
		get(Stack?graphicals, nth1, Before, BeforeGr)
	    ->  send(Tab, expose, BeforeGr)
	    ;   send(Tab, expose)		% make last one
	    ),
	    send(Stack, layout_labels)
	).

untab(Tab, W:window) :<-
	"Remove a tab from the tabbed window and return the window"::
	get(Tab, window, W),
	send(W, lock_object, @on),
	send(Tab, delete_hypers, window),
	free(Tab),
	get(W, unlock, _).
	
untab(Tab) :->
	"Turn the window into a toplevel window"::
	get(Tab, rank, Rank),
	get(Tab, name, Name),
	get(Tab, container, dialog, TabbedWindow),
	get(Tab, display_position, point(X, Y)),
	get(Tab, untab, Window),
	send(new(window_tab_frame(Window, Name, Rank)), open, point(X, Y+20)),
	new(_, partof_hyper(TabbedWindow, Window, toplevel, tab)).

%	->close_other_tabs
%	
%	Close all tabs but be. To work   around scheduled resize for the
%	subwindows we first indicate we are about to close the tabs. See
%	also ->size.

close_other_tabs(Tab) :->
	"Destroy all tabs except for me"::
	get(Tab, device, Stack),
	send(Stack?graphicals, for_all,
	     if(@arg1 \== Tab,
		message(@arg1, slot, closing, @on))),
	send(Stack?graphicals, for_all,
	     if(@arg1 \== Tab,
		message(@arg1, destroy))).

:- pce_end_class(window_tab).


:- pce_begin_class(window_tab_frame, frame,
		   "Temporary frame for an untabbed window").

variable(rank, '1..', get, "Saved position in tabbed window").

initialise(F, Window:window, Name:name, Rank:'1..') :->
	send(F, slot, rank, Rank),
	send_super(F, initialise, Name?label_name),
	send(F, append, Window),
	send(F, done_message, message(F, retab)).


window(F, Window:window) :<-
	"Get the un-tabbed window"::
	get(F?members, head, Window).

retab(F) :->
	"Bring the window back to its tab"::
	get(F, window, Window),
	get(Window, hypered, tab, TabbedWindow),
	get(F, rank, Rank),
	send(F, delete, Window),
	send(Window, delete_hypers, tab),
	send(TabbedWindow, append, Window),
	get(Window, container, tab, Tab),
	send(Tab, rank, Rank),
	send(F, destroy).

contained_in(F, TabbedWindow:tabbed_window) :<-
	"An untabbed window is consider part of the tab"::
	get(F, window, Window),
	get(Window, hypered, tab, TabbedWindow).

:- pce_end_class(window_tab_frame).
