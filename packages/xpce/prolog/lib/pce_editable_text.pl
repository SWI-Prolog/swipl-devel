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


:- module(pce_editable_text, []).
:- use_module(library(pce)).


:- pce_begin_class(editable_text, text, "Editable short graphical text").

variable(editable,	bool := @on,	get,	"Text is editable").
variable(message,	'name|code*',	both,	"Action on enter").

:- pce_global(@editable_text_gesture, make_edit_text_recogniser).
:- pce_global(@editable_text_key_binding, make_key_binding).

make_edit_text_recogniser(R) :-
	new(Text, @event?receiver),
	new(Window, Text?window),
	new(Pointed, ?(Text, pointed, @event?position)),
	new(C, click_gesture(left, '', single,
			     and(message(Text, caret, Pointed),
				 message(Window, keyboard_focus, Text)),
			     Text?editable == @on)),
	new(R, handler_group(handler(obtain_keyboard_focus,
				     message(Text, obtain_focus)),
			     handler(release_keyboard_focus,
				     message(Text, release_focus)),
			     C)).

make_key_binding(KB) :-
	new(KB, key_binding(editable_text, text)),
	send(KB, function, 'TAB', advance),
	send(KB, function, '\\e',  enter),
	send(KB, function, 'RET', enter).


editable(T, Val:bool) :->
	"Remove caret when switched to @off"::
	(   Val == @off
	->  send(T?window, keyboard_focus, @nil)
	;   true
	),
	send(T, slot, editable, Val).


cancel(T) :->
	"Stop editing"::
	send(T?window, keyboard_focus, @nil).


obtain_focus(T) :->
	"Called when focus is obtained: show the caret"::
	(   get(T, attribute, edit_saved_parms, _)  % pointer in/out of window
	->  true
	;   send(T, save_parameter, pen),
	    send(T, save_parameter, border),
	    get(T, pen, OldPen),
	    get(T, border, OldBorder),
	    NewPen is OldPen+1,
	    send(T, pen, NewPen),
	    NewBorder is max(2, OldBorder),
	    send(T, border, NewBorder)
	),
	send(T, show_caret, @on).


save_parameter(T, Parm:name) :->
	"Save some property to be restored after edit"::
	get(T, Parm, Value),
	(   get(T, attribute, edit_saved_parms, Sheet)
	->  true
	;   send(T, attribute, edit_saved_parms, new(Sheet, sheet))
	),
	send(Sheet, value, Parm, Value).


release_focus(T) :->
	"Called when focus is lost: remove the caret"::
	(   get(T, attribute, edit_saved_parms, Sheet)
	->  send(Sheet, for_all,
		 message(T, @arg1?name, @arg1?value)),
	    send(T, delete_attribute, edit_saved_parms)
	;   true
	),
	send(T, show_caret, @off).


enter(T) :->
	"Stop typing"::
	send(T?window, keyboard_focus, @nil),
	send(T, forward).

forward(T) :->
	"Typing has completed; forward change"::
	get(T, device, Dev),
	get(T, message, Msg),
	(   Msg == @nil
	->  true
	;   send(Msg, instance_of, name)
	->  send(Dev, Msg, T?string?value)
	;   send(Msg, forward, T?string?value)
	).


advance(T) :->
	"Advance to next editable item"::
	send(T?device, advance, T).


'_wants_keyboard_focus'(T) :->
	"True if text is <-editable"::
	get(T, editable, @on).


typed(T, Id:event_id) :->
	"Handle keyboard input"::
	get(T, show_caret, @on),
	send(@editable_text_key_binding, typed, Id, T).


event(T, Ev:event) :->
	(   send(@editable_text_gesture, event, Ev)
	;   send(T, send_super, event, Ev)
	).

:- pce_end_class.

