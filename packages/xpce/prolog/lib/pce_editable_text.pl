/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
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
	;   get(T, pen, OldPen),
	    get(T, border, OldBorder),
	    send(T, attribute, edit_saved_parms, chain(OldPen, OldBorder)),
	    send(T, pen, 1),
	    send(T, border, 2)
	),
	send(T, show_caret, @on).


release_focus(T) :->
	"Called when focus is lost: remove the caret"::
	(   get(T, attribute, edit_saved_parms, chain(OldPen, OldBorder))
	->  send(T, pen, OldPen),
	    send(T, border, OldBorder),
	    send(T, delete_attribute, edit_saved_parms)
	;   true
	),
	send(T, show_caret, @off).


enter(T) :->
	"Stop typing"::
	send(T?window, keyboard_focus, @nil),
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

